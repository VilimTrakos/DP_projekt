(load "C:/Users/vilim/quicklisp/setup.lisp")

(ql:quickload '(:usocket :cl-json :babel))

(defpackage :game-of-life-client
  (:use :cl)
  (:import-from :usocket
    :socket-connect
    :socket-close
    :socket-stream)
  (:import-from :cl-json
    :decode-json-from-string
    :encode-json-to-string)
  (:export :main))

(in-package :game-of-life-client)

(defvar *socket* nil
  "Socket object connected to the server.")

(defvar *stream* nil
  "Stream associated with the socket.")

(defvar *local-cells* (make-hash-table :test 'equal)
  "Local representation of the cells.")

;; 1) CONNECT TO SERVER

(defun connect-to-server (host port)
  (format t "~&[DEBUG] Attempting to connect to ~a:~a ...~%" host port)
  (finish-output)
  (let* ((socket (socket-connect host port))
         (stream (socket-stream socket)))
    (format t "~&[INFO] Connected to server ~a:~a~%" host port)
    (finish-output)
    (values socket stream)))

;; 2) GAME OF LIFE HELPERS
(defun neighbors (x y)
  "Return the 8 neighbor coordinates around (x,y)."
  (loop for dx from -1 to 1
        nconc
        (loop for dy from -1 to 1
              unless (and (= dx 0) (= dy 0))
              collect (list (+ x dx) (+ y dy)))))

(defun next-generation (cells)
  "Compute the next generation of Conway's Game of Life for given live CELLS."
  (format t "~&[DEBUG] next-generation called with ~D cells: ~a~%"
          (length cells) cells)
  (finish-output)
  (let ((alive (make-hash-table :test 'equal)))
    (dolist (c cells)
      (setf (gethash c alive) t))
    (let ((candidates (make-hash-table :test 'equal))
          (new-cells '()))
      (dolist (c cells)
        (setf (gethash c candidates) t)
        (destructuring-bind (cx cy) c
          (dolist (nbr (neighbors cx cy))
            (setf (gethash nbr candidates) t))))
      (maphash
       (lambda (cand _val)
         (destructuring-bind (x y) cand
           (let ((alive-neighbors
                  (count t (mapcar (lambda (n)
                                     (gethash n alive nil))
                                   (neighbors x y))))
                 (currently-alive (gethash cand alive nil)))
             (cond
               ((and currently-alive (or (= alive-neighbors 2) (= alive-neighbors 3)))
                (push cand new-cells))
               ((and (not currently-alive) (= alive-neighbors 3))
                (push cand new-cells))))))
       candidates)
      (nreverse new-cells))))

;; 3) SENDING UPDATES
(defun send-update-command (stream cells)

  (let* ((msg `(("cmd" . "UPDATE")
                ("cells" . ,cells)))
         (json-str (encode-json-to-string msg)))
    (format t "~&[DEBUG] Sending UPDATE => ~D cells => ~a~%"
            (length cells) cells)
    (finish-output)

    (format t "~&[DEBUG] JSON => ~a~%" json-str)
    (finish-output)

    (format stream "~a~%" json-str)
    (finish-output stream)))

;; 4) PROCESS SERVER MESSAGES

(defun process-json (json-data stream)
  (when (and (listp json-data)
             (not (null json-data)))
    (let ((first-elt (first json-data)))
      (when (and (consp first-elt)
                 (eq (car first-elt) :CELLS))
        (let ((cells (cdr first-elt)))
          (let ((next-gen (next-generation cells)))

            (sleep 0.05)
            (send-update-command stream next-gen)))))))

;; 5) READ SERVER RESPONSES

(defun read-server-responses (stream)
  (loop
    for line = (read-line stream nil :eof)
    while (and line (not (eq line :eof))) do
      (handler-case
          (progn
            (format t "~&[DEBUG] Got line from server: ~a~%" line)
            (finish-output)

            (let ((json-data (decode-json-from-string line)))
              (format t "~&[DEBUG] Decoded JSON from server: ~a~%" json-data)
              (finish-output)

              (format t "~&[DEBUG] Before calling process-json...~%")
              (finish-output)

              (process-json json-data stream)

              (format t "~&[DEBUG] After process-json call~%")
              (finish-output)))
        (error (e)
          (format t "~&[ERROR] Could not parse/process JSON. Error: ~a~%" e)
          (finish-output)))))

;; 6) MAIN
(defun main ()
  (let ((host "127.0.0.1")
        (port 50007))
    (handler-case
        (progn
          (multiple-value-bind (socket stream)
              (connect-to-server host port)
            (setf *socket* socket
                  *stream* stream)
            (read-server-responses stream)))
      (error (e)
        (format t "~&[ERROR] Could not connect or read from server: ~a~%" e)
        (finish-output)))
    (when *socket*
      (format t "~&[DEBUG] Closing socket...~%")
      (finish-output)
      (socket-close *socket*)
      (format t "~&[INFO] Socket closed.~%")
      (finish-output))))

(main)
