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

(defvar *simulation-running* nil
  "If T, we continuously step the Game of Life.")

(defvar *current-cells* nil
  "Holds the most recent set of cells from the server.")

(defvar *simulation-thread* nil
  "Background thread for stepping the simulation.")

(defvar *stop-thread* nil
  "Flag to ask the background thread to stop.")

;; 1) CONNECT TO SERVER

(defun connect-to-server (host port)
  (format t "~&[INFO] Attempting to connect to ~a:~a ...~%" host port)
  (finish-output)
  (let* ((socket (socket-connect host port))
         (stream (socket-stream socket)))
    (format t "~&[INFO] Connected to server ~a:~a~%" host port)
    (finish-output)
    (values socket stream)))

;; 2) GAME OF LIFE HELPERS
(defun neighbors (x y)
  (loop for dx from -1 to 1
        nconc
        (loop for dy from -1 to 1
              unless (and (= dx 0) (= dy 0))
              collect (list (+ x dx) (+ y dy)))))

(defun next-generation (cells)
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
    (format stream "~a~%" json-str)
    (finish-output stream)))

;; 4) BACKGROUND SIMULATION LOOP
(defun simulation-loop (stream)
  (loop
    while (not *stop-thread*) do
      (when (and *simulation-running* *current-cells*)
        (let ((next-gen (next-generation *current-cells*)))
          (setf *current-cells* next-gen)
          (send-update-command stream next-gen)
          (sleep 0.05)))
      (sleep 0.1)))

(defun start-simulation-thread (stream)
  (unless *simulation-thread*
    (setf *stop-thread* nil)
    (setf *simulation-thread*
          (sb-thread:make-thread
           (lambda ()
             (simulation-loop stream))
           :name "simulation-loop-thread"))))

(defun stop-simulation-thread ()
  (when *simulation-thread*
    (setf *stop-thread* t)
    (sb-thread:join-thread *simulation-thread*)
    (setf *simulation-thread* nil)))

;; 5) PROCESS SERVER MESSAGES

(defun process-json (json-data stream)
  (when (and (listp json-data)
             (not (null json-data)))
    (let ((cmd-assoc (assoc :CMD json-data)))
      (when cmd-assoc
        (let ((cmd (cdr cmd-assoc)))
          (cond
            ((string= cmd "START")
             (let ((cells-assoc (assoc :CELLS json-data)))
               (when cells-assoc
                 (setf *current-cells* (cdr cells-assoc))))
             (setf *simulation-running* t)
             (format t "~&[INFO] Simulation START~%")
             (finish-output)
             (start-simulation-thread stream))

            ((string= cmd "STOP")
             (setf *simulation-running* nil)
             (format t "~&[INFO] Simulation STOP~%")
             (finish-output)
             (stop-simulation-thread))
            (t
             (format t "~&[WARN] Unrecognized CMD => ~A~%" cmd)
             (finish-output))))))
    (let ((cells-assoc (assoc :CELLS json-data)))
      (when (and cells-assoc (null (assoc :CMD json-data)))
        (setf *current-cells* (cdr cells-assoc))))))

(defun read-server-responses (stream)
  (loop
    for line = (read-line stream nil :eof)
    while (and line (not (eq line :eof))) do
      (handler-case
          (let ((json-data (decode-json-from-string line)))
            (process-json json-data stream))
        (error (e)
          (format t "~&[ERROR] Could not parse JSON => ~a~%" e)
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
            (start-simulation-thread stream)
            (read-server-responses stream)))
      (error (e)
        (format t "~&[ERROR] Could not connect or read from server: ~a~%" e)
        (finish-output)))
    (when *socket*
      (format t "~&[INFO] Closing socket...~%")
      (finish-output)
      (socket-close *socket*)
      (stop-simulation-thread)
      (format t "~&[INFO] Socket closed.~%")
      (finish-output))))

(main)
