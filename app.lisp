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

;; 2) PROCESS SERVER MESSAGES

(defun process-json (json-data)
  (format t "~&[DEBUG] Received JSON Data: ~S~%" json-data)
  (finish-output)
  
  (when (and (hash-table-p json-data)
             (gethash "cells" json-data))
    (let ((cells (gethash "cells" json-data)))
      (clrhash *local-cells*) 
      (dolist (cell cells)
        (setf (gethash cell *local-cells*) t))
      (format t "~&[INFO] Updated local cells: ~a~%" (hash-table-keys *local-cells*))
      (finish-output)

      )))

;; 3) READ SERVER RESPONSES

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

              (process-json json-data)

              (format t "~&[DEBUG] After process-json call~%")
              (finish-output)))
        (error (e)
          (format t "~&[ERROR] Could not parse/process JSON. Error: ~a~%" e)
          (finish-output)))))

;; 4) MAIN
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
