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

;; GLOBALS

(defvar *socket* nil
  "Socket object connected to the server.")

(defvar *stream* nil
  "Stream associated with the socket.")


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
  
  ;; Provjeri da li JSON sadrži ključ "cells"
  (when (and (hash-table-p json-data)
             (gethash "cells" json-data))
    (let ((cells (gethash "cells" json-data)))
      (format t "~&[INFO] Received Cells: ~a~%" cells)
      (finish-output))))


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

              (process-json json-data stream)

              (format t "~&[DEBUG] After process-json call~%")
              (finish-output)))
        (error (e)
          (format t "~&[ERROR] Could not parse/process JSON. Error: ~a~%" e)
          (finish-output)))))

;; 4) MAIN

(defun main ()
  (let ((host "127.0.0.1")
        (port 50007)
        (socket nil)
        (stream nil))
    (handler-case
        (progn
          (multiple-value-setq (socket stream) (connect-to-server host port))
          (read-server-responses stream))
      (error (e)
        (format t "~&[ERROR] Could not connect or read from server: ~a~%" e)
        (finish-output)))
    (when socket
      (format t "~&[DEBUG] Closing socket...~%")
      (finish-output)
      (socket-close socket)
      (format t "~&[INFO] Socket closed.~%")
      (finish-output))))

(main)
