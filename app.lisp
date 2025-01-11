(load "C:/Users/vilim/quicklisp/setup.lisp")

(ql:quickload '(:usocket))

(defpackage :simple-client
  (:use :cl)
  (:import-from :usocket
    :socket-connect
    :socket-stream
    :socket-close))

(in-package :simple-client)

(defun connect-and-receive-message (host port)
  (format t "~&Connecting to server at ~a:~a...~%" host port)
  (let* ((socket (socket-connect host port))
         (stream (socket-stream socket)))
    (unwind-protect
         (progn
           (let ((message (read-line stream nil :eof)))
             (when message
               (format t "~&Received message: ~a~%" message))))
      (socket-close socket))))

(defun main ()
  (connect-and-receive-message "127.0.0.1" 50007))

(main)
