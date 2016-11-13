(in-package :cl-user)

(defun r ()
  (load "load.lisp"))

(r)

(setf hunchentoot:*acceptor* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 5555)))
(setf (hunchentoot:acceptor-document-root hunchentoot:*acceptor*) "./static/")
