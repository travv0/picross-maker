(in-package :cl-user)

(ql:quickload "spinneret")
(ql:quickload "hunchentoot")
(ql:quickload "cl-utilities")

(setf *print-pretty* nil)

(require "asdf")
(asdf:load-system :picross-maker)
