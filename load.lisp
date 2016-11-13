(in-package :cl-user)

(ql:quickload "spinneret")
(ql:quickload "hunchentoot")

(setf *print-pretty* nil)

(require "asdf")
(asdf:load-system :picross-maker)
