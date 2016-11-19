(in-package :cl-user)

(ql:quickload "spinneret")
(ql:quickload "hunchentoot")
(ql:quickload "cl-utilities")
(ql:quickload "sha3")
(ql:quickload "binascii")
(ql:quickload "uuid")

(setf *print-pretty* nil)

(require "asdf")
(asdf:load-system :web-util)
(asdf:load-system :picross-maker)
