(asdf:defsystem "picross-maker"
  :description "a website for creating and sharing picross puzzles"
  :version "0.0.1"
  :author "Travis Sunderland"
  :depends-on ("cl-utilities"
               "hunchentoot"
               "spinneret"
               "sha3"
               "binascii"
               "uuid"
               "web-util")
  :components ((:file "package")
               (:file "src/picross" :depends-on ("package"))))
