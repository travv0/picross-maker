(asdf:defsystem "picross-maker"
  :description "a website for creating and sharing picross puzzles"
  :version "0.0.1"
  :author "Travis Sunderland"
  :depends-on ("hunchentoot"
               "spinneret")
  :components ((:file "package")
               (:file "src/picross" :depends-on ("package"))))
