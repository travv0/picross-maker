(in-package :picross-maker)

(defparameter *site-name* "Picross Maker")
(defparameter *favicon-path* "/static/favicon.png")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *head*
    `((:meta :charset "UTF-8")
      (:meta :name "viewport"
             :content "width=device-width, initial-scale=1, maximum-scale=1")

      (:link :rel "shortcut icon"
             :type "image/png"
             :href *favicon-path*)

      (:link :rel "stylesheet"
             :href "static/style.css")

      (:script :src "//code.jquery.com/jquery-1.11.0.min.js")
      (:script :src "/static/picross.js"))))

;;; The basic format that every viewable page will follow.
(defmacro standard-page ((&key title) &body body)
  `(with-html-string
     (:doctype)
     (:html
      (:head (:title (concatenate 'string
                                  ,title
                                  (if (equal ,title "")
                                      ""
                                      " - ")
                                  *site-name*))
             ,@*head*)
      (:body
       (:div :style "overflow-x: hidden;"
             (:div :class "container"
                   (unless (equal ,title "")
                     (:h2 ,title))
                   ,@body))))))

;;; this macro creates and publishes page <name> at https://your-site.com/<name>
(defmacro publish-page (name &body body)
  `(hunchentoot:define-easy-handler (,name
                                     :uri ,(string-downcase
                                            (if (equal 'index name)
                                                "/"
                                                (concatenate 'string "/" (symbol-name name)))))
       ()
     (setf (hunchentoot:content-type*) "text/html")
     ,@body))

(publish-page index
  (standard-page
      (:title "Picross Maker")
    (:body (:table :id "picrossTable"))))
