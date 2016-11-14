(in-package :picross-maker)

(defparameter *site-name* "Picross Maker")
(defparameter *board-width* 10)
(defparameter *board-height* 10)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *head*
    `((:meta :charset "UTF-8")
      (:meta :name "viewport"
             :content "width=device-width, initial-scale=1, maximum-scale=1")

      (:link :rel "stylesheet" :href "/style.css")
      (:script :src "https://code.jquery.com/jquery-3.1.1.min.js"
               :integrity "sha256-hVVnYaiADRTO2PzUGmuLJr8BLUSjGIZsDYGmIJLv2b8 "
               :crossorigin "anonymous")
      (:script :src "/picross.js"))))

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
       (unless (equal ,title "")
         (:h2 ,title))
       ,@body))))

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
    (:body
     (:form :id "picrossForm"
            :action "submit-picross"
            :method "post"
            (:div :id "picrossDiv")
            (:input :type "hidden"
                    :id "picrossList"
                    :name "picrossList")
            (:input :type "hidden"
                    :id "boardWidth"
                    :name "boardWidth")
            (:input :type "hidden"
                    :id "boardHeight"
                    :name "boardHeight")
            (:input :type "submit")))))

(publish-page submit-picross
  (let* ((*board-width* (parse-integer (post-parameter "boardWidth")))
         (*board-height* (parse-integer (post-parameter "boardHeight")))
         (picross-grid (picross-list-to-grid (parse-picross-string (post-parameter "picrossList")))))
    (standard-page
        (:title "")
      (:body (:h1 (format nil
                          "~a: ~a"
                          (post-parameter "picrossList")
                          (parse-picross-string (post-parameter "picrossList"))))))))

(defun parse-picross-string (picross-string)
  (let ((result-list '()))
    (dolist (cell-name (split-sequence #\, picross-string))
      (setf result-list (cons (coordinates-to-list cell-name) result-list)))
    (reverse result-list)))

(defun coordinates-to-list (coordinates)
  (split-sequence #\y (remove #\x coordinates)))

(defun picross-list-to-grid (picross-list)
  (let ((grid (make-hash-table)))
    (dolist (coordinates picross-list)
      (setf (gethash (picross-key (parse-integer (first coordinates))
                                  (parse-integer (second coordinates)))
                     grid)
            t))
    grid))

(defun picross-key (x y)
  (+ (* *board-width* y) x))
