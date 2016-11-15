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
     (let (*conn*)
       (with-db *conn*
         ,@body))))

(defparameter +max-size+ 20)
(defparameter +size-step+ 5)

(publish-page index
  (standard-page
      (:title "Picross Maker")
    (:body
     (:form :id "picrossForm"
            :action "submit-picross"
            :method "post"
            (size-dropdown "boardWidth")
            (size-dropdown "boardHeight")
            (:div :id "picrossDiv")
            (:input :type "hidden"
                    :id "picrossList"
                    :name "picrossList")
            (:input :type "submit")))
    (:script "
$(function() {
    setUpPicross(10, 10);
    $('#picrossForm').submit(function () { submitPicross($('#picrossDiv')); });
});
")))

(defmacro defhtml (name params &body body)
  `(defun ,name ,params
     (with-html
       ,@body)))

(defhtml size-dropdown (name)
  (:select :id name
           :name name
           :onchange "updatePicrossTable()"
           (loop for i from +size-step+ to +max-size+
                 when (= (mod i +size-step+) 0)
                   collect (:option :value i
                                    i))))

(publish-page submit-picross
  (let ((*board-width* (parse-integer (post-parameter "boardWidth")))
        (*board-height* (parse-integer (post-parameter "boardHeight")))
        (picross-list (post-parameter "picrossList")))
    (execute-query-modify "INSERT INTO picross (
                                picross_cells,
                                picross_width,
                                picross_height,
                                picross_date
                           )
                           VALUES (
                                ?,
                                ?,
                                ?,
                                current_timestamp
                           )"
                          (picross-list
                           *board-width*
                           *board-height*)))
  (redirect "/"))

(publish-page picross
  (execute-query-one picross
      "SELECT picross_width,
              picross_height,
              picross_cells
       FROM picross
       WHERE picross_id = ?"
      ((get-parameter "id"))
    (let* ((*board-width* (getf picross :|picross_width|))
           (*board-height* (getf picross :|picross_height|)))
      (standard-page
          (:title "Picross Maker")
        (:body
         (:form :id "picrossForm"
                :method "post"
                (picross-grid (picross-string-to-grid (getf picross :|picross_cells|)))
                (:input :type "hidden"
                        :id "picrossList"
                        :name "picrossList")
                (:input :type "hidden"
                        :id "id"
                        :name "id"
                        :value (get-parameter "id"))
                (:input :type "submit")))))))

(defhtml picross-grid (grid)
  (let ((column-counts (get-column-counts grid))
        (row-counts (get-row-counts grid)))
    (:table :id "picrossTable"
            (:tr :class "columnCounts"
                 (:td)
                 (dotimes (x *board-width*)
                   (:td (loop for count in (gethash x column-counts)
                              collect (:span count (:br))))))
            (dotimes (y *board-height*)
              (:tr :id (format nil "row~d" y)
                   (:td :class "rowCounts"
                        (:div :class "rowCountsDiv"
                              (loop for count in (gethash y row-counts)
                                    collect (:span count ("&nbsp;")))))
                   (dotimes (x *board-width*)
                     (:td :onclick "toggleCell($(this))"
                          :class "picrossCell"
                          :id (format nil "x~dy~d" x y))))))))

(publish-page submit-solution
  (let ((picross-list (get-parameter "cells")))
    (execute-query-one picross "SELECT picross_cells
                                FROM picross
                                WHERE picross_id = ?"
        ((get-parameter "id"))
      (if (equal (getf picross :|picross_cells|)
                 picross-list)
          (with-html-string ("1"))
          (with-html-string ("0"))))))

(defun picross-string-to-grid (picross-string)
  (picross-list-to-grid (parse-picross-string picross-string)))

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

(defun get-column-counts (grid)
  (let ((column-counts (make-hash-table)))
    (dotimes (x *board-width*)
      (let ((count 0)
            (counts '()))
        (dotimes (y *board-height*)
          (if (picross-lookup grid x y)
              (incf count)
              (unless (= count 0)
                (setf counts (cons count counts))
                (setf count 0))))
        (unless (= count 0)
          (setf counts (cons count counts))
          (setf count 0))
        (setf (gethash x column-counts) (if counts
                                            (reverse counts)
                                            '(0)))))
    column-counts))

(defun get-row-counts (grid)
  (let ((row-counts (make-hash-table)))
    (dotimes (y *board-height*)
      (let ((count 0)
            (counts '()))
        (dotimes (x *board-width*)
          (if (picross-lookup grid x y)
              (incf count)
              (unless (= count 0)
                (setf counts (cons count counts))
                (setf count 0))))
        (unless (= count 0)
          (setf counts (cons count counts))
          (setf count 0))
        (setf (gethash y row-counts) (if counts
                                         (reverse counts)
                                         '(0)))))
    row-counts))

(defun picross-lookup (grid x y)
  (gethash (picross-key x y) grid))
