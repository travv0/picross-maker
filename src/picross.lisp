(in-package :picross-maker)

(defparameter *site-name* "Picross Maker")
(defparameter *board-width* 5)
(defparameter *board-height* 5)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *head*
    `((:meta :charset "UTF-8")
      (:meta :name "viewport"
             :content "width=device-width, initial-scale=1")

      ;; (:link :rel "stylesheet"
      ;;        :href "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css")
      ;; (:link :rel "stylesheet"
      ;;        :href "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css")
      (:link :rel "stylesheet" :href "/style.css")

      ;; (:script :src "https://code.jquery.com/jquery-3.1.1.min.js"
      ;;          :integrity "sha256-hVVnYaiADRTO2PzUGmuLJr8BLUSjGIZsDYGmIJLv2b8 "
      ;;          :crossorigin "anonymous")
      ;; (:script :src "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js")
      (:script :src "/picross.js"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *header*
    '((:header :class "header"
       (:h1 (:a :href "/"
                "Picross Maker"))
       (:a :href "/browse" "browse puzzles")
       (" | ")
       (if (logged-in-p)
           (progn (:a :href (format nil "/browse?user=~a"
                                    (get-session-var 'username))
                      "your puzzles")
                  (" | ")
                  (:a :href "/b/logout" "log out"))
           (progn (:a :href "/login" "log in")
                  (" | ")
                  (:a :href "/create-account" "create account")))))))

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
       (:div :class "container"
             ,@*header*
             (unless (equal ,title "")
               (with-html (:h2 ,title)))
             ,@body)))))

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
      (:title "")
    (:body
     (:form :id "picrossForm"
            :action "submit-picross"
            :method "post"
            (row
              (:div :class "col-xs-12 col-sm-8"
                    (:input :type "text"
                            :name "picrossName"
                            :id "picrossName"
                            :required t)))
            (row
              (col 12
                (size-dropdown "boardWidth")
                (size-dropdown "boardHeight")))
            (row
              (:div :class "col-xs-12 col-sm-8"
                    (:div :id "picrossDiv")
                    (:input :type "hidden"
                            :id "picrossList"
                            :name "picrossList")
                    (:input :type "submit"))))
     (:script (format nil "
document.addEventListener('DOMContentLoaded', function() {
    setUpPicross(~d, ~d);
    document.querySelectorAll('#picrossForm').onsubmit = function (event) {
        if (!submitPicross(document.getElementById('#picrossDiv'))) {
            event.preventDefault();
            alert('Don\\'t forget to make your puzzle!');
        }
    };
});
"
                      *board-width*
                      *board-height*)))))

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
        (*board-height* (parse-integer (post-parameter "boardHeight"))))
    (execute-query-one picross
        "INSERT INTO picross (
              picross_cells,
              picross_width,
              picross_height,
              picross_date,
              picross_name,
              user_id
         )
         VALUES (
              ?,
              ?,
              ?,
              current_timestamp,
              ?,
              ?
         )
         RETURNING picross_id"
        ((post-parameter "picrossList")
         *board-width*
         *board-height*
         (post-parameter "picrossName")
         (if (get-session-var 'userid)
             (get-session-var 'userid)
             :null))
      (redirect (format nil "/picross?id=~d" (getf picross :|picross_id|))))))

(publish-page picross
  (execute-query-one picross
      "UPDATE picross
       SET picross_view_count = picross_view_count + 1
       WHERE picross_id = ?
       RETURNING picross_width,
                 picross_height,
                 picross_cells,
                 picross_name"
      ((get-parameter "id"))
    (let* ((*board-width* (getf picross :|picross_width|))
           (*board-height* (getf picross :|picross_height|)))
      (standard-page
          (:title (getf picross :|picross_name|))
        (:body
         (row
           (col 6
             (mode-toggle))
           (col 6
             (:div "Penalties: "
                   (:span :id "penaltyCounter" "0"))))
         (row
           (:div :class "col-xs-12 col-sm-8"
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
                        (:input :type "submit")))))
        (:script "
$(function() {
    $('#picrossForm').submit(function (event) {
        submitSolution($('#picrossDiv'));
        event.preventDefault();
    });
});
")))))

(defhtml mode-toggle ()
  (:a :href "#"
      :id "modeLink"
      :onclick "toggleMode()"
      "switch to mark mode")
  (:br)
  ("(You can also shift+click to mark cells)")
  (:input :type "hidden"
          :id "mode"
          :value "play"))

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
                     (:td :class "picrossCell solve"
                          :id (format nil "x~dy~d" x y)
                          (:div :class "cellContent"))))))))

(publish-page submit-solution
  (let ((picross-list (get-parameter "cells")))
    (execute-query-one picross "SELECT picross_cells
                                FROM picross
                                WHERE picross_id = ?"
        ((get-parameter "id"))
      (if (equal (getf picross :|picross_cells|)
                 picross-list)
          (progn (execute-query-modify
                  "UPDATE picross
                   SET picross_complete_count = picross_complete_count + 1,
                       picross_attempt_count = picross_attempt_count + 1
                   WHERE picross_id = ?"
                  ((get-parameter "id")))
                 (with-html-string ("1")))
          (progn (execute-query-modify
                  "UPDATE picross
                   SET picross_attempt_count = picross_attempt_count + 1
                   WHERE picross_id = ?"
                  ((get-parameter "id")))
                 (with-html-string ("0")))))))

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

(publish-page check-cell
  (execute-query-one picross "SELECT picross_cells
                              FROM picross
                              WHERE picross_id = ?"
      ((get-parameter "id"))
    (if (cell-in-list-p (get-parameter "cell")
                        (getf picross :|picross_cells|))
        (with-html-string ("1"))
        (progn (execute-query-modify
                "UPDATE picross
                 SET picross_attempt_count = picross_attempt_count + 1
                 WHERE picross_id = ?"
                ((get-parameter "id")))
               (with-html-string ("0"))))))

(defun cell-in-list-p (cell list)
  (search (concatenate 'string "," cell ",")
          (concatenate 'string "," list ",")))

(publish-page browse
  (standard-page
      (:title "Browse Puzzles")
    (:body
     (execute-query-loop picross
         (format nil
                 "SELECT picross_id,
                         picross_name,
                         picross_width,
                         picross_height,
                         picross_date,
                         picross_complete_count,
                         picross_attempt_count,
                         user_name
                  FROM picross
                  LEFT JOIN users ON picross.user_id = users.user_id
                  ~a
                  ORDER BY picross_date DESC"
                 (let ((user-name (get-parameter "user")))
                   (if user-name
                       (format nil
                               "WHERE user_name = '~a'"
                               (dbi.driver:escape-sql *conn* user-name))
                       "")))
         ()
       (row
         (col 12
           (:b (:a :href (format nil "/picross?id=~d"
                                 (getf picross :|picross_id|))
                   (getf picross :|picross_name|)))))
       (row
         (col 2
           (:span (format nil "~dx~d"
                          (getf picross :|picross_width|)
                          (getf picross :|picross_height|))))
         (col 3
           (let ((scale 5))
             (:span (format nil
                            "Difficulty: ~d/~d"
                            (round (* scale
                                      (- 1
                                         (picross-difficulty (getf picross :|picross_attempt_count|)
                                                             (getf picross :|picross_complete_count|)))))
                            scale))))
         (col 3
           (let ((user-name (getf picross :|user_name|)))
             (if (null-p user-name)
                 "Anonymous"
                 user-name)))
         (col 4
           (:span :class "time"
                  (universal-to-unix (getf picross :|picross_date|)))))))))

(publish-page login
  (standard-page
      (:title "Log in")
    (:body
     (:form :id "loginForm" :method "POST" :action "/b/login"
            (:div (:input :id "username" :name "username" :type "text" :required t))
            (:div (:input :id "password" :name "password" :type "password" :required t))
            (:div (:input :type "submit"
                          :class "btn btn-sm btn-default"
                          :value "Submit"
                          :onclick "submitLogin()")
                  (:input :type "button"
                          :value "Main Page"
                          :class "btn btn-sm btn-default"
                          :onclick "window.location='../'"))))))

(publish-page b/login
  (let ((user-name (post-parameter "username"))
        (password (post-parameter "password")))
    (set-password-if-unset user-name
                           password)
    (login-user user-name password)))

(defun login-user (user-name password)
  (if (and (is-user-p user-name)
           (is-correct-password-p user-name
                                  password))
      (let ((session-id nil))
        ;; find an id not in use and set it to session-id
        (loop while (gethash
                     (setf session-id (write-to-string (make-v4-uuid)))
                     *sessions*))

        (setf (gethash session-id *sessions*) (make-hash-table :test 'equal))

        ;; make life easier by making sure username in session is capitalized like in the DB
        (execute-query-one user
            "SELECT user_id, user_name FROM users WHERE lower(user_name) = lower(?)"
            (user-name)
          (setf (gethash 'username (gethash session-id *sessions*)) (getf user :|user_name|))
          (setf (gethash 'userid (gethash session-id *sessions*)) (getf user :|user_id|))
          (set-user-last-login (getf user :|user_id|)))

        (setf (gethash 'userlastactive (gethash session-id *sessions*)) (get-universal-time))

        (set-cookie *session-id-cookie-name*
                    :value session-id
                    :path "/"
                    :expires (+ (get-universal-time) (* 10 365 24 60 60)))
        (redirect "/"))
      (redirect "/login-failed")))

(defun is-correct-password-p (user-name password)
  (execute-query-one user
      "SELECT user_password
       FROM users
       WHERE lower(user_name) = lower(?)" (user-name)
    (equal (signature password) (getf user :|user_password|))))

(defun is-user-p (user-name)
  (execute-query-one user
      "SELECT 1 AS user_exists
       FROM users
       WHERE lower(user_name) = lower(?)" (user-name)
    (when (getf user :|user_exists|)
      t)))

(defun set-user-last-login (user-id)
  (execute-query-modify
   "UPDATE users
    SET user_last_login_date = current_timestamp
    WHERE user_id = ?" (user-id)))

(defun logged-in-p ()
  (get-session-var 'username))

(defun set-password-if-unset (user-name password)
  (execute-query-one user
      "SELECT user_password
       FROM users
       WHERE lower(user_name) = lower(?)" (user-name)
    (when (equal (getf user :|user_password|) :null)
      (execute-query-modify
       "UPDATE users
        SET user_password = ?
        WHERE lower(user_name) = lower(?)"
       ((signature password)
        user-name)))))

(publish-page b/logout
  (remhash (cookie-in *session-id-cookie-name*) *sessions*)
  (redirect "/"))

(defconstant +hash-size+ 32)
(defconstant +encoded-hash-size+ (* 5/4 +hash-size+))

(defvar *signing-key*)

(defun randomize-signing-key ()
  (setf *signing-key*
        (map-into (make-array +hash-size+ :element-type '(unsigned-byte 8))
                  (lambda () (random 256)))))

(defun signature (string &key (start 0))
  (unless (boundp '*signing-key*)
    (log-message* :warn "Signing key is unbound.  Using Lisp's RANDOM function to initialize it.")
    (randomize-signing-key))
  (let ((state (sha3:sha3-init :output-bit-length (* 8 +hash-size+))))
    (sha3:sha3-update state (babel:string-to-octets string :start start))
    (sha3:sha3-update state *signing-key*)
    (binascii:encode-base85 (sha3:sha3-final state))))

(publish-page create-account
  (standard-page
      (:title "Create Account")
    (:body
     (:form :id "loginForm" :method "POST" :action "/b/create-account"
            (:div (:input :id "username" :name "username" :type "text" :required t))
            (:div (:input :id "password" :name "password" :type "password" :required t))
            (:div (:input :type "submit"
                          :class "btn btn-sm btn-default"
                          :value "Submit"
                          :onclick "submitLogin()")
                  (:input :type "button"
                          :value "Main Page"
                          :class "btn btn-sm btn-default"
                          :onclick "window.location='../'"))))))

(publish-page b/create-account
  (let ((user-name (post-parameter "username"))
        (password (post-parameter "password")))
    (if (not (is-user-p user-name))
        (create-user user-name password)
        (with-html-string (:span "user already exists")))))

(defun create-user (user-name password)
  (execute-query-modify
   "INSERT INTO users (
        user_name,
        user_password,
        user_last_login_date
    )
    VALUES (
        ?,
        ?,
        current_timestamp
    )"
   (user-name (signature password)))
  (login-user user-name password))

(defun picross-difficulty (attempts completions)
  (when (not (integerp attempts))
    (parse-integer attempts))
  (when (not (integerp completions))
    (parse-integer completions))
  (if (= attempts 0)
      1
      (coerce (/ completions attempts)
              'float)))
