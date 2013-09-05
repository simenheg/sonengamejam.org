(ql:quickload :alexandria)
(ql:quickload :cl-json)
(ql:quickload :cl-who)
(ql:quickload :ht-simple-ajax)
(ql:quickload :hunchentoot)
(ql:quickload :hunchentoot-test)

(defpackage :gamejam
  (:use :cl :hunchentoot :cl-who :ht-simple-ajax :json :alexandria)
  (:export :start-server))

(in-package :gamejam)

(load "entries.lisp")

;; ---------------------------------------------------------------- [ Misc. ]
;; *ENGLISH-LIST* format directive:
;;   (format nil *english-list* '(1))       ==> "1"
;;   (format nil *english-list* '(1 2))     ==> "1 and 2"
;;   (format nil *english-list* '(1 2 3))   ==> "1, 2, and 3"

(defparameter *english-list*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")

(defun ordinal (number)
  (let* ((str (format nil "~:r" number))
         (postfix (subseq str (- (length str) 2))))
    (format nil "~a~a" number postfix)))

(defun symcat (&rest syms)
  (intern
   (with-output-to-string (s)
     (dolist (a syms) (princ a s)))
   :gamejam))

(defun lisp-value-from-file (filename)
  (with-open-file (stream filename)
    (read stream)))

;; ----------------------------------------------------------------- [ AJAX ]
(defparameter *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/ajax"))

;; ---------------------------------------------------------- [ HTML macros ]
(defmacro with-html-page (&body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent nil)
     ,@body))

(defmacro with-html (&body body)
  `(with-html-output-to-string
       (*standard-output* nil :indent nil)
     ,@body))

;; ------------------------------------------------------- [ Site structure ]
(defstruct subsite id title color)

(defmacro defsite (id title color)
  `(progn
     (define-easy-handler (,id) ()
       (no-cache)
       (html-render-site (find ',id *subsites* :key #'subsite-id)))
     (make-subsite :id ',id :title ,title :color ,color)))

(defparameter *subsites*
  (list
   (defsite index "Info" "#a7e1ed")
   (defsite matchmaking "Matching" "#daef80")
   (defsite entries "Entries" "#f49896")
   (defsite prizes "Prizes" "#f4f896")))

(defun subsite-url (subsite)
  (string-downcase (subsite-id subsite)))

(defun subsite-render-function (subsite)
  (symcat 'html-render-body- (subsite-id subsite)))

;; ------------------------------------------------------ [ Server settings ]
(setq
 *dispatch-table*
 (list
  (create-ajax-dispatcher *ajax-processor*)
  (create-static-file-dispatcher-and-handler
   "/style.css" (first (directory "css/style.css")))
  (create-static-file-dispatcher-and-handler
   "/js.js" (first (directory "js/js.js")))
  (create-folder-dispatcher-and-handler
   "/icons/" (first (directory "icons/")))
  (create-folder-dispatcher-and-handler
   "/images/" (first (directory "images/")))
  (create-folder-dispatcher-and-handler
   "/screenshots/" (first (directory "screenshots/")))
  (create-prefix-dispatcher nil 'index)))

(dolist (site *subsites*)
  (let ((id (subsite-id site)))
    (push (create-prefix-dispatcher
           (string-downcase (format nil "/~a" id)) id)
          *dispatch-table*)))

;; ------------------------------------------------------------ [ Timetable ]
(defparameter *timetable* (lisp-value-from-file "timetable.lisp"))

(defun html-render-timetable (timetable)
  (with-html
    (:table
     :id "timetable"
     (loop for (day . schedule) in timetable do
       (htm (:th :colspan "2" (:strong (princ day))))
       (loop for (time event) in schedule do
         (htm (:tr (:td :class "timestamp" (princ time))
                   (:td (princ event)))))))))

;; ------------------------------------------------------ [ HTML generation ]
(defun html-render-header ()
  (with-html
    (:header
     (:img :src "images/header.png"))))

(defun html-render-menu-item (name ref)
  (with-html
    (:div
     :class "menu-item-container"
     (:a
      :href ref
      (:div
       :class "menu-item"
       :id (concatenate 'string "menu-item-" ref)
       (fmt name))))))

(defun html-render-menu ()
  (with-html
    (:div
     :id "menu"
     (dolist (site *subsites*)
       (fmt
        (html-render-menu-item
         (subsite-title site)
         (subsite-url site)))))))

(defun html-render-body-index ()
  (with-html
    (:p
     "Make a game in 48 hours! The purpose of the Game Jam is to gather
       aspiring game developers, rookies and veterans alike. Everyone is
       eligible enter, and the entry is no fee. No prior registration is
       needed, all you have to do is to show up at Sonen, Ole-Johan Dahls
       hus, September 27th!")
    (:h3 "Rules")
    (:p (:i "Note, all rules are subject to change before September 27th!"))
    (:ul
     (:li "One game submission per team.")
     (:li "There is no limit on the number of team members.")
     (:li "Time limit: 48 hours.")
     (:li "Game must conform with the given theme.")
     (:li "All programming languages allowed.")
     (:li "All publicly available frameworks, libraries & assets allowed.")
     (:li "Source code and a screen shot must be included in the final delivery."))
    (:h3 "Timetable")
    (fmt (html-render-timetable *timetable*))
    (:h3 "Matchmaking")
    (:p
     "Still missing that special someone on your team that can make you
       feel all warm and fuzzy inside (or at least turn your pixel-poop into
       something you might dare to call \"art\" when nobody is listening?)")
    (:p
     "Show up at 15:00 on Friday the 27th, or register in our "
     (:a :href "matchmaking" "matchmaking service")
     ".")
    (:h3 "Not associated with UiO?")
    (:p
     "Guest accounts will be provided, allowing you to use the machines at
       Ifi during the event.")
    (:p "When you need access to the building, please call:")
    (:ul
     (:li "Simen (96 82 24 48)"))
    (:h3 "Pizza, coffee & other foods")
    (:p
     "Free pizza will be served Sunday afternoon. Please inform us
       beforehand if you've got any specific dietary needs. ")
    (:p "Free coffee will of course be available at all times.")))

(defun html-render-body-matchmaking ()
  (with-html
    (:center (:i "( matchmaking service to come )"))))

(defun html-render-entry-title (entry)
  (with-html
    (:h2
     (fmt (entry-game-title entry))
     (when-let ((rank (entry-rank entry)))
       (htm (:img :class "medal"
                  :src (rank-icon rank)
                  :title (format nil "~a place" (ordinal rank))))))))

(defun html-render-entry-screenshot (entry)
  (with-html
    (when-let* ((screenshot (entry-screenshot entry))
                (uri (concatenate 'string "screenshots/" screenshot)))
      (htm (:div :class "screenshot" (:a :href uri (:img :src uri)))))))

(defun html-render-entry-team-members (entry)
  (with-html
    (:p
     (fmt "By ~a " (with-html (:strong (fmt (entry-team-name entry)))))
     (:br)
     (:span :style "font-size: 0.75em;"
            (fmt (concatenate 'string "(" *english-list* ")")
                 (entry-team-members entry))))))

(defun html-render-entry-framework (entry)
  (with-html
    (:p
     (fmt
      "Framework: ~a"
      (let ((framework (entry-framework entry)))
        (with-html
          (:a :href (framework-url framework)
              (fmt (framework-written framework)))))))))

(defun html-render-entry-downloads (entry)
  (with-html
    (:p
     (doplist (format url (entry-downloads entry))
       (htm (:a :href url
                (:img :class "download-link"
                      :src (format-image format))))
       (fmt " ")))))

(defun html-render-entry (entry)
  (with-html
    (:div
     :class "entry"
     (fmt (html-render-entry-title entry))
     (fmt (html-render-entry-screenshot entry))
     (fmt (html-render-entry-team-members entry))
     (fmt (html-render-entry-framework entry))
     (fmt (html-render-entry-downloads entry)))))

(defun html-render-entries (gamejam)
  (with-html
    (:h1 (fmt "~a: ~a" (gamejam-date gamejam) (gamejam-theme gamejam)))
    (let ((entries ; the entry list, sorted on game title
           (sort (copy-list (gethash (gamejam-id gamejam) *entries*))
                 #'string< :key #'entry-game-title)))
      (dolist (entry entries)
        (fmt (html-render-entry entry))))))

(defun html-render-body-entries ()
  (with-html
    (dolist (gamejam *gamejams*)
      (fmt (html-render-entries gamejam)))))

;; --------------------------------------------------------------- [ Prizes ]
(defparameter *prizes* (lisp-value-from-file "prizes.lisp"))

(defun html-render-body-prizes ()
  (with-html
    (:table
     :id "prizes"
     (loop for (category . prizes) in *prizes* do
       (htm (:th (:strong (princ category))))
       (loop for (prize url) in prizes do
         (htm
          (:tr (:td (:a :href url (princ prize))))))))))

(defun html-render-footer ()
  (with-html
    (:footer
     (fmt "Contact: ~a"
          (with-html (:a :href "mailto:simenheg@ifi.uio.no"
                         "Simen Heggestøyl"))))))

(defun html-render-site (site)
  (with-html-page
    (:head (:meta
            :charset "utf-8")
           (princ
            (generate-prologue *ajax-processor*))
           (:script
            :type "text/javascript"
            :src "http://code.jquery.com/jquery-latest.min.js")
           (:script
            :src "js.js")
           (:link
            :rel "stylesheet"
            :type "text/css"
            :href "style.css")
           (:title "Game Jam 2013"))
    (:body
     (:div
      :id "frame"
      (fmt (html-render-header))
      (fmt (html-render-menu))
      (:div
       :id "body"
       :style (format nil "background-color: ~a;" (subsite-color site))
       (fmt (funcall (subsite-render-function site))))
      (fmt (html-render-footer))))))

;; ----------------------------------------------------------------- [ Run! ]
(defun start-server (&key (port 9001))
  (start
   (make-instance
    'easy-acceptor
    :port port
    :address "127.0.0.1")))

(handler-case (start-server)
  (usocket:address-in-use-error ()
    (format t "No server started; address in use.~%")))
