(ql:quickload :alexandria)
(ql:quickload :cl-who)
(ql:quickload :ht-simple-ajax)
(ql:quickload :hunchentoot)

(defpackage :gamejam
  (:use :cl :hunchentoot :cl-who :ht-simple-ajax :alexandria)
  (:export :start-server))

(in-package :gamejam)

(load "util.lisp")
(load "entries.lisp")

;; ----------------------------------------------------------------- [ AJAX ]
(defparameter *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/ajax"))

;; ---------------------------------------------------------- [ HTML macros ]
(setf *prologue* "<!DOCTYPE html>")

(defmacro with-html-page (&body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     ,@body))

(defmacro with-html (&body body)
  `(with-html-output-to-string
       (*standard-output* nil :indent t)
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
   (defsite index "Info" "#ffb868")
   (defsite timetable "Timetable" "#a7e1ed")
   (defsite entries "Entries" "#f49896")
   (defsite tips "Tips" "#c6dcc8")
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
     (:img :src "images/header-2014-03.png"
           :alt "Åpen Sone Game Jam"))))

(defun html-render-menu-item (name ref color)
  (with-html
    (:div
     :class "menu-item-container"
     (:a
      :href ref
      (:div
       :class "menu-item"
       :style (format nil "background-color: ~a;" color)
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
         (subsite-url site)
         (subsite-color site)))))))

(defun html-render-body-index ()
  (with-html
    (:p
     "Make a game in 48 hours! The purpose of Sonen Game Jam is to gather
       aspiring game makers, rookies and veterans alike. Everyone is eligible
       to enter, and the entry is free. No prior registration is needed, all
       you have to do is to show up at Sonen, Ole-Johan Dahls hus, March 14!")
    (:h3 "Physics talk source code")
    (:p
     "We've uploaded the "
     (:a :href "http://pastebin.com/U8uHH5bP" "Processing source code")
     "from the physics talk held by Andreas.")
    (:h3 "Livestream")
    (:p
     "Check out our livestream at"
     (:a :href "http://www.twitch.tv/sonengamejam" "Twitch.tv") "!")
    (:h3 "Venue")
    (:p
     (:strong "Sonen Game Jam will take place at room Ada 3407, also known as
  Sonen, located on the third floor of Ole-Johan Dahls hus, close to Forskningsparken station. " (:a :href "http://www.openstreetmap.org/way/35201513#map=17/59.94351/10.71872" "(map)")))
    (:p
     "If you already have a UiO student card, and need extended access to
     Ole-Johan Dahls hus during the weekend, please send us your full name
     and student number beforehand.")
    (:p
     "Guest accounts will be provided for those of you who aren't associated
     with UiO, allowing you to use the machines at Ole-Johan Dahls hus
     during the event.")
    (:p "If you need access to the building during the event, please
    call one of us:")
    (:ul
     (:li "Simen (96 82 24 48)")
     (:li "Jonny (45 88 50 35)"))
    (:h3 "Rules")
    (:ul
     (:li "One game submission per team.")
     (:li "There is no limit on the number of team members.")
     (:li "Time limit: 48 hours.")
     (:li "Game must conform with the given theme.")
     (:li "All programming languages allowed.")
     (:li "All publicly available frameworks, libraries & assets allowed.")
     (:li "Source code and a screen shot must be included in the final delivery."))
    (:h3 "Matchmaking")
    (:p
     "Still missing that special someone on your team that can make you
       feel all warm and fuzzy inside (or at least turn your pixel-poop into
       something you might dare to call \"art\" when nobody is listening?).")
    (:p
     "Show up at 14:00 on Friday the 14th, or chip an email to "
     (:a :href "mailto:simenheg@ifi.uio.no" "simenheg@ifi.uio.no")
     "or"
     (:a :href "mailto:jwringstad@gmail.com" "jwringstad@gmail.com"))
    (:h3 "Pizza, coffee & other foods")
    (:p
     "Free pizza will be served Sunday afternoon. Please inform us
       beforehand if you've got any specific dietary needs. ")
    (:p "Free coffee will of course be available at all times.")))

(defun html-render-body-timetable ()
  (with-html
    (fmt (html-render-timetable *timetable*))))

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
                (uri (format nil "screenshots/~a" screenshot))
                (thumb (format nil "screenshots/thumbs/~a" screenshot)))
      (htm
       (:div :class "screenshot"
             (:a :href uri (:img :src (if (probe-file thumb) thumb uri))))))))

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
                :class "download-link"
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

(defun html-render-body-tips ()
  (with-html
    (print-file "tips.html")))

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
           (:title "Sonen Game Jam 2014"))
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
