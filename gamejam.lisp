;; Hunchentoot examples are found at:
;;  ~/quicklisp/dists/quicklisp/software/hunchentoot-1.2.3/test
;;
;; Launch hunchentoot tests:
;;  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080))
;;
;; Hunchentoot test URI:
;;  http://127.0.0.1:8080/hunchentoot/test

(ql:quickload :hunchentoot)
(ql:quickload :hunchentoot-test)
(ql:quickload :cl-who)
(ql:quickload :ht-simple-ajax)
(ql:quickload :cl-json)
(ql:quickload :alexandria)

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

;; ----------------------------------------------------------------- [ AJAX ]
(defparameter *ajax-processor* 
  (make-instance 'ajax-processor :server-uri "/ajax")) 

;; (defun-ajax post-note (text x y) (*ajax-processor*)
;;   (push (make-instance 'note :text text :x x :y y) *notes*)
;;   "ok")

;; (defun-ajax get-notes () (*ajax-processor*)
;;   (encode-json-to-string *notes*))

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
   "/screenshots/" (first (directory "screenshots/")))
  (create-prefix-dispatcher "/matchmaking" 'matchmaking)
  (create-prefix-dispatcher "/entries" 'entries)
  (create-prefix-dispatcher "/prizes" 'prizes)
  (create-prefix-dispatcher nil 'index)))

;; ---------------------------------------------------------- [ HTML macros ]
(defmacro with-html-page (&body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent nil)
     ,@body))

(defmacro with-html (&body body)
  `(with-html-output-to-string
       (*standard-output* nil :indent nil)
     ,@body))

;; ------------------------------------------------------ [ HTML generation ]
(defparameter +index-color+ "#a7e1ed")
(defparameter +matchmaking-color+ "#daef80")
(defparameter +entries-color+ "#f49896")
(defparameter +prizes-color+ "#f4f896")

(defun html-render-header ()
  (with-html
    (:div
     :id "header"
     "( maybe header. or maybe not )")))

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
     (fmt (html-render-menu-item "Info" "index"))
     (fmt (html-render-menu-item "Matching" "matchmaking"))
     (fmt (html-render-menu-item "Entries" "entries"))
     (fmt (html-render-menu-item "Prizes" "prizes")))))

(defun html-render-body-index ()
  (with-html
    (:div
     :id "body"
     :style (format nil "background-color: ~a;" +index-color+)
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
      (:li "Source code must be included in the final delivery."))
     (:h3 "Timetable")
     (:table
      (:tr (:td :colspan "2" (:strong "Friday 27th")))
      (:tr (:td "15:30") (:td "Registrations open"))
      (:tr (:td "16:00") (:td "Crashcourse in LÖVE (Lua)"))
      (:tr (:td "16:30") (:td "Crashcourse in Processing (Java)"))
      (:tr (:td "17:00") (:td "Theme announcement & starting shot"))
      (:tr (:td "17:15") (:td "Crashcourse in game physics MOVED FROM
                               SATURDAY"))
      (:tr (:td "24:00") (:td "Time to go home "))

      (:tr (:td :colspan "2" (:strong "Saturday 28th")))
      (:tr (:td "10:00") (:td "Doors open"))
      (:tr (:td "18:00") (:td "Crashcourse in pixelart"))
      (:tr (:td "19:00") (:td "Crashcourse in game music"))
      (:tr (:td "24:00") (:td "Time to go home "))

      (:tr (:td :colspan "2" (:strong "Sunday 29th")))
      (:tr (:td "10:00") (:td "Doors open"))
      (:tr (:td "17:00") (:td "Submission deadline"))
      (:tr (:td "17:15") (:td "Showoff on the big screen"))
      (:tr (:td "Afterwards") (:td "Prizes & pizza "))
      )
     (:h3 "Matchmaking")
     (:p
      "Still missing that special someone on your team that can make you
       feel all warm and fuzzy inside (or at least turn your pixel-poop into
       something you might dare to call \"art\" when nobody is listening?)")
     (:i "Matchmaking service to come!")
     (:h3 "Not associated with UiO?")
     (:p
      "Guest accounts will be provided, allowing you to use the machines at
       Ifi during the event.")
     (:p "When you need access to the building, please call one of us:")
     (:ul
      (:li "Simen (96 82 24 48)")
      (:li "Ilyá (96 82 24 48)"))
     (:h3 "Pizza, coffee & other foods")
     (:p
      "Free pizza will be served Sunday afternoon. Please inform us
       beforehand if you've got any specific dietary needs. ")
     (:p "Free coffee will of course be available at all times."))))

(defun html-render-body-matchmaking ()
  (with-html
    (:div
     :id "body"
     :style (format nil "background-color: ~a;" +matchmaking-color+)
     (:i "( matchmaking service in the future )"))))

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
    (:h1 (fmt gamejam))
    (let ((entries ; the entry list, sorted on game title
           (sort (copy-list (gethash gamejam *entries*))
                 #'string< :key #'entry-game-title)))
      (dolist (entry entries)
        (fmt (html-render-entry entry))))))

(defun html-render-body-entries ()
  (with-html
    (:div
     :id "body"
     :style (format nil "background-color: ~a;" +entries-color+)
     (fmt (html-render-entries "May 2013")))))

(defun html-render-body-prizes ()
  (with-html
    (:div
     :id "body"
     :style (format nil "background-color: ~a;" +prizes-color+)
     (:i "( prize list in the future )"))))

(defun html-render-footer ()
  (with-html
    (:div
     :id "footer"
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
      (fmt (ecase site
             (index (html-render-body-index))
             (matchmaking (html-render-body-matchmaking))
             (entries (html-render-body-entries))
             (prizes (html-render-body-prizes))))
      (fmt (html-render-footer))))))

(define-easy-handler (index) ()
  (no-cache)
  (html-render-site 'index))

(define-easy-handler (matchmaking) ()
  (no-cache)
  (html-render-site 'matchmaking))

(define-easy-handler (entries) ()
  (no-cache)
  (html-render-site 'entries))

(define-easy-handler (prizes) ()
  (no-cache)
  (html-render-site 'prizes))

;; ----------------------------------------------------------------- [ Run! ]

(defun start-server (&key (port 8080))
  (start (make-instance 'easy-acceptor :port port)))

(handler-case (start-server :port 8080)
  (usocket:address-in-use-error ()
    (format t "No server started; address in use.~%")))
