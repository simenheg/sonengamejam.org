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

(defun print-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line do (princ line))))
