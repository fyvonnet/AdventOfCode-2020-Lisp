(defpackage :day02
  (:use :cl :aoc-misc :cl-ppcre)
  (:export main))

(in-package :day02)


(defun parse-line (line)
  (multiple-value-bind (_ matches)
    (scan-to-strings "^(\\d+)-(\\d+) (\\w): (\\w+)$" line)
    (declare (ignore _))
    (list :low  (parse-integer (aref matches 0))
          :high (parse-integer (aref matches 1))
          :letter (coerce (aref matches 2) 'character)
          :password (aref matches 3))))

(defun count-valid (predicate lst)
  (if (null lst)
    0
    (let ((ret (count-valid predicate (rest lst))))
      (if (funcall predicate (first lst)) (1+ ret) ret))))

(defun valid-record (record)
  (let*
    ((password (coerce (getf record :password) 'list))
     (valid-letters
       (count-valid
         (lambda (c) (char= (getf record :letter) c))
         password)))
    (and (>= valid-letters (getf record :low)) (<= valid-letters (getf record :high)))))




(defun main ()
  (let ((input (read-input-as-list 2 #'parse-line)))
    (print (count-valid #'valid-record input))))
