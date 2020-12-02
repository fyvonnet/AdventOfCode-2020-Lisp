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
    (+
      (if (funcall predicate (first lst)) 1 0)
      (count-valid predicate (rest lst)))))

(defun policy-1 (record)
  (let
    ((valid-letters
       (count-valid
         (lambda (c) (char= (getf record :letter) c))
         (coerce (getf record :password) 'list))))
    (and (>= valid-letters (getf record :low)) (<= valid-letters (getf record :high)))))

(defun policy-2 (record)
  (= 1 (count-valid
         (lambda (i) (char= (getf record :letter) (aref (getf record :password) (1- i))))
         (list (getf record :low) (getf record :high)))))

(defun main ()
  (let ((input (read-input-as-list 2 #'parse-line)))
    (dolist (policy (list #'policy-1 #'policy-2))
      (format t "~a~%" (count-valid policy input)))))
