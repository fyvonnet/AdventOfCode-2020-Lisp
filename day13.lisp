(defpackage :day13
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main))

(in-package :day13)

(defun min-second (a b)
  (if (< (second a) (second b)) a b))

(defun main ()
  (let*
    ((input (read-input-as-list 13))
     (earliest-timestamp (parse-integer (first input)))
     (bus-ids (remove-if #'null (mapcar (lambda (s) (parse-integer s :junk-allowed t)) (split "," (second input))))))
    (match
      (reduce
        (lambda (a b) (if (< (cdr a) (cdr b)) a b))
        (mapcar (lambda (i) (cons i (* i (ceiling earliest-timestamp i)))) bus-ids))
      ((cons line departure)
       (print (* line (- departure earliest-timestamp)))))))
