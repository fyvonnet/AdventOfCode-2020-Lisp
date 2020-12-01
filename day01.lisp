(defpackage :day01
  (:use :cl :aoc-misc :trivia)
  (:export main))

(in-package :day01)

(defun find-solution (entries needed &optional candidates)
  (if (zerop needed)
    (when (= 2020 (reduce #'+ candidates))
      (reduce #'* candidates))
    (when entries
        (match (find-solution (rest entries) (1- needed) (cons (first entries) candidates))
          (nil (find-solution (rest entries) needed candidates))
          (solution solution)))))

(defun main ()
  (let
    ((entries (read-input-as-list 1 #'parse-integer)))
    (dolist (n '(2 3)) (print (find-solution entries n)))))

