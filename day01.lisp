(defpackage :day01
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day01)


(defun find-solution (entries needed &optional candidates)
  (if (zerop needed)
    (when (= 2020 (reduce #'+ candidates))
      (reduce #'* candidates))
    (when entries
      (let
        ((result (find-solution (rest entries) (1- needed) (cons (first entries) candidates))))
        (if result result (find-solution (rest entries) needed candidates))))))

(defun main ()
  (let
    ((entries (read-input-as-list 1 #'parse-integer)))
    (dolist (n '(2 3)) (print (find-solution entries n)))))
