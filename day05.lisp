(defpackage :day05
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day05)


(defun seat-id (str)
  (labels
    ((rec (lst bitvalue)
          (if (null lst)
            0
            (+
              (if (or (char= (first lst) #\F) (char= (first lst) #\L))
                0
                bitvalue)
              (rec (rest lst) (* 2 bitvalue))))))
    (rec (reverse (coerce str 'list)) 1)))


(defun main ()
  (print (reduce #'max (read-input-as-list 5 #'seat-id))))
