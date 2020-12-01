(defpackage :day01
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day01)

(defun find-part1 (elm1 lst)
  (labels ((rec (elm2 lst)
                (unless (null elm2)
                  (if (= 2020 (+ elm1 elm2))
                    (* elm1 elm2)
                    (rec (first lst) (rest lst))))))
    (rec (first lst) (rest lst))))

(defun find-part2-sub (elm1 elm2 lst)
  (labels ((rec (elm3 lst)
                (unless (null elm3)
                  (if (= 2020 (+ elm1 elm2 elm3))
                    (* elm1 elm2 elm3)
                    (rec (first lst) (rest lst))))))
    (rec (first lst) (rest lst))))

(defun find-part2 (elm1 lst)
  (unless (null lst)
    (let ((res (find-part2-sub elm1 (first lst) (rest lst))))
      (if (null res)
        (find-part2 elm1 (rest lst))
        res))))

(defun find-solution (lst func)
  (let ((res (funcall func (first lst) (rest lst))))
    (if (null res)
      (find-solution (rest lst) func)
      res)))

(defun main ()
  (let
    ((input (read-input-as-list 1 #'parse-integer)))
    (format t "~a~%" (find-solution input #'find-part1))
    (format t "~a~%" (find-solution input #'find-part2))))
