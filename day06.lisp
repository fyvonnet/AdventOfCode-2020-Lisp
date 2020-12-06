(defpackage :day06
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :fset
                :empty-set
                :less
                :size
                :with))

(in-package :day06)


(defun concatenate-lines (lst space? &optional str)
  (cond
    ((and (null lst) (null str)) nil)
    ((zerop (length (first lst))) (cons str (concatenate-lines (rest lst) space?)))
    (t
      (concatenate-lines
        (rest lst)
        space?
        (if (null str)
          (first lst)
          (concatenate 'string str (when space? " ") (first lst)))))))

(defun count-questions (str)
  (labels
    ((rec (lst set)
          (if (null lst)
            (size (less set #\Space))
            (rec (rest lst) (with set (car lst))))))
    (rec (coerce str 'list) (empty-set))))

(defun main ()
  (let
    ((input (concatenate-lines (read-input-as-list 06) t)))
    (print (reduce #'+ (mapcar #'count-questions input)))))
