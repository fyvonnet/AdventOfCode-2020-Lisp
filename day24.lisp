(defpackage :day24
  (:use :cl :aoc-misc :trivia)
  (:export main)
  (:import-from :fset :empty-set :contains? :less :size :with)
  (:import-from :serapeum :nlet))

(in-package :day24)

(defun find-turned-tile (movements)
  (nlet rec ((x 0) (y 0) (lst (coerce movements 'list)))
        (if (null lst)
          (cons x y)
          (case (car lst)
            (#\w (rec (1- x) y (cdr lst)))
            (#\e (rec (1+ x) y (cdr lst)))
            (#\s
             (case (cadr lst)
               (#\w (rec (1- x) (1- y) (cddr lst)))
               (#\e (rec     x  (1- y) (cddr lst)))))
            (#\n
             (case (cadr lst)
               (#\w (rec     x  (1+ y) (cddr lst)))
               (#\e (rec (1+ x) (1+ y) (cddr lst)))))))))

(defun main ()
  (let
    ((input (read-input-as-list 24 #'find-turned-tile)))
    (print
      (size
        (reduce
          (lambda (s c) (if (contains? s c) (less s c) (with s c)))
          input
          :initial-value (empty-set))))))

