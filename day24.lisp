(defpackage :day24
  (:use :cl :aoc-misc :trivia)
  (:export main)
  (:import-from :fset :empty-set :contains? :less :size :with)
  (:import-from :serapeum :nlet))

(in-package :day24)

(defun find-turned-tile (movements)
  (nlet rec ((x 0) (y 0) (z 0) (lst (coerce movements 'list)))
        (if (null lst)
          (list x y z)
          (case (car lst)
            (#\e (rec (1+ x) (1- y) z (cdr lst)))
            (#\w (rec (1- x) (1+ y) z (cdr lst)))
            (#\n
             (case (cadr lst)
               (#\e (rec (1+ x)    y  (1- z) (cddr lst)))
               (#\w (rec     x (1+ y) (1- z) (cddr lst)))))
            (#\s
             (case (cadr lst)
               (#\e (rec     x (1- y) (1+ z) (cddr lst)))
               (#\w (rec (1- x)    y  (1+ z) (cddr lst)))))))))

(defun main ()
  (let
    ((input (read-input-as-list 24 #'find-turned-tile)))
    (print
      (size
        (reduce
          (lambda (s c) (if (contains? s c) (less s c) (with s c)))
          input
          :initial-value (empty-set))))))

