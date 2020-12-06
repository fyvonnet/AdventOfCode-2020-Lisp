(defpackage :day06
  (:use :cl :aoc-misc :cl-ppcre)
  (:export main)
  (:import-from :fset
                :empty-set
                :less
                :size
                :with))

(in-package :day06)


(defun make-chars-set (str)
  (labels
    ((rec (lst set)
          (if (null lst)
            set
            (rec (rest lst) (with set (first lst))))))
    (rec (coerce str 'list) (empty-set))))

(defun count-questions (str)
  (size (less (make-chars-set str) #\Space)))

(defun count-questions-2 (str)
  (let
    ((strs (split "\\s" str)))
    (size
      (reduce
        #'fset:intersection
        (mapcar #'make-chars-set strs)))))

(defun main ()
  (let
    ((input (read-broken-lines 6 t)))
    (dolist (func (list #'count-questions #'count-questions-2))
      (print (reduce #'+ (mapcar func input))))))
