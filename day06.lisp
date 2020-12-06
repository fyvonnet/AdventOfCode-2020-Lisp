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
  (let ((len (length str)))
    (labels
      ((rec (i set)
            (if (= i len)
              set
              (rec (1+ i) (with set (aref str i))))))
      (rec 0 (empty-set)))))

(defun count-questions (str)
  (size (less (make-chars-set str) #\Space)))

(defun count-questions-2 (str)
  (size
    (reduce
      #'fset:intersection
      (mapcar #'make-chars-set (split "\\s" str)))))

(defun main ()
  (let
    ((input (read-broken-lines 6 t)))
    (dolist (func (list #'count-questions #'count-questions-2))
      (print (reduce #'+ (mapcar func input))))))
