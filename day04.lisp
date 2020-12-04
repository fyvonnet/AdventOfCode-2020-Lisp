(defpackage :day04
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main)
  (:import-from :fset
                :empty-map
                :lookup
                :with))

(in-package :day04)


(defun make-map (lst)
  (reduce
    (lambda (m r)
      (match (split ":" r)
             ((list field value) (with m field value))))
      lst :initial-value (empty-map)))

(defun concat-lines (lst &optional str)
  (cond
    ((null lst) (cons str nil))
    ((zerop (length (first lst))) (cons str (concat-lines (rest lst))))
    (t (concat-lines
         (rest lst)
         (if (null str)
           (first lst)
           (concatenate 'string str " " (first lst)))))))

(defun check-valid (passport)
  (reduce
    (lambda (b f) (and b (not (null (lookup passport f)))))
    '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")
    :initial-value t))

(defun main ()
  (let*
    ((input (read-input-as-list 4))
     (maps (mapcar (lambda (s) (make-map (split "\\s" s))) (concat-lines input))))
    ;(print maps)
    ;(print (length input))
    ;(print (length maps))
    (print (count-valid #'check-valid maps))))
