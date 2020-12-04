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

(defun make-passports (lst &optional str)
  (cond
    ((and (null lst) (null str)) nil)
    ((zerop (length (first lst)))
     (cons
       (make-map (split "\\s" str))
       (make-passports (rest lst))))
    (t (make-passports
         (rest lst)
         (if (null str)
           (first lst)
           (concatenate 'string str " " (first lst)))))))

(defun all-fields (passport)
  (reduce
    (lambda (b f) (and b (lookup passport f)))
    '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")
    :initial-value t))

(defun main ()
  (let*
    ((passports (make-passports (read-input-as-list 4)))
     (valid-passports (remove-if-not #'all-fields passports)))
    (print (length valid-passports))))
