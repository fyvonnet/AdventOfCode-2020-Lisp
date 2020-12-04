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

(defun fully-valid (passport)
  (and
    (let ((byr (parse-integer (lookup passport "byr"))))
      (and (>= byr 1920) (<= byr 2002)))
    (let ((iyr (parse-integer (lookup passport "iyr"))))
      (and (>= iyr 2010) (<= iyr 2020)))
    (let ((eyr (parse-integer (lookup passport "eyr"))))
      (and (>= eyr 2020) (<= eyr 2030)))
    (multiple-value-bind (_ matches)
      (scan-to-strings "^(\\d+)(\\w*)$" (lookup passport "hgt"))
      (match (cons (parse-integer (aref matches 0)) (aref matches 1))
             ((cons hgt "cm") (and (>= hgt 150) (<= hgt 193)))
             ((cons hgt "in") (and (>= hgt  59) (<= hgt  76)))))
    (multiple-value-bind (hcl _)
      (scan-to-strings "^#([0-9]|[a-f]){6}$" (lookup passport "hcl"))
      hcl)
    (multiple-value-bind (ecl _)
      (scan-to-strings "^(amb|blu|brn|gry|grn|hzl|oth)$" (lookup passport "ecl"))
      ecl)
    (multiple-value-bind (pid _)
      (scan-to-strings "^([\\d]){9}$" (lookup passport "pid"))
      pid)))

(defun main ()
  (let*
    ((passports (make-passports (read-input-as-list 4)))
     (valid-passports (remove-if-not #'all-fields passports)))
    (print (length valid-passports))
    (print (count-valid #'fully-valid valid-passports))))

