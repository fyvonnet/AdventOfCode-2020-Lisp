(defpackage :day09
  (:use :cl :aoc-misc :trivia)
  (:export main)
  (:import-from :fset
                :empty-set
                :contains?
                :with))

(in-package :day09)


(defparameter *preamble-length* 25)

(defun split-at (n lst &optional lst2)
  (if (zerop n)
    (values (reverse lst2) lst)
    (split-at (1- n) (cdr lst) (cons (car lst) lst2))))

(defun snoc (lst elm)
  (append lst (list elm)))

(defun make-sums (preamble)
  (match preamble
         ((list _) nil)
         ((cons x xs)
          (cons
            (mapcar (lambda (y) (+ x y)) xs)
            (make-sums xs)))))

(defun is-in-list (n lst)
  (when lst
    (if (= n (car lst))
      t
      (is-in-list n (cdr lst)))))

(defun is-in-sums (n sums)
  (when sums
    (if (is-in-list n (car sums))
      t
      (is-in-sums n (cdr sums)))))

(defun find-invalid-number (preamble numbers sums)
  (let
    ((n (car numbers)))
    (if (is-in-sums n sums)
      (let*
        ((new-preamble (snoc (cdr preamble) n))
         (new-sums
           (mapcar
             (lambda (l p) (snoc l (+ p n)))
             (snoc (cdr sums) nil)
             new-preamble)))
        (find-invalid-number new-preamble (cdr numbers) new-sums))
      n)))

(defun main ()
  (multiple-value-bind (preamble numbers)
    (split-at *preamble-length* (read-input-as-list 9 #'parse-integer))
    (print (find-invalid-number preamble numbers (make-sums preamble)))))

