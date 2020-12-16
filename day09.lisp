(defpackage :day09
  (:use :cl :aoc-misc :trivia)
  (:export main))

(in-package :day09)


(defparameter *preamble-length* 25)

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

(defun update-sums-seqs (n sums nums seqs &optional new-sums new-seqs)
  (if nums
    (let*
      ((sum (car sums)) (num (car nums)) (seq (car seqs))
       (new-sum (+ num sum)) (new-seq (cons num seq)))
      (if (= new-sum n)
        (+ (reduce #'min new-seq) (reduce #'max new-seq))
        (update-sums-seqs
          n (cdr sums) (cdr nums) (cdr seqs)
          (cons new-sum new-sums)
          (cons new-seq new-seqs))))
    (cons (reverse new-sums) (reverse new-seqs))))

(defun find-encryption-weakness (n sums nums seqs)
  (when nums
    (match (update-sums-seqs n sums nums seqs)
           ((cons new-sums new-seqs)
            (find-encryption-weakness n new-sums (cdr nums) new-seqs))
           (solution solution))))

(defun main ()
  (let ((input (read-input-as-list 9 #'parse-integer)))
    (multiple-value-bind (preamble numbers) (split-at *preamble-length* input)
      (let ((invalid-number (find-invalid-number preamble numbers (make-sums preamble))))
        (print invalid-number)
        (print (find-encryption-weakness invalid-number input (cdr input) (mapcar #'list input)))))))

