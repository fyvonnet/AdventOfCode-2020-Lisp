(defpackage :day25
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day25)


(defun transform (value subject-number)
  (rem (* value subject-number) 20201227))

(defun find-loop-size (pubkey &optional (loop-size 0) (value 1))
  (if (= value pubkey)
    loop-size
    (find-loop-size
      pubkey
      (1+ loop-size)
      (transform value 7))))

(defun find-encryption-key (public-key loop-size &optional (value 1))
  (if (zerop loop-size)
    value
    (find-encryption-key
      public-key
      (1- loop-size)
      (transform value public-key))))

(defun main ()
  (let*
    ((public-keys (read-input-as-list 25 #'parse-integer))
     (loop-sizes (mapcar #'find-loop-size public-keys))
     (encryption-keys
       (mapcar
         #'find-encryption-key
         (reverse public-keys)
         loop-sizes)))
    (if (= (first encryption-keys) (second encryption-keys))
      (print (first encryption-keys))
      (error "encryption keys not equal"))))
