(defpackage :day15
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main)
  (:import-from :fset
                :empty-seq
                :lookup
                :with))

(in-package :day15)


(defun initialize-seq (numbers &optional (turn 1) (seq (empty-seq)))
  (if (null numbers)
    seq
    (initialize-seq
      (cdr numbers)
      (1+ turn)
      (with seq (car numbers) turn))))

(defun play-game (seq number turn)
  (if (= 2020 turn)
    number
    (match (lookup seq number)
           (nil (play-game (with seq number turn) 0 (1+ turn)))
           (last-turn (play-game (with seq number turn) (- turn last-turn) (1+ turn))))))

(defun main ()
  (let*
    ((starting-numbers
       (mapcar 
         #'parse-integer
         (split "," (first (read-input-as-list 15)))))
     (seq (initialize-seq (butlast starting-numbers))))
    (print (play-game seq (first (last starting-numbers)) (length starting-numbers)))))

