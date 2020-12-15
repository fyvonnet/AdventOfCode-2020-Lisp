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

(defun play-game (seq number turn last-turn)
  (if (= last-turn turn)
    number
    (play-game
      (with seq number turn)
      (match (lookup seq number) (nil 0) (last-seen (- turn last-seen)))
      (1+ turn)
      last-turn)))

(defun main ()
  (let*
    ((starting-numbers
       (mapcar 
         #'parse-integer
         (split "," (first (read-input-as-list 15)))))
     (seq (initialize-seq (butlast starting-numbers))))
    (dolist (last-turn '(2020 30000000))
      (print (play-game seq (first (last starting-numbers)) (length starting-numbers) last-turn)))))

