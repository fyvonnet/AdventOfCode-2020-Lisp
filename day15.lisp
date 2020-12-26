(defpackage :day15
  (:use :cl :aoc-misc :cl-ppcre)
  (:export main))

(in-package :day15)


(defun main ()
  (let*
    ((starting-numbers
       (mapcar 
         #'parse-integer
         (split "," (first (read-input-as-list 15)))))
     (seen  (make-array 30000000 :initial-element nil))
     (turns (make-array 30000000))
     (start-turn
       (loop 
         for i from 0
         for n in (butlast starting-numbers)
         do (setf (aref seen n) i)
         finally (return i))))

    (loop
      with last = (first (last starting-numbers))
      for i from start-turn below 30000000
      do (setf (aref turns i) last)
      do (let
           ((found (aref seen last)))
           (setf (aref seen last) i)
           (setf last (if (null found) 0 (- i found)))))

    (dolist (i '(2020 30000000))
      (print (aref turns (1- i))))))
