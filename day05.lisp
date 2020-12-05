(defpackage :day05
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day05)


(defun decode-char (c)
  (case c
    (#\F 0) (#\B 1) (#\L 0) (#\R 1)
    (otherwise (error (format nil "Unexpected character: ~a~%" c)))))

(defun seat-id (str)
  (labels
    ((rec (lst bitvalue)
          (if (null lst)
            0
            (+ (* (decode-char (first lst)) bitvalue)
               (rec (rest lst) (* 2 bitvalue))))))
    (rec (reverse (coerce str 'list)) 1)))

(defun search-seat (lst)
  (let*
    ((fst (first lst))
     (snd (second lst)))
    (unless (null snd)
      (let ((ret (search-seat (rest lst))))
        (if (= snd (+ 2 fst)) (cons (1+ fst) ret) ret)))))

(defun main ()
  (let
    ((seat-ids (read-input-as-list 5 #'seat-id)))
    (print (reduce #'max seat-ids))
    (print (search-seat (sort seat-ids #'<)))))

