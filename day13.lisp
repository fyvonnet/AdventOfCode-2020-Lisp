(defpackage :day13
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main))

(in-package :day13)


; https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
(defun euclidian (a b)
  (labels
    ((rec (ra rb sa sb ta tb)
          (multiple-value-bind (q r) (floor ra rb)
            (let
              ((new-s (- sa (* q sb)))
               (new-t (- ta (* q tb))))
              (if (zerop r)
                (cons sb tb)
                (rec rb r sb new-s tb new-t))))))
    (rec a b 1 0 0 1)))

; https://en.wikipedia.org/wiki/Chinese_remainder_theorem
(defun crt (as ns)
  (let*
    ((great-N  (reduce #'* ns))
     (great-Ns (mapcar (lambda (n) (/ great-N n)) ns)))
    (mod
      (reduce
        #'+
        (mapcar
          (lambda (great-N n a) (* (car (euclidian great-N n)) a great-N))
          great-Ns ns as))
      great-N)))


(defun get-ids (str)
  (labels
    ((rec (ids offset)
          (when ids
            (match (parse-integer (car ids) :junk-allowed t)
                   (nil (rec (cdr ids) (1+ offset)))
                   (n (cons (cons n offset) (rec (cdr ids) (1+ offset))))))))
    (rec (split "," str) 0)))


(defun main ()
  (let*
    ((input (read-input-as-list 13))
     (earliest-timestamp (parse-integer (first input)))
     (bus-ids (get-ids (second input)))
     (ids (mapcar #'car bus-ids))
     (offsets (mapcar #'cdr bus-ids)))

    (match
      (reduce
        (lambda (a b) (if (< (cdr a) (cdr b)) a b))
        (mapcar
          (lambda (i) (cons i (* i (ceiling earliest-timestamp i))))
          (mapcar #'first bus-ids)))
      ((cons line departure) (print (* line (- departure earliest-timestamp)))))

    (print (crt (mapcar (lambda (i o) (- i o)) ids offsets) ids))))

