(defpackage :day24
  (:use :cl :aoc-misc :trivia)
  (:export main)
  (:import-from :fset :empty-set :contains? :convert :less :size :with)
  (:import-from :serapeum :nlet))

(in-package :day24)

(defun find-turned-tile (movements)
  (nlet rec ((x 0) (y 0) (z 0) (lst (coerce movements 'list)))
        (if (null lst)
          (list x y z)
          (case (car lst)
            (#\e (rec (1+ x) (1- y) z (cdr lst)))
            (#\w (rec (1- x) (1+ y) z (cdr lst)))
            (#\n
             (case (cadr lst)
               (#\e (rec (1+ x)    y  (1- z) (cddr lst)))
               (#\w (rec     x (1+ y) (1- z) (cddr lst)))))
            (#\s
             (case (cadr lst)
               (#\e (rec     x (1- y) (1+ z) (cddr lst)))
               (#\w (rec (1- x)    y  (1+ z) (cddr lst)))))))))

(defun coords-range (n)
  (loop
    with results
    for x from (- n) to n
    do (loop
         for y from (max (- n) (- (- x) n)) to (min n (+ (- x) n))
         for coord = (list x y (- (- x) y))
         do (setf results (cons coord results)))
    finally (return results)))

(defparameter *surroundings* (remove-if (lambda (c) (equal '(0 0 0) c)) (coords-range 1)))

(defun count-surrounding-black-tiles (floor coord)
  (count-valid
    (lambda (c) (contains? floor c))
    (mapcar (lambda (s) (mapcar #'+ s coord)) *surroundings*)))

(defun update-floor (n floor)
  (if (zerop n)
    floor
    (update-floor
      (1- n)
      (reduce
        (lambda (s c)
          (let ((n (count-surrounding-black-tiles floor c)))
            (if (contains? floor c)
              (if (or (zerop n) (> n 2)) s (with s c))
              (if (= 2 n) (with s c) s))))
        (coords-range (1+ (reduce (lambda (a b) (max (abs a) (abs b))) (reduce #'append (convert 'list floor)))))
        :initial-value (empty-set)))))

(defun main ()
  (let*
    ((black-tiles
       (reduce
         (lambda (s c) (if (contains? s c) (less s c) (with s c)))
         (read-input-as-list 24 #'find-turned-tile)
         :initial-value (empty-set))))
    (print (size black-tiles))
    (print (size (update-floor 100 black-tiles)))))
