(defpackage :day17
  (:use :cl :aoc-misc :trivia)
  (:export main)
  (:import-from :serapeum
                :nlet)
  (:import-from :fset
                :contains?
                :convert
                :empty-set
                :with))

(in-package :day17)


(defvar *surroundings*
  (list
    '(-1 -1 -1) '(0 -1 -1) '(1 -1 -1)
    '(-1  0 -1) '(0  0 -1) '(1  0 -1)
    '(-1  1 -1) '(0  1 -1) '(1  1 -1)

    '(-1 -1  0) '(0 -1  0) '(1 -1  0)
    '(-1  0  0)            '(1  0  0)
    '(-1  1  0) '(0  1  0) '(1  1  0)

    '(-1 -1  1) '(0 -1  1) '(1 -1  1)
    '(-1  0  1) '(0  0  1) '(1  0  1)
    '(-1  1  1) '(0  1  1) '(1  1  1)))


(defun make-initial-set (arr)
  (nlet rec ((y 0) (x 0) (set (empty-set)))
        (cond
          ((= x (array-dimension arr 0))
           (rec (1+ y) 0 set))
          ((= y (array-dimension arr 1)) set)
          (t
            (rec y (1+ x)
                 (case (aref arr y x)
                   (#\# (with set (list x y 0)))
                   (#\. set)))))))

(defun update-space (space new-min-coord new-dim-space)
  (let*
    ((new-max-coord (mapcar (lambda (d) (+ new-min-coord d)) new-dim-space))
     (new-space (empty-set)))
    (loop
      for z from new-min-coord below (third new-max-coord)
      do (loop
           for y from new-min-coord below (second new-max-coord)
           do (loop
                for x from new-min-coord below (first new-max-coord)
                do (let*
                     ((coord (list x y z))
                      (active-around
                        (count-valid
                          (lambda (c) (contains? space c))
                          (mapcar (lambda (s) (mapcar #'+ s coord)) *surroundings*))))
                     (if (contains? space coord)
                       (when (or (= 2 active-around) (= 3 active-around)) (setf new-space (with new-space coord)))
                       (when (= 3 active-around) (setf new-space (with new-space coord))))))))
    new-space))

(defun update-space-times (n space min-coord dim-space)
  (if (zerop n)
    (length (convert 'list space))
    (let*
      ((new-min-coord (1- min-coord))
       (new-dim-space (mapcar (lambda (d) (+ 2 d)) dim-space)))
      (update-space-times
        (1- n)
        (update-space space new-min-coord new-dim-space)
        new-min-coord
        new-dim-space))))

(defun main ()
  (let*
    ((input (read-input-as-array 17))
     (initial-set (make-initial-set input))
     (height (array-dimension input 0))
     (width  (array-dimension input 1)))
    (print (update-space-times 6 initial-set 0 (list width height 1)))))

