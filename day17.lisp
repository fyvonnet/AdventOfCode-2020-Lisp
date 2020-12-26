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


(defvar *surroundings*)

(defun modulos (n divs)
  (when divs
    (multiple-value-bind (q r) (floor n (car divs))
      (cons r (modulos q (cdr divs))))))

(defun all-coords (dims &optional (shift 0))
  (nlet rec ((n (reduce #'* dims)) (coords nil))
        (if (zerop n)
          coords
          (rec (1- n)
               (cons
                 (mapcar
                   (lambda (x) (+ shift x))
                   (modulos (1- n) dims))
                 coords)))))

(defun update-space (n space min-coord space-dims)
  (if (zerop n)
    (length (convert 'list space))
    (let*
      ((new-min-coord  (1- min-coord))
       (new-space-dims (mapcar (lambda (d) (+ 2 d)) space-dims)))
      (update-space
        (1- n)
        (reduce
          (lambda (new-space coord)
            (let*
              ((active-around
                 (count-if
                   (lambda (c) (contains? space c))
                   (mapcar (lambda (s) (mapcar #'+ s coord)) *surroundings*))))
              (if (contains? space coord)
                (if (or (= 2 active-around) (= 3 active-around)) (with new-space coord) new-space)
                (if (= 3 active-around) (with new-space coord) new-space))))
          (all-coords new-space-dims new-min-coord)
          :initial-value (empty-set))
        new-min-coord new-space-dims))))

(defun run-space (input dim)
  (let*
    ( (height (array-dimension input 0))
     (width  (array-dimension input 1))
     (dims (append (list height width) (replicate (- dim 2) 1)))
     (initial-set 
       (reduce 
         (lambda (set coord)
           (match coord 
                  ((cons x (cons y _))
                   (case (aref input y x)
                     (#\# (with set coord))
                     (#\. set)))))
         (all-coords dims)
         :initial-value (empty-set))))
    (setf *surroundings*
          (remove-if
            (lambda (c) (every #'zerop c))
            (all-coords (replicate dim 3) -1)
            :count 1))
    (update-space 6 initial-set 0 dims)))

(defun main ()
  (let*
    ((input (read-input-as-array 17)))
    (dolist (dim '(3 4))
      (format t "~a~%" (run-space input dim)))))

