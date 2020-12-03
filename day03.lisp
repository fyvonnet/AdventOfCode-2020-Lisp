(defpackage :day03
  (:use :cl aoc-misc)
  (:export main))

(in-package :day03)


(defun move-down (forest-map width height)
  (lambda (right-slope down-slope)
    (labels
      ((rec (r d count)
            (if (>= d height)
              count
              (rec
                (mod (+ r right-slope) width)
                (+ d down-slope)
                (if (char= #\# (aref forest-map d r))
                  (1+ count)
                  count)))))
      (rec 0 0 0))))

(defun main ()
  (let*
    ((input (read-input-as-array 3))
     (height (array-dimension input 0))
     (width  (array-dimension input 1))
     (func (move-down input width height))
     (part1-solution (funcall func 3 1)))

    (print part1-solution)

    (print 
      (reduce
        (lambda (p s)
          (* p (funcall func (car s) (cdr s))))
        '((1 . 1) (5 . 1) (7 . 1) (1 . 2))
        :initial-value part1-solution))))
