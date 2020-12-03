(defpackage :day03
  (:use :cl aoc-misc)
  (:export main))

(in-package :day03)

(defun move-down (forest-map width height right-slope down-slope)
  (labels
    ((rec (r d count)
          (if (= d height)
            count
            (rec
              (mod (+ r right-slope) width)
              (+ d down-slope)
              (if (char= #\# (aref forest-map d r))
                (1+ count)
                count)))))
    (rec 0 0 0)))


(defun main ()
  (let*
    ((input (read-input-as-array 3))
     (height (array-dimension input 0))
     (width  (array-dimension input 1)))
    (print (move-down input width height 3 1))))
