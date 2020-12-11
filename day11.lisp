(defpackage :day11
  (:use :cl :aoc-coord :aoc-misc)
  (:export main))

(in-package :day11)


(defun occupied-adjacent (layout coord)
  (let
    ((coords
       (mapcar
         (lambda (c) (coord+ coord c))
         (list (make-coord -1 -1) (make-coord 0 -1) (make-coord 1 -1)
               (make-coord -1  0)                   (make-coord 1  0)
               (make-coord -1  1) (make-coord 0  1) (make-coord 1  1)))))
    (count-valid
      (lambda (c) (and c (char= c #\#)))
      (mapcar (lambda (c) (coord-array-get layout c)) coords))))

(defun update-layout (layout)
  (let*
    ((height (array-dimension layout 0))
     (width  (array-dimension layout 1))
     (new-layout (make-array (list height width) :initial-element nil))
     (has-changed nil))
    (labels
      ((rec (coord)
            (cond
              ((= (get-x coord) width) (rec (next-row coord)))
              ((= (get-y coord) height) new-layout)
              (t
                (case (coord-array-get layout coord)
                  (#\L
                   (coord-array-set
                     new-layout coord
                     (if (zerop (occupied-adjacent layout coord)) (progn (setf has-changed t) #\#) #\L)))
                  (#\#
                   (coord-array-set
                     new-layout coord
                     (if (>= (occupied-adjacent layout coord) 4) (progn (setf has-changed t) #\L) #\#))))
                (rec (next-column coord))))))
      (rec (make-coord 0 0))
      (if has-changed
        (update-layout new-layout)
        new-layout))))

(defun main ()
  (let*
    ((layout (read-input-as-array 11))
     (new-layout (update-layout layout)))
    (loop with count = 0
          for y below (array-dimension new-layout 0)
          do (loop for x below (array-dimension new-layout 1)
                   for c = (coord-array-get new-layout (make-coord x y))
                   do (when (and c (char= #\# c)) (incf count)))
          finally (print count))))

