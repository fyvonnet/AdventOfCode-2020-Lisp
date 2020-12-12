(defpackage :day11
  (:use :cl :alexandria :aoc-coord :aoc-misc :trivia)
  (:export main))

(in-package :day11)


(defparameter *all-directions*
  (list (make-coord -1 -1) (make-coord 0 -1) (make-coord 1 -1)
        (make-coord -1  0)                   (make-coord 1  0)
        (make-coord -1  1) (make-coord 0  1) (make-coord 1  1)))


(defun occupied-adjacent (layout coord)
  (count-valid
    (lambda (c) (and c (char= c #\#)))
    (mapcar
      (lambda (d) (coord-array-get layout (coord+ coord d)))
      *all-directions*)))


(defun see-occupied-seat (layout dir coord)
  (let ((new-coord (coord+ coord dir)))
    (case (coord-array-get layout new-coord)
      (#\# t)
      (#\L nil)
      (#\. (see-occupied-seat layout dir new-coord))
      (otherwise nil))))

(defun occupied-visible (layout coord)
  (count-valid
    (lambda (dir) (see-occupied-seat layout dir coord))
    *all-directions*))


(defun update-layout (layout func max-occ)
  (loop
    with height = (array-dimension layout 0)
    with width  = (array-dimension layout 1)
    with new-layout = (make-array (list height width))
    with has-changed = nil
    for y below height
    do (loop
         for x below width
         do (let
              ((coord (make-coord x y)))
              (coord-array-set
                new-layout coord
                (case (coord-array-get layout coord)
                  (#\L (if (zerop (funcall func layout coord)) (progn (setf has-changed t) #\#) #\L))
                  (#\# (if (>= (funcall func layout coord) max-occ) (progn (setf has-changed t) #\L) #\#))
                  (#\. #\.)))))
    finally (return (values has-changed new-layout))))

(defun count-all-occupied (layout)
  (loop with count = 0
        for y below (array-dimension layout 0)
        do (loop for x below (array-dimension layout 1)
                 for c = (coord-array-get layout (make-coord x y))
                 do (when (and c (char= #\# c)) (incf count)))
        finally (return count)))

(defun get-final-layout (layout func max-occ)
  (multiple-value-bind (has-changed new-layout) (update-layout layout func max-occ)
    (if has-changed
      (get-final-layout new-layout func max-occ)
      (count-all-occupied layout))))


(defun main ()
  (let*
    ((layout (read-input-as-array 11 #'identity "test")))
    (print (get-final-layout layout #'occupied-adjacent 4))
    (print (get-final-layout layout #'occupied-visible  5))))

