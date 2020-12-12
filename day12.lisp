(defpackage :day12
  (:use :cl :aoc-coord :aoc-misc :cl-ppcre :trivia)
  (:export main))

(in-package :day12)


(defun decode (line)
  (multiple-value-bind (_ matches)
    (scan-to-strings "^(\\w)(\\d+)" line)
    (cons
      (coerce (aref matches 0) 'character)
      (parse-integer (aref matches 1)))))


(defun rotate (angle)
  (case (mod angle 360)
    ( 90 'left)
    (180 'back)
    (270 'right)))

(defun navigate (instructions direction coord)
  (if (null instructions)
    (manhattan-distance-from-origin coord)
    (match instructions
           ((cons (cons action value) cdr-inst)
            (case action
              (#\N (navigate cdr-inst direction (next-coord 'north coord value)))
              (#\S (navigate cdr-inst direction (next-coord 'south coord value)))
              (#\E (navigate cdr-inst direction (next-coord 'east  coord value)))
              (#\W (navigate cdr-inst direction (next-coord 'west  coord value)))
              (#\L (navigate cdr-inst (turn (rotate    value ) direction) coord))
              (#\R (navigate cdr-inst (turn (rotate (- value)) direction) coord))
              (#\F (navigate cdr-inst direction (next-coord direction coord value))))))))


(defun rotate2 (angle coord)
  (let ((x (get-x coord)) (y (get-y coord)))
    (case (mod angle 360)
      ( 90 (make-coord    y  (- x)))
      (180 (make-coord (- x) (- y)))
      (270 (make-coord (- y)    x)))))

(defun navigate2 (instructions waypoint ship)
  (if (null instructions)
    (manhattan-distance-from-origin ship)
    (match instructions
           ((cons (cons action value) cdr-inst)
            (case action
              (#\N (navigate2 cdr-inst (next-coord 'north waypoint value) ship))
              (#\S (navigate2 cdr-inst (next-coord 'south waypoint value) ship))
              (#\E (navigate2 cdr-inst (next-coord 'east  waypoint value) ship))
              (#\W (navigate2 cdr-inst (next-coord 'west  waypoint value) ship))
              (#\L (navigate2 cdr-inst (rotate2    value  waypoint) ship))
              (#\R (navigate2 cdr-inst (rotate2 (- value) waypoint) ship))
              (#\F (navigate2 cdr-inst waypoint (coord+ ship (make-coord
                                                               (* value (get-x waypoint))
                                                               (* value (get-y waypoint)))))))))))


(defun main ()
  (let
    ((instructions(read-input-as-list 12 #'decode)))
    (print (navigate  instructions 'east (make-coord 0 0)))
    (print (navigate2 instructions (make-coord 10 -1) (make-coord 0 0)))))
