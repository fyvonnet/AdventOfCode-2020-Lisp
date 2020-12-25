(defpackage :day23
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day23)


(defun main()
  (loop
    with cups = (map 'vector (lambda (c) (1- (parse-integer (string c)))) (first (read-input-as-list 23)))
    for times below 100
    do (let* 
         ((current-cup (aref cups 0))
          (picked-cups (make-array 3))
          (remain-cups (make-array 6))
          (temp-cups (make-array 9))
          (destination (mod (1- current-cup) 9)))

         (setf (aref remain-cups 0) (aref cups 0))

         (loop
           for i from 0 below 3
           do (setf (aref picked-cups i) (aref cups (1+ i))))

         (loop
           while (position destination picked-cups)
           do (setf destination (mod (1- destination) 9)))

         (loop
           for i from 1 below 6
           do (setf (aref remain-cups i) (aref cups (+ 3 i))))

         ; create new vector

         (let
           ((rem-index 0))
           (loop
             for i from 0
             for c across remain-cups
             do (setf (aref temp-cups i) c)
             do (incf rem-index)
             until (= c destination))

           (loop
             for i from rem-index
             for c across picked-cups
             do (setf (aref temp-cups i) c))

           (loop
             for i from rem-index below 6
             do (setf (aref temp-cups (+ 3 i)) (aref remain-cups i))))

         (loop
           for i from 0 below 9
           do (setf (aref cups (mod (1- i) 9)) (aref temp-cups i))))


    finally (let
              ((index 0))
              (loop
                for c across cups
                do (incf index)
                until (= c 0))
              (loop
                for i from index below (+ index 8)
                do (format t "~a" (1+ (aref cups (mod i 9)))))
              (format t "~%"))))

