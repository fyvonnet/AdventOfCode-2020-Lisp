(defpackage :day16
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main)
  (:import-from :serapeum :nlet))

(in-package :day16)


(defun read-fields (stream)
  (let
    ((str (read-line stream)))
    (multiple-value-bind (is-valid matches) (scan-to-strings "^[\\w ]+: (\\d+)-(\\d+) or (\\d+)-(\\d+)$" str)
      (when is-valid
        (append
          (multiple-value-bind (a b) (split-at 2 (mapcar #'parse-integer (coerce matches 'list)))
            (list a b))
          (read-fields stream))))))

(defun add-values (arr field)
  (match field
         ((list from to)
          (nlet rec ((i from))
                (if (> i to)
                  arr
                  (progn
                    (setf (aref arr i) t)
                    (rec (1+ i))))))))

(defun read-tickets (stream)
  (match (read-line stream nil)
         (nil nil)
         (line
           (match (scan "^\\d" line)
                  (nil (read-tickets stream))
                  (_ (cons 
                       (mapcar #'parse-integer (split "," line))
                       (read-tickets stream)))))))

(defun error-rate (valid-values)
  (lambda (rate ticket)
    (reduce
      #'+
      (remove-if (lambda (x) (aref valid-values x)) ticket)
      :initial-value rate)))

(defun main ()
  (let*
    ((stream (open "inputs/day16"))
     (fields (read-fields stream))
     (valid-values
       (reduce
         (lambda (a f) (add-values a f))
         fields
         :initial-value (make-array 1000 :initial-element nil)))
     (tickets (read-tickets stream))
     (your-ticket (car tickets))
     (nearby-tickets (cdr tickets)))
    (print
      (reduce
        (error-rate valid-values)
        nearby-tickets
        :initial-value 0))))
