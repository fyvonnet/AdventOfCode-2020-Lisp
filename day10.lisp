(defpackage :day10
  (:use :cl :aoc-misc :trivia)
  (:export main)
  (:import-from :fset
                :empty-map
                :lookup
                :with))

(in-package :day10)


(defun chain-adapters (graph end-node)
  (let ((count1 0) (count3 0))
  (labels
    ((rec (node)
          (if (= node end-node)
            (* count1 count3)
            (let ((next-node (car (lookup graph node))))
              (case (- next-node node) (1 (incf count1)) (3 (incf count3)))
              (rec next-node)))))
    (rec 0))))

(defun make-graph (lst)
  (if (null lst)
    (empty-map)
    (with
      (make-graph (cdr lst))
      (car lst)
      (remove-if (lambda (r) (> (- r (car lst)) 3)) (cdr lst)) )))

(defun main ()
  (let*
    ((input (read-input-as-list 10 #'parse-integer))
     (built-in (+ 3 (reduce #'max input)))
     (adapters (cons built-in input))
     (graph (make-graph (sort (cons 0 adapters) #'<))))
    (print (chain-adapters graph built-in))))

