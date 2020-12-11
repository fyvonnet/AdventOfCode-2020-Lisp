(defpackage :day10
  (:use :cl :aoc-misc :functional-queue :trivia)
  (:export main)
  (:import-from :fset
                :empty-map
                :lookup
                :with
                :with-default))

(in-package :day10)


(defun count-diffs (lst &optional (counts (list 0 0)))
  (match lst
         ((list _) (reduce #'* counts))
         ((cons a (cons b _))
          (count-diffs
            (cdr lst)
            (case (- b a)
              (1 (list (1+ (car counts)) (cadr counts)))
              (3 (list (car counts) (1+ (cadr counts))))
              (otherwise counts))))))

(defun count-combinations (adapters &optional (map (with (with-default (empty-map) 0) 0 1)))
  (match adapters
         ((list last-adapter) (lookup map last-adapter))
         ((cons curr-adapter next-adapters)
          (count-combinations
            next-adapters
            (reduce
              (lambda (m a) (with m a (+ (lookup map curr-adapter) (lookup m a))))
              (take-until (lambda (a) (> (- a curr-adapter) 3)) next-adapters)
              :initial-value map)))))

(defun main ()
  (let*
    ((input (sort (read-input-as-list 10 #'parse-integer) #'<))
     (adapters (cons 0 (snoc input (+ 3 (car (last input)))))))
    (print (count-diffs adapters))
    (print (count-combinations adapters))))
