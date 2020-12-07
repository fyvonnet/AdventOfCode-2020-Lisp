(defpackage :day07
  (:use :cl :aoc-misc :functional-queue :cl-ppcre :trivia)
  (:export main)
  (:import-from :fset
                :empty-map
                :empty-set
                :lookup
                :size
                :with))

(in-package :day07)

(defun decode-rule (str)
  (match (split " contain " (regex-replace-all " bag(s?)(\\.?)" str ""))
         ((list container content) 
          (cons
            container
            (mapcar
              (lambda (s)
                (match (split " " s :limit 2)
                       ((list "no" "other") nil)
                       ((list n color) (cons (parse-integer n) color))))
              (split ", " content))))))

(defun add-rule (graph rule)
  (reduce 
    (lambda (g r) (with g (cdr r) (cons (car rule) (lookup g (cdr r)))))
    (cdr rule)
    :initial-value graph))

(defun add-rule-2 (graph rule)
  (reduce 
    (lambda (g r) (with g (car rule) (cons r (lookup g (car rule)))))
    (cdr rule)
    :initial-value graph))

(defun make-graph (func rules)
  (reduce func rules :initial-value (empty-map)))

(defun count-bags (graph queue set)
  (let ((color (queue-head queue)))
    (if (null color)
      (1- (size set))
      (count-bags
        graph
        (reduce
          #'queue-snoc 
          (lookup graph color)
          :initial-value (queue-tail queue))
        (with set color)))))

(defun count-bags-2 (graph queue)
  (match (queue-head queue)
         (nil -1)
         ((cons n color)
          (+ n (count-bags-2
                 graph
                 (reduce 
                   (lambda (q e) (queue-snoc q (cons (* n (car e)) (cdr e))))
                   (lookup graph color)
                   :initial-value (queue-tail queue)))))))

(defun main ()
  (let
    ((rules (remove-if-not #'second (read-input-as-list 7 #'decode-rule))))
    ;((rules (remove-if-not #'second (read-input-as-list 7 #'decode-rule "test"))))
    (print 
      (count-bags
        (make-graph #'add-rule rules)
        (queue-snoc (empty-queue) "shiny gold")
        (empty-set)))
    (print 
      (count-bags-2
        (make-graph #'add-rule-2 rules)
        (queue-snoc (empty-queue) (cons 1 "shiny gold"))
        ))))
