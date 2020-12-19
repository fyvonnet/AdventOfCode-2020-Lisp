(defpackage :day19
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main)
  (:import-from :serapeum :nlet))

(in-package :day19)

(defun parse-rule-body (str)
  (if (char= #\" (aref str 0))
    (aref str 1)
    (nlet rec ((rules (split " " str)) (lst nil) (sublst nil))
          (cond ((null rules) (reverse (cons (reverse sublst) lst)))
                ((char= #\| (aref (car rules) 0)) (rec (cdr rules) (cons (reverse sublst) lst) nil))
                (t (rec (cdr rules) lst (cons (parse-integer (car rules)) sublst)))))))

(defun read-rules (stream)
  (multiple-value-bind (is-valid matches)
    (scan-to-strings "^(\\d+): (.+)$" (read-line stream))
    (when is-valid
      (let
        ((rule-num (parse-integer (aref matches 0)))
         (rule-body (parse-rule-body (aref matches 1))))
        (cons (cons rule-num rule-body) (read-rules stream))))))

(defun check-and (message rules-arr rules)
  (if (null rules)
    message
    (match (check-or message rules-arr (car rules))
           ('invalid 'invalid)
           (m (check-and m rules-arr (cdr rules))))))

(defun check-or (message rules-arr rule-num)
  (nlet rec ((rules (aref rules-arr rule-num)))
        (cond
          ((null rules) 'invalid)
          ((characterp rules) 
           (if (char= rules (car message))
             (cdr message)
             'invalid))
          (t (match (check-and message rules-arr (car rules))
                    ('invalid (rec (cdr rules)))
                    (x x))))))

(defun check (rules message)
  (null (check-or (coerce message 'list) rules 0)))

(defun main ()
  (let*
    ((stream (open "inputs/day19"))
     (rules-lst (read-rules stream))
     (rules
       (loop
         with array = (make-array (length rules-lst))
         for r in rules-lst
         do (setf (aref array (car r)) (cdr r))
         finally (return array)))
     (messages
       (loop
         for str = (read-line stream nil)
         while str
         collect str)))
    (print (count-valid (lambda (m) (check rules m)) messages))))
