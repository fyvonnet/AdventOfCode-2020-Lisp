(defpackage :day18
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main)
  (:import-from :serapeum :nlet))

(in-package :day18)


(defun parse-expression (str)
  (let
    ((index 0) (len (length str)) )
    (labels
      ((rec ()
            (when (< index len)
              (let ((c (aref str index)))
                (incf index)
                (case c
                  (#\+ (cons 'add (rec)))
                  (#\* (cons 'mul (rec)))
                  (#\( (cons (rec) (rec)))
                  (#\) nil)
                  (#\Space (rec))
                  (otherwise
                    (cons
                      (parse-integer (coerce (list c) 'string))
                      (rec))))))))
      (rec))))

(defun evaluate-expression (expression)
  (if (integerp expression)
    expression
    (nlet rec 
          ((operators (remove-if-not #'symbolp expression))
           (values (remove-if #'symbolp expression)))
          (if (null operators)
            (car values)
            (rec
              (cdr operators)
              (cons
                (funcall
                  (case (car operators) ('add #'+) ('mul #'*))
                  (evaluate-expression (car values))
                  (evaluate-expression (cadr values)))
                (cddr values)))))))

(defun evaluate-expression2 (expression)
  (labels
    ((rec (operators values)
          (if (null operators)
            (car values)
            (case (car operators)
              ('add
               (rec
                 (cdr operators)
                 (cons 
                   (+
                     (evaluate-expression2 (car  values))
                     (evaluate-expression2 (cadr values)))
                   (cddr values))))
              ('mul
               (*
                 (evaluate-expression2 (car values))
                 (evaluate-expression2
                   (rec (cdr operators) (cdr values)))))))))
    (if (integerp expression)
      expression
      (rec
        (remove-if-not #'symbolp expression)
        (remove-if #'symbolp expression)))))

(defun main ()
  (let
    ((expressions (read-input-as-list 18 #'parse-expression)))
    (dolist (func '(evaluate-expression evaluate-expression2))
      (print
        (reduce
          (lambda (s e) (+ s (funcall func e)))
          expressions :initial-value 0)))))
