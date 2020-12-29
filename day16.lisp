(defpackage :day16
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main)
  (:import-from :serapeum :nlet))

(in-package :day16)


(defun read-rules (stream)
  (multiple-value-bind (is-valid matches) (scan-to-strings "^([\\w ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)$" (read-line stream))
    (when is-valid
      (let ((r (coerce matches 'list)))
        (cons
          (cons (car r) (mapcar #'parse-integer (cdr r)))
          (read-rules stream))))))

(defun read-tickets (stream)
  (let
    ((ticket (read-line stream nil)))
    (cond
      ((null ticket) nil)
      ((scan "^\\d" ticket) 
       (cons
         (mapcar #'parse-integer (split "," ticket))
         (read-tickets stream)))
      (t (read-tickets stream)))))

(defun add-values (arr field)
  (match field
         ((cons from to)
          (nlet rec ((i from))
                (if (> i to)
                  arr
                  (progn
                    (setf (aref arr i) t)
                    (rec (1+ i))))))))

(defun check-one-value (rules value)
  (reduce
    (lambda (lst r)
      (match r
             ((list _ a b c d)
              (if (or (and (>= value a) (<= value b)) 
                      (and (>= value c) (<= value d)))
                (cons r lst)
                lst))))
    rules :initial-value nil))

(defun match-rules (tickets rules)
  (unless (null (car tickets))
    (cons
      (mapcar #'first (reduce #'check-one-value (mapcar #'car tickets) :initial-value rules))
      (match-rules (mapcar #'cdr tickets) rules))))

(defun refine-matches (rules-names)
  (labels ((has-length-of-one (lst) (= 1 (length lst))))
    (if (every #'has-length-of-one rules-names)
      (mapcar #'car rules-names)
      (refine-matches
        (let
          ((already-matched (mapcar #'car (remove-if-not #'has-length-of-one rules-names))))
          (mapcar
            (lambda (lst)
              (if (has-length-of-one lst)
                lst
                (remove-if (lambda (n) (member n already-matched)) lst :count 1)))
            rules-names))))))

(defun departure-product (ticket)
  (if (null ticket)
    1
    (*
      (if (scan "^departure" (caar ticket)) (cdar ticket) 1)
      (departure-product (cdr ticket)))))

(defun main ()
  (let*
    ((stream (open-input 16))
     (rules (read-rules stream))
     (tickets (read-tickets stream))
     (all-ranges
       (reduce 
         (lambda (l f)
           (match f ((list _ a b c d) (cons (cons a b) (cons (cons c d) l)))))
         rules :initial-value nil))
     (valid-values
       (reduce
         #'add-values all-ranges
         :initial-value (make-array 1000 :initial-element nil)))
     (checked-tickets 
       (mapcar
         (lambda (ticket) 
           (cons
             (reduce #'+ (remove-if (lambda (value) (aref valid-values value)) ticket))
             ticket))
         tickets))
     (valid-tickets
       (mapcar #'cdr (remove-if-not (lambda (ticket) (zerop (car ticket))) checked-tickets)))
     (my-ticket
       (mapcar
         #'cons
         (refine-matches (match-rules valid-tickets rules))
         (car tickets))))

    (print (reduce #'+ (mapcar #'car checked-tickets)))
    (print (departure-product my-ticket))))
