(defpackage :day10
  (:use :cl :aoc-misc :trivia)
  (:export main))

(in-package :day10)


(defun count-diffs (lst)
  (apply #'*
         (reduce
           (lambda (c x)
             (case x
               (1 (list (car c) (1+ (cadr c))))
               (3 (list (1+ (car c)) (cadr c)))
               (otherwise c)))
           lst :initial-value '(0 0))))

(defun chain-adapters (j adapters &optional diffs)
  (if (null adapters)
    diffs
    (labels
      ((rec (as)
            (when as
              (match (chain-adapters
                       (car as)
                       (remove (car as) adapters :count 1)
                       (cons (- (car as) j) diffs))
                     (nil (rec (cdr as)))
                     (x x)))))
      (rec
        (sort (remove-if (lambda (r) (> (- r j) 3)) adapters) #'<)))))

(defun main ()
  (let*
    ((adapters (read-input-as-list 10 #'parse-integer))
     (built-in (+ 3 (reduce #'max adapters))))
    (print (count-diffs (chain-adapters 0 (cons built-in adapters))))))
