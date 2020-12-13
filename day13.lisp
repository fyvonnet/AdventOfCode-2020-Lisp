(defpackage :day13
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main))

(in-package :day13)


(defun get-ids (str)
  (labels
    ((rec (ids offset)
          (when ids
            (match (parse-integer (car ids) :junk-allowed t)
                   (nil (rec (cdr ids) (1+ offset)))
                   (n (cons (cons n offset) (rec (cdr ids) (1+ offset))))))))
    (rec (split "," str) 0)))

;;
;; Calculates the GCD of a and b based on the Extended Euclidean Algorithm. The function also returns
;; the Bézout coefficients s and t, such that gcd(a, b) = as + bt.
;;
;; The algorithm is described on page http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Iterative_method_2
;;
(defun egcd (a b)
  (do ((r (cons b a) (cons (- (cdr r) (* (car r) q)) (car r))) ; (r+1 r) i.e. the latest is first.
       (s (cons 0 1) (cons (- (cdr s) (* (car s) q)) (car s))) ; (s+1 s)
       (u (cons 1 0) (cons (- (cdr u) (* (car u) q)) (car u))) ; (t+1 t)
       (q nil))
    ((zerop (car r)) (values (cdr r) (cdr s) (cdr u)))       ; exit when r+1 = 0 and return r s t
    (setq q (floor (/ (cdr r) (car r))))))                     ; inside loop; calculate the q

;;
;; Calculates the inverse module for a = 1 (mod m). 
;;
;; Note: The inverse is only defined when a and m are coprimes, i.e. gcd(a, m) = 1.”
;;
(defun invmod (a m)
  (multiple-value-bind (r s k) (egcd a m)
    (unless (= 1 r) (error "invmod: Values ~a and ~a are not coprimes." a m))  
    s))

(defun chinese-remainder (am)
  "Calculates the Chinese Remainder for the given set of integer modulo pairs.
  Note: All the ni and the N must be coprimes."
  (loop :for (a . m) :in am
        :with mtot = (reduce #'* (mapcar #'(lambda(X) (cdr X)) am))
        :with sum  = 0
        :finally (return (mod sum mtot))
        :do
        (incf sum (* a (invmod (/ mtot m) m) (/ mtot m)))))

(defun main ()
  (let*
    ((input (read-input-as-list 13))
     (earliest-timestamp (parse-integer (first input)))
     (bus-ids (get-ids (second input)))
     (ids (mapcar #'car bus-ids))
     (offsets (mapcar #'cdr bus-ids)))

    (match
      (reduce
        (lambda (a b) (if (< (cdr a) (cdr b)) a b))
        (mapcar
          (lambda (i) (cons i (* i (ceiling earliest-timestamp i))))
          (mapcar #'first bus-ids)))
      ((cons line departure) (print (* line (- departure earliest-timestamp)))))

    (print (chinese-remainder (mapcar (lambda (i o) (cons (- i o) i)) ids offsets)))))

