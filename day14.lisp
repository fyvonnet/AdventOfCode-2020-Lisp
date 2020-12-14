(defpackage :day14
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main)
  (:import-from :fset
                :convert
                :empty-map
                :with))


(in-package :day14)


(defun decode-line (str)
  (multiple-value-bind (s matches)
    (scan-to-strings "^mem\\[(\\d+)\\] = (\\d+)$" str)
    (if s
      (list 'mem (parse-integer (aref matches 0)) (parse-integer (aref matches 1)))
      (multiple-value-bind (_ matches)
        (scan-to-strings "^mask = ([01X]+)$" str)
        (list 'mask (coerce (reverse (aref matches 0)) 'list))))))

(defun apply-mask (mask val &optional (n 1))
  (if (null mask)
    val
    (apply-mask
      (cdr mask)
      (case (car mask)
        (#\0 (logand val (lognot n)))
        (#\1 (logior val n))
        (#\X val))
      (* 2 n))))

(defun apply-bitmask (mask addr)
  (if (null mask)
    nil
    (multiple-value-bind (q r) (floor addr 2)
      (cons
        (if (char= #\0 (car mask))
          (if (zerop r) #\0 #\1)
          (car mask))
        (apply-bitmask (cdr mask) q)))))

(defun convert-to-int (addr &optional (n 1))
  (if (null addr)
    0
    (case (car addr)
      (#\0 (convert-to-int (cdr addr) (* 2 n)))
      (#\1 (+ n (convert-to-int (cdr addr) (* 2 n)))))))

(defun make-address (base-addr n)
    (when base-addr
      (if (char= #\X (car base-addr))
        (multiple-value-bind (q r) (floor n 2)
          (cons (if (zerop r) #\0 #\1) (make-address (cdr base-addr) q)))
        (cons (car base-addr) (make-address (cdr base-addr) n)))))

(defun make-all-addresses (base-addr count)
  (unless (zerop count)
    (cons
      (convert-to-int (make-address base-addr (1- count)))
      (make-all-addresses base-addr (1- count)))))

(defun all-addresses (mask addr)
  (let* 
    ((base-addr (apply-bitmask mask addr))
     (addr-count (expt 2 (count-valid (lambda (c) (char= c #\X)) base-addr))))
    (make-all-addresses base-addr addr-count)))

(defun run-program (program &optional mask (memory (empty-map)))
  (match (car program)
         (nil (convert 'list memory))
         ((list 'mask m) (run-program (cdr program) m memory))
         ((list 'mem addr value) (run-program (cdr program ) mask (with memory addr (apply-mask mask value))))))

(defun run-program2 (program &optional mask (memory (empty-map)))
  (match (car program)
         (nil (convert 'list memory))
         ((list 'mask m) (run-program2 (cdr program) m memory))
         ((list 'mem addr value)
          (run-program2
            (cdr program)
            mask
            (reduce
              (lambda (m a) (with m a value))
              (all-addresses mask addr)
              :initial-value memory)))))

(defun main ()
  (let
    ((input (read-input-as-list 14 #'decode-line)))
    (print (reduce #'+ (mapcar #'cdr (run-program input))))
    (print (reduce #'+ (mapcar #'cdr (run-program2 input))))))

