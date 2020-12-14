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

(defun turn-to-zero (bit-position value)
  (logand value (lognot (expt 2 bit-position))))

(defun turn-to-one (bit-position value)
  (logior value (expt 2 bit-position)))

(defun apply-value-mask (mask val &optional (bit-position 0))
  (if (null mask)
    val
    (apply-value-mask
      (cdr mask)
      (case (car mask)
        (#\0 (turn-to-zero bit-position val))
        (#\1 (turn-to-one  bit-position val))
        (#\X val))
      (1+ bit-position))))

(defun get-base-address (mask address &optional (bit-position 0))
  (if (null mask)
    address
    (get-base-address
      (cdr mask)
      (case (car mask)
        (#\0 address)
        (#\1 (turn-to-one  bit-position address))
        (#\X (turn-to-zero bit-position address)))
      (1+ bit-position))))

(defun get-x-positions (mask &optional (bit-position 0) positions)
  (if (null mask)
    (reverse positions)
    (get-x-positions
      (cdr mask)
      (1+ bit-position)
      (if (char= #\X (car mask))
        (cons bit-position positions)
        positions))))

(defun change-address (address x-positions bit-array)
  (if (null x-positions)
    address
    (multiple-value-bind (q r) (floor bit-array 2)
      (change-address 
        (if (zerop r)
          address
          (turn-to-one (car x-positions) address))
        (cdr x-positions) q))))

(defun get-all-addresses (address mask x-positions)
  (let ((base-address (get-base-address mask address)))
    (labels
      ((rec (bit-array)
            (if (zerop bit-array)
              (list base-address)
              (cons
                (change-address base-address x-positions bit-array)
                (rec (1- bit-array))))))
      (rec (1- (expt 2 (length x-positions)))))))

(defun run-program (program &optional mask (memory (empty-map)))
  (match (car program)
         (nil (convert 'list memory))
         ((list 'mask m) (run-program (cdr program) m memory))
         ((list 'mem addr value)
          (run-program (cdr program) mask (with memory addr (apply-value-mask mask value))))))

(defun run-program2 (program &optional mask x-positions (memory (empty-map)))
  (match (car program)
         (nil (convert 'list memory))
         ((list 'mask m) (run-program2 (cdr program) m (get-x-positions m) memory))
         ((list 'mem addr value)
          (run-program2
            (cdr program)
            mask
            x-positions
            (reduce
              (lambda (m a) (with m a value))
              (get-all-addresses addr mask x-positions)
              :initial-value memory)))))

(defun main ()
  (let
    ((program (read-input-as-list 14 #'decode-line)))
    (dolist (func '(run-program run-program2))
      (print (reduce #'+ (mapcar #'cdr (funcall func program)))))))

