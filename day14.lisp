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

(defun run-program (program &optional mask (memory (empty-map)))
  (match (car program)
         (nil (convert 'list memory))
         ((list 'mask m) (run-program (cdr program) m memory))
         ((list 'mem addr value) (run-program (cdr program ) mask (with memory addr (apply-mask mask value))))))

(defun main ()
  (let
    ((input (read-input-as-list 14 #'decode-line)))
    (print (reduce #'+ (mapcar #'cdr (run-program input))))))
