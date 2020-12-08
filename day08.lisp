(defpackage :day08
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main)
  (:import-from :fset
                :empty-set
                :contains?
                :with))

(in-package :day08)

(defun decode-line (str)
  (match (split "\\s" str)
         ((list instr arg) (cons instr (parse-integer arg)))))

(defun run-program (program)
  (labels
    ((rec (i acc set)
          (if (contains? set i)
            acc
            (let ((new-set (with set i)))
              (match (aref program i)
                     ((cons instr arg)
                      (cond
                        ((string= instr "nop") (rec (1+ i)     acc      new-set))
                        ((string= instr "acc") (rec (1+ i) (+  acc arg) new-set))
                        ((string= instr "jmp") (rec (+  i arg) acc      new-set)))))))))
    (rec 0 0 (empty-set))))

(defun main ()
  (let
    ((program (coerce (read-input-as-list 8 #'decode-line) 'vector)))
     (print (run-program program))))
