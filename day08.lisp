(defpackage :day08
  (:use :cl :aoc-misc :alexandria :cl-ppcre :trivia)
  (:export main)
  (:import-from :fset
                :empty-set
                :contains?
                :with))

(in-package :day08)


(defun decode-line (str)
  (match (split "\\s" str)
         ((list instr arg)
          (cons
            (cond 
              ((string= instr "acc") 0)
              ((string= instr "jmp") 1)
              ((string= instr "nop") 2))
            (parse-integer arg)))))

(defun run-program (program)
  (let ((len (length program)))
    (labels
      ((rec (i acc set)
            (cond
              ((contains? set i) (cons nil acc))
              ((>= i len) (cons t acc))
              (t
                (let ((new-set (with set i)))
                  (match (aref program i)
                         ((cons instr arg)
                          (case instr
                            (0 (rec (1+ i) (+ acc arg) new-set))
                            (1 (rec (+ i arg) acc new-set))
                            (2 (rec (1+ i) acc new-set))))))))))
      (rec 0 0 (empty-set)))))

(defun change-program (program)
  (labels
    ((rec (i)
          (match (aref program i)
                 ((cons 0 _) (rec (1+ i)))
                 ((cons instr arg)
                  (let ((program-copy (copy-array program)))
                    (setf (aref program-copy i) (cons (if (= instr 2) 1 2) arg))
                    (match (run-program program-copy)
                           ((cons nil _) (rec (1+ i)))
                           ((cons t acc) acc)))))))
    (rec 0)))

(defun main ()
  (let
    ((program (coerce (read-input-as-list 8 #'decode-line) 'vector)))
    (print (cdr (run-program program)))
    (print (change-program program))))
