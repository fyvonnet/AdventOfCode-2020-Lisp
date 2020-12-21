(defpackage :day21
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main)
  (:import-from :fset :contains? :convert :lookup
                      :empty-map :empty-set :with))

(in-package :day21)


(defun read-input (line)
  (multiple-value-bind (_ matches) (scan-to-strings "^(.+) \\(contains (.+)\\)$" line)
    (cons
      (split " " (aref matches 0))
      (split ", " (aref matches 1)))))


(defun add-to-map (map line)
  (match line 
         ((cons ingredients allergens)
          (reduce
            (lambda (m a) (with m a (cons (list-to-set ingredients) (lookup m a))))
            allergens :initial-value map))))

(defun main ()
  (let*
    ((input (read-input-as-list 21 #'read-input))
     (all-ingredients-list (reduce #'append (mapcar #'car input)))
     (allergens-map
       (reduce
         #'add-to-map input
         :initial-value (empty-map)))
     (nonallergenic-ingredients-set
       (fset:set-difference
         (list-to-set all-ingredients-list)
         (reduce
           #'fset:union
           (mapcar
             (lambda (i) (reduce #'fset:intersection i))
             (mapcar 'cdr (convert 'list allergens-map)))))))
    (print (count-valid (lambda (i) (contains? nonallergenic-ingredients-set i)) all-ingredients-list))
    (print allergens-map)))
