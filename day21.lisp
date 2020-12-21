(defpackage :day21
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main)
  (:import-from :fset :contains? :convert :lookup
                :empty-map :empty-set :set-size :with))

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
            (lambda (m a) (with m a (cons (convert 'fset:set ingredients) (lookup m a))))
            allergens :initial-value map))))

(defun has-one-ingredient (allergen)
  (= 1 (set-size (cdr allergen))))

(defun confirm-ingredients (lst &optional confirmed-pairs)
  (if (null lst)
    confirmed-pairs
    (let* 
      ((confirmed-ingredients (remove-if-not #'has-one-ingredient lst))
       (unconfirmed-ingredients (remove-if #'has-one-ingredient lst))
       (ingredients-set (reduce #'fset:union (mapcar #'cdr confirmed-ingredients))))
       (confirm-ingredients
         (mapcar 
           (lambda (i)
             (match i
                    ((cons name ingredients)
                     (cons
                       name
                       (fset:set-difference ingredients ingredients-set)))))
           unconfirmed-ingredients)
         (reduce 
           (lambda (l i) (cons (cons (car i) (first (convert 'list (cdr i)))) l))
           confirmed-ingredients :initial-value confirmed-pairs)))))

(defun main ()
  (let*
    ((input (read-input-as-list 21 #'read-input))
     (all-ingredients-list (reduce #'append (mapcar #'car input)))
     (allergens-map
       (reduce
         #'add-to-map input
         :initial-value (empty-map)))
     (allergens-list (convert 'list allergens-map))
     (intersected-allergens
       (mapcar
         #'cons
         (mapcar #'car allergens-list)
         (mapcar 
           (lambda (i) (reduce #'fset:intersection i))
           (mapcar #'cdr allergens-list))))
     (nonallergenic-ingredients-set
       (fset:set-difference
         (convert 'fset:set all-ingredients-list)
         (reduce
           #'fset:union
           (mapcar 'cdr (convert 'list intersected-allergens)))))
     (ordered-allergenic-ingredients
       (mapcar 
         #'cdr 
         (sort 
           (confirm-ingredients intersected-allergens)
           (lambda (a b) (string< (car a) (car b)))))))

    (format t "~a~%" (count-valid (lambda (i) (contains? nonallergenic-ingredients-set i)) all-ingredients-list))
    (format t "~a" (car ordered-allergenic-ingredients))
    (dolist (i (cdr ordered-allergenic-ingredients)) (format t ",~a" i))
    (format t "~%")))

