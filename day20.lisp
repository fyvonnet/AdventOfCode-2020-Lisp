(defpackage :day20
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main)
  (:import-from :fset :convert :empty-map :empty-set :lookup :size :with :with-default))

(in-package :day20)


(defun read-tile (stream &optional lst)
  (multiple-value-bind (is-valid matches) (scan-to-strings "^([.#]+)$" (read-line stream))
    (if is-valid
      (read-tile 
        stream
        (cons (aref matches 0) lst))
      (let ((side (length lst)))
        (make-array
          (list side side)
          :initial-contents (reverse lst))))))

(defun read-input (stream)
  (match (read-line stream nil)
         (nil nil)
         (line
           (multiple-value-bind (_ matches) (scan-to-strings "^Tile (\\d+):" line)
             (cons
               (cons
                 (parse-integer (aref matches 0))
                 (read-tile stream))
               (read-input stream))))))

(defun set-from-list (lst)
  (reduce #'with lst :initial-value (empty-set)))

(defun match-borders (borders borders-sets)
  (when borders-sets
    (cons
      (let
        ((reference-set (set-from-list (car borders))))
        (mapcar
          (lambda (set) (fset:intersection reference-set set))
          borders-sets))
      (match-borders (cdr borders) (cdr borders-sets)))))

(defun create-ids-map (lst &optional (map (with-default (empty-map) (empty-set))))
  (if (null lst)
    map
    (create-ids-map
      (cdr lst)
      (with map (caar lst) (with (lookup map (caar lst)) (cdar lst))))))


(defun main ()
  (let*
    ((stream (open "inputs/day20"))
     (input (read-input stream))
     (tiles-ids (mapcar #'car input))
     (borders 
       (mapcar
         (lambda (tile)
           (mapcar
             (lambda (func)
               (coerce
                 (loop
                   for i below 10
                   collect (funcall func tile i))
                 'string))
             (list
               (lambda (tile i) (aref tile 0 i))
               (lambda (tile i) (aref tile i 9))
               (lambda (tile i) (aref tile 9 (- 9 i)))
               (lambda (tile i) (aref tile (- 9 i) 0))
               (lambda (tile i) (aref tile 9 i))
               (lambda (tile i) (aref tile i 0))
               (lambda (tile i) (aref tile 0 (- 9 i)))
               (lambda (tile i) (aref tile (- 9 i) 9))
               )))
         (mapcar #'cdr input)))
     (borders-sets (mapcar #'set-from-list borders))
     (borders-map
       (reduce
         (lambda (map square)
           (match square
                  ((cons num borders)
                   (reduce
                     (lambda (m b) (with m b (cons num (lookup m b))))
                     borders :initial-value map))))
         (mapcar #'cons tiles-ids borders)
         :initial-value (empty-map)))
     (id-matches (remove-if-not (lambda (l) (= 2 (length l))) (mapcar #'cdr (convert 'list borders-map)))))

    (print
      (reduce
        #'*
        (mapcar 
          #'car
          (remove-if-not
            (lambda (x) (= 2 (size (cdr x))))
            (convert 'list (create-ids-map (append id-matches (mapcar #'reverse id-matches))))))))))
