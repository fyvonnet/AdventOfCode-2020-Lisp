(defpackage :day22
  (:use :cl :aoc-misc :functional-queue :trivia)
  (:export main)
  (:import-from :fset :empty-set :with :contains?))

(in-package :day22)


(defun make-decks (lst &optional (deck (empty-queue)))
  (if lst
    (let ((card (parse-integer (car lst) :junk-allowed t)))
      (if (null card)
        (if (queue-empty-p deck)
          (make-decks (cdr lst))
          (cons deck (make-decks (cdr lst))))
        (make-decks (cdr lst) (queue-snoc deck card))))
    (list deck)))

(defun combat (deck-a deck-b)
  (cond 
    ((queue-empty-p deck-a) deck-b)
    ((queue-empty-p deck-b) deck-a)
    (t
      (let 
        ((card-a (queue-head deck-a))
         (card-b (queue-head deck-b)))
        (cond
          ((> card-a card-b)
           (combat
             (reduce
               #'queue-snoc (list card-a card-b)
               :initial-value (queue-tail deck-a))
             (queue-tail deck-b)))
          ((> card-b card-a)
           (combat
             (queue-tail deck-a)
             (reduce
               #'queue-snoc (list card-b card-a)
               :initial-value (queue-tail deck-b)))))))))

(defun queue-to-list (queue)
  (unless (queue-empty-p queue)
    (cons
      (queue-head queue)
      (queue-to-list (queue-tail queue)))))

(defun compute-score (deck)
  (labels
    ((rec (lst m)
          (if (null lst)
            0
            (+ (* m (car lst)) (rec (cdr lst) (1+ m))))))
    (rec (reverse (queue-to-list deck)) 1)))

(defun deck-size (deck)
  (if (queue-empty-p deck)
    0
    (1+ (deck-size (queue-tail deck)))))

(defun copy-cards (deck n &optional (new-deck (empty-queue)))
  (if (zerop n)
    new-deck
    (copy-cards
      (queue-tail deck)
      (1- n)
      (queue-snoc new-deck (queue-head deck)))))

(defun rounds (deck-a deck-b configurations)
  (if (or
        (queue-empty-p deck-a)
        (queue-empty-p deck-b))
    (cons deck-a deck-b)
    (if (contains? configurations (cons deck-a deck-b))
      (cons deck-a (empty-queue))
      (let*
        ((card-a (queue-head deck-a))
         (card-b (queue-head deck-b))
         (rest-deck-a (queue-tail deck-a))
         (rest-deck-b (queue-tail deck-b))
         (new-configurations (with configurations (cons deck-a deck-b)))
         (winner
           (if (and
                 (<= card-a (deck-size rest-deck-a))
                 (<= card-b (deck-size rest-deck-b)))
             (match (recursive-combat
                      (copy-cards rest-deck-a card-a)
                      (copy-cards rest-deck-b card-b))
                    ((cons new-deck-a new-deck-b)
                     (cond
                       ((queue-empty-p new-deck-a) 'b)
                       ((queue-empty-p new-deck-b) 'a))))
             (cond
               ((> card-a card-b) 'a)
               ((> card-b card-a) 'b)))))
        (setf configurations new-configurations)
        (if (equal winner 'a)
          (rounds
            (reduce
              #'queue-snoc (list card-a card-b)
              :initial-value rest-deck-a)
            rest-deck-b
            new-configurations)
          (rounds
            rest-deck-a
            (reduce
              #'queue-snoc (list card-b card-a)
              :initial-value rest-deck-b)
            new-configurations))))))

(defun recursive-combat (deck-a deck-b)
  (rounds deck-a deck-b (empty-set)))

(defun main ()
  (let*
    ((decks (make-decks (read-input-as-list 22)))
     (final-decks (recursive-combat (first decks) (second decks))))
    (format t "~a~%" (compute-score (combat (first decks) (second decks))))
    (format t "~a~%" 
            (compute-score
              (if (queue-empty-p (car final-decks))
                (cdr final-decks)
                (car final-decks))))))
