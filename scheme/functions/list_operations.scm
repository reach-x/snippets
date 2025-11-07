#!/usr/bin/env scheme
;;; List operations in Scheme
;;; Demonstrating functional programming with lists

(display "=== List Operations in Scheme ===\n\n")

;; Creating lists
(define numbers '(1 2 3 4 5))
(define fruits '("apple" "banana" "cherry"))

(display "Numbers: ")
(display numbers)
(newline)

;; List length
(display "Length: ")
(display (length numbers))
(newline)

;; Car (first element)
(display "\nFirst element (car): ")
(display (car numbers))
(newline)

;; Cdr (rest of list)
(display "Rest of list (cdr): ")
(display (cdr numbers))
(newline)

;; Cadr (second element)
(display "Second element (cadr): ")
(display (cadr numbers))
(newline)

;; Cons (prepend element)
(display "\nPrepend 0 (cons): ")
(display (cons 0 numbers))
(newline)

;; Append lists
(display "Append (6 7 8): ")
(display (append numbers '(6 7 8)))
(newline)

;; Reverse list
(display "Reversed: ")
(display (reverse numbers))
(newline)

;; Member (check if element in list)
(display "\nMember 3: ")
(display (member 3 numbers))
(newline)

(display "Member 10: ")
(display (member 10 numbers))
(newline)

;; Map function over list
(display "\nMap (square): ")
(display (map (lambda (x) (* x x)) numbers))
(newline)

;; Filter list
(display "Filter (even?): ")
(display (filter even? numbers))
(newline)

(display "Filter (odd?): ")
(display (filter odd? numbers))
(newline)

;; Fold (reduce)
(display "\nFold-left (sum): ")
(display (fold-left + 0 numbers))
(newline)

(display "Fold-left (product): ")
(display (fold-left * 1 numbers))
(newline)

;; For-each (side effects)
(display "\nFor-each loop:\n")
(for-each (lambda (x)
            (display "  Number: ")
            (display x)
            (newline))
          '(1 2 3))

;; List comprehension using named let
(display "\nList comprehension (squares):\n")
(define (squares n)
  (let loop ((i 1) (result '()))
    (if (> i n)
        (reverse result)
        (loop (+ i 1) (cons (* i i) result)))))

(display "Squares 1-5: ")
(display (squares 5))
(newline)

;; Helper functions

(define (list-sum lst)
  "Calculate sum of list"
  (fold-left + 0 lst))

(define (list-product lst)
  "Calculate product of list"
  (fold-left * 1 lst))

(define (list-max lst)
  "Find maximum in list"
  (fold-left max (car lst) (cdr lst)))

(define (list-min lst)
  "Find minimum in list"
  (fold-left min (car lst) (cdr lst)))

(define (take n lst)
  "Take first n elements"
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst)
            (take (- n 1) (cdr lst)))))

(define (drop n lst)
  "Drop first n elements"
  (if (<= n 0)
      lst
      (drop (- n 1) (cdr lst))))

(define (range start end)
  "Generate list from start to end-1"
  (if (>= start end)
      '()
      (cons start (range (+ start 1) end))))

(define (zip lst1 lst2)
  "Zip two lists together"
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (list (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

(define (flatten lst)
  "Flatten nested list"
  (cond
    ((null? lst) '())
    ((pair? (car lst))
     (append (flatten (car lst))
             (flatten (cdr lst))))
    (else (cons (car lst) (flatten (cdr lst))))))

;; Test helper functions
(display "\n=== Helper Functions ===\n")

(display "Sum of (1 2 3 4 5): ")
(display (list-sum '(1 2 3 4 5)))
(newline)

(display "Product of (1 2 3 4 5): ")
(display (list-product '(1 2 3 4 5)))
(newline)

(display "Max of (5 2 8 1 9): ")
(display (list-max '(5 2 8 1 9)))
(newline)

(display "Min of (5 2 8 1 9): ")
(display (list-min '(5 2 8 1 9)))
(newline)

(display "Take 3 from (1 2 3 4 5): ")
(display (take 3 '(1 2 3 4 5)))
(newline)

(display "Drop 2 from (1 2 3 4 5): ")
(display (drop 2 '(1 2 3 4 5)))
(newline)

(display "Range 1 to 10: ")
(display (range 1 10))
(newline)

(display "Zip (1 2 3) (a b c): ")
(display (zip '(1 2 3) '(a b c)))
(newline)

(display "Flatten ((1 2) (3 (4 5)) 6): ")
(display (flatten '((1 2) (3 (4 5)) 6)))
(newline)
