#!/usr/bin/env scheme
;;; Utility functions library for Scheme

;; List utilities

(define (range start end)
  "Generate list from start to end-1"
  (if (>= start end)
      '()
      (cons start (range (+ start 1) end))))

(define (take n lst)
  "Take first n elements from list"
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst)
            (take (- n 1) (cdr lst)))))

(define (drop n lst)
  "Drop first n elements from list"
  (if (<= n 0)
      lst
      (drop (- n 1) (cdr lst))))

(define (zip lst1 lst2)
  "Zip two lists together"
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (list (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

(define (flatten lst)
  "Flatten nested list structure"
  (cond
    ((null? lst) '())
    ((pair? (car lst))
     (append (flatten (car lst))
             (flatten (cdr lst))))
    (else (cons (car lst) (flatten (cdr lst))))))

(define (unique lst)
  "Remove duplicates from list"
  (define (helper lst seen)
    (cond
      ((null? lst) '())
      ((member (car lst) seen)
       (helper (cdr lst) seen))
      (else (cons (car lst)
                  (helper (cdr lst)
                         (cons (car lst) seen))))))
  (reverse (helper lst '())))

(define (group-by key-fn lst)
  "Group list elements by key function"
  (define (insert-into-groups item groups)
    (let ((key (key-fn item)))
      (let ((group (assoc key groups)))
        (if group
            (map (lambda (g)
                   (if (equal? (car g) key)
                       (cons key (append (cdr g) (list item)))
                       g))
                 groups)
            (append groups (list (cons key (list item))))))))
  (fold-left (lambda (groups item)
               (insert-into-groups item groups))
             '()
             lst))

(define (partition pred lst)
  "Partition list into two lists based on predicate"
  (define (helper lst yes no)
    (cond
      ((null? lst) (values (reverse yes) (reverse no)))
      ((pred (car lst))
       (helper (cdr lst) (cons (car lst) yes) no))
      (else
       (helper (cdr lst) yes (cons (car lst) no)))))
  (helper lst '() '()))

(define (rotate n lst)
  "Rotate list left by n positions"
  (let ((len (length lst)))
    (if (zero? len)
        lst
        (let ((n-mod (modulo n len)))
          (append (drop n-mod lst)
                  (take n-mod lst))))))

(define (interleave lst1 lst2)
  "Interleave two lists"
  (cond
    ((null? lst1) lst2)
    ((null? lst2) lst1)
    (else (cons (car lst1)
                (cons (car lst2)
                      (interleave (cdr lst1) (cdr lst2)))))))

;; String utilities

(define (string-join lst separator)
  "Join list of strings with separator"
  (if (null? lst)
      ""
      (fold-left (lambda (acc s)
                   (if (string=? acc "")
                       s
                       (string-append acc separator s)))
                 ""
                 lst)))

;; Math utilities

(define (sum lst)
  "Sum of list"
  (fold-left + 0 lst))

(define (product lst)
  "Product of list"
  (fold-left * 1 lst))

(define (average lst)
  "Average of list"
  (/ (sum lst) (length lst)))

(define (square x)
  "Square of number"
  (* x x))

(define (cube x)
  "Cube of number"
  (* x x x))

(define (even? n)
  "Check if number is even"
  (= (modulo n 2) 0))

(define (odd? n)
  "Check if number is odd"
  (not (even? n)))

;; Function utilities

(define (compose . fns)
  "Compose multiple functions"
  (if (null? fns)
      (lambda (x) x)
      (let ((fn (car fns))
            (rest (apply compose (cdr fns))))
        (lambda (x) (fn (rest x))))))

(define (identity x)
  "Identity function"
  x)

(define (const x)
  "Constant function"
  (lambda (_) x))

;; Demo
(display "=== Utils Library Demo ===\n\n")

(display "Range 1-10: ")
(display (range 1 10))
(newline)

(display "Take 3 from (1 2 3 4 5): ")
(display (take 3 '(1 2 3 4 5)))
(newline)

(display "Zip (1 2 3) (a b c): ")
(display (zip '(1 2 3) '(a b c)))
(newline)

(display "Flatten ((1 2) (3 (4 5)) 6): ")
(display (flatten '((1 2) (3 (4 5)) 6)))
(newline)

(display "Unique (1 2 2 3 3 3 4): ")
(display (unique '(1 2 2 3 3 3 4)))
(newline)

(display "Group by (mod 3): ")
(display (group-by (lambda (x) (modulo x 3)) '(1 2 3 4 5 6 7 8 9)))
(newline)

(display "Partition even: \n")
(call-with-values
    (lambda () (partition even? '(1 2 3 4 5 6)))
  (lambda (evens odds)
    (display "  Evens: ")
    (display evens)
    (newline)
    (display "  Odds: ")
    (display odds)
    (newline)))

(display "Rotate 2 (1 2 3 4 5): ")
(display (rotate 2 '(1 2 3 4 5)))
(newline)

(display "Interleave (1 2 3) (a b c): ")
(display (interleave '(1 2 3) '(a b c)))
(newline)

(display "\nString join: ")
(display (string-join '("hello" "world" "scheme") " "))
(newline)

(display "\nSum (1 2 3 4 5): ")
(display (sum '(1 2 3 4 5)))
(newline)

(display "Average (1 2 3 4 5): ")
(display (average '(1 2 3 4 5)))
(newline)

(display "\nCompose (add1 . double): ")
(define add1 (lambda (x) (+ x 1)))
(define double (lambda (x) (* x 2)))
(define add1-then-double (compose double add1))
(display (add1-then-double 5))
(newline)
