#lang racket
;; Functions in Racket

(displayln "\n=== Functions in Racket ===\n")

;; Simple function
(define (greet name)
  (format "Hello, ~a!" name))

(displayln (greet "Alice"))

;; Recursive function
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(displayln (format "\nFactorial 5: ~a" (factorial 5)))

;; Lambda functions
(define double (λ (x) (* x 2)))
(displayln (format "\nDouble 5: ~a" (double 5)))

;; Higher-order function
(define (apply-twice f x)
  (f (f x)))

(define (add1 x) (+ x 1))
(displayln (format "Apply-twice add1 to 5: ~a"
                   (apply-twice add1 5)))

;; Currying
(define (make-adder n)
  (λ (x) (+ x n)))

(define add5 (make-adder 5))
(displayln (format "\nAdd5 to 3: ~a" (add5 3)))

;; Function composition
(define (compose f g)
  (λ (x) (f (g x))))

(define add1-then-double (compose double add1))
(displayln (format "Compose (add1 then double) 5: ~a"
                   (add1-then-double 5)))

;; Let expressions
(let ([x 10]
      [y 20])
  (displayln (format "\nLet x+y: ~a" (+ x y))))

;; Structs
(struct point (x y) #:transparent)

(define p (point 3 4))
(displayln (format "\nPoint: ~a" p))
(displayln (format "Point x: ~a" (point-x p)))
(displayln (format "Point y: ~a" (point-y p)))
