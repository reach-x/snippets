#!/usr/bin/env scheme
;;; Functions and closures in Scheme

(display "=== Functions and Closures in Scheme ===\n\n")

;; Simple function
(define (greet name)
  (string-append "Hello, " name "!"))

(display "Greet: ")
(display (greet "Alice"))
(newline)

;; Recursive function
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(display "\nFactorial 5: ")
(display (factorial 5))
(newline)

;; Tail-recursive function
(define (factorial-tail n)
  (define (iter n acc)
    (if (<= n 1)
        acc
        (iter (- n 1) (* n acc))))
  (iter n 1))

(display "Factorial (tail) 10: ")
(display (factorial-tail 10))
(newline)

;; Higher-order function
(define (apply-twice f x)
  (f (f x)))

(define (add1 x) (+ x 1))

(display "\nApply-twice add1 to 5: ")
(display (apply-twice add1 5))
(newline)

;; Function returning function (closure)
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(define add10 (make-adder 10))

(display "\nClosures:\n")
(display "Add5 to 3: ")
(display (add5 3))
(newline)

(display "Add10 to 3: ")
(display (add10 3))
(newline)

;; Closure with mutable state
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define counter (make-counter))

(display "\nCounter with state:\n")
(display "Count: ")
(display (counter))
(newline)

(display "Count: ")
(display (counter))
(newline)

(display "Count: ")
(display (counter))
(newline)

;; Function composition
(define (compose f g)
  (lambda (x) (f (g x))))

(define (double x) (* x 2))
(define double-then-add1 (compose add1 double))

(display "\nComposition:\n")
(display "Double then add1 to 5: ")
(display (double-then-add1 5))
(newline)

;; Currying
(define (curry f)
  (lambda (x)
    (lambda (y)
      (f x y))))

(define curried-add (curry +))
(define add-5 (curried-add 5))

(display "\nCurrying:\n")
(display "Curried add 5 to 3: ")
(display (add-5 3))
(newline)

;; Fibonacci with memoization
(define fibonacci
  (let ((memo '()))
    (lambda (n)
      (let ((cached (assoc n memo)))
        (if cached
            (cdr cached)
            (let ((result
                   (if (< n 2)
                       n
                       (+ (fibonacci (- n 1))
                          (fibonacci (- n 2))))))
              (set! memo (cons (cons n result) memo))
              result))))))

(display "\nFibonacci (memoized):\n")
(do ((i 0 (+ i 1)))
    ((> i 10))
  (display "fib(")
  (display i)
  (display ") = ")
  (display (fibonacci i))
  (newline))

;; Lambda functions
(display "\n=== Lambda Functions ===\n")

(display "Map lambda: ")
(display (map (lambda (x) (* x x)) '(1 2 3 4 5)))
(newline)

(display "Filter lambda: ")
(display (filter (lambda (x) (> x 3)) '(1 2 3 4 5 6)))
(newline)

;; Named let (iteration)
(display "\n=== Named Let ===\n")
(display "Sum 1-10 with named let: ")
(display
 (let loop ((i 1) (sum 0))
   (if (> i 10)
       sum
       (loop (+ i 1) (+ sum i)))))
(newline)

;; Multiple return values
(define (quotient-remainder dividend divisor)
  (values (quotient dividend divisor)
          (remainder dividend divisor)))

(display "\n=== Multiple Values ===\n")
(call-with-values
    (lambda () (quotient-remainder 17 5))
  (lambda (q r)
    (display "17 / 5 = ")
    (display q)
    (display " remainder ")
    (display r)
    (newline)))

;; Variadic functions
(define (sum-all . numbers)
  (fold-left + 0 numbers))

(display "\nVariadic function:\n")
(display "Sum-all 1 2 3 4 5: ")
(display (sum-all 1 2 3 4 5))
(newline)

;; Continuation passing style
(define (factorial-cps n cont)
  (if (<= n 1)
      (cont 1)
      (factorial-cps (- n 1)
                     (lambda (result)
                       (cont (* n result))))))

(display "\nContinuation-passing style:\n")
(display "Factorial-cps 5: ")
(factorial-cps 5 (lambda (result)
                   (display result)
                   (newline)))

;; Y-combinator (fixed point combinator)
(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (y) ((x x) y))))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

(define factorial-y
  (Y (lambda (f)
       (lambda (n)
         (if (<= n 1)
             1
             (* n (f (- n 1))))))))

(display "\nY-Combinator:\n")
(display "Factorial-y 5: ")
(display (factorial-y 5))
(newline)
