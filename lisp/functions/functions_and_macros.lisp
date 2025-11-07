;;;; Functions, closures, and macros in Common Lisp

;; Simple function
(defun greet (name)
  "Simple greeting function"
  (format nil "Hello, ~A!" name))

;; Function with multiple return values
(defun divide-with-remainder (dividend divisor)
  "Return quotient and remainder"
  (values (floor dividend divisor)
          (mod dividend divisor)))

;; Function with optional parameters
(defun make-greeting (name &optional (greeting "Hello"))
  "Greeting with optional parameter"
  (format nil "~A, ~A!" greeting name))

;; Function with keyword parameters
(defun make-person (&key name age city)
  "Create person plist with keyword parameters"
  (list :name name :age age :city city))

;; Function with rest parameters
(defun sum (&rest numbers)
  "Sum any number of arguments"
  (reduce #'+ numbers :initial-value 0))

;; Recursive function
(defun factorial (n)
  "Calculate factorial recursively"
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

;; Tail-recursive function
(defun factorial-tail (n &optional (acc 1))
  "Calculate factorial with tail recursion"
  (if (<= n 1)
      acc
      (factorial-tail (1- n) (* n acc))))

;; Higher-order function
(defun apply-twice (f x)
  "Apply function f twice to x"
  (funcall f (funcall f x)))

;; Function returning function (closure)
(defun make-adder (n)
  "Return function that adds n to its argument"
  (lambda (x) (+ x n)))

;; Closure with mutable state
(defun make-counter (&optional (initial 0))
  "Return function that increments and returns counter"
  (let ((count initial))
    (lambda ()
      (incf count))))

;; Function composition
(defun compose (f g)
  "Compose two functions: (f ∘ g)(x) = f(g(x))"
  (lambda (x) (funcall f (funcall g x))))

;; Lambda functions
(defun demo-lambdas ()
  "Demonstrate anonymous functions"
  (format t "~%=== Lambda Functions ===~%")

  ;; Simple lambda
  (let ((double (lambda (x) (* x 2))))
    (format t "Double 5: ~D~%" (funcall double 5)))

  ;; Lambda with mapcar
  (format t "Map lambda: ~A~%"
          (mapcar (lambda (x) (* x x)) '(1 2 3 4 5)))

  ;; Lambda in filter
  (format t "Filter lambda: ~A~%"
          (remove-if-not (lambda (x) (> x 3)) '(1 2 3 4 5 6))))

;; Macros
(defmacro when-not (condition &body body)
  "Execute body when condition is false"
  `(if (not ,condition)
       (progn ,@body)))

(defmacro while (condition &body body)
  "While loop macro"
  `(loop while ,condition
         do (progn ,@body)))

(defmacro swap (a b)
  "Swap values of two variables"
  `(let ((temp ,a))
     (setf ,a ,b)
     (setf ,b temp)))

;; Demo function
(defun demo-functions ()
  "Demonstrate various function features"

  (format t "~%=== Functions in Common Lisp ===~%~%")

  ;; Simple function
  (format t "Greet: ~A~%" (greet "Alice"))

  ;; Multiple return values
  (format t "~%Multiple return values:~%")
  (multiple-value-bind (quotient remainder)
      (divide-with-remainder 17 5)
    (format t "17 / 5 = ~D remainder ~D~%" quotient remainder))

  ;; Optional parameters
  (format t "~%Optional parameters:~%")
  (format t "~A~%" (make-greeting "Bob"))
  (format t "~A~%" (make-greeting "Bob" "Hi"))

  ;; Keyword parameters
  (format t "~%Keyword parameters:~%")
  (format t "~A~%"
          (make-person :name "Charlie" :age 30 :city "NYC"))

  ;; Rest parameters
  (format t "~%Rest parameters:~%")
  (format t "Sum: ~D~%" (sum 1 2 3 4 5))

  ;; Recursion
  (format t "~%Recursion:~%")
  (format t "5! = ~D~%" (factorial 5))
  (format t "10! = ~D~%" (factorial-tail 10))

  ;; Higher-order functions
  (format t "~%Higher-order functions:~%")
  (let ((add1 (lambda (x) (+ x 1))))
    (format t "Apply-twice add1 to 5: ~D~%"
            (apply-twice add1 5)))

  ;; Closures
  (format t "~%Closures:~%")
  (let ((add5 (make-adder 5))
        (add10 (make-adder 10)))
    (format t "Add5 to 3: ~D~%" (funcall add5 3))
    (format t "Add10 to 3: ~D~%" (funcall add10 3)))

  ;; Closure with state
  (format t "~%Closure with state:~%")
  (let ((counter (make-counter)))
    (format t "Count: ~D~%" (funcall counter))
    (format t "Count: ~D~%" (funcall counter))
    (format t "Count: ~D~%" (funcall counter)))

  ;; Function composition
  (format t "~%Function composition:~%")
  (let ((add1 (lambda (x) (+ x 1)))
        (double (lambda (x) (* x 2))))
    (let ((double-then-add1 (compose add1 double)))
      (format t "Double then add1 to 5: ~D~%"
              (funcall double-then-add1 5))))

  ;; Lambda functions
  (demo-lambdas)

  ;; Macros
  (format t "~%=== Macros ===~%")
  (let ((x 10))
    (when-not (< x 5)
      (format t "x is not less than 5~%")))

  (format t "~%While loop:~%")
  (let ((i 0))
    (while (< i 3)
      (format t "  i = ~D~%" i)
      (incf i)))

  (format t "~%Swap:~%")
  (let ((a 1)
        (b 2))
    (format t "Before: a=~D, b=~D~%" a b)
    (swap a b)
    (format t "After: a=~D, b=~D~%" a b)))

;; Currying example
(defun curry (f arg)
  "Curry a binary function with first argument"
  (lambda (x) (funcall f arg x)))

;; Partial application
(defun partial (f &rest args)
  "Partially apply function with given arguments"
  (lambda (&rest more-args)
    (apply f (append args more-args))))

;; Fibonacci with memoization
(let ((memo (make-hash-table)))
  (defun fib-memo (n)
    "Fibonacci with memoization"
    (or (gethash n memo)
        (setf (gethash n memo)
              (if (< n 2)
                  n
                  (+ (fib-memo (- n 1))
                     (fib-memo (- n 2))))))))

;; Run demo
(demo-functions)

;; Test additional functions
(format t "~%=== Advanced Functions ===~%")

;; Currying
(let ((add (lambda (x y) (+ x y))))
  (let ((add5 (curry add 5)))
    (format t "Curried add 5 to 3: ~D~%" (funcall add5 3))))

;; Partial application
(let ((add (lambda (&rest nums) (reduce #'+ nums))))
  (let ((add-5-and-10 (partial add 5 10)))
    (format t "Partial add 5,10 to 3,7: ~D~%"
            (funcall add-5-and-10 3 7))))

;; Memoized fibonacci
(format t "Fibonacci (memoized):~%")
(dotimes (i 11)
  (format t "  fib(~D) = ~D~%" i (fib-memo i)))
