; Arc programming language examples
; Minimalist Lisp dialect by Paul Graham

; Variables
(= x 10)
(= name "Alice")
(prn "Name: " name ", X: " x)

; Lists
(= numbers '(1 2 3 4 5))
(= colors (list "red" "green" "blue"))

; Functions
(def factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(prn "Factorial of 5: " (factorial 5))

; Anonymous functions
(= square (fn (x) (* x x)))
(prn "Square of 7: " (square 7))

; Shorthand function syntax
(= add-ten [+ _ 10])
(prn "10 + 10: " (add-ten 10))

; Map and filter
(prn "Squares: " (map square '(1 2 3 4 5)))
(prn "Evens: " (keep even '(1 2 3 4 5 6)))

; Each (iteration)
(each x '(1 2 3)
  (prn "Number: " x))

; For loop
(for i 1 5
  (prn "i = " i))

; Conditionals
(if (> x 5)
    (prn "Greater than 5")
    (prn "Not greater than 5"))

; Case statement
(= day "Monday")
(case day
  "Monday"    (prn "Start of week")
  "Friday"    (prn "End of week")
              (prn "Midweek"))

; Let binding
(let (a 5
      b 3)
  (prn "Sum: " (+ a b)))

; With binding
(with (x 10
       y 20)
  (prn "Product: " (* x y)))

; Macros
(mac when (test . body)
  `(if ,test (do ,@body)))

(when (> x 5)
  (prn "x is greater than 5")
  (prn "This is inside when"))

; Tables (hash tables)
(= person (table))
(= (person 'name) "Bob")
(= (person 'age) 30)
(prn "Person name: " (person 'name))

; Objects (using tables)
(= point (obj x 10 y 20))
(prn "Point: " point!x ", " point!y)

; Strings
(= str "hello world")
(prn "Uppercase: " (upcase str))
(prn "Length: " (len str))

; String concatenation
(prn "Concat: " (+ "Hello" " " "World"))

; List operations
(prn "First: " (car colors))
(prn "Rest: " (cdr colors))
(prn "Length: " (len colors))

; Append
(= extended (+ numbers '(6 7 8)))
(prn "Extended: " extended)

; Reduce
(prn "Sum: " (apply + numbers))

; Sorting
(prn "Sorted: " (sort < '(5 2 8 1 9)))

; Web programming (Arc's strength)
; (defop hello req
;   (w/link (hello)
;     "Click me!")
;   "Hello, World!")

; Compositions
(def compose (f g)
  (fn args (f (apply g args))))

(= add1 [+ _ 1])
(= mul2 [* _ 2])
(= add1-then-mul2 (compose mul2 add1))
(prn "Composed: " (add1-then-mul2 5))

; Association lists
(= alist '((name "Alice") (age 25) (city "NYC")))
(prn "Assoc: " (alref alist 'name))

; Memoization
(def memo (f)
  (with (cache (table))
    (fn args
      (or (cache args)
          (= (cache args) (apply f args))))))

; Error handling
(on-err (fn (ex) (prn "Error: " ex))
        (fn () (/ 10 0)))

; File I/O
; (w/outfile f "output.txt"
;   (write "Hello, File!" f))
;
; (w/infile f "input.txt"
;   (whilet line (readline f)
;     (prn line)))

; Threading macro
(def thread-first (x . forms)
  (if (no forms)
      x
      (let (form . rest) forms
        (apply thread-first
               (if (acons form)
                   (cons (car form) (cons x (cdr form)))
                   (list form x))
               rest))))

; Range
(def range (start end)
  (if (> start end)
      nil
      (cons start (range (+ start 1) end))))

(prn "Range: " (range 1 5))

; Lazy evaluation
(def lazy-seq (f)
  (fn () (f)))

; Arc is particularly elegant for web apps
(prn "Arc examples complete!")
