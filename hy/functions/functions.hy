#!/usr/bin/env hy
; Hy programming language examples
; Lisp that compiles to Python AST

(print "")
(print "=== Hy Examples ===")
(print "")

; Variables
(setv name "Alice")
(setv age 30)
(print (+ "Name: " name ", Age: " (str age)))

; Lists
(setv numbers [1 2 3 4 5])
(setv colors ["red" "green" "blue"])
(print "Numbers:" numbers)

; Functions
(defn factorial [n]
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(print "Factorial of 5:" (factorial 5))

; Anonymous functions
(setv square (fn [x] (* x x)))
(print "Square of 7:" (square 7))

; Lambda shorthand
(setv add-ten (fn [x] (+ x 10)))
(print "10 + 10:" (add-ten 10))

; Map and filter
(print "Squares:" (list (map square numbers)))
(print "Evens:" (list (filter even? numbers)))

; List comprehensions
(setv squares (lfor x numbers (* x x)))
(print "List comp squares:" squares)

; Conditionals
(if (> age 18)
    (print "Adult")
    (print "Minor"))

; Cond (multi-condition)
(cond
  [(< age 13) (print "Child")]
  [(< age 18) (print "Teen")]
  [True (print "Adult")])

; When/unless
(when (> age 18)
  (print "Can vote"))

(unless (< age 18)
  (print "Not a minor"))

; Let binding
(let [a 5
      b 3]
  (print "Sum:" (+ a b)))

; Do block (multiple expressions)
(do
  (setv x 10)
  (setv y 20)
  (print "Product:" (* x y)))

; For loop
(for [i (range 5)]
  (print "i =" i))

; While loop
(setv counter 0)
(while (< counter 5)
  (print "Counter:" counter)
  (setv counter (+ counter 1)))

; Doseq (iterate over sequence)
(doseq [color colors]
  (print "Color:" color))

; Macros
(defmacro unless [test #* body]
  `(if (not ~test)
     (do ~@body)))

; Threading macros
(-> 5
    (+ 3)
    (* 2)
    (- 1)
    print)

(->> [1 2 3 4 5]
     (map square)
     (filter even?)
     list
     print)

; Dictionaries
(setv person {"name" "Bob"
              "age" 30
              "city" "NYC"})

(print "Person name:" (get person "name"))

; Assoc (add to dict)
(assoc person "email" "bob@example.com")

; Classes
(defclass Person []
  (defn __init__ [self name age]
    (setv self.name name)
    (setv self.age age))

  (defn greet [self]
    (print (+ "Hello, I'm " self.name))))

(setv alice (Person "Alice" 25))
(.greet alice)

; Method calls
(print (.upper "hello world"))
(print (.split "one,two,three" ","))

; Python interop
(import json)
(setv data {"key" "value" "number" 42})
(print "JSON:" (json.dumps data))

(import datetime)
(print "Now:" (datetime.datetime.now))

; List operations
(print "First:" (first colors))
(print "Rest:" (rest colors))
(print "Last:" (last numbers))

; Slicing
(print "Slice [1:3]:" (cut numbers 1 3))

; Reduce
(import functools [reduce])
(print "Sum:" (reduce + numbers 0))

; With (context manager)
; (with [f (open "file.txt" "w")]
;   (.write f "Hello, Hy!"))

; Try/except
(try
  (/ 10 0)
  (except [ZeroDivisionError]
    (print "Cannot divide by zero")))

; Decorators
(defn my-decorator [func]
  (fn [#* args #** kwargs]
    (print "Before function")
    (setv result (func #* args #** kwargs))
    (print "After function")
    result))

(with-decorator my-decorator
  (defn say-hello []
    (print "Hello!")))

; List operations
(print "Concatenate:" (+ [1 2 3] [4 5 6]))
(print "Repeat:" (* [1 2] 3))

; Set operations
(setv set1 #{1 2 3 4})
(setv set2 #{3 4 5 6})
(print "Union:" (| set1 set2))
(print "Intersection:" (& set1 set2))

; String operations
(setv text "hello world")
(print "Uppercase:" (.upper text))
(print "Replace:" (.replace text "world" "Hy"))
(print "Split:" (.split text))

; Generators
(defn count-up-to [n]
  (for [i (range n)]
    (yield i)))

(print "Generator:" (list (count-up-to 5)))

; Multiple return values
(defn get-stats [nums]
  (, (min nums) (max nums) (sum nums)))

(setv [minimum maximum total] (get-stats numbers))
(print "Min:" minimum "Max:" maximum "Total:" total)

; Keyword arguments
(defn greet [name #** kwargs]
  (print (+ "Hello, " name))
  (when (in "title" kwargs)
    (print "Title:" (get kwargs "title"))))

(greet "Alice" :title "Dr.")

; Recursive tail calls
(defn factorial-tail [n acc]
  (if (<= n 1)
      acc
      (factorial-tail (- n 1) (* n acc))))

(print "Tail factorial:" (factorial-tail 5 1))

; Pattern matching (via match in Hy 1.0+)
; (match value
;   [0] "zero"
;   [1] "one"
;   [_] "other")

(print "")
(print "Hy examples complete!")
