;; Fennel programming language examples
;; Lisp that compiles to Lua

(print "")
(print "=== Fennel Examples ===")
(print "")

;; Variables
(local name "Alice")
(local age 30)
(print (.. "Name: " name ", Age: " (tostring age)))

;; Global variables
(global app-version "1.0.0")

;; Tables (Lua tables)
(local numbers [1 2 3 4 5])
(local colors ["red" "green" "blue"])
(print "Numbers:" (table.concat numbers ", "))

;; Functions
(fn factorial [n]
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(print "Factorial of 5:" (factorial 5))

;; Lambda functions
(local square (fn [x] (* x x)))
(print "Square of 7:" (square 7))

;; Lambdas with shorthand
(lambda add-ten [x] (+ x 10))

;; Let binding
(let [a 5
      b 3]
  (print "Sum:" (+ a b)))

;; Multiple values
(fn get-stats [nums]
  (values (math.min (unpack nums))
          (math.max (unpack nums))))

(local (min-val max-val) (get-stats numbers))
(print "Min:" min-val "Max:" max-val)

;; Conditionals
(if (> age 18)
    (print "Adult")
    (print "Minor"))

;; When
(when (> age 18)
  (print "Can vote")
  (print "Can drive"))

;; Match (pattern matching)
(match age
  18 (print "Just became adult")
  (where x (> x 18)) (print "Adult")
  _ (print "Minor"))

;; Each (iteration)
(each [i v (ipairs colors)]
  (print i v))

;; For numeric loop
(for [i 1 5]
  (print "i =" i))

;; While loop
(var counter 0)
(while (< counter 5)
  (print "Counter:" counter)
  (set counter (+ counter 1)))

;; Tables/Maps
(local person {:name "Bob"
               :age 30
               :city "NYC"})

(print "Person name:" person.name)

;; Table with functions (methods)
(local calculator
  {:add (fn [self a b] (+ a b))
   :multiply (fn [self a b] (* a b))})

(print "Add: " (calculator:add 5 3))

;; Destructuring
(let [{:name n :age a} person]
  (print "Name:" n "Age:" a))

(let [[first second & rest] numbers]
  (print "First:" first "Second:" second "Rest:" rest))

;; Macros
(macro when-not [test & body]
  `(if (not ,test)
     (do ,...body)))

(when-not (< age 18)
  (print "Not a minor"))

;; Threading macro
(-> 5
    (+ 3)
    (* 2)
    (- 1)
    print)

(->> numbers
     (icollect [_ v (ipairs $1)] (* v v))
     print)

;; icollect (map)
(local squares
  (icollect [_ v (ipairs numbers)]
    (* v v)))

(print "Squares:" (table.concat squares ", "))

;; collect (table comprehension)
(local user-map
  (collect [_ name (ipairs ["Alice" "Bob" "Charlie"])]
    name
    true))

;; accumulate (reduce)
(local sum
  (accumulate [total 0
               _ v (ipairs numbers)]
    (+ total v)))

(print "Sum:" sum)

;; doto (method chaining)
(local t {})
(doto t
  (tset :name "Alice")
  (tset :age 25))

;; partial application
(fn add [a b] (+ a b))
(local add5 (partial add 5))
(print "5 + 3:" (add5 3))

;; Length operator
(print "Length:" (length numbers))

;; Table operations
(table.insert numbers 6)
(print "After insert:" (table.concat numbers ", "))

;; pcall (protected call)
(local (success result) (pcall (fn [] (/ 10 0))))
(if success
    (print "Result:" result)
    (print "Error occurred"))

;; require and modules
;; (local mylib (require :mylib))

;; Fennel-specific features

;; pick-values (select from multiple returns)
(fn returns-many []
  (values 1 2 3 4 5))

(local (a b) (pick-values 2 (returns-many)))
(print "Picked:" a b)

;; pick-args (select arguments)
(fn variadic [...]
  (local args [...])
  (print "Got" (length args) "arguments"))

(variadic 1 2 3 4)

;; include-str (compile-time file inclusion)
;; (local config (include-str "config.txt"))

;; eval-compiler (compile-time evaluation)
;; (eval-compiler
;;   (print "This runs at compile time"))

;; import-macros
;; (import-macros {: my-macro} :my-macros)

;; remap (rename on require)
;; (local {: old-name : new-name} (require :module))

;; Lua interop
(print "Lua version:" _VERSION)

;; Metatables
(local mt {:__index {:default 0}})
(local t (setmetatable {} mt))
(print "Metatable test:" t.default)

;; Coroutines
(local co (coroutine.create
            (fn []
              (for [i 1 3]
                (print "Coroutine:" i)
                (coroutine.yield i)))))

(coroutine.resume co)
(coroutine.resume co)

;; String operations
(local text "hello world")
(print "Uppercase:" (string.upper text))
(print "Find:" (string.find text "world"))
(print "Substring:" (string.sub text 1 5))

;; Math operations
(print "Random:" (math.random 1 10))
(print "Floor:" (math.floor 3.7))
(print "Ceil:" (math.ceil 3.2))

;; Closures
(fn make-counter []
  (var count 0)
  (fn []
    (set count (+ count 1))
    count))

(local counter (make-counter))
(print "Counter:" (counter))
(print "Counter:" (counter))

(print "")
(print "Fennel examples complete!")
