#!/usr/bin/env clojure
;; Functions in Clojure

(println "\n=== Functions in Clojure ===\n")

;; Simple function
(defn greet [name]
  (str "Hello, " name "!"))

(println (greet "Alice"))

;; Multiple arity
(defn power
  ([x] (* x x))
  ([x n] (Math/pow x n)))

(println "\nPower(3):" (power 3))
(println "Power(3, 3):" (power 3 3))

;; Recursive function
(defn factorial [n]
  (if (<= n 1)
    1
    (* n (factorial (dec n)))))

(println "\nFactorial 5:" (factorial 5))

;; Higher-order functions
(defn apply-twice [f x]
  (f (f x)))

(defn add1 [x] (+ x 1))

(println "\nApply-twice add1 to 5:" (apply-twice add1 5))

;; Anonymous functions
(println "Map (double):" (map #(* % 2) [1 2 3 4 5]))

;; Partial application
(def add5 (partial + 5))
(println "\nPartial add5 to 3:" (add5 3))

;; Composition
(def add1-then-double (comp #(* % 2) #(+ % 1)))
(println "Compose (add1 then double) 5:" (add1-then-double 5))

;; Let bindings
(let [x 10
      y 20
      sum (+ x y)]
  (println "\nLet binding sum:" sum))

;; Destructuring
(let [[a b c] [1 2 3]]
  (println "\nDestructure vector:" a b c))

(let [{:keys [name age]} {:name "Bob" :age 30}]
  (println "Destructure map:" name age))
