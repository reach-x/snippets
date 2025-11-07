#!/usr/bin/env clojure
;; Collection operations in Clojure

(println "\n=== Collection Operations in Clojure ===\n")

;; Vectors
(def numbers [1 2 3 4 5])
(def fruits ["apple" "banana" "cherry"])

(println "Numbers:" numbers)
(println "Fruits:" fruits)

;; Vector operations
(println "\nCount:" (count numbers))
(println "First:" (first numbers))
(println "Last:" (last numbers))
(println "nth (2):" (nth numbers 2))

;; Conj (add to end for vectors)
(println "\nConj 6:" (conj numbers 6))

;; Concat
(println "Concat:" (concat numbers [6 7 8]))

;; Map
(println "\nMap (square):" (map #(* % %) numbers))

;; Filter
(println "Filter (even):" (filter even? numbers))

;; Reduce
(println "\nReduce (sum):" (reduce + numbers))
(println "Reduce (product):" (reduce * numbers))

;; Take and drop
(println "\nTake 3:" (take 3 numbers))
(println "Drop 2:" (drop 2 numbers))

;; Sort
(def unsorted [5 2 8 1 9 3])
(println "\nSorted:" (sort unsorted))
(println "Sorted (reverse):" (sort > unsorted))

;; Reverse
(println "Reversed:" (reverse numbers))

;; Distinct
(def with-dupes [1 2 2 3 3 3 4])
(println "\nDistinct:" (distinct with-dupes))

;; Lists
(def lst '(1 2 3 4 5))
(println "\nList:" lst)

;; Maps (hash maps)
(def person {:name "Alice" :age 30 :city "NYC"})
(println "\nPerson:" person)
(println "Name:" (:name person))
(println "Age:" (get person :age))

;; Sets
(def nums #{1 2 3 4 5})
(println "\nSet:" nums)
(println "Contains 3:" (contains? nums 3))

;; Range
(println "\nRange 1-10:" (range 1 11))

;; Sequence comprehension
(def squares (for [x (range 1 6)] (* x x)))
(println "Squares:" squares)

;; Threading macros
(println "\nThreading:")
(println (->> numbers
             (map #(* % %))
             (filter even?)
             (reduce +)))
