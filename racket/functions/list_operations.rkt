#lang racket
;; List operations in Racket

(displayln "\n=== List Operations in Racket ===\n")

;; Create lists
(define numbers '(1 2 3 4 5))
(define fruits '("apple" "banana" "cherry"))

(displayln (format "Numbers: ~a" numbers))
(displayln (format "Fruits: ~a" fruits))

;; List length
(displayln (format "\nLength: ~a" (length numbers)))

;; Car and cdr
(displayln (format "First (car): ~a" (car numbers)))
(displayln (format "Rest (cdr): ~a" (cdr numbers)))

;; Cons
(displayln (format "\nCons 0: ~a" (cons 0 numbers)))

;; Append
(displayln (format "Append: ~a" (append numbers '(6 7 8))))

;; Map
(displayln (format "\nMap (square): ~a"
                   (map (λ (x) (* x x)) numbers)))

;; Filter
(displayln (format "Filter (even): ~a"
                   (filter even? numbers)))

;; Foldl (reduce)
(displayln (format "\nFoldl (sum): ~a"
                   (foldl + 0 numbers)))

;; Reverse
(displayln (format "Reverse: ~a" (reverse numbers)))

;; Member
(displayln (format "\nMember 3: ~a" (member 3 numbers)))
(displayln (format "Member 10: ~a" (member 10 numbers)))

;; Take and drop
(displayln (format "\nTake 3: ~a" (take numbers 3)))
(displayln (format "Drop 2: ~a" (drop numbers 2)))

;; Sort
(define unsorted '(5 2 8 1 9 3))
(displayln (format "\nSorted: ~a" (sort unsorted <)))

;; Remove duplicates
(define with-dupes '(1 2 2 3 3 3 4))
(displayln (format "Remove-duplicates: ~a"
                   (remove-duplicates with-dupes)))

;; Range
(displayln (format "\nRange 1-10: ~a" (range 1 11)))

;; List comprehension
(define squares
  (for/list ([x (range 1 6)])
    (* x x)))
(displayln (format "Squares: ~a" squares))

;; Ormap and andmap
(displayln (format "\nOrmap (> 4): ~a"
                   (ormap (λ (x) (> x 4)) numbers)))
(displayln (format "Andmap (> 0): ~a"
                   (andmap (λ (x) (> x 0)) numbers)))
