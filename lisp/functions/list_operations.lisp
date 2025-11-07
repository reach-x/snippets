;;;; List operations in Common Lisp
;;;; Lists are fundamental data structures in Lisp

(defun demo-list-operations ()
  "Demonstrate various list operations in Common Lisp"

  (format t "~%=== List Operations in Common Lisp ===~%~%")

  ;; Creating lists
  (let ((numbers '(1 2 3 4 5))
        (fruits '("apple" "banana" "cherry")))

    (format t "Numbers: ~A~%" numbers)
    (format t "Fruits: ~A~%" fruits)

    ;; List length
    (format t "~%Length: ~D~%" (length numbers))

    ;; First element (car)
    (format t "First element: ~D~%" (car numbers))
    (format t "First element (first): ~D~%" (first numbers))

    ;; Rest of list (cdr)
    (format t "Rest of list: ~A~%" (cdr numbers))
    (format t "Rest (rest): ~A~%" (rest numbers))

    ;; nth element (0-indexed)
    (format t "Third element (nth 2): ~D~%" (nth 2 numbers))

    ;; Prepend element (cons)
    (let ((new-list (cons 0 numbers)))
      (format t "~%Prepend 0: ~A~%" new-list))

    ;; Append lists
    (let ((combined (append numbers '(6 7 8))))
      (format t "Append (6 7 8): ~A~%" combined))

    ;; Reverse list
    (format t "Reversed: ~A~%" (reverse numbers))

    ;; Member (check if element in list)
    (format t "~%Member 3: ~A~%" (member 3 numbers))
    (format t "Member 10: ~A~%" (member 10 numbers))

    ;; Remove element
    (format t "Remove 3: ~A~%" (remove 3 numbers))

    ;; Remove duplicates
    (let ((with-dupes '(1 2 2 3 3 3 4 4)))
      (format t "~%With duplicates: ~A~%" with-dupes)
      (format t "Remove duplicates: ~A~%" (remove-duplicates with-dupes)))

    ;; Map function over list
    (format t "~%Map (square): ~A~%"
            (mapcar (lambda (x) (* x x)) numbers))

    ;; Filter list
    (format t "Filter (evens): ~A~%"
            (remove-if-not #'evenp numbers))

    (format t "Filter (odds): ~A~%"
            (remove-if #'evenp numbers))

    ;; Reduce (fold)
    (format t "~%Sum (reduce +): ~D~%"
            (reduce #'+ numbers))

    (format t "Product (reduce *): ~D~%"
            (reduce #'* numbers))

    ;; Find element
    (format t "~%Find (> 3): ~A~%"
            (find-if (lambda (x) (> x 3)) numbers))

    ;; Count elements matching predicate
    (format t "Count (> 3): ~D~%"
            (count-if (lambda (x) (> x 3)) numbers))

    ;; Every and some
    (format t "~%Every positive: ~A~%"
            (every #'plusp numbers))

    (format t "Some > 4: ~A~%"
            (some (lambda (x) (> x 4)) numbers))

    ;; Sort list
    (let ((unsorted '(5 2 8 1 9 3)))
      (format t "~%Unsorted: ~A~%" unsorted)
      (format t "Sorted: ~A~%" (sort (copy-list unsorted) #'<)))

    ;; Take first n elements
    (format t "~%First 3 (subseq): ~A~%"
            (subseq numbers 0 3))

    ;; Range of elements
    (format t "Elements 1-3 (subseq): ~A~%"
            (subseq numbers 1 4))

    ;; Association lists (alists)
    (format t "~%=== Association Lists ===~%")
    (let ((person '((name . "Alice")
                    (age . 30)
                    (city . "New York"))))
      (format t "Person: ~A~%" person)
      (format t "Name: ~A~%" (cdr (assoc 'name person)))
      (format t "Age: ~A~%" (cdr (assoc 'age person))))

    ;; Property lists (plists)
    (format t "~%=== Property Lists ===~%")
    (let ((book '(:title "The Book" :author "John Doe" :year 2024)))
      (format t "Book: ~A~%" book)
      (format t "Title: ~A~%" (getf book :title))
      (format t "Author: ~A~%" (getf book :author)))

    ;; Nested lists
    (format t "~%=== Nested Lists ===~%")
    (let ((matrix '((1 2 3)
                    (4 5 6)
                    (7 8 9))))
      (format t "Matrix: ~A~%" matrix)
      (format t "Second row: ~A~%" (second matrix))
      (format t "Element [1,1]: ~D~%" (nth 1 (nth 1 matrix))))

    ;; List comprehension (using loop)
    (format t "~%=== List Comprehension ===~%")
    (let ((squares (loop for i from 1 to 5 collect (* i i))))
      (format t "Squares 1-5: ~A~%" squares))

    (let ((evens (loop for i from 1 to 10
                      when (evenp i)
                      collect i)))
      (format t "Evens 1-10: ~A~%" evens))))

;; Helper functions

(defun list-sum (lst)
  "Calculate sum of list"
  (reduce #'+ lst :initial-value 0))

(defun list-product (lst)
  "Calculate product of list"
  (reduce #'* lst :initial-value 1))

(defun list-max (lst)
  "Find maximum in list"
  (reduce #'max lst))

(defun list-min (lst)
  "Find minimum in list"
  (reduce #'min lst))

(defun flatten-list (lst)
  "Flatten nested list"
  (cond
    ((null lst) nil)
    ((atom lst) (list lst))
    (t (append (flatten-list (car lst))
               (flatten-list (cdr lst))))))

;; Run demo
(demo-list-operations)

;; Test helper functions
(format t "~%=== Helper Functions ===~%")
(format t "Sum of (1 2 3 4 5): ~D~%" (list-sum '(1 2 3 4 5)))
(format t "Product of (1 2 3 4 5): ~D~%" (list-product '(1 2 3 4 5)))
(format t "Max of (5 2 8 1 9): ~D~%" (list-max '(5 2 8 1 9)))
(format t "Min of (5 2 8 1 9): ~D~%" (list-min '(5 2 8 1 9)))
(format t "Flatten ((1 2) (3 (4 5)) 6): ~A~%"
        (flatten-list '((1 2) (3 (4 5)) 6)))
