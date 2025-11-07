;;;; List utilities library for Common Lisp
;;;; Reusable functions for list manipulation

(defpackage :list-utils
  (:use :cl)
  (:export
   #:range
   #:take
   #:drop
   #:take-while
   #:drop-while
   #:chunk
   #:flatten
   #:zip
   #:unzip
   #:cartesian-product
   #:group-by
   #:partition
   #:rotate
   #:interleave
   #:frequencies
   #:unique
   #:indexed))

(in-package :list-utils)

(defun range (start end &optional (step 1))
  "Generate list of numbers from start to end (exclusive) with step"
  (loop for i from start below end by step
        collect i))

(defun take (n list)
  "Take first n elements from list"
  (if (or (<= n 0) (null list))
      nil
      (cons (car list)
            (take (1- n) (cdr list)))))

(defun drop (n list)
  "Drop first n elements from list"
  (if (<= n 0)
      list
      (drop (1- n) (cdr list))))

(defun take-while (predicate list)
  "Take elements while predicate is true"
  (if (or (null list) (not (funcall predicate (car list))))
      nil
      (cons (car list)
            (take-while predicate (cdr list)))))

(defun drop-while (predicate list)
  "Drop elements while predicate is true"
  (cond
    ((null list) nil)
    ((funcall predicate (car list))
     (drop-while predicate (cdr list)))
    (t list)))

(defun chunk (n list)
  "Split list into chunks of size n"
  (if (null list)
      nil
      (cons (take n list)
            (chunk n (drop n list)))))

(defun flatten (tree)
  "Flatten nested list structure"
  (cond
    ((null tree) nil)
    ((atom tree) (list tree))
    (t (append (flatten (car tree))
               (flatten (cdr tree))))))

(defun zip (&rest lists)
  "Zip multiple lists together"
  (if (some #'null lists)
      nil
      (cons (mapcar #'car lists)
            (apply #'zip (mapcar #'cdr lists)))))

(defun unzip (list-of-lists)
  "Unzip list of lists into separate lists"
  (if (null list-of-lists)
      nil
      (let ((first-elements (mapcar #'car list-of-lists))
            (rest-elements (mapcar #'cdr list-of-lists)))
        (cons first-elements
              (when (some #'identity rest-elements)
                (unzip rest-elements))))))

(defun cartesian-product (list1 list2)
  "Compute cartesian product of two lists"
  (loop for x in list1
        append (loop for y in list2
                    collect (list x y))))

(defun group-by (key-fn list)
  "Group list elements by key function"
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (item list)
      (let ((key (funcall key-fn item)))
        (push item (gethash key groups))))
    (loop for key being the hash-keys of groups
          collect (cons key (reverse (gethash key groups))))))

(defun partition (predicate list)
  "Partition list into two lists based on predicate"
  (let ((true-list '())
        (false-list '()))
    (dolist (item list)
      (if (funcall predicate item)
          (push item true-list)
          (push item false-list)))
    (values (reverse true-list) (reverse false-list))))

(defun rotate (n list)
  "Rotate list left by n positions"
  (let ((len (length list)))
    (if (zerop len)
        list
        (let ((n-mod (mod n len)))
          (append (drop n-mod list)
                  (take n-mod list))))))

(defun interleave (list1 list2)
  "Interleave two lists"
  (cond
    ((null list1) list2)
    ((null list2) list1)
    (t (cons (car list1)
             (cons (car list2)
                   (interleave (cdr list1) (cdr list2)))))))

(defun frequencies (list)
  "Count frequency of each element"
  (let ((freq (make-hash-table :test 'equal)))
    (dolist (item list)
      (incf (gethash item freq 0)))
    (loop for key being the hash-keys of freq
          collect (cons key (gethash key freq)))))

(defun unique (list)
  "Remove duplicates preserving order"
  (let ((seen (make-hash-table :test 'equal))
        (result '()))
    (dolist (item list)
      (unless (gethash item seen)
        (setf (gethash item seen) t)
        (push item result)))
    (reverse result)))

(defun indexed (list)
  "Return list of (index . element) pairs"
  (loop for item in list
        for i from 0
        collect (cons i item)))

;; Demo usage
(defun demo ()
  "Demonstrate list utilities"
  (format t "~%=== List Utils Library Demo ===~%")

  (format t "~%Range (1, 10):~%  ~A~%" (range 1 10))

  (format t "~%Take 3 from (1 2 3 4 5):~%  ~A~%"
          (take 3 '(1 2 3 4 5)))

  (format t "~%Drop 2 from (1 2 3 4 5):~%  ~A~%"
          (drop 2 '(1 2 3 4 5)))

  (format t "~%Take while (< x 5):~%  ~A~%"
          (take-while (lambda (x) (< x 5)) '(1 2 3 4 5 6 7)))

  (format t "~%Chunk 3 (1..10):~%  ~A~%"
          (chunk 3 (range 1 11)))

  (format t "~%Flatten ((1 2) (3 (4 5)) 6):~%  ~A~%"
          (flatten '((1 2) (3 (4 5)) 6)))

  (format t "~%Zip (1 2 3) (a b c):~%  ~A~%"
          (zip '(1 2 3) '(a b c)))

  (format t "~%Cartesian product (1 2) (a b):~%  ~A~%"
          (cartesian-product '(1 2) '(a b)))

  (format t "~%Group by (mod 3):~%  ~A~%"
          (group-by (lambda (x) (mod x 3)) '(1 2 3 4 5 6 7 8 9)))

  (format t "~%Partition even:~%")
  (multiple-value-bind (evens odds)
      (partition #'evenp '(1 2 3 4 5 6))
    (format t "  Evens: ~A~%" evens)
    (format t "  Odds: ~A~%" odds))

  (format t "~%Rotate 2 (1 2 3 4 5):~%  ~A~%"
          (rotate 2 '(1 2 3 4 5)))

  (format t "~%Interleave (1 2 3) (a b c):~%  ~A~%"
          (interleave '(1 2 3) '(a b c)))

  (format t "~%Frequencies (a b a c b a):~%  ~A~%"
          (frequencies '(a b a c b a)))

  (format t "~%Unique (1 2 2 3 3 3 4):~%  ~A~%"
          (unique '(1 2 2 3 3 3 4)))

  (format t "~%Indexed (a b c):~%  ~A~%"
          (indexed '(a b c))))

;; Run demo
(demo)
