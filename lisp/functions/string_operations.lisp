;;;; String operations in Common Lisp

(defun demo-string-operations ()
  "Demonstrate various string operations in Common Lisp"

  (format t "~%=== String Operations in Common Lisp ===~%~%")

  ;; String concatenation
  (let ((str1 "Hello")
        (str2 "World"))
    (format t "Concatenate: ~A~%" (concatenate 'string str1 " " str2)))

  ;; String length
  (let ((text "Hello, Lisp!"))
    (format t "~%Text: ~A~%" text)
    (format t "Length: ~D~%" (length text))

    ;; String comparison
    (format t "~%String comparison:~%")
    (format t "  Equal to 'Hello, Lisp!': ~A~%"
            (string= text "Hello, Lisp!"))
    (format t "  Equal (case-insensitive): ~A~%"
            (string-equal text "hello, lisp!"))
    (format t "  Less than 'Zebra': ~A~%"
            (string< text "Zebra"))

    ;; Substring
    (format t "~%Substring [0,5]: ~A~%"
            (subseq text 0 5))
    (format t "Substring [7,11]: ~A~%"
            (subseq text 7 11))

    ;; String search
    (format t "~%Search 'Lisp': ~A~%"
            (search "Lisp" text))
    (format t "Search 'Python': ~A~%"
            (search "Python" text)))

  ;; Case conversion
  (let ((mixed "Hello World"))
    (format t "~%Mixed case: ~A~%" mixed)
    (format t "Uppercase: ~A~%" (string-upcase mixed))
    (format t "Lowercase: ~A~%" (string-downcase mixed))
    (format t "Capitalize: ~A~%" (string-capitalize mixed)))

  ;; String trim
  (let ((padded "  hello  "))
    (format t "~%Padded: '~A'~%" padded)
    (format t "Trim: '~A'~%" (string-trim " " padded))
    (format t "Trim left: '~A'~%" (string-left-trim " " padded))
    (format t "Trim right: '~A'~%" (string-right-trim " " padded)))

  ;; Character access
  (let ((text "Lisp"))
    (format t "~%Text: ~A~%" text)
    (format t "First char: ~A~%" (char text 0))
    (format t "Last char: ~A~%" (char text (1- (length text)))))

  ;; String to list and back
  (let ((text "Hello"))
    (format t "~%String to list: ~A~%"
            (coerce text 'list))
    (format t "List to string: ~A~%"
            (coerce '(#\H #\e #\l #\l #\o) 'string)))

  ;; Format strings (powerful string formatting)
  (format t "~%=== Format Strings ===~%")
  (format t "Name: ~A, Age: ~D~%" "Alice" 30)
  (format t "Float: ~,2F~%" 3.14159)
  (format t "Binary: ~B, Octal: ~O, Hex: ~X~%" 42 42 42)
  (format t "Right-aligned: ~10A~%" "test")
  (format t "Zero-padded: ~5,'0D~%" 42)

  ;; String building
  (format t "~%=== String Building ===~%")
  (let ((result (with-output-to-string (s)
                  (format s "Line 1~%")
                  (format s "Line 2~%")
                  (format s "Line 3"))))
    (format t "Built string:~%~A~%" result))

  ;; String splitting (custom function)
  (format t "~%=== String Splitting ===~%")
  (let ((csv "apple,banana,cherry"))
    (format t "CSV: ~A~%" csv)
    (format t "Split: ~A~%" (split-string csv #\,)))

  ;; String replacement
  (let ((text "Hello World"))
    (format t "~%Replace: ~A~%"
            (substitute #\_ #\Space text)))

  ;; Character predicates
  (format t "~%=== Character Predicates ===~%")
  (format t "Is 'A' alphabetic: ~A~%" (alpha-char-p #\A))
  (format t "Is '5' digit: ~A~%" (digit-char-p #\5))
  (format t "Is ' ' whitespace: ~A~%" (char= #\Space #\ ))
  (format t "Is 'a' lowercase: ~A~%" (lower-case-p #\a))
  (format t "Is 'A' uppercase: ~A~%" (upper-case-p #\A))

  ;; String reversal
  (let ((text "Hello"))
    (format t "~%Original: ~A~%" text)
    (format t "Reversed: ~A~%" (reverse text))))

;; Helper function for string splitting
(defun split-string (string delimiter)
  "Split string by delimiter character"
  (loop with len = (length string)
        for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end len))
        until (null end)))

;; Helper function to check if string contains substring
(defun string-contains-p (string substring)
  "Check if string contains substring"
  (not (null (search substring string))))

;; Helper function to count occurrences
(defun count-char (string char)
  "Count occurrences of character in string"
  (count char string))

;; Helper function to remove all occurrences
(defun remove-char (string char)
  "Remove all occurrences of character from string"
  (remove char string))

;; Helper function to replace substring
(defun replace-substring (string old new)
  "Replace all occurrences of old substring with new"
  (let ((old-len (length old))
        (new-len (length new)))
    (with-output-to-string (out)
      (loop for pos = 0 then (+ match old-len)
            for match = (search old string :start2 pos)
            do (write-string string out :start pos :end (or match (length string)))
            when match do (write-string new out)
            while match))))

;; Run demo
(demo-string-operations)

;; Test helper functions
(format t "~%=== Helper Functions ===~%")
(format t "Contains 'test': ~A~%"
        (string-contains-p "This is a test" "test"))
(format t "Count 'l' in 'Hello': ~D~%"
        (count-char "Hello" #\l))
(format t "Remove 'l' from 'Hello': ~A~%"
        (remove-char "Hello" #\l))
(format t "Replace 'World' with 'Lisp': ~A~%"
        (replace-substring "Hello World" "World" "Lisp"))
