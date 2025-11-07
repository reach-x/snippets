#!/usr/bin/env sbcl --script
;;;; File statistics script
;;;; Reads a file and provides various statistics

(defun count-lines (filename)
  "Count number of lines in file"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          count t)))

(defun count-words (filename)
  "Count number of words in file"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          sum (length (split-string line #\Space)))))

(defun count-chars (filename)
  "Count number of characters in file"
  (with-open-file (stream filename)
    (loop for char = (read-char stream nil)
          while char
          count t)))

(defun split-string (string delimiter)
  "Split string by delimiter"
  (loop with len = (length string)
        for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        for part = (subseq string start (or end len))
        unless (string= part "")
        collect part
        until (null end)))

(defun word-frequency (filename)
  "Count frequency of each word"
  (let ((freq (make-hash-table :test 'equal)))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil)
            while line
            do (loop for word in (split-string line #\Space)
                    do (incf (gethash (string-downcase word) freq 0)))))
    freq))

(defun top-words (filename n)
  "Get top N most frequent words"
  (let ((freq (word-frequency filename))
        (words '()))
    (maphash (lambda (word count)
               (push (cons word count) words))
             freq)
    (subseq (sort words #'> :key #'cdr) 0 (min n (length words)))))

(defun longest-line-length (filename)
  "Find length of longest line"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          maximize (length line))))

(defun print-stats (filename)
  "Print file statistics"
  (format t "~%=== File Statistics: ~A ===~%" filename)
  (format t "Lines: ~D~%" (count-lines filename))
  (format t "Words: ~D~%" (count-words filename))
  (format t "Characters: ~D~%" (count-chars filename))
  (format t "Longest line: ~D characters~%" (longest-line-length filename))

  (format t "~%Top 10 most frequent words:~%")
  (loop for (word . count) in (top-words filename 10)
        do (format t "  ~A: ~D~%" word count)))

(defun create-sample-file ()
  "Create a sample file for testing"
  (let ((filename "../tmp/sample.txt"))
    (ensure-directories-exist "../tmp/")
    (with-open-file (stream filename
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "Hello World~%")
      (format stream "This is a sample file~%")
      (format stream "With multiple lines~%")
      (format stream "For testing Lisp~%")
      (format stream "File processing capabilities~%")
      (format stream "Hello again~%"))
    filename))

;; Main execution
(defun main ()
  "Main function"
  (format t "~%Creating sample file...~%")
  (let ((filename (create-sample-file)))
    (format t "Sample file created: ~A~%~%" filename)
    (print-stats filename)))

;; Run main if script
(main)
