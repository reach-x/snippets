;;; init.el --- Emacs Lisp configuration and examples

;;; Commentary:
;; This file demonstrates Emacs Lisp programming

;;; Code:

;; Basic settings
(setq inhibit-startup-screen t)
(setq visible-bell t)
(setq column-number-mode t)
(setq line-number-mode t)

;; Display line numbers
(global-display-line-numbers-mode t)

;; Variables
(defvar my-name "John Doe"
  "User's full name.")

(defvar my-email "john@example.com"
  "User's email address.")

(setq tab-width 4)
(setq indent-tabs-mode nil)

;; Let binding (local variables)
(let ((x 10)
      (y 20))
  (message "Sum: %d" (+ x y)))

;; Functions
(defun greet (name)
  "Greet NAME with a message."
  (message "Hello, %s!" name))

(greet "World")

;; Function with optional arguments
(defun greet-optional (name &optional title)
  "Greet NAME with optional TITLE."
  (if title
      (message "Hello, %s %s!" title name)
    (message "Hello, %s!" name)))

;; Function with rest arguments
(defun sum (&rest numbers)
  "Sum all NUMBERS."
  (apply '+ numbers))

(message "Sum: %d" (sum 1 2 3 4 5))

;; Interactive function (command)
(defun insert-date ()
  "Insert current date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; Interactive with arguments
(defun greet-user (name)
  "Greet user by NAME."
  (interactive "sEnter your name: ")
  (message "Hello, %s!" name))

;; Conditionals
(if (> 10 5)
    (message "Greater")
  (message "Less"))

;; When (no else clause)
(when (> 10 5)
  (message "Greater than 5")
  (message "Much greater"))

;; Unless
(unless (< 10 5)
  (message "Not less than 5"))

;; Cond (multiple conditions)
(cond
 ((< 10 5) (message "Less"))
 ((= 10 5) (message "Equal"))
 ((> 10 5) (message "Greater"))
 (t (message "Default")))

;; Loops
;; While loop
(let ((counter 0))
  (while (< counter 5)
    (message "Counter: %d" counter)
    (setq counter (1+ counter))))

;; Dotimes
(dotimes (i 5)
  (message "i = %d" i))

;; Dolist
(dolist (item '("red" "green" "blue"))
  (message "Color: %s" item))

;; Lists
(setq colors '("red" "green" "blue"))
(setq numbers (list 1 2 3 4 5))

;; List operations
(car colors)                    ; First element
(cdr colors)                    ; Rest of list
(nth 1 colors)                  ; Element at index
(length colors)                 ; List length
(append colors '("yellow"))     ; Append lists
(cons "purple" colors)          ; Add to front
(reverse colors)                ; Reverse list

;; Map functions
(mapcar (lambda (x) (* x x)) numbers)
(mapcar #'1+ numbers)

;; Filter
(seq-filter (lambda (x) (> x 2)) numbers)

;; Reduce
(seq-reduce #'+ numbers 0)

;; Lambda functions
(setq square (lambda (x) (* x x)))
(funcall square 5)

;; Shorter lambda syntax
(setq add-ten #'(lambda (x) (+ x 10)))

;; Association lists (alists)
(setq person '((name . "Alice")
               (age . 30)
               (city . "NYC")))

(assoc 'name person)
(cdr (assoc 'name person))

;; Property lists (plists)
(setq user '(:name "Bob" :age 25 :city "SF"))
(plist-get user :name)

;; Hash tables
(setq my-hash (make-hash-table :test 'equal))
(puthash "name" "Alice" my-hash)
(puthash "age" 30 my-hash)
(gethash "name" my-hash)

;; Strings
(setq text "hello world")
(upcase text)
(downcase text)
(capitalize text)
(length text)
(substring text 0 5)
(concat "Hello" " " "World")
(split-string text)
(string-match "world" text)

;; String formatting
(format "Name: %s, Age: %d" "Alice" 30)
(format-time-string "%Y-%m-%d %H:%M:%S")

;; Buffers
(defun create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*scratch*")))

;; Buffer operations
;; (current-buffer)
;; (buffer-name)
;; (buffer-file-name)
;; (save-buffer)
;; (kill-buffer)
;; (with-current-buffer buffer-name
;;   (insert "Text"))

;; File operations
(defun read-file-contents (filename)
  "Read and return contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

;; (write-region "content" nil "file.txt")
;; (delete-file "file.txt")
;; (file-exists-p "file.txt")
;; (directory-files ".")

;; Regular expressions
(string-match "\\([0-9]+\\)" "The answer is 42")
(match-string 1 "The answer is 42")
(replace-regexp-in-string "[0-9]" "X" "Test 123")

;; Key bindings
(global-set-key (kbd "C-c g") 'greet-user)
(global-set-key (kbd "C-c d") 'insert-date)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Local key bindings
(define-key emacs-lisp-mode-map (kbd "C-c e") 'eval-buffer)

;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Advice (modify existing functions)
(defun my-advice (orig-fun &rest args)
  "Advice for ORIG-FUN with ARGS."
  (message "Before function call")
  (apply orig-fun args)
  (message "After function call"))

(advice-add 'save-buffer :around #'my-advice)

;; Macros
(defmacro when-not (condition &rest body)
  "Execute BODY when CONDITION is nil."
  `(if (not ,condition)
       (progn ,@body)))

(when-not nil
  (message "Condition is false"))

;; Backquote and unquote
(defmacro my-setq (var val)
  "Set VAR to VAL."
  `(setq ,var ,val))

;; Error handling
(condition-case err
    (/ 10 0)
  (arith-error (message "Arithmetic error: %s" err))
  (error (message "General error: %s" err)))

;; Unwind-protect (like finally)
(unwind-protect
    (progn
      (message "Try block")
      (/ 10 0))
  (message "Cleanup"))

;; Timers
(defun my-timer-function ()
  "Function called by timer."
  (message "Timer fired at %s" (current-time-string)))

;; (run-with-timer 5 nil 'my-timer-function)
;; (run-at-time "10:00am" 3600 'my-timer-function)

;; Processes
;; (start-process "process-name" "*output-buffer*" "ls" "-la")
;; (shell-command "ls -la" "*output*")
;; (shell-command-to-string "date")

;; Packages (using package.el)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install package if not installed
(defun ensure-package-installed (package)
  "Install PACKAGE if not already installed."
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

;; Custom functions
(defun comment-or-uncomment-region-or-line ()
  "Comment or uncomment current line or region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; Mode definition
(define-derived-mode my-mode text-mode "MyMode"
  "Major mode for editing my files."
  (setq-local comment-start "#")
  (setq-local comment-end ""))

;; Minor mode
(define-minor-mode my-minor-mode
  "Toggle my minor mode."
  :lighter " MyMin"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c m") 'my-command)
            map)
  (if my-minor-mode
      (message "My minor mode enabled")
    (message "My minor mode disabled")))

;; Face (text appearance)
(defface my-face
  '((t :foreground "blue" :weight bold))
  "Face for highlighting.")

;; Syntax highlighting
(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\<\\(FIXME\\|TODO\\|NOTE\\):" 1 font-lock-warning-face t)))

;; Custom variables
(defcustom my-custom-var "default"
  "A custom variable."
  :type 'string
  :group 'my-group)

;; Custom group
(defgroup my-group nil
  "My customization group."
  :group 'applications)

;; Point and mark
;; (point)                      ; Current cursor position
;; (point-min)                  ; Beginning of buffer
;; (point-max)                  ; End of buffer
;; (goto-char position)         ; Move to position
;; (forward-line n)             ; Move n lines forward
;; (beginning-of-line)          ; Move to line start
;; (end-of-line)                ; Move to line end

;; Region operations
;; (region-beginning)
;; (region-end)
;; (region-active-p)
;; (delete-region start end)
;; (buffer-substring start end)

;; Threading macros (from dash.el)
;; (require 'dash)
;; (-> 5 (+ 3) (* 2) (- 1))
;; (->> '(1 2 3 4 5) (-map 'square) (-filter 'evenp))

;; Use-package for package management
;; (use-package company
;;   :ensure t
;;   :config
;;   (global-company-mode t))

;; Org mode example
;; (with-temp-buffer
;;   (org-mode)
;;   (insert "* TODO Task\n** DONE Subtask\n")
;;   (org-export-to-file 'html "output.html"))

(message "Emacs Lisp examples complete!")

(provide 'init)
;;; init.el ends here
