;;; mojo-mode.el --- Major mode for Mojo  -*- lexical-binding: t; -*-
;; Author: Richard Johnsson <>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages
;; URL: https://github.com/jmikaelr/mojo-mode
;;
;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; Major mode for Mojo language.
;; Features:
;; - Syntax highlighting
;; - Python-style indentation (indent after ":" lines, dedent on elif/else/except/finally)
;; - Imenu for functions and structs
;; - Auto-detect .mojo files

;;; Code:

(require 'rx)

(defgroup mojo-mode nil
  "Major mode for the Mojo language."
  :group 'languages)

(defcustom mojo-indent-offset 4
  "Indentation width for `mojo-mode`."
  :type 'integer :safe 'integerp)

;; Keywords, types, constants
(defconst mojo-keywords
  '("fn" "let" "var" "struct" "enum" "trait" "impl" "for" "while"
    "if" "elif" "else" "match" "return" "break" "continue"
    "from" "import" "as" "in" "where" "pub" "owned" "borrowed"
    "const" "extern" "try" "except" "finally" "with" "yield"))

(defconst mojo-constants '("True" "False" "None"))
(defconst mojo-types     '("Int" "Float" "Bool" "String" "Vector" "Tensor"))

(defconst mojo-font-lock
  `((,(regexp-opt mojo-types 'symbols)     . font-lock-type-face)
    (,(regexp-opt mojo-constants 'symbols) . font-lock-constant-face)
    (,(regexp-opt mojo-keywords 'symbols)  . font-lock-keyword-face)
    ;; fn name(
    (,(rx line-start (* space) "fn" (+ space)
          (group (+ (or word ?_))) (* space) "(")
     (1 font-lock-function-name-face))
    ;; struct Name
    (,(rx line-start (* space) "struct" (+ space)
          (group (+ (or word ?_))))
     (1 font-lock-type-face))))

;; Python-like indentation helpers
(defun mojo--prev-indent ()
  "Indentation of the nearest non-blank line above point."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp)) (looking-at-p "^[[:space:]]*$"))
      (forward-line -1))
    (current-indentation)))

(defun mojo--dedent-start-p ()
  "Return non-nil if current line begins with a dedent keyword."
  (save-excursion
    (back-to-indentation)
    (looking-at
     (rx (or "elif" "else" "except" "finally") symbol-end))))

(defun mojo--prev-ends-with-colon-p ()
  "Return non-nil if the previous non-blank line ends with a colon."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp)) (looking-at-p "^[[:space:]]*$"))
      (forward-line -1))
    (end-of-line)
    (looking-back (rx ":" (* space)) (line-beginning-position))))

(defun mojo-calc-indent ()
  "Compute indentation for the current line."
  (cond
   ((bobp) 0)
   ((mojo--dedent-start-p)
    (max 0 (- (mojo--prev-indent) mojo-indent-offset)))
   ((mojo--prev-ends-with-colon-p)
    (+ (mojo--prev-indent) mojo-indent-offset))
   (t (mojo--prev-indent))))

(defun mojo-indent-line ()
  "Indent current line as Mojo code."
  (interactive)
  (let* ((target (mojo-calc-indent))
         (pos (- (point-max) (point))))
    (indent-line-to target)
    (when (> (- (point-max) pos) (point))
      (goto-char (- (point-max) pos)))))

(defun mojo-newline-and-indent ()
  "Insert newline and indent like Python."
  (interactive)
  (newline)
  (mojo-indent-line))

;; Syntax table
(defvar mojo-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    ;; Treat underscore as word
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table for `mojo-mode`.")

;; Imenu
(defvar mojo-imenu-generic-expression
  `(("Functions" ,(rx line-start (* space) "fn" (+ space)
                      (group (+ (or word ?_)))) 1)
    ("Structs"   ,(rx line-start (* space) "struct" (+ space)
                      (group (+ (or word ?_)))) 1))
  "Imenu expressions for `mojo-mode`.")

;;;###autoload
(define-derived-mode mojo-mode prog-mode "Mojo"
  "Major mode for editing Mojo source."
  :syntax-table mojo-mode-syntax-table
  (setq-local comment-start "#"
              comment-end ""
              comment-start-skip "#+\\s-*")
  (setq-local indent-line-function #'mojo-indent-line)
  (setq-local font-lock-defaults '(mojo-font-lock))
  (setq-local imenu-generic-expression mojo-imenu-generic-expression)
  (electric-indent-local-mode 1)
  (use-local-map (copy-keymap (current-local-map)))
  (define-key (current-local-map) (kbd "RET") #'mojo-newline-and-indent))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mojo\\'" . mojo-mode))

;; --------------------------
;; LSP integration
;; --------------------------
;; Only hooks in after lsp-mode is loaded. Does not require lsp-mode here.

(require 'lsp-mode)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     ;; Run inside Pixi environment
                     `("pixi" "run" "mojo-lsp-server")))
  :major-modes '(mojo-mode)
  :server-id 'mojo-lsp))

(provide 'mojo-mode)
;;; mojo-mode.el ends here
