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

;; Faces
(defface mojo-decorator-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for Mojo decorators like @fieldinit_wise."
  :group 'mojo-mode)

;; ;; Keywords, types, constants
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

    ;; decorators: @name or @pkg.name
    (,(rx line-start (* space) "@" (group (+ (or word ?_ ?.))))
     (1 'mojo-decorator-face))

    ;; fn name(
    (,(rx line-start (* space) "fn" (+ space)
          (group (+ (or word ?_))) (* space) "(")
     (1 font-lock-function-name-face))

    ;; struct Name
    (,(rx line-start (* space) "struct" (+ space)
          (group (+ (or word ?_))))
     (1 font-lock-type-face))))

;;; Indentention

(defcustom mojo-indent-offset 4
  "Indent width for `mojo-mode`."
  :type 'integer :safe 'integerp)

(defcustom mojo-indent-hanging nil
  "If non-nil, inside ( [ { indent under first token after opener (hanging indent).
If nil, indent to opener's indentation plus `mojo-indent-offset`."
  :type 'boolean :group 'mojo-mode)

(defcustom mojo-indent-def-block-scale 2
  "Extra multiplier for lines inside parens of a block header (like fn ...():).
Applied when the opening paren is at end of header line and the items start on the next line."
  :type 'integer :group 'mojo-mode)

(defcustom mojo-indent-block-paren-deeper nil
  "If non-nil, when a block header has content after the opening paren
on the same line, indent inner lines one extra `mojo-indent-offset`."
  :type 'boolean :group 'mojo-mode)

(defcustom mojo-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that may trigger cycling/adjusting indentation."
  :type '(repeat symbol))

(defun mojo--prev-indent ()
  "Indentation of the nearest non-blank line above point."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp)) (looking-at-p "^[[:space:]]*$"))
      (forward-line -1))
    (current-indentation)))

(defun mojo--dedent-start-p ()
  "Non-nil if this line starts with a dedent keyword."
  (save-excursion
    (back-to-indentation)
    (looking-at
     (rx (or "elif" "else" "except" "finally") symbol-end))))

(defun mojo--prev-ends-with-colon-p ()
  "Non-nil if the previous non-blank line ends with a colon (ignoring trailing spaces)."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp)) (looking-at-p "^[[:space:]]*$"))
      (forward-line -1))
    (end-of-line)
    (looking-back (rx ":" (* space)) (line-beginning-position))))

(defun mojo--line-starts-with-close-bracket-p ()
  "Non-nil if current line (ignoring indent) starts with ) ] or }."
  (save-excursion
    (back-to-indentation)
    (looking-at-p (rx (in ")]}")))))

(defun mojo--line-starts-with-top-level-starter-p ()
  "Non-nil if current line begins with a top-level construct keyword."
  (save-excursion
    (back-to-indentation)
    (looking-at-p (rx (or "fn" "struct" "enum" "trait" "impl") symbol-end))))

(defun mojo--enclosing-type-indent ()
  "Indent column of the nearest enclosing type-level header (struct/enum/trait/impl), else 0."
  (save-excursion
    (let ((indent 0) (done nil))
      (while (and (not done) (not (bobp)))
        (forward-line -1)
        (unless (looking-at-p "^[[:space:]]*$")
          (save-excursion
            (back-to-indentation)
            (when (looking-at (rx (or "struct" "enum" "trait" "impl") symbol-end))
              (setq indent (current-indentation)
                    done t)))))
      indent)))

(defun mojo--enclosing-type-pos ()
  "Return buffer position of nearest enclosing type header (struct/enum/trait/impl), or nil."
  (save-excursion
    (let (pos)
      (while (and (not pos) (not (bobp)))
        (forward-line -1)
        (unless (looking-at-p "^[[:space:]]*$")
          (back-to-indentation)
          (when (looking-at (rx (or "struct" "enum" "trait" "impl") symbol-end))
            (setq pos (point)))))
      pos)))

(defun mojo--decorator-line-p ()
  "Non-nil if current line (ignoring indent) starts with '@'."
  (save-excursion
    (back-to-indentation)
    (eq (char-after) ?@)))

(defun mojo--member-base-indent ()
  "Indent for a member under the nearest struct/enum/trait/impl.
If the current line's indent is <= the header's indent, return 0
(module level). Otherwise header indent + `mojo-indent-offset`."
  (let ((cur (current-indentation))
        (pos (mojo--enclosing-type-pos)))
    (if (not pos)
        0
      (save-excursion
        (goto-char pos)
        (let ((hdr (current-indentation)))
          (if (<= cur hdr) 0 (+ hdr mojo-indent-offset)))))))

(defun mojo--line-ends-with-colon-at (pos)
  "Non-nil if the line at buffer position POS ends with a colon."
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (looking-back (rx ":" (* space)) (line-beginning-position))))

(defun mojo--hang-column-after (open-pos)
  "Column of first non-space after the opener at OPEN-POS."
  (save-excursion
    (goto-char open-pos)
    (forward-char 1)
    (skip-chars-forward " \t")
    (current-column)))

(defun mojo-calc-indent ()
  "Compute indentation for the current line."
  (let* ((ppss (syntax-ppss))
         (in-string  (nth 3 ppss))
         (in-comment (nth 4 ppss))
         (open-pos   (nth 1 ppss))     ; innermost unclosed ( [ {
         (prev-indent (mojo--prev-indent)))
    (cond
     ((bobp) 0)

     ((or in-string in-comment)
      (current-indentation))

     ;; Lines starting with a closer align with opener
     ((mojo--line-starts-with-close-bracket-p)
      (if open-pos
          (save-excursion (goto-char open-pos) (current-column))
        (max 0 (- prev-indent mojo-indent-offset))))

     ;; Decorators act like members under type header, else module level
     ((and (mojo--decorator-line-p) (null open-pos))
      (mojo--member-base-indent))

     ;; New fn/struct/enum/trait/impl lines
     ((and (mojo--line-starts-with-top-level-starter-p) (null open-pos))
      (mojo--member-base-indent))

     ;; Inside any unclosed bracket list
     (open-pos
      (let* ((open-col (save-excursion (goto-char open-pos) (current-indentation)))
             (items-start-same-line
              (save-excursion
                (goto-char open-pos) (forward-char 1) (skip-chars-forward " \t")
                (not (eolp))))
             (header-line-p
              (mojo--line-ends-with-colon-at
               (save-excursion (goto-char open-pos) (line-beginning-position)))))
        (cond
         (mojo-indent-hanging
          (mojo--hang-column-after open-pos))
         ((and header-line-p (not items-start-same-line))
          (+ open-col (* mojo-indent-offset mojo-indent-def-block-scale)))
         ((and header-line-p items-start-same-line mojo-indent-block-paren-deeper)
          (+ open-col (* 2 mojo-indent-offset)))
         (t
          (+ open-col mojo-indent-offset)))))

     ;; Dedent keywords
     ((mojo--dedent-start-p)
      (max 0 (- prev-indent mojo-indent-offset)))

     ;; After a ':' line, indent one level
     ((mojo--prev-ends-with-colon-p)
      (+ prev-indent mojo-indent-offset))

     ;; Default
     (t prev-indent))))

(defun mojo-indent-line (&optional _previous)
  "Indent current line according to Mojo rules."
  (interactive)
  (let ((follow
         (and (<= (line-beginning-position) (point))
              (>= (+ (line-beginning-position) (current-indentation)) (point)))))
    (indent-line-to (mojo-calc-indent))
    (when follow (back-to-indentation))))

(defun mojo-indent-dedent-line ()
  "De-indent current line one level if point is in indentation."
  (interactive "*")
  (when (and (= (current-indentation) (current-column))
             (not (bolp)))
    (indent-line-to (max 0 (- (current-indentation) mojo-indent-offset)))
    t))

(defun mojo-indent-dedent-line-backspace (arg)
  "De-indent when backspacing in indentation; otherwise delete."
  (interactive "*p")
  (unless (mojo-indent-dedent-line)
    (backward-delete-char-untabify arg)))

(defun mojo-indent-post-self-insert-function ()
  "Adjust indentation after inserting certain characters."
  (when (and electric-indent-mode
             (eq (char-before) last-command-event)
             (not (nth 3 (syntax-ppss))))    ;; not in string
    (cond
     ;; Reindent when typing before a closer, or after comma
     ((or (memq (char-after) '(?\) ?\] ?\}))
          (eq (char-before) ?,))
      (save-excursion
        (beginning-of-line)
        (mojo-indent-line)))
     ;; Electric colon at EOL
     ((and (eq ?: last-command-event) (eolp))
      (save-excursion
        (beginning-of-line)
        (mojo-indent-line)))
     ;; NEW: when starting a decorator or a member line
     ((memq last-command-event '(?@ ?f))
      (save-excursion
        (beginning-of-line)
        (when (save-excursion
                (back-to-indentation)
                (or (eq (char-after) ?@)
                    (looking-at (rx "fn" symbol-end))))
          (mojo-indent-line)))))))

;;; Syntax table
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

;;; Imenu
(defvar mojo-imenu-generic-expression
  `(("Functions" ,(rx line-start (* space) "fn" (+ space)
                      (group (+ (or word ?_)))) 1)
    ("Structs"   ,(rx line-start (* space) "struct" (+ space)
                      (group (+ (or word ?_)))) 1))
  "Imenu expressions for `mojo-mode`.")

;;; Keymap
(defvar mojo-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (define-key map (kbd "RET") #'newline-and-indent)
    (define-key map (kbd "DEL") #'mojo-indent-dedent-line-backspace)
    (define-key map (kbd "<backtab>") #'mojo-indent-dedent-line)  ; Shift-TAB
    map)
  "Keymap for `mojo-mode`.")

;;;###autoload
(define-derived-mode mojo-mode prog-mode "Mojo"
  "Major mode for editing Mojo source."
  :syntax-table mojo-mode-syntax-table
  :keymap mojo-mode-map
  (setq-local comment-start "#"
              comment-end ""
              comment-start-skip "#+\\s-*")
  (setq-local indent-line-function #'mojo-indent-line)
  (setq-local font-lock-defaults '(mojo-font-lock))
  (setq-local imenu-generic-expression mojo-imenu-generic-expression)
  (electric-indent-local-mode 1)
  ;; reindent after typing ) ] } , :
  (add-hook 'post-self-insert-hook #'mojo-indent-post-self-insert-function nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mojo\\'" . mojo-mode))

;; --------------------------
;; LSP integration
;; --------------------------
;; Only hooks in after lsp-mode is loaded. Does not require lsp-mode here.

;; (require 'lsp-mode)

;; (lsp-register-client
;;  (make-lsp-client
;;   :new-connection (lsp-stdio-connection
;;                    (lambda ()
;;                      ;; Run inside Pixi environment
;;                      `("pixi" "run" "mojo-lsp-server")))
;;   :major-modes '(mojo-mode)
;;   :server-id 'mojo-lsp))

(provide 'mojo-mode)
;;; mojo-mode.el ends here
