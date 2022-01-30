;;; binfmt-mode.el -- Major mode for editing binfmt files. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'rx))

(defvar binfmt-font-lock-keywords
  `(("\\(@foreign-\\(un\\)?parsers\\)" 1 font-lock-preprocessor-face)
    ("\\(?:^[[:space:]]*\\(\\(?:[[:alnum:]]\\|[[:digit:]]\\|_\\)+\\(?:[[:alnum:]]\\|[[:digit:]]\\|[_-]\\)*\\)[[:space:]]*=\\)" 1 font-lock-function-name-face)
    ("\\(?:\\_<\\(=\\)\\_>\\)" 1 font-lock-builtin-face)
    ("\\(?:\\_<\\(|\\)\\_>\\)" 1 font-lock-warning-face))
  "Font lock keywords for binfmt-mode.")

(defvar binfmt-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\| "_"  table)
    (modify-syntax-entry ?\# "<"  table)
    (modify-syntax-entry ?\n ">"  table)
    table)
  "Syntax table for binfmt-mode.")

;;;###autoload
(define-derived-mode binfmt-mode prog-mode "binfmt"
  "A major mode for editing binfmt grammars."
  :syntax-table binfmt-mode-syntax-table

  (setq-local comment-use-syntax nil)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")
  (setq font-lock-defaults '(binfmt-font-lock-keywords nil t)))

;;; binfmt-mode.el ends here
