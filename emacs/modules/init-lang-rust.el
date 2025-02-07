;;; init-lang-rust.el ---
;;; Commentary:
;;;
;;; -----------------------------------------------------------------------------
;;; NOTE: The documentation below is automatically generated for all
;;; *init-lang-*.el files.
;;;
;;; This file defines all rust specific configuration. It contains some or all
;;; of the following logical sections, in order:
;;; 1. Major mode: Customize the default major mode for rust buffers.
;;; 2. Language server protocol (LSP): LSP defines a way for a 'client' - e.g. a
;;;    text editor like Emacs or Vim - to feed information to, and communicate
;;;    with, a 'language server' - a program that understands rust
;;;    projects, including dependency resolution, namespacing, class
;;;    hierarchies, etc. Emacs can act as a client of a running language server,
;;;    thereby providing "IDE-like" functionality. There are several popular
;;;    packages that provide LSP client implementations, including 'eglot' and
;;;    'lsp-mode'. The exact realized functionality depends on both the language
;;;    server, of which there are typically several competing for a given
;;;    language, and on the lsp client.
;;;    Read more at: https://microsoft.github.io/language-server-protocol/
;;; 3. Formatting: Autoformat on save, define indentation rules, etc.
;;; 4. Build targets: Common compile targets for rust buffers. These
;;;    should be available via a single entrypoint, currently 'compile-multi'.
;;; 5. Miscellaneous
;;; -----------------------------------------------------------------------------
;;;
;;; Code:

;; = MAJOR MODE =

;; Use the built-in tree-sitter based 'rust-ts-mode'.
;; Inform 'treesit-install-language-grammar' how to install the rust treesitter
;; grammar required for 'rust-ts-mode'.
(add-to-list 'treesit-language-source-alist '(rust "https://github.com/tree-sitter/tree-sitter-rust"))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

;; = LANGUAGE SERVER PROTOCOL =

;; TODO: This code assumes that rust-analyzer, or another rust lsp, is
;; installed. Automatically install a language server if one dne.
(use-package lsp-mode :hook (rust-ts-mode . lsp-deferred))

;; = FORMATTING =

;; Autoformat buffers using 'rustfmt' on save.
(use-package reformatter
  :init
  (reformatter-define rust-reformatter
    :program "rustfmt"
    :args '("--quiet" "--emit" "stdout"))
  (add-hook 'rust-ts-mode-hook 'rust-reformatter-on-save-mode))

;; = BUILD TARGETS =

(use-package compile-multi
  :config
  (add-to-list 'compile-multi-config '(rust-ts-mode
                                       ("rust:check" . "cargo check")
                                       ("rust:build" . "cargo build")
                                       ("rust:release" . "cargo build --release")
                                       ("rust:run" . "cargo run")
                                       ("rust:test" . "cargo test"))))

;; (require 'compile)

;;; Copied from `rust-mode`
(defvar rustc-compilation-location
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col "\\([0-9]+\\)"))
    (concat "\\(" file ":" start-line ":" start-col "\\)")))

(defvar rustc-compilation-regexps
  (let ((re (concat "^\\(?:error\\|\\(warning\\)\\|\\(note\\)\\)[^\0]+?--> "
                    rustc-compilation-location)))
    (cons re '(4 5 6 (1 . 2) 3)))
  "Specifications for matching errors in rustc invocations.
See `compilation-error-regexp-alist' for help on their format.")

(defvar rustc-colon-compilation-regexps
  (let ((re (concat "^ *::: " rustc-compilation-location)))
    (cons re '(2 3 4 0 1)))
  "Specifications for matching `:::` hints in rustc invocations.
See `compilation-error-regexp-alist' for help on their format.")

(defvar rustc-refs-compilation-regexps
  (let ((re "^\\([0-9]+\\)[[:space:]]*|"))
    (cons re '(nil 1 nil 0 1)))
  "Specifications for matching code references in rustc invocations.
See `compilation-error-regexp-alist' for help on their format.")

(defvar rustc-panics-compilation-regexps
   (let ((re (concat "thread '[^']+' panicked at " rustc-compilation-location)))
     (cons re '(2 3 4 nil 1)))
   "Specifications for matching panics in rustc invocations.
See `compilation-error-regexp-alist' for help on their format.")

;; Match test run failures and panics during compilation as
;; compilation warnings
(defvar cargo-compilation-regexps
  '("', \\(\\([^:]+\\):\\([0-9]+\\)\\)"
    2 3 nil nil 1)
  "Specifications for matching panics in cargo test invocations.
See `compilation-error-regexp-alist' for help on their format.")

(defun rustc-scroll-down-after-next-error ()
  "In the new style error messages, the regular expression
matches on the file name (which appears after `-->`), but the
start of the error appears a few lines earlier.  This hook runs
after `next-error' (\\[next-error]); it simply scrolls down a few lines in
the compilation window until the top of the error is visible."
  (save-selected-window
    (when (eq major-mode 'rust-mode)
      (select-window (get-buffer-window next-error-last-buffer 'visible))
      (when (save-excursion
              (beginning-of-line)
              (looking-at " *-->"))
        (let ((start-of-error
               (save-excursion
                 (beginning-of-line)
                 (while (not (looking-at "^[a-z]+:\\|^[a-z]+\\[E[0-9]+\\]:"))
                   (forward-line -1))
                 (point))))
          (set-window-start (selected-window) start-of-error))))))

(eval-after-load 'compile
  '(progn
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'rustc-refs rustc-refs-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'rustc-refs)
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'rustc rustc-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'rustc)
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'rustc-colon rustc-colon-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'rustc-colon)
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'cargo cargo-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'rustc-panics rustc-panics-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'rustc-panics)
     (add-to-list 'compilation-error-regexp-alist 'cargo)
     (add-hook 'next-error-hook #'rustc-scroll-down-after-next-error)))

;; = MISCELLANEOUS =

(provide 'init-lang-rust)
;;; init-lang-rust.el ends here.
