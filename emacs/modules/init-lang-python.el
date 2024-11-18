;;; init-lang-python.el ---
;;; Commentary:
;;;
;;; -----------------------------------------------------------------------------
;;; NOTE: The documentation below is automatically generated for all
;;; *init-lang-*.el files.
;;;
;;; This file defines all python specific configuration. It contains the
;;; following logical sections, in order:
;;; 1. Major mode: Customize the default major mode for python buffers.
;;; 2. Language server protocol (LSP): LSP defines a way for a 'client' - e.g. a
;;;    text editor like Emacs or Vim - to feed information to, and communicate
;;;    with, a 'language server' - a program that understands python
;;;    projects, including dependency resolution, namespacing, class
;;;    hierarchies, etc. Emacs can act as a client of a running language server,
;;;    thereby providing "IDE-like" functionality. There are several popular
;;;    packages that provide LSP client implementations, including 'eglot' and
;;;    'lsp-mode'. The exact realized functionality depends on both the language
;;;    server, of which there are typically several competing for a given
;;;    language, and on the lsp client.
;;;    Read more at: https://microsoft.github.io/language-server-protocol/
;;; 3. Formatting: Autoformat on save, define indentation rules, etc.
;;; 4. Build targets: Common compile targets for python buffers. These
;;;    should be available via a single entrypoint, currently 'compile-multi'.
;;; 5. Miscellaneous
;;; -----------------------------------------------------------------------------
;;;
;;; Code:

;; = MAJOR MODE =

;; Use the built-in tree-sitter based 'python-ts-mode'.
;; Inform 'treesit-install-language-grammar' how to install the python treesitter
;; grammar required for 'python-ts-mode'.
(add-to-list 'treesit-language-source-alist '(python "https://github.com/tree-sitter/tree-sitter-python"))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

;; = LANGUAGE SERVER PROTOCOL =

;; = FORMATTING =

;; = BUILD TARGETS =

;; = MISCELLANEOUS =

(provide 'init-lang-python)
;;; init-lang-python.el ends here.
