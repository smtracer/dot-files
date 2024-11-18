;;; init-lang-java.el ---
;;; Commentary:
;;;
;;; -----------------------------------------------------------------------------
;;; NOTE: The documentation below is automatically generated for all
;;; *init-lang-*.el files.
;;;
;;; This file defines all java specific configuration. It contains the
;;; following logical sections, in order:
;;; 1. Major mode: Customize the default major mode for java buffers.
;;; 2. Language server protocol (LSP): LSP defines a way for a 'client' - e.g. a
;;;    text editor like Emacs or Vim - to feed information to, and communicate
;;;    with, a 'language server' - a program that understands java
;;;    projects, including dependency resolution, namespacing, class
;;;    hierarchies, etc. Emacs can act as a client of a running language server,
;;;    thereby providing "IDE-like" functionality. There are several popular
;;;    packages that provide LSP client implementations, including 'eglot' and
;;;    'lsp-mode'. The exact realized functionality depends on both the language
;;;    server, of which there are typically several competing for a given
;;;    language, and on the lsp client.
;;;    Read more at: https://microsoft.github.io/language-server-protocol/
;;; 3. Formatting: Autoformat on save, define indentation rules, etc.
;;; 4. Build targets: Common compile targets for java buffers. These
;;;    should be available via a single entrypoint, currently 'compile-multi'.
;;; 5. Miscellaneous
;;; -----------------------------------------------------------------------------
;;;
;;; Code:

;; = MAJOR MODE =

;; Use the built-in tree-sitter based 'java-ts-mode'.
;; Inform 'treesit-install-language-grammar' how to install the java treesitter
;; grammar required for 'java-ts-mode'.
(add-to-list 'treesit-language-source-alist '(java "https://github.com/tree-sitter/tree-sitter-java"))
(add-to-list 'auto-mode-alist '("\\.py\\'" . java-ts-mode))

;; = LANGUAGE SERVER PROTOCOL =

;; = FORMATTING =

;; = BUILD TARGETS =

;; = MISCELLANEOUS =

(provide 'init-lang-java)
;;; init-lang-java.el ends here.
