;;; init-lang-json.el ---
;;; Commentary:
;;;
;;; -----------------------------------------------------------------------------
;;; NOTE: The documentation below is automatically generated for all
;;; *init-lang-*.el files.
;;;
;;; This file defines all json specific configuration. It contains the
;;; following logical sections, in order:
;;; 1. Major mode: Customize the default major mode for json buffers.
;;; 2. Language server protocol (LSP): LSP defines a way for a 'client' - e.g. a
;;;    text editor like Emacs or Vim - to feed information to, and communicate
;;;    with, a 'language server' - a program that understands json
;;;    projects, including dependency resolution, namespacing, class
;;;    hierarchies, etc. Emacs can act as a client of a running language server,
;;;    thereby providing "IDE-like" functionality. There are several popular
;;;    packages that provide LSP client implementations, including 'eglot' and
;;;    'lsp-mode'. The exact realized functionality depends on both the language
;;;    server, of which there are typically several competing for a given
;;;    language, and on the lsp client.
;;;    Read more at: https://microsoft.github.io/language-server-protocol/
;;; 3. Formatting: Autoformat on save, define indentation rules, etc.
;;; 4. Build targets: Common compile targets for json buffers. These
;;;    should be available via a single entrypoint, currently 'compile-multi'.
;;; 5. Miscellaneous
;;; -----------------------------------------------------------------------------
;;;
;;; Code:

;; = MAJOR MODE =

;; Use the built-in tree-sitter based 'json-ts-mode'.
;; Inform 'treesit-install-language-grammar' how to install the json treesitter
;; grammar required for 'json-ts-mode'.
(add-to-list 'treesit-language-source-alist '(json "https://github.com/tree-sitter/tree-sitter-json"))
(add-to-list 'auto-mode-alist '("\\.py\\'" . json-ts-mode))

;; = LANGUAGE SERVER PROTOCOL =

;; = FORMATTING =

;; = BUILD TARGETS =

;; = MISCELLANEOUS =

(provide 'init-lang-json)
;;; init-lang-json.el ends here.
