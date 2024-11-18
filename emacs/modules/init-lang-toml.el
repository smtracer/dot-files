;;; init-lang-toml.el ---
;;; Commentary:
;;;
;;; -----------------------------------------------------------------------------
;;; NOTE: The documentation below is automatically generated for all
;;; *init-lang-*.el files.
;;;
;;; This file defines all toml specific configuration. It contains some or all
;;; of the following logical sections, in order:
;;; 1. Major mode: Customize the default major mode for toml buffers.
;;; 2. Language server protocol (LSP): LSP defines a way for a 'client' - e.g. a
;;;    text editor like Emacs or Vim - to feed information to, and communicate
;;;    with, a 'language server' - a program that understands toml
;;;    projects, including dependency resolution, namespacing, class
;;;    hierarchies, etc. Emacs can act as a client of a running language server,
;;;    thereby providing "IDE-like" functionality. There are several popular
;;;    packages that provide LSP client implementations, including 'eglot' and
;;;    'lsp-mode'. The exact realized functionality depends on both the language
;;;    server, of which there are typically several competing for a given
;;;    language, and on the lsp client.
;;;    Read more at: https://microsoft.github.io/language-server-protocol/
;;; 3. Formatting: Autoformat on save, define indentation rules, etc.
;;; 4. Build targets: Common compile targets for toml buffers. These
;;;    should be available via a single entrypoint, currently 'compile-multi'.
;;; 5. Miscellaneous
;;; -----------------------------------------------------------------------------
;;;
;;; Code:

;; = MAJOR MODE =

;; Use the built-in tree-sitter based 'toml-ts-mode'.
;; Inform 'treesit-install-language-grammar' how to install the toml treesitter
;; grammar required for 'toml-ts-mode'.
(add-to-list 'treesit-language-source-alist '(toml "https://github.com/tree-sitter/tree-sitter-toml"))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))

;; = LANGUAGE SERVER PROTOCOL =

;; = FORMATTING =

;; = BUILD TARGETS =

;; = MISCELLANEOUS =

(provide 'init-lang-toml)
;;; init-lang-toml.el ends here.
