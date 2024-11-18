;;; init-lang-ruby.el ---
;;; Commentary:
;;;
;;; -----------------------------------------------------------------------------
;;; NOTE: The documentation below is automatically generated for all
;;; *init-lang-*.el files.
;;;
;;; This file defines all ruby specific configuration. It contains some or all
;;; of the following logical sections, in order:
;;; 1. Major mode: Customize the default major mode for ruby buffers.
;;; 2. Language server protocol (LSP): LSP defines a way for a 'client' - e.g. a
;;;    text editor like Emacs or Vim - to feed information to, and communicate
;;;    with, a 'language server' - a program that understands ruby
;;;    projects, including dependency resolution, namespacing, class
;;;    hierarchies, etc. Emacs can act as a client of a running language server,
;;;    thereby providing "IDE-like" functionality. There are several popular
;;;    packages that provide LSP client implementations, including 'eglot' and
;;;    'lsp-mode'. The exact realized functionality depends on both the language
;;;    server, of which there are typically several competing for a given
;;;    language, and on the lsp client.
;;;    Read more at: https://microsoft.github.io/language-server-protocol/
;;; 3. Formatting: Autoformat on save, define indentation rules, etc.
;;; 4. Build targets: Common compile targets for ruby buffers. These
;;;    should be available via a single entrypoint, currently 'compile-multi'.
;;; 5. Miscellaneous
;;; -----------------------------------------------------------------------------
;;;
;;; Code:

;; = MAJOR MODE =

;; Enable 'treesit-install-language-grammar' to install the ruby
;; treesitter grammar required for 'ruby-ts-mode'
(add-to-list 'treesit-language-source-alist '(ruby "https://github.com/tree-sitter/tree-sitter-ruby"))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile\\'" . ruby-ts-mode))

;; = LANGUAGE SERVER PROTOCOL =

;; TODO: This code assumes that solargraph, or another ruby lsp, is
;; installed. Automatically install a language server if one dne.
;; (use-package lsp-mode :hook (ruby-ts-mode . lsp-deferred))

;; = FORMATTING =

;; TODO: setup auto formatting

;; = BUILD TARGETS =

;; = MISCELLANEOUS =

(provide 'init-lang-ruby)
;;; init-lang-ruby.el ends here.
