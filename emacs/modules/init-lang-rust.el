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

;; = MISCELLANEOUS =

(provide 'init-lang-rust)
;;; init-lang-rust.el ends here.
