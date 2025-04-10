;;; init-builtin-programming.el --- Built-in programming-related configuration.
;;; Commentary:
;;; Code:

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(use-package flymake
  :straight (:type built-in)
  :bind
  (:map user-overlay-prefix-map
        ("e n" . flymake-goto-next-error)
        ("e p" . flymake-goto-prev-error)))

(use-package treesit
  :straight (:type built-in)
  :config
  (setq treesit-font-lock-level 4
        ;; Configures the sources for 'treesit-install-language-grammar'
        treesit-language-source-alist '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                                        (c "https://github.com/tree-sitter/tree-sitter-c")
                                        (cmake "https://github.com/uyha/tree-sitter-cmake")
                                        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                                        (java "https://github.com/tree-sitter/tree-sitter-java")
                                        (json "https://github.com/tree-sitter/tree-sitter-json")
                                        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                                        (python "https://github.com/tree-sitter/tree-sitter-python")
                                        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
                                        (rust "https://github.com/tree-sitter/tree-sitter-rust")
                                        (toml "https://github.com/tree-sitter/tree-sitter-toml")
                                        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                                        (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

(provide 'init-builtin-programming)
;;; init-builtin-programming.el ends here
