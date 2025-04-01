;;; init-programming.el ---
;;; Commentary:
;;; Code:

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode))
  :init
  (setq hl-todo-keyword-faces '(("TODO" . warning)
                               ("HACK" . warning)
                               ("BUG" . error)
                               ("FIXME" . error))))

(use-package centaur-tabs
  :hook
  ((dired-mode . centaur-tabs-local-mode)
   (org-mode . centaur-tabs-local-mode)
   (org-agenda-mode . centaur-tabs-local-mode)
   (org-capture-mode . centaur-tabs-local-mode)
   (compilation-mode . centaur-tabs-local-mode))
  :init
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-close-button nil
        centaur-tabs-show-new-tab-button nil))
;; :hook
;;   (dired-mode . centaur-tabs-local-mode)
;;   ...)

(use-package treemacs
  :config
  (treemacs-project-follow-mode t)
  :bind
  (:map ctl-j-map
        ("t" . treemacs)))

(use-package flymake
  :straight (:type built-in)
  :bind
  (:map ctl-j-map
        ("e n" . flymake-goto-next-error)
        ("e p" . flymake-goto-prev-error)))

(use-package treesit
  :straight (:type built-in)
  :init
  (setq treesit-font-lock-level 4))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-j a"
        ;; Suggested by 'lsp-doctor'
        lsp-modeline-code-actions-enable nil
        read-process-output-max (* 1024 1024)
        lsp-lens-auto-enable nil
        lsp-auto-execute-action nil
        lsp-signature-render-documentation nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-delay 0
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-diagnostics nil)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package magit
  :config
  (setq magit-save-repository-buffers 'dontask))

;; - Language Configuration -

;; -- (ba)sh --

(add-to-list 'treesit-language-source-alist '(bash "https://github.com/tree-sitter/tree-sitter-bash"))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.bash_profile\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("bashrc" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("bash_profile" . bash-ts-mode))
(use-package lsp-mode :hook (bash-ts-mode . lsp-deferred))

;; -- emacs lisp --
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

;; -- json --

(add-to-list 'treesit-language-source-alist '(json "https://github.com/tree-sitter/tree-sitter-json"))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

;; -- python --

(add-to-list 'treesit-language-source-alist '(python "https://github.com/tree-sitter/tree-sitter-python"))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
;; TODO: add lsp(?)

;; -- ruby --

(add-to-list 'treesit-language-source-alist '(ruby "https://github.com/tree-sitter/tree-sitter-ruby"))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile\\'" . ruby-ts-mode))

;; -- rust --

(add-to-list 'treesit-language-source-alist '(rust "https://github.com/tree-sitter/tree-sitter-rust"))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
;; TODO: This code assumes that rust-analyzer, or another rust lsp, is
;; installed.
(use-package lsp-mode
  :hook (rust-ts-mode . lsp-deferred))
;; Disable rust-ts-mode's diagnostics in favor of lsp
(setq-default rust-ts-flymake-command nil)

;; Autoformat buffers using 'rustfmt' on save.
(use-package reformatter
  :init
  (reformatter-define rust-reformatter
    :program "rustfmt"
    :args '("--quiet" "--emit" "stdout"))
  (add-hook 'rust-ts-mode-hook 'rust-reformatter-on-save-mode))

;; -- toml --

(add-to-list 'treesit-language-source-alist '(toml "https://github.com/tree-sitter/tree-sitter-toml"))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))

;; -- typescript --

(add-to-list 'treesit-language-source-alist '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
;; TODO: use lsp(?)

;; -- yaml --

(add-to-list 'treesit-language-source-alist '(yaml "https://github.com/ikatyang/tree-sitter-yaml"))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

(provide 'init-programming)
