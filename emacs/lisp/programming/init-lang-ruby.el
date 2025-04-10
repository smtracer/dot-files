;;; init-lang-ruby.el ---
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile\\'" . ruby-ts-mode))

(provide 'init-lang-ruby)
;;; init-lang-ruby.el ends here
