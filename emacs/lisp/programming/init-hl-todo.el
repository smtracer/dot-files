;;; init-hl-todo.el ---
;;; Commentary:
;;; Code:

(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode) (conf-mode . hl-todo-mode))
  :config
  (setq hl-todo-keyword-faces '(("DON'T" . warning)
                                ("DONT" . warning)
                                ("HACK" . warning)
                                ("Note" . warning)
                                ("NOTE" . warning)
                                ("TODO" . warning)
                                ("BUG" . error)
                                ("FIXME" . error))))

(provide 'init-hl-todo)
;;; init-hl-todo.el ends here
