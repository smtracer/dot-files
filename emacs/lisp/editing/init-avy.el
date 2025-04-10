;;; init-avy.el ---
;;; Commentary:
;;; Code:

(use-package avy
  :bind
  (:map user-overlay-prefix-map
        ("C-l" . avy-goto-line)
        ("k l" . avy-kill-whole-line)
        ("k r" . avy-kill-region)
        ("C-s" . avy-goto-char-timer)))

(provide 'init-avy)
;;; init-avy.el ends here
