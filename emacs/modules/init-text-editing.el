;;; init-text-editing.el --- ...
;;; Commentary:
;;; Code:

;; Builtin ----------------------------------------------------------------------

(setq-default indent-tabs-mode nil)

(electric-pair-mode 1)

(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "C-k") 'kill-whole-line)
(define-key ctl-j-map (kbd "C-d") 'zap-up-to-char)

;; Always leave point at the beginning of 'isearch' match.
(defun user/goto-isearch-match-beginning ()
  (when (and (not (eq this-command 'isearch-exit))
         isearch-forward
             (number-or-marker-p isearch-other-end)
             (not mark-active)
             (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))
(add-hook 'isearch-update-post-hook 'user/goto-isearch-match-beginning)

;; 3P ---------------------------------------------------------------------------

(use-package avy
  :bind
  (:map ctl-j-map
        ("C-s" . 'avy-goto-char-timer)
        ("C-l" . 'avy-goto-line)
        ("c l" . 'avy-copy-line)
        ("c r" . 'avy-copy-region)
        ("k l" . 'avy-kill-whole-line)
        ("k r" . 'avy-kill-region)))

;; Like vim 'ci'/'ca'
(use-package change-inner
  :bind
  (:map ctl-j-map
        ("C-a" . change-outer)
        ("TAB" . change-inner))) ; HACK: 'C-i' sends tabs in most terminals.

(use-package crux
  :demand t
  :bind
  (:map global-map
        ("C-q" . 'crux-smart-open-line-above)
        ("C-o" . 'crux-smart-open-line)))

(use-package whitespace-cleanup-mode
  :init
  (global-whitespace-cleanup-mode 1))

(provide 'init-text-editing)
;;; init-text-editing.el ends here
