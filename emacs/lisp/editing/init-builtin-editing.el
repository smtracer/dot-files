;;; init-builtin-editing.el ---
;;; Commentary:
;;; Code:

(setq-default indent-tabs-mode nil)
(electric-pair-mode 1)
(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "C-k") 'kill-whole-line)

(defun vl/save-region-to-new-file (filename)
  "Save the active region to a new file with FILENAME."
  (interactive "FSave region to file: ")
  (if (use-region-p)
      (write-region (region-beginning) (region-end) filename)
    (message "No region selected.")))
(define-key user-overlay-prefix-map (kbd "C-e w") 'vl/save-region-to-new-file)

(defun vl/isearch-keep-point-at-start ()
  "Move point to the beginning of the search string after isearch."
  (when (and isearch-forward (boundp 'isearch-other-end) isearch-other-end)
    (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook 'vl/isearch-keep-point-at-start)

;; Copy head of emacs kill-ring to the system clipboard via ANSI OSC 52 escape
;; sequences.
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

(provide 'init-builtin-editing)
;;; init-builtin-editing.el ends here
