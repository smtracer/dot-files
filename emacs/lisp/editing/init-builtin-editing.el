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
  (when isearch-forward
    (goto-char isearch-other-end)))
(add-hook 'isearch-update-post-hook 'vl/isearch-keep-point-at-start)

(provide 'init-builtin-editing)
;;; init-builtin-editing.el ends here
