;;; init-keyboard-pill.el --- TODO
;;; Commentary:
;;; Code:
;;; "www.google.com"

(use-package avy
  :bind
  (:map ctl-j-map
	("C-s" . 'avy-goto-char-timer)
	("C-l" . 'avy-goto-line)
	("c l" . 'avy-copy-line)
	("c r" . 'avy-copy-region)
	("k l" . 'avy-kill-whole-line)
	("k r" . 'avy-kill-region))
  :config
  (setq avy-single-candidate-jump nil)  
  (defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package embark
  :demand t
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
		 (window-parameters (mode-line-format . none))
                 (window-parameters (header-line-format . none))))
  :bind
  (:map global-map
	("M-'" . 'embark-act)))

(provide 'init-keyboard-pill)
;;; init-keyboard-pill.el ends here
