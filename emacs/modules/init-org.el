;;; init-org.el --- ...
;;; Commentary:
;;; Code:

(use-package org
  :straight (:type built-in)
  :config
  (setq org-hide-emphasis-markers t
        org-agenda-files (directory-files-recursively org-directory "\\.org$")
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-scheduled-if-done t
        org-agenda-span 'day
        org-default-notes-file "~/org/inbox.org"
        org-todo-keywords '("TODO(t)" "WIP(w)" "WAIT(b)" "|" "DONE")
        org-capture-templates
        '(("t" "Task" entry (file "~/org/inbox.org")
           "* TODO %?")
          ("r" "Reminder" entry (file "~/org/reminders.org")
           "* TODO %?\n DEADLINE: <%<%Y-%m-%d>>")))
  :bind
  (:map ctl-j-map
        ("o a" . 'org-agenda)
        ("o C" . (lambda() (interactive)(org-capture nil "r")))
        ("o c" . (lambda() (interactive)(org-capture nil "t")))))


;;   :bind
;;   (:map ctl-j-map
;;         ("o a" . 'org-agenda)
;;         ("O" . 'org-capture)
;;         ("o f" . 'ext/org-find-file)
;;         ("o j" . 'ext/org-clock-goto)
;;         ("o o" . 'org-clock-out)
;;         ("o i" . 'org-clock-in-last))
;;   (:map org-mode-map
;;         ("C-c C-n" . 'ext/org-next-heading)
;;         ("C-c C-p" . 'ext/org-previous-heading)
;;         ("C-c C-f" . 'ext/org-next-heading-same-level)
;;         ("C-c C-b" . 'ext/org-previous-heading-same-level)
;;         ("C-c RET" . 'org-insert-heading-after-current)))

;; ;; (with-eval-after-load 'org
;; ;;   (set-face-attribute 'bold nil
;; ;;                       :weight 'bold    ;; Make the bold weight different (e.g., 'light, 'regular, 'bold)
;; ;;                       :foreground "blue" ;; Change the color (blue in this example)
;; ;;                       ;; :family "Courier"  ;; You can specify a different font family (e.g., "Courier", "Arial", etc.)
;; ;;                       ))

;; (defun my/org-bold-font-face-setup ()
;;   "Customize the bold font face for Org mode buffers only."
;;   (set-face-attribute 'bold nil
;;                       :weight 'bold      ;; You can set the weight to different values
;;                       :foreground "#eb6f92")) ;; Change the color

;; (add-hook 'org-mode-hook 'my/org-bold-font-face-setup)



(provide 'init-org)
;; ;;; init-org.el ends here
