;;; init-builtin-org.el --- ...
;;; Commentary:
;;; Code:

(use-package org
  :straight (:type built-in)
  :hook (org-mode . org-indent-mode)
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
           "* TODO %?\n DEADLINE: <%<%Y-%m-%d>>"))
        org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-insert-heading-respect-content t
        org-pretty-entities t
        org-agenda-tags-column 0
        org-indent-mode t)
  :bind
  (:map user-overlay-prefix-map
        ("o a" . 'org-agenda)
        ("o C" . (lambda() (interactive)(org-capture nil "r")))
        ("o c" . (lambda() (interactive)(org-capture nil "t")))))

(provide 'init-builtin-org)
;;; init-builtin-org.el ends here
