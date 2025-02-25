;;; init-org.el --- ...
;;; Commentary:
;;; Code:

(defun ext/org-find-file ()
  (interactive)
  (let ((default-directory org-directory))
    (ido-find-file)))

(defun ext/org-clock-goto ()
  (interactive)
  (org-clock-goto)
  (org-narrow-to-subtree))

(use-package org
  :straight (:type built-in)
  :init
  (setq org-hide-emphasis-markers t
        org-agenda-files (directory-files-recursively "~/org/" "\\.org$")
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-span 'day
        org-log-into-drawer t
        org-default-notes-file "~/org/inbox.org"
        org-todo-keywords '("TODO(t)" "WIP(w)" "WAIT(b)"  "|" "DONE")
        org-capture-templates
        '(("t" "Todo" entry (file "~/org/inbox.org") "* TODO %?")
          ("f" "Fast" entry (file+headline "~/org/inbox.org" "Refile") "* %?")
          ("w" "Note on clock" plain (clock) "--------------------\nNote from %a\n%?" :prepend t)))
  :bind
  (:map ctl-j-map
        ("o a" . 'org-agenda)
        ("O" . 'org-capture)
        ("o f" . 'ext/org-find-file)
        ("o j" . 'ext/org-clock-goto)
        ("o o" . 'org-clock-out)
        ("o i" . 'org-clock-in-last))
  (:map org-mode-map
        ("C-c RET" . 'org-insert-heading-after-current)))

(provide 'init-org)
;;; init-org.el ends here
