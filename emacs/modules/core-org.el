;;; core-org.el --- ...
;;; Commentary:
;;; Code:

(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$")
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-span 'day
      org-default-notes-file "~/org/inbox.org"
      org-todo-keywords '("TODO" "PLAN" "IMPL" "TEST" "REVW" "DEPL" "|" "DONE" "BLOC")
      org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks") "* TODO %?\n %a")
        ("c" "Clock" plain (clock) "--------------------\nNote from %a\n\n%?" :prepend t)))

(defun goto-org-index ()
  (interactive)
  (find-file "~/org/index.org"))

(defun goto-clocked-task ()
  (interactive)
  (org-clock-goto)
  (org-narrow-to-subtree))

(define-key ctl-j-map (kbd "o a") 'org-agenda)
(define-key ctl-j-map (kbd "o c") 'org-capture)
(define-key ctl-j-map (kbd "o f") 'goto-org-index)
(define-key ctl-j-map (kbd "o j") 'goto-clocked-task)
(define-key ctl-j-map (kbd "o o") 'org-clock-out)
(define-key ctl-j-map (kbd "o i") 'org-clock-in-last)
(define-key ctl-j-map (kbd "o z") (lambda () (interactive) (org-capture nil "c")))

(provide 'core-org)
;;; core-org.el ends here
