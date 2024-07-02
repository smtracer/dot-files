;;; init-org.el --- TODO
;;; Commentary:
;;
;; TODO: use 'org-directory' instead of duplicating "~/org"
;;
;;; Code:

(use-package org
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-directory "~/org"
	org-agenda-files '("~/org")
	org-default-notes-file (concat org-directory "/notes.org")
	org-cycle-separator-lines 1
	org-fold-catch-invisible-edits "show-and-error"
	org-todo-keywords '((sequence "TODO" "WIP" "BUILD" "REVIEW"
				      "DEPLOY" "|" "DONE" "CANCELED")))
  :bind
  (:map ctl-j-map
	("o a" . org-agenda)
	("o c" . org-capture)
	;; Break with the 'o-*' keybind pattern to mirror the default
	;; 'org-clock-*' function keybinds in 'org-mode', replacing
	;; prefix 'C-c' with 'C-j' for global access.
	("C-x C-j" . org-clock-goto)
	("C-x C-i" . org-clock-in)
	("C-x C-o" . org-clock-out)))

(use-package org-journal
  :config
  (setq ;; Search string used to find carryover items.
        ;; Format is the same as an agenda match search:
        ;; https://orgmode.org/manual/Matching-tags-and-properties.html.
        org-journal-carryover-items "TODO=\"TODO\"|\
                                     TODO=\"WIP\"|\
                                     TODO=\"BUILD\"|\
                                     TODO=\"REVIEW\"|\
                                     TODO=\"DEPLOY\""
	org-journal-dir "~/org"
	org-journal-file-type 'daily
	org-journal-skip-carryover-drawers nil
	;; Suffix journal files with ".org" to make them discoverable
	;; by 'org-agenda' (more precisely, by the default
	;; implementation of 'org-agenda-files', which returns a
	;; list of the files for 'org-agenda').
	org-journal-file-format "%Y%m%d.org"
	;; Disable per-entry time prefix. 
	org-journal-time-format "")
  :bind
  (:map ctl-j-map
	("o j" . org-journal-new-entry)))

(use-package org-modern
  :init
  (with-eval-after-load 'org (global-org-modern-mode)))

;; C-c C-x b - to indirect buffer
;; C-c C-r - 'org-reveal' useful for sparse trees
;; C-f/C-b forward same level heading
;; C-n/C-p next header
;; C-c C-u up heading
;; C-c * 'org-toggle-heading'
;;
;; C-x n [s|b|w] - narrow [s]ubtree, code [b]lock, [w]iden
;;
;; C-c C-c on a checkbox toggles state

;; #+STARTUP: overview
;; #+STARTUP: content
;; #+STARTUP: showall
;; #+STARTUP: show2levels
;; #+STARTUP: show3levels
;; #+STARTUP: show4levels
;; #+STARTUP: show5levels
;; #+STARTUP: showeverything

(provide 'init-org)
;;; init-org.el ends here.
