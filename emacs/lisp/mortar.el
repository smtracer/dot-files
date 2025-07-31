;;; mortar.el ---
;;; Commentary:
;;; Code:

;; (defun rust-collect-test-commands ()
;;   "Return a list of alists (TEST-NAME . \"cargo test TEST-NAME\") for #[test] functions in a Rust file."
;;   ;; (interactive)
;;   (if (and buffer-file-name
;;            (string-match-p "\\.rs\\'" buffer-file-name))
;;       (save-excursion
;;         (goto-char (point-min))
;;         (let ((results '()))
;;           (while (re-search-forward "^\\s-*#\\[test\\]" nil t)
;;             ;; Move to the next line to find the function declaration
;;             (when (re-search-forward "^\\s-*fn\\s-+\\([a-zA-Z0-9_]+\\)" nil t)
;;               (let* ((test-name (match-string 1))
;;                      (transformed-test-name (format "file:%s" test-name)))
;;                 (push (cons transformed-test-name (format "cargo test %s" test-name)) results))))
;;           (reverse results)))
;;     (message "Not a Rust file.")
;;     nil))

(use-package compile-multi
  :config
  (setq compile-multi-annotate-string-cmds nil)
  (push `((locate-dominating-file default-directory "Cargo.toml")
          ("cargo:build" . "cargo build && sleep 5 && echo -e '\a'")
          ("cargo:run" . "cargo run")
          ("cargo:test" . "cargo test"))
          ;; ,#'rust-collect-test-commands)
        compile-multi-config))

(provide 'mortar)
;;; mortar.el ends here.
