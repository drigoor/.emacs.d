;;; extra-gitgrep.el --- utility for faster search using git grep with history

;;; (setq extra-gitgrep-file-extensions "*.lisp *.cl")
;;;
;;; (global-set-key (kbd "C-S-s") 'extra-gitgrep)
;;; (global-set-key (kbd "C-M-S-s") 'extra-gitgrep-with-comments)

;;; Code:

(provide 'extra-gitgrep)

(require 'grep)

(defvar extra-gitgrep-default-git-repo nil
  "If a git repository is not found, this specifies where to search by default.")

(defvar extra-gitgrep-file-extensions "*.lisp *.bil *.cl"
  "Default file extensions to search.")

(defvar extra-gitgrep-default-comment-string ";"
  "Default string for comments.")

(defun extra-gitgrep-command (search-in-comments-p)
  (let ((dir (or (vc-root-dir)
                 extra-gitgrep-default-git-repo)))
    (let ((regexp (grep-read-regexp)))
      (when (and (stringp regexp) (> (length regexp) 0))
        (let ((command (grep-expand-template "git --no-pager grep -n -i -e <R> -- <F>"
                                             (if search-in-comments-p
                                                 regexp
                                                 (format "^[^%s]*%s" extra-gitgrep-default-comment-string regexp))
                                             extra-gitgrep-file-extensions)))
          (when command
            (add-to-history 'grep-history command)
            (let ((default-directory dir)
                  (compilation-environment (cons "PAGER=" compilation-environment)))
              ;; Setting process-setup-function makes exit-message-function work
              ;; even when async processes aren't supported.
              (compilation-start command 'grep-mode))
            (when (eq next-error-last-buffer (current-buffer))
              (setq default-directory dir))))))))

(defun extra-gitgrep ()
  (interactive)
  (extra-gitgrep-command nil))

(defun extra-gitgrep-with-comments ()
  (interactive)
  (extra-gitgrep-command t))

;;; extra-gitgrep.el ends here
