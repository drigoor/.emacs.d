;;; extra-utils.el --- utilities for emacs

;;; Code:

(provide 'extra-utils)

;; from: https://gitorious.org/gnu-emacs-config/mainline/blobs/a3fe6e69d9a752ef094448bfdf1794ce39916f4d/dotemacs.el
(defun comment-or-uncomment-region-or-line ()
  "Like comment-or-uncomment-region, but if there's no mark \(that means no 194 region\) apply comment-or-uncomment to the current line"
  (interactive)
  (if (not mark-active)
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
      (if (< (point) (mark))
          (comment-or-uncomment-region (point) (mark))
          (comment-or-uncomment-region (mark) (point)))))

;; from: http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

;; from: http://www.emacswiki.org/emacs/RevertBuffer
(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
      (error "The buffer has been modified")))

;; from: http://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
;; original: https://github.com/bbatsov/prelude
(defun prelude-copy-file-name-to-clipboard ()
  "Display and copy to the clipboard the current buffer file name."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                      (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "%s" filename))))

;; ;; from: https://github.com/luismbo/dot-emacs/blob/master/siscog/site.el
;; ;; and: https://www.emacswiki.org/emacs/download/w32-browser.el
;; (defun open-buffer-path ()
;;   (interactive)
;;   (if (eq major-mode 'dired-mode)
;;       (w32-shell-execute "explore" (expand-file-name default-directory))
;;       (w32-shell-execute "open" "explorer" (concat "/e,/select," (convert-standard-filename buffer-file-name)))))
;;
;;
;;
;; from LBO > scg-open-explorer
(defun open-buffer-path ()
  (interactive)
  (cl-flet ((w32ify-path (path)
              (convert-standard-filename (replace-regexp-in-string "/" "\\" path t t))))
    (cond (buffer-file-name
            (w32-shell-execute "open" "explorer"
               (concat "/e,/select," (w32ify-path buffer-file-name))))
          (default-directory
           (w32-shell-execute "explore" (w32ify-path default-directory)))
          (t
           (user-error "Current buffer not associated with any path")))))

;;; extra-utils.el ends here
