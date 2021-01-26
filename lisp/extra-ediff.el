;;; extra-ediff.el --- make ediff great

;;; Notes:
;;; Currently only works for ediffing files, buffers or regions linewise. All
;;; other ediff commands behave as default.

;;; Code:

(provide 'extra-ediff)

(require 'ediff)

(custom-set-variables
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain))

;; from: https://emacs.stackexchange.com/questions/7482/restoring-windows-and-layout-after-an-ediff-session/7486

(defvar ediff-last-windows nil
  "Last ediff window configuration.")

(defun ediff-restore-windows ()
  "Restore window configuration to `ediff-last-windows'."
  (set-window-configuration ediff-last-windows)
  (remove-hook 'ediff-after-quit-hook-internal 'ediff-restore-windows))

(defun ediff-save-windows ()
  "Save window configuration to `ediff-last-windows'."
  (setq ediff-last-windows (current-window-configuration))
  (add-hook 'ediff-after-quit-hook-internal 'ediff-restore-windows))

(defadvice ediff-files (around ediff-restore-windows activate)
  (ediff-save-windows)
  ad-do-it)

(defadvice ediff-buffers (around ediff-restore-windows activate)
  (ediff-save-windows)
  ad-do-it)

(defadvice ediff-regions-linewise (around ediff-restore-windows activate)
  (ediff-save-windows)
  ad-do-it)

;;; extra-ediff.el ends here
