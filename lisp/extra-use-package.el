;;; extra-use-package.el --- simplifying package management using use-package

;;; Code:

(provide 'extra-use-package)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; from: https://github.com/luismbo/dot-emacs/blob/master/init.el
(defvar lbo:*auto-refreshed-packages* nil
  "True if `lbo:ensure-package' has already refreshed the package
  list in the current session")

(defun lbo:ensure-package (name)
  (unless (package-installed-p name)
    (unless lbo:*auto-refreshed-packages*
      (package-refresh-contents)
      (setq lbo:*auto-refreshed-packages* t))
    (package-install name)))

(lbo:ensure-package 'use-package)

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;;; extra-use-package.el ends here
