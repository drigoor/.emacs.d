;;; etc.el --- separation of concerns

;;; Code:

;; (defvar windows-p (string-match "windows" (symbol-name system-type)))
;;
;; (when windows-p
;;   (add-to-list 'exec-path "C:/home/bin/git/usr/bin")) ; to allow ediff et al. in emacs
;;
;; (add-to-list 'exec-path "C:/msys64/mingw64/bin")
;; (add-to-list 'exec-path "C:/msys64/mingw64/lib")

;; -- other packages -----------------------------------------------------------


;; -- magit ----------------------------
;; references:
;; https://github.com/bradwright/emacs-d/blob/master/packages/init-magit.el
;; http://whattheemacsd.com/setup-magit.el-01.html
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c b" . magit-blame)
         :map magit-status-mode-map
         ("q" . magit-quit-session))
  :config (defun magit-status-around (orig-fun &rest args)
            (window-configuration-to-register 'x)
            (delete-other-windows)
            (apply orig-fun args))
          (advice-add 'magit-status :around #'magit-status-around) ; check: https://www.gnu.org/software/emacs/manual/html_node/elisp/Porting-old-advice.html
          (defun magit-quit-session ()
            (interactive)
            (kill-buffer)
            (jump-to-register 'x))
          (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))


;; -- lisp ---------------------------------------------------------------------

;; to install sbcl:
;; INSTALL_ROOT=/home/user/bin/sbcl sh install.sh

(use-package sly
  :config (setq inferior-lisp-program (expand-file-name "C:/home/scoop/apps/sbcl/current/sbcl.exe")))


;; -- clojure ------------------------------------------------------------------

(lbo:ensure-package 'cider) ; https://gist.github.com/matheusemm/d3b216a0369c3af6f4d83549276bacb1
;; clj-refactor

;; from: http://www.lisperati.com/#!Forbidden_Fruits_of_the_Clojure_REPL
(defun wrap-clojure-command ()
    "takes a command written at the cider prompt and executes it as (repl-command \"...\")"
  (interactive)
  (move-beginning-of-line nil)
  (insert "(repl-command \"")
  (move-end-of-line nil)
  (insert "\")")
  (cider-repl-return))
(global-set-key "\M-z" 'wrap-clojure-command)

;; ;; -- shell-pop ------------------------
;; (use-package shell-pop ; from: http://pragmaticemacs.com/emacs/pop-up-a-quick-shell-with-shell-pop/
;;   :bind (("C-x t" . shell-pop))
;;   :config (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
;;           (setq shell-pop-term-shell "/bin/bash")
;;           ;; need to do this manually or not picked up by `shell-pop'
;;           (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))
;;
;; FIXME -- 
;; shell pop is not working with windows, maybe checking the following code...
;;
;; from: https://stackoverflow.com/questions/20263012/m-x-term-with-emacs-on-ms-windows-error-spawning-child-process-invalid-argum
;; (apply 'start-process name buffer
;;            (getenv "SHELL") "-c"
;;        (format "stty -nl echo rows %d columns %d sane 2>/dev/null;\
;; if [ $1 = .. ]; then shift; fi; exec \"$@\""
;;            term-height term-width)
;;        ".."
;;        command "-i" switches)

;;; etc.el ends here
