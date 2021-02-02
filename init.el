;;; -*- coding: utf-8 -*-

(add-hook 'after-init-hook
          (lambda ()
            (message (format "Initialization time: %s" (emacs-init-time)))))

(setq frame-title-format
      '((:eval "%b")
        (:eval (when (and (not buffer-read-only)
                          (buffer-modified-p))
                 "  •"))))

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

(menu-bar-mode 1)
(tool-bar-mode 1)
(tooltip-mode 1)
(scroll-bar-mode 1)
(blink-cursor-mode 0)
(fringe-mode '(8 . 8))
(global-hl-line-mode 1)

(set-frame-font "Consolas 10" nil t)

(set-frame-parameter (selected-frame) 'internal-border-width 8)

(set-background-color "#fefefc")

(set-face-background 'cursor "orange")
(set-face-background 'region "#ffffcc")
(set-face-background 'fringe (face-background 'default))
(set-face-background 'hl-line "#faf8f4")
(set-face-background 'trailing-whitespace "red")
(set-face-background 'internal-border "#faf8f4")

(set-face-foreground 'window-divider "#faf8f4")
(set-face-foreground 'window-divider-first-pixel "#f5f2ef")
(set-face-foreground 'window-divider-last-pixel "#f5f2ef")

(setq confirm-nonexistent-file-or-buffer nil)

(setq window-divider-default-right-width 12)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(fset 'display-startup-echo-area-message 'ignore) ; the official "hack" takes to long: '(inhibit-startup-echo-area-message (user-login-name))

(setq-default fill-column 80)
(setq-default tab-width 8)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq-default truncate-lines t)
(setq-default line-spacing 0.1)

;; from: https://github.com/luismbo/dot-emacs/blob/master/init.el
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil '(("\\<\\(TODO\\|FIXME\\|CAUTION\\)"
                                           1 font-lock-warning-face prepend)))))

;; from: https://github.com/magnars/.emacs.d/blob/master/settings/appearance.el
;; from: https://www.emacswiki.org/emacs/AlarmBell
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq visible-bell nil)
(setq ring-bell-function 'flash-mode-line)

(set-face-attribute 'mode-line nil
                    :height 1.0
                    :foreground (face-foreground 'default)
                    :foreground "#605e57"
                    :background "#f2ecdb"
                    :overline nil
                    :underline nil
                    :box `(:line-width 3
                           :color ,"#f0e0d0"
                           :style nil))
(set-face-attribute 'mode-line-inactive nil
                    :height 1.0
                    :foreground "#aba9a7"
                    :background "#faf8f4"
                    :overline nil
                    :underline nil
                    :inherit nil
                    :box `(:line-width 3
                           :color ,"#f5f2ef"
                           :style nil))

;; -----------------------------------------------------------------------------
(setq custom-file (concat user-emacs-directory "custom.el"))

(custom-set-variables
  '(echo-keystrokes 0.1)
  '(auto-save-default nil)
  '(make-backup-files nil)
  '(delete-selection-mode t)
  '(scroll-step 1)
  '(mouse-wheel-progressive-speed nil)
  '(mouse-wheel-scroll-amount '(7 ((shift) . 1)))
  '(split-width-threshold 200)
  '(split-height-threshold nil))

;; -- ido-mode -------------------------
(ido-mode t)

;; -- electric-pair-mode ---------------
(electric-pair-mode)

;; -- paren-mode -----------------------
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match (face-background 'region))
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; -- uniquify -------------------------
;; from: http://trey-jackson.blogspot.pt/2008/01/emacs-tip-11-uniquify.html -- for reverse
;; from: http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; -- extra --------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'extra-use-package) ; (package-initialize) -- for the why check package--ensure-init-file
(require 'extra-ediff)
(require 'extra-xml)
(require 'extra-utils)
(require 'extra-navigation)

;; -- grep -----------------------------
(require 'extra-gitgrep)

(setq extra-gitgrep-default-git-repo nil)
(setq extra-gitgrep-file-extensions "*.lisp *.cl *.el *.java")

(global-set-key (kbd "C-S-s") 'extra-gitgrep)
(global-set-key (kbd "C-M-S-s") 'extra-gitgrep-with-comments)

;; (setq-default compilation-context-lines t) ; when there is no fringe in grep this will show an arrow and does not scroll

;; -- hide/show ------------------------
(require 'extra-hide-show)

;; from: http://emacs-fu.blogspot.pt/2008/12/showing-and-hiding-blocks-of-code.html
(defun hide-show-fn ()
  (local-set-key (kbd "C-c <right>")  'hs-show-block)
  (local-set-key (kbd "C-c <left>")   'hs-hide-block)
  (local-set-key (kbd "C-c S-<up>")   'hs-toggle-all-comments)
  (local-set-key (kbd "C-c S-<down>") 'hs-show-all)
  (hs-minor-mode 1))

(add-hook 'prog-mode-hook 'hide-show-fn)

;; -- other packages -----------------------------------------------------------
(use-package popwin
  :config (popwin-mode 1)
          (global-set-key (kbd "C-z") popwin:keymap))

(use-package anzu
  :bind (("<remap> <query-replace>" . 'anzu-query-replace)
         ("<remap> <query-replace-regexp>" . 'anzu-query-replace-regexp))
  :config (global-anzu-mode t)
          (set-face-foreground 'anzu-mode-line "#FF6F00"))

(use-package company ; from: https://github.com/portacle/emacsd/blob/master/portacle-company.el
  :config (global-company-mode)
          (setq company-tooltip-limit 10)
          (setq company-dabbrev-downcase 0)
          (setq company-idle-delay 0.5)
          (setq company-echo-delay 0)
          (setq company-minimum-prefix-length 2)
          (setq company-require-match nil)
          (setq company-selection-wrap-around t)
          (setq company-tooltip-align-annotations t)
          ;; (setq company-tooltip-flip-when-above t)
          (setq company-transformers '(company-sort-by-occurrence)) ; weight by frequency
          (define-key company-active-map (kbd "<up>") 'company-select-previous)
          (define-key company-active-map (kbd "<down>") 'company-select-next)
          (define-key company-active-map (kbd "C-n") 'company-select-next)
          (define-key company-active-map (kbd "C-p") 'company-select-previous)
          (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
          (define-key company-active-map (kbd "M-.") 'company-show-location)
          (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
          (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
          (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
          (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
          (define-key (current-global-map) (kbd "TAB") 'company-indent-or-complete-common))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground 'unspecified :inherit 'error) ; from: https://writequit.org/eos/eos-appearance.html
          (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "DarkGoldenrod3")
          (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "DarkGoldenrod3")
          (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "DarkGoldenrod3")
          (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "DarkGoldenrod3")
          (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "DarkGoldenrod3")
          (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "DarkGoldenrod3")
          (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "DarkGoldenrod3")
          (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "DarkGoldenrod3")
          (set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground "DarkGoldenrod3"))

;; -- etc ----------------------------------------------------------------------
(let ((user-settings-dir (concat user-emacs-directory "users/" user-login-name))) ; from: http://whattheemacsd.com/init.el-06.html
  (cond ((file-exists-p user-settings-dir)
         (mapc 'load (directory-files user-settings-dir t "^[^#].*el$"))
         (message "%s loaded" user-settings-dir))
        (t
         (message "%s not found" user-settings-dir))))

(defun open-file (path)
  `(lambda ()
     (interactive)
     (find-file ,path)))

(defun my-kill-line ()
  (interactive)
  (let ((col (current-column)))
    (kill-whole-line)
    (move-to-column col)))

(defun my-just-one-space()
  (interactive "*")
  (just-one-space -1))

(defun halve-other-window-height ()
  (interactive)
  (enlarge-window (/ (window-height) 2)))

(defun halve-other-window-height2 ()
  (interactive)
  (shrink-window (/ (window-height) 2)))

(defun my-layout2 ()
  (interactive)
  (delete-other-windows)
  (split-window-right 120)
  (switch-to-buffer (other-buffer (current-buffer) 1)) ; https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
  (other-window 1))

;; -- global keys --------------------------------------------------------------
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-tab>") (lambda () (interactive) (other-window -1)))

(global-set-key [C-M-wheel-up] 'text-scale-increase)
(global-set-key [C-M-wheel-down] 'text-scale-decrease)

(global-set-key (kbd "C-M-#") (open-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c c") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
(global-set-key (kbd "C-S-k") 'my-kill-line)

(global-set-key [remap just-one-space] 'my-just-one-space)
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

(global-set-key [f5] 'revert-buffer-no-confirm)
(global-set-key [f9] 'whitespace-mode)
(global-set-key [f10] 'toggle-truncate-lines)
(global-set-key [f11] 'prelude-copy-file-name-to-clipboard)
(global-set-key [f12] 'open-buffer-path)

(define-key isearch-mode-map (kbd "C-d") 'sacha/isearch-yank-current-word) ; Type C-s (isearch-forward) to start interactively searching forward, and type C-x to get the current word.
(global-set-key '[M-up] 'sacha/search-word-backward)
(global-set-key '[M-down] 'sacha/search-word-forward)

(global-set-key [C-return] 'insert-todo)
(global-set-key [C-M-up] 'search-todo-backward)
(global-set-key [C-M-down] 'search-todo-forward)

(global-set-key (kbd "C-M-S-a") 'reposition-point-at-top)

(global-set-key (kbd "C-c v") 'halve-other-window-height)
(global-set-key (kbd "C-S-c v") 'halve-other-window-height2)
(global-set-key (kbd "C-c 2") 'my-layout2)
