;;; -*- coding: utf-8 -*-

;; refs: https://github.com/bbatsov/emacs.d/blob/master/init.el
;; refs: https://github.com/luismbo/dot-emacs/blob/master/init.el
;; refs: https://github.com/zamansky/dot-emacs/blob/main/init.el
;;       https://cestlaz.github.io/stories/emacs/

;; USAR: https://ebzzry.com/en/emacs-pairs/

;; USAR: https://andreyorst.gitlab.io/posts/2020-05-10-making-emacs-tabs-look-like-in-atom/
;; USAR: https://andreyorst.gitlab.io/posts/2020-07-21-programming-ligatures-in-emacs/

;; LEARNING: https://github.com/ssloy/tinyrenderer
;; LEARNING: https://github.com/ssloy/tinyraytracer
;;           https://andreyorst.gitlab.io/posts/2020-10-15-raymarching-with-fennel-and-love/

;;;; Load Path

                                        ; (add-to-list 'load-path "~/.emacs.d/lisp")


;; emacs focus

;;;; package.el

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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
  (setq use-package-always-ensure t)
  (setq use-package-verbose t))

;;;;

(setq large-file-warning-threshold 100000000) ; warn when opening files bigger than 100MB

(setq gc-cons-threshold most-positive-fixnum) ; Minimize garbage collection during startup

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23)) ; Lower threshold back to 8 MiB (default is 800kB)
            (message (format "Initialization time: %seconds" (emacs-init-time)))))

(setq confirm-kill-processes nil) ; quit Emacs directly even if there are running processes

(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(set-fringe-mode 10)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;; nice scrolling
(setq scroll-margin 0
      ;; scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; (when (fboundp 'pixel-scroll-precision-mode)
;;   (pixel-scroll-precision-mode t))

(cond
 ((find-font (font-spec :name "Consolas 10")) ; windows
  (set-frame-font "Consolas 10"))
 ((find-font (font-spec :name "DejaVu Sans Mono-9")) ; linux
  (set-frame-font "DejaVu Sans Mono-9")))

(setq-default line-spacing 0.15)

(line-number-mode 1)
(column-number-mode 1)

(fset 'yes-or-no-p 'y-or-n-p) ; enable y/n answers

;; (add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; maximize the initial frame automatically

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        (:eval (when (and (not buffer-read-only)
                          (buffer-modified-p))
                 "  •"))))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(setq require-final-newline t)

(setq-default fill-column 80)

(setq custom-file (concat user-emacs-directory "custom.el"))

(custom-set-variables
 ;;  '(echo-keystrokes 0.1)
 '(auto-save-default nil)
 '(make-backup-files nil)
 '(delete-selection-mode t)
 '(scroll-step 1)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(7 ((shift) . 1) ((meta) . hscroll) ((control) . text-scale)))
 ;;  '(split-width-threshold 200)
 ;;  '(split-height-threshold nil)
 )

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(define-key 'help-command (kbd "C-i") #'info-display-manual)

(setq tab-always-indent 'complete) ; smart tab behavior - indent or complete

(defconst savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

(use-package saveplace ; saveplace remembers your location in a file when saving files
  :config
  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  (setq-default save-place t)) ; activate it for all buffers

(use-package desktop ; state of emacs is saved from one session to another
  :config
  (desktop-save-mode +1))

(use-package windmove
  :config
  (windmove-default-keybindings 'M)) ; use Meta + arrow keys to switch between visible buffers

;; -- ido-mode -------------------------
(ido-mode t)

(use-package elec-pair
  :config
  (electric-pair-mode +1))

;; -- paren-mode -----------------------
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
;; (set-face-background 'show-paren-match (face-background 'region))
(set-face-attribute 'show-paren-match nil :weight 'bold)

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode +1))


;; -- uniquify -------------------------
;; http://trey-jackson.blogspot.pt/2008/01/emacs-tip-11-uniquify.html -- for reverse
;; http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
;; (use-package uniquify
;;   :config
;;   (setq uniquify-buffer-name-style 'forward)
;;   (setq uniquify-separator "/")
;;   (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
;;   (setq uniquify-ignore-buffers-re "^\\*"))
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'flyspell-mode)
  (diminish 'flyspell-prog-mode)
  (diminish 'eldoc-mode))

(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char)
  :config
  (setq avy-background t))

(use-package whitespace
  :init
  ;; (dolist (hook '(prog-mode-hook text-mode-hook))
  ;;   (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail))
  (diminish 'whitespace-mode))

;; from: http://nschum.de/src/emacs/
(use-package highlight-symbol
  :config
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
  (setq highlight-symbol-colors '("orange"))
  (setq highlight-symbol-foreground-color "red")
  (set-face-attribute 'highlight-symbol-face nil
                      :foreground "red"
                      :background "red"))
;; improvements:
;;   + show status in modeline!
;;   + colors VS faces

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)))

(use-package anzu
  :ensure t
  :bind
  (("<remap> <query-replace>" . 'anzu-query-replace)
   ("<remap> <query-replace-regexp>" . 'anzu-query-replace-regexp))
  :config
  (global-anzu-mode t)
  (set-face-foreground 'anzu-mode-line "#FF6F00")
  (diminish 'anzu-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
  (diminish 'rainbow-mode))


(use-package clojure-mode
  :ensure t
  :config
  ;; teach clojure-mode about some macros that I use on projects like
  ;; nREPL and Orchard
  (define-clojure-indent
    (returning 1)
    (testing-dynamic 1)
    (testing-print 1))

;;  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package inf-clojure
  :ensure t
  :config
  (add-hook 'inf-clojure-mode-hook #'paredit-mode)
  (add-hook 'inf-clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package flycheck-joker
  :ensure t)

(use-package web-mode
  :ensure t
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-quick-access t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode)
  (diminish 'company-mode))

;; TODO
;;

(use-package hl-todo ; from: https://www.reddit.com/r/emacs/comments/f8tox6/todo_highlighting/
  :hook
  (prog-mode . hl-todo-mode)
  :bind
  ("C-c p" . 'hl-todo-previous)
  ("C-c n" . 'hl-todo-next)
  ("C-c o" . 'hl-todo-occur)
  ("C-c i" . 'hl-todo-insert)
  :config
  ;; (global-hl-todo-mode)
  (setq hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))
;; hl-todo-require-punctuation (not nil)
;; hl-todo-highlight-punctuation ":"
;;
;;   (setq hl-todo-color-background t
;;
;; (defface hl-todo
;;   '((t (:bold t :foreground "#ffffff")))
;;   "Face for highlighting the HOLD keyword."
;;   :group 'hl-todo)
;; (defface hl-todo-TODO
;;   '((t :background "#f0ffd0" :foreground "#ff0000" :inherit (hl-todo)))
;;   "Face for highlighting the HOLD keyword."
;;   :group 'hl-todo)



(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-eldev
  :ensure t)

(use-package crux
  :ensure t
  ;; :bind (("C-c o" . crux-open-with)
  ;;        ("M-o" . crux-smart-open-line)
  ;;        ("C-c n" . crux-cleanup-buffer-or-region)
  ;;        ("C-c f" . crux-recentf-find-file)
  ;;        ("C-M-z" . crux-indent-defun)
  ;;        ("C-c u" . crux-view-url)
  ;;        ("C-c e" . crux-eval-and-replace)
  ;;        ("C-c w" . crux-swap-windows)
  ;;        ("C-c D" . crux-delete-file-and-buffer)
  ;;        ("C-c r" . crux-rename-buffer-and-file)
  ;;        ("C-c t" . crux-visit-term-buffer)
  ;;        ("C-c k" . crux-kill-other-buffers)
  ;;        ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
  ;;        ("C-c I" . crux-find-user-init-file)
  ;;        ("C-c S" . crux-find-shell-init-file)
  ;;        ("s-r" . crux-recentf-find-file)
  ;;        ("s-j" . crux-top-join-line)
  ;;        ("C-^" . crux-top-join-line)
  ;;        ("s-k" . crux-kill-whole-line)
  ;;        ("C-<backspace>" . crux-kill-line-backwards)
  ;;        ("s-o" . crux-smart-open-line-above)
  ;;        ([remap move-beginning-of-line] . crux-move-beginning-of-line)
  ;;        ([(shift return)] . crux-smart-open-line)
  ;;        ([(control shift return)] . crux-smart-open-line-above)
  ;;        ([remap kill-whole-line] . crux-kill-whole-line)
  ;;        ("C-c s" . crux-ispell-word-then-abbrev))
  )

(use-package consult
  :ensure t
  :bind (
         ;; ;; C-x bindings (ctl-x-map)
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ;; ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; ;; Other custom bindings
         ;; ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; ;; M-g bindings (goto-map)
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flycheck)
         ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ;; ("M-g i" . consult-imenu)
         ;; ("M-g I" . consult-imenu-multi)
         ;; ;; M-s bindings (search-map)
         ;; ;; ("M-s f" . consult-find)
         ;; ;; ("M-s F" . consult-locate)
         ;; ;; ("M-s g" . consult-grep)
         ;; ;; ("M-s G" . consult-git-grep)
         ;; ;; ("M-s r" . consult-ripgrep)
         ;; ;; ("M-s l" . consult-line)
         ;; ;; ("M-s L" . consult-line-multi)
         ;; ;; ("M-s m" . consult-multi-occur)
         ;; ;; ("M-s k" . consult-keep-lines)
         ;; ;; ("M-s u" . consult-focus-lines)
         ))



;; -- global keys --------------------------------------------------------------

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "C-<iso-lefttab>") (lambda () (interactive) (other-window -1)))

;; (use-package marginalia
;;   :config
;;   (marginalia-mode)
;;   (marginalia-cycle))


;; Set the modeline to tell me the filename, hostname, etc..
;; (setq-default mode-line-format
;;               (list " "
;;                                         ; */% indicators if the file has been modified
;;                     'mode-line-modified
;;                     "--"
;;                                         ; the name of the buffer (i.e. filename)
;;                                         ; note this gets automatically highlighted
;;                     'mode-line-buffer-identification
;;                     "--"
;;                                         ; major and minor modes in effect
;;                     'mode-line-modes
;;                                         ; if which-func-mode is in effect, display which
;;                                         ; function we are currently in.
;;                     '(which-func-mode ("" which-func-format "--"))
;;                                         ; line, column, file %
;;                     'mode-line-position
;;                     "--"
;;                                         ; if vc-mode is in effect, display version control
;;                                         ; info here
;;                     `(vc-mode vc-mode)
;;                     "--"
;;                                         ; hostname
;;                     'system-name
;;                                         ; dashes sufficient to fill rest of modeline.
;;                     "-%-"
;;                     )
;;               )



;;; PREVIOUS


;; (set-frame-parameter (selected-frame) 'internal-border-width 8)

(set-background-color "#fefefc")

(set-face-background 'cursor "orange")
;; (set-face-background 'region "#ffffcc")
;; (set-face-background 'fringe (face-background 'default))
;; (set-face-background 'hl-line "#faf8f4")
;; (set-face-background 'trailing-whitespace "red")
;; (set-face-background 'internal-border "#faf8f4")

;; (set-face-foreground 'window-divider "#faf8f4")
;; (set-face-foreground 'window-divider-first-pixel "#f5f2ef")
;; (set-face-foreground 'window-divider-last-pixel "#f5f2ef")

(setq confirm-nonexistent-file-or-buffer nil)

;; (setq window-divider-default-right-width 12)
;; (setq window-divider-default-places 'right-only)
;; (window-divider-mode 1)

;; (fset 'display-startup-echo-area-message 'ignore) ; the official "hack" takes to long: '(inhibit-startup-echo-area-message (user-login-name))

;; (setq-default tab-width 4)
;; (setq-default show-trailing-whitespace t)
(setq-default truncate-lines t)


;; ;; from: https://github.com/magnars/.emacs.d/blob/master/settings/appearance.el
;; ;; from: https://www.emacswiki.org/emacs/AlarmBell
;; (defun flash-mode-line ()
;;   (invert-face 'mode-line)
;;   (run-with-timer 0.1 nil #'invert-face 'mode-line))
;; (setq ring-bell-function 'flash-mode-line)
;; (setq visible-bell nil)

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

;; (require 'extra-use-package) ; (package-initialize) -- for the why check package--ensure-init-file
;; (require 'extra-ediff)
;; (require 'extra-xml)
;; (require 'extra-utils)
;; (require 'extra-navigation)

;; ;; -- grep -----------------------------
;; (require 'extra-gitgrep)

;; (setq extra-gitgrep-default-git-repo nil)
;; (setq extra-gitgrep-file-extensions "*") ;; "*.lisp *.cl *.el *.java"

;; (global-set-key (kbd "C-S-s") 'extra-gitgrep)
;; (global-set-key (kbd "C-M-S-s") 'extra-gitgrep-with-comments)

;; ;; (setq-default compilation-context-lines t) ; when there is no fringe in grep this will show an arrow and does not scroll

;; ;; -- hide/show ------------------------
;; (require 'extra-hide-show)

;; ;; from: http://emacs-fu.blogspot.pt/2008/12/showing-and-hiding-blocks-of-code.html
;; (defun hide-show-fn ()
;;   (local-set-key (kbd "C-c <right>")  'hs-show-block)
;;   (local-set-key (kbd "C-c <left>")   'hs-hide-block)
;;   (local-set-key (kbd "C-c S-<up>")   'hs-toggle-all-comments)
;;   (local-set-key (kbd "C-c S-<down>") 'hs-show-all)
;;   (hs-minor-mode 1))

;; (add-hook 'prog-mode-hook 'hide-show-fn)

;; (use-package company ; from: https://github.com/portacle/emacsd/blob/master/portacle-company.el
;;   :config (global-company-mode)
;;           (setq company-tooltip-limit 10)
;;           (setq company-dabbrev-downcase 0)
;;           (setq company-idle-delay 1)
;;           (setq company-echo-delay 0)
;;           (setq company-minimum-prefix-length 2)
;;           (setq company-require-match nil)
;;           (setq company-selection-wrap-around t)
;;           (setq company-tooltip-align-annotations t)
;;           ;; (setq company-tooltip-flip-when-above t)
;;           (setq company-transformers '(company-sort-by-occurrence)) ; weight by frequency
;;           (define-key company-active-map (kbd "<up>") 'company-select-previous)
;;           (define-key company-active-map (kbd "<down>") 'company-select-next)
;;           (define-key company-active-map (kbd "C-n") 'company-select-next)
;;           (define-key company-active-map (kbd "C-p") 'company-select-previous)
;;           (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
;;           (define-key company-active-map (kbd "M-.") 'company-show-location)
;;           (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
;;           (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
;;           (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
;;           (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
;;           (define-key (current-global-map) (kbd "TAB") 'company-indent-or-complete-common))

;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode)
;;   :config (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground (face-background 'default) :background (face-foreground 'error)) ; from: https://writequit.org/eos/eos-appearance.html
;;           (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "DarkGoldenrod3")
;;           (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "DarkGoldenrod3")
;;           (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "DarkGoldenrod3")
;;           (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "DarkGoldenrod3")
;;           (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "DarkGoldenrod3")
;;           (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "DarkGoldenrod3")
;;           (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "DarkGoldenrod3")
;;           (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "DarkGoldenrod3")
;;           (set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground "DarkGoldenrod3"))


;; ;; -- etc ----------------------------------------------------------------------
;; (let ((user-settings-dir (concat user-emacs-directory "users/" user-login-name))) ; from: http://whattheemacsd.com/init.el-06.html
;;   (cond ((file-exists-p user-settings-dir)
;;          (mapc 'load (directory-files user-settings-dir t "^[^#].*el$"))
;;          (message "%s loaded" user-settings-dir))
;;         (t
;;          (message "%s not found" user-settings-dir))))

;; (defun open-file (path)
;;   `(lambda ()
;;      (interactive)
;;      (find-file ,path)))

;; (defun my-kill-line ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (kill-whole-line)
;;     (move-to-column col)))

;; (defun my-just-one-space()
;;   (interactive "*")
;;   (just-one-space -1))

;; (defun halve-other-window-height ()
;;   (interactive)
;;   (enlarge-window (/ (window-height) 2)))

;; (defun halve-other-window-height2 ()
;;   (interactive)
;;   (shrink-window (/ (window-height) 2)))

;; (defun my-layout2 ()
;;   (interactive)
;;   (delete-other-windows)
;;   (split-window-right 120)
;;   (switch-to-buffer (other-buffer (current-buffer) 1)) ; https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
;;   (other-window 1))

;; (defun find-my-buffer (name)
;;   (dolist (buffer (buffer-list))
;;     (when (string-match name (buffer-name buffer))
;;       (return buffer))))

;; (defun my-debug-layout ()
;;   (interactive)
;;   (delete-other-windows)
;;   (split-window-right 100)
;;   (switch-to-buffer (or (find-my-buffer "repl")
;;                         (find-my-buffer "scratch")))
;;   (split-window-below)
;;   (other-window 1)
;;   (switch-to-buffer (or (find-my-buffer "trace")
;;                         (find-my-buffer "messages")))
;;   (other-window 1))

;; (defun scroll-up-with-fixed-cursor ()
;;   (interactive)
;;   (forward-line 1)
;;   (scroll-up 1))
;;
;; (defun scroll-down-with-fixed-cursor ()
;;   (interactive)
;;   (forward-line -1)
;;   (scroll-down 1))




;; (global-set-key (kbd "C-M-#") (open-file "~/.emacs.d/init.el"))

;; (global-set-key (kbd "C-c c") 'comment-or-uncomment-region-or-line) ; mudar isto para colocar #| |# ?
;; (global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
;; (global-set-key (kbd "C-S-k") 'my-kill-line)

;; (global-set-key [remap just-one-space] 'my-just-one-space)
;; (global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;; (global-set-key [f5] 'revert-buffer-no-confirm)
;; (global-set-key [f6] (lambda ()
;;                        (interactive)
;;                        (revert-buffer-no-confirm)
;;                        (hs-hide-all-comments)))
;; (global-set-key [f9] 'whitespace-mode)
;; (global-set-key [f10] 'toggle-truncate-lines)
;; (global-set-key [f11] 'prelude-copy-file-name-to-clipboard)
;; (global-set-key [f12] 'open-buffer-path)

;; (define-key isearch-mode-map (kbd "C-d") 'sacha/isearch-yank-current-word) ; Type C-s (isearch-forward) to start interactively searching forward, and type C-x to get the current word.
;; (global-set-key '[M-up] 'sacha/search-word-backward)
;; (global-set-key '[M-down] 'sacha/search-word-forward)

;; (global-set-key [C-return] 'insert-todo)
;; (global-set-key [C-M-up] 'search-todo-backward)
;; (global-set-key [C-M-down] 'search-todo-forward)

;; (global-set-key (kbd "C-M-S-a") 'reposition-point-at-top)

;; ;; layouts
;; (global-set-key (kbd "C-c v") 'halve-other-window-height)
;; (global-set-key (kbd "C-S-c v") 'halve-other-window-height2)
;; (global-set-key (kbd "C-c 2") 'my-layout2)

;; scroll
(global-set-key "\M-i" "\C-u1\M-v")
(global-set-key "\M-k" "\C-u1\C-v")
;; (global-set-key (kbd "C-M-i") 'scroll-down-with-fixed-cursor)
;; (global-set-key (kbd "C-M-k") 'scroll-up-with-fixed-cursor)

;; (setq org-return-follows-link  t)


;; (defun my-browse-url-maybe-privately (url &optional new-window)
;;   "Ask whether URL should be browsed in a private browsing window."
;;   (interactive "sURL: ")
;;   (if (y-or-n-p "Private Browsing? ")
;;       (my-browse-url-firefox-privately url)
;;     (browse-url-default-browser url new-window)))

;; (defun my-browse-url-firefox-privately (url &optional new-window)
;;   "Make firefox open URL in private-browsing window."
;;   (interactive (browse-url-interactive-arg "URL: "))
;;   (let ((process-environment (browse-url-process-environment)))
;;     (apply 'start-process
;;            (concat "firefox " url)
;;            nil
;;            browse-url-firefox-program
;;            (list "-private-window" url))))

;; (setq browse-url-browser-function
;;       '(("^https?://github" . my-browse-url-firefox-privately)
;;         ("^https?://t\\.co" . my-browse-url-firefox-privately)
;;         ("^https?://instagram\\.com" . my-browse-url-firefox-privately)
;;         ;; ("." . my-browse-url-maybe-privately)))
;;         ("." . my-browse-url-firefox-privately)))


;; ;; from: https://stackoverflow.com/questions/43765/pin-emacs-buffers-to-windows-for-cscope
;; (defun toggle-window-dedicated ()
;;   "Toggle whether the current active window is dedicated or not"
;;   (interactive)
;;   (message (if (let (window (get-buffer-window (current-buffer)))
;;                  (set-window-dedicated-p window (not (window-dedicated-p window))))
;;                "Window '%s' is dedicated"
;;              "Window '%s' is normal")
;;            (current-buffer)))


;; (global-set-key [f8] 'toggle-window-dedicated)
