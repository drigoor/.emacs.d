;;; init.el --- -*- coding: utf-8 -*-

;;; Commentary:

;;; Code:

;; -----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar lbo:*auto-refreshed-packages* nil
  "Non-nil if the package list was refrehsed in the current session.
The package list is refreshed in `lbo:ensure-package'.")

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

;; -------------------------------------

(setq large-file-warning-threshold 100000000) ; warn when opening files bigger than 100MB

(setq gc-cons-threshold most-positive-fixnum) ; Minimize garbage collection during startup

(add-hook 'emacs-startup-hook ; NOTE may be disabled by `inhibit-startup-hooks'
          (lambda ()
            (setq gc-cons-threshold (expt 2 23)) ; Lower threshold back to 8 MiB (default is 800kB)
            (message (format "Initialization time: %seconds" (emacs-init-time)))))

(setq confirm-kill-processes nil) ; quit Emacs directly even if there are running processes

(setq confirm-nonexistent-file-or-buffer nil)

;; -------------------------------------

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; -------------------------------------

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(set-fringe-mode 10)

(line-number-mode)
(column-number-mode)
(global-hl-line-mode)

(setq mode-line-percent-position "")

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

(setq window-divider-default-right-width 12)
(setq window-divider-default-places 'right-only)
(window-divider-mode 10)

(global-auto-revert-mode t) ; revert buffers automatically when underlying files are changed externally

(fset 'yes-or-no-p 'y-or-n-p)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        (:eval (when (and (not buffer-read-only)
                          (buffer-modified-p))
                 "  •"))))

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

(cond
 ((find-font (font-spec :name "Consolas 10")) ; windows
  (set-frame-font "Consolas 10"))
 ((find-font (font-spec :name "DejaVu Sans Mono-9")) ; linux
  (set-frame-font "DejaVu Sans Mono-9")))

(setq-default line-spacing 0.15)

(setq tab-always-indent 'complete) ; smart tab behavior - indent or complete
(add-hook 'completion-list-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(setq completion-auto-help t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq-default fill-column 80)

(setq-default show-trailing-whitespace t)
(setq-default truncate-lines t)

(setq require-final-newline t)

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

(global-prettify-symbols-mode +1) ; fancy lambdas

(defun mode-name-as-lambda-symbol ()
  (setq mode-name "λ"))

;; (add-hook 'emacs-lisp-mode-hook #'mode-name-as-lambda-symbol)

;; -------------------------------------

(ido-mode t)

(electric-pair-mode -1)

;; -- paren ----------------------------

(require 'paren)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match (face-background 'region))
(set-face-attribute 'show-paren-match nil :weight 'bold)
;; (set-face-foreground 'show-paren-match "#ffff33") ; from: http://nullman.net/emacs/files/init-emacs.el.html
(show-paren-mode t)

;; -- uniquify -------------------------

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; -- org -----------------------------

(require 'org)
(setq org-support-shift-select +1)
(setq org-return-follows-link  t)

;; -----------------------------------------------------------------------------

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package minions ; from: https://susamn.medium.com/ultimate-emacs-setup-with-documentation-in-org-mode-8ed32e2b3487
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-paren-mode t))
;; https://github.com/leeorengel/my-emacs-keybindings#smartparens

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

(use-package whitespace
  :init
  ;; (dolist (hook '(prog-mode-hook text-mode-hook))
  ;;   (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  ;; (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;; from: http://nschum.de/src/emacs/
(use-package highlight-symbol
  :config
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
  (setq highlight-symbol-colors '("orange"))
  (setq highlight-symbol-foreground-color "red")
  (set-face-attribute 'highlight-symbol-face nil :foreground "red" :background "red"))
;; improvements:
;;   + show status in modeline!
;;   + colors VS faces

(use-package anzu
  :ensure t
  :bind
  (("<remap> <query-replace>" . 'anzu-query-replace)
   ("<remap> <query-replace-regexp>" . 'anzu-query-replace-regexp))
  :config
  (global-anzu-mode t)
  (set-face-foreground 'anzu-mode-line "#FF6F00"))

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode-hook . rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;; from: https://writequit.org/eos/eos-appearance.html
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground (face-background 'default) :background (face-foreground 'error))
  ;; adapted: https://yoo2080.wordpress.com/2013/12/21/small-rainbow-delimiters-tutorial/
  (require 'cl-lib)
  (cl-loop for index from 1 to rainbow-delimiters-max-face-count do
           (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
             (set-face-attribute face nil :foreground "DarkGoldenrod3"))))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-eldev
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
  (global-company-mode))

(use-package hl-todo ; from: https://www.reddit.com/r/emacs/comments/f8tox6/todo_highlighting/
  :hook (prog-mode . hl-todo-mode)
  :bind
  ("C-c p" . 'hl-todo-previous)
  ("C-c n" . 'hl-todo-next)
  ("C-c o" . 'hl-todo-occur)
  ("C-c i" . 'hl-todo-insert)
  :config
  (setq hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package crux
  :ensure t
  :bind
  (("C-a"        . crux-move-beginning-of-line)
   ("C-S-k"      . crux-kill-whole-line)
   ("C-c k"      . crux-kill-other-buffers)
   ("C-M-z"      . crux-indent-defun)
   ("C-c d"      . crux-duplicate-current-line-or-region)
   ("C-c M-d"    . crux-duplicate-and-comment-current-line-or-region)
   ("C-c r"      . crux-rename-buffer-and-file)
   ([(M return)] . crux-smart-open-line)))

(use-package marginalia
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package highlight-parentheses
  :ensure t)

(use-package ahk-mode
  :ensure t)

;; -- magit --------------------------------------------------------------------

(defun magit-status-around (orig-fun &rest args)
  (window-configuration-to-register 'x)
  (delete-other-windows)
  (apply orig-fun args))

;; references:
;; https://github.com/bradwright/emacs-d/blob/master/packages/init-magit.el
;; http://whattheemacsd.com/setup-magit.el-01.html
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c b" . magit-blame)
         :map magit-status-mode-map
         ("q" . magit-quit-session))
  :config
  (advice-add 'magit-status :around #'magit-status-around) ; check: https://www.gnu.org/software/emacs/manual/html_node/elisp/Porting-old-advice.html
  (defun magit-quit-session ()
    (interactive)
    (kill-buffer)
    (jump-to-register 'x))
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

;; -- lisp ---------------------------------------------------------------------

;; (use-package sly
;;   :config (setq inferior-lisp-program (expand-file-name "C:/home/scoop/apps/sbcl/current/sbcl.exe")))

;; from: https://github.com/arecker/emacs.d
;; (use-package slime
;;   :ensure t
;;   :defer t
;;   :config (setq inferior-lisp-program (executable-find "sbcl")))
;;
;; (use-package slime-company
;;   :ensure t
;;   :after (slime company)
;;   :config (setq slime-company-completion 'fuzzy
;;                 slime-company-after-completion 'slime-company-just-one-space))

;; -- clojure ------------------------------------------------------------------

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'mode-name-as-lambda-symbol))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)

  ;; from: https://www.john2x.com/emacs.html
  ;; (setq nrepl-hide-special-buffers t)
  (setq cider-repl-use-clojure-font-lock t)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook (lambda () (setq show-trailing-whitespace nil))))

;; consider:
;;    clj-refactor
;;    flycheck-clj-kondo

(use-package flycheck-joker
  :ensure t)

;; -----------------------------------------------------------------------------

;; from: https://www.john2x.com/emacs.html
(which-function-mode)

(require 'derived)

;; elisp only
(defconst elispy-modes
  '(emacs-lisp-mode ielm-mode))

;; all lisps
(defconst lispy-modes
  (append elispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode clojure-mode cider-mode-hook cider-repl-mode-hook))
  "All lispy major modes.")

(defun my-lisp-setup ()
  "Enable features useful in any Lisp mode."
  ;; (rainbow-delimiters-mode t)
  ;; (hl-sexp-mode)
  ;; (enable-paredit-mode)
  (turn-on-eldoc-mode)
  (highlight-parentheses-mode))

(dolist (hook (mapcar #'derived-mode-hook-name lispy-modes))
  (add-hook hook 'my-lisp-setup))

(defun maybe-check-parens ()
  "Run `check-parens' if this is a lispy mode."
  (when (memq major-mode lispy-modes)
    (check-parens)))

(add-hook 'after-save-hook 'maybe-check-parens)

;; -- global keys --------------------------------------------------------------

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "C-<iso-lefttab>") (lambda () (interactive) (other-window -1)))

;; -----------------------------------------------------------------------------

;; from: https://github.com/magnars/.emacs.d/blob/master/settings/appearance.el
;; from: https://www.emacswiki.org/emacs/AlarmBell
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq ring-bell-function 'flash-mode-line)
(setq visible-bell nil)

(set-face-attribute 'mode-line nil
                    :height 1.0
                    :foreground (face-foreground 'default)
                    :foreground "#605e57"
                    :background "#f2ecdb"
                    :overline nil
                    :underline nil
                    :box `(:line-width 3 :color ,"#f0e0d0" :style nil))
(set-face-attribute 'mode-line-inactive nil
                    :height 1.0
                    :foreground "#aba9a7"
                    :background "#faf8f4"
                    :overline nil
                    :underline nil
                    :inherit nil
                    :box `(:line-width 3 :color ,"#f5f2ef" :style nil))

;; (require 'extra-use-package) ; (package-initialize) -- for the why check package--ensure-init-file
;; (require 'extra-ediff)
;; (require 'extra-xml)
;; (require 'extra-utils)
;; (require 'extra-navigation)

;; -- extra --------------------------------------------------------------------
;; (add-to-list 'load-path "~/.emacs.d/lisp")

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


;; ;; -- etc ----------------------------------------------------------------------
;;
;; (let ((user-settings-dir (concat user-emacs-directory "users/" user-login-name))) ; from: http://whattheemacsd.com/init.el-06.html
;;   (cond ((file-exists-p user-settings-dir)
;;          (mapc 'load (directory-files user-settings-dir t "^[^#].*el$"))
;;          (message "%s loaded" user-settings-dir))
;;         (t
;;          (message "%s not found" user-settings-dir))))

(defun open-file (path)
  `(lambda ()
     (interactive)
     (find-file ,path)))

(defun my-just-one-space()
  (interactive "*")
  (just-one-space -1))

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

;; from: http://sachachua.com/blog/2008/07/emacs-keyboard-shortcuts-for-navigating-code/
(defun sacha/isearch-yank-current-word ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
     (lambda ()
       (skip-syntax-forward "w_")
       (point)))))

;; modified to display message
(defun sacha/search-word-forward ()
  "Find the next occurrance of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (let ((text (with-current-buffer (current-buffer)
                  (buffer-substring-no-properties cur (point)))))
      (goto-char
       (cond ((re-search-forward (concat "\\_<" (regexp-quote (current-word)) "\\_>") nil t)
              (message "found forward: %s" text)
              (match-beginning 0))
             (t
              (message "'%s' not found forward in buffer" text)
              cur))))))

;; modified to display message
(defun sacha/search-word-backward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (let* ((found? (re-search-backward (concat "\\_<" (regexp-quote (current-word)) "\\_>") nil t))
           (pt2 (save-excursion
                  (re-search-forward (concat "\\_<" (regexp-quote (current-word)) "\\_>") nil t)
                  (point)))
           (text (with-current-buffer (current-buffer)
                   (buffer-substring-no-properties (point) pt2))))
      (cond (found?
             (message "found backward: %s" text)
             (match-beginning 0))
            (t
             (message "'%s' not found backward in buffer" text)
             cur)))))

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

;; from LBO > scg-open-explorer
(defun open-buffer-path ()
  (interactive)
  (cl-flet ((w32ify-path (path)
                         (convert-standard-filename (replace-regexp-in-string "/" "\\" path t t))))
    (cond (buffer-file-name
           (w32-shell-execute "open" "explorer" (concat "/e,/select," (w32ify-path buffer-file-name))))
          (default-directory
            (w32-shell-execute "explore" (w32ify-path default-directory)))
          (t
           (user-error "Current buffer not associated with any path")))))

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

(global-set-key (kbd "C-M-#") (open-file user-init-file))
;; (global-set-key (kbd "C-M-!") (open-file "C:/home/setup-windows.ps1"))
(global-set-key (kbd "C-M-$") (open-file "c:/home/.autohotkey/autohotkey.ahk"))
(global-set-key (kbd "C-M-&") (open-file "c:/home/.bashrc"))
;; (global-set-key (kbd "C-M-(") (open-file "c:/home/projects/lisp/peas-with-raylib/notes.org"))
;; (global-set-key (kbd "C-M-)") (open-file "c:/home/projects/lisp/peas-with-raylib/README.md"))
;; (global-set-key (kbd "C-M-(") (open-file "c:/home/projects/clojure/links.txt"))

(global-set-key (kbd "C-c c") 'comment-or-uncomment-region-or-line)
(global-set-key [remap just-one-space] 'my-just-one-space)

(global-set-key [f5] 'revert-buffer-no-confirm)
;; (global-set-key [f6] (lambda ()
;;                        (interactive)
;;                        (revert-buffer-no-confirm)
;;                        (hs-hide-all-comments)))
(global-set-key [f9] 'whitespace-mode)
(global-set-key [f10] 'toggle-truncate-lines)
(global-set-key [f11] 'prelude-copy-file-name-to-clipboard)
(global-set-key [f12] 'open-buffer-path)

(define-key isearch-mode-map (kbd "C-d") 'sacha/isearch-yank-current-word) ; Type C-s (isearch-forward) to start interactively searching forward, and type C-x to get the current word.
(global-set-key '[C-M-up] 'sacha/search-word-backward)
(global-set-key '[C-M-down] 'sacha/search-word-forward)

;; scroll
(global-set-key "\M-i" "\C-u1\M-v")
(global-set-key "\M-k" "\C-u1\C-v")

;; -------------------------------------

(defun my-browse-url-maybe-privately (url &optional new-window)
  "Ask whether URL should be browsed in a private browsing window."
  (interactive "sURL: ")
  (if (y-or-n-p "Private Browsing? ")
      (my-browse-url-firefox-privately url)
    (browse-url-default-browser url new-window)))

(defun my-browse-url-firefox-privately (url &optional new-window)
  "Make firefox open URL in private-browsing window."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           browse-url-firefox-program
           (list "-private-window" url))))

(setq browse-url-browser-function
      '(("^https?://github" . my-browse-url-firefox-privately)
        ("^https?://t\\.co" . my-browse-url-firefox-privately)
        ("^https?://instagram\\.com" . my-browse-url-firefox-privately)
        ;; ("." . my-browse-url-maybe-privately)))
        ("." . my-browse-url-firefox-privately)))

;; -------------------------------------

;; from: https://stackoverflow.com/questions/43765/pin-emacs-buffers-to-windows-for-cscope
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message (if (let (window (get-buffer-window (current-buffer)))
                 (set-window-dedicated-p window (not (window-dedicated-p window))))
               "Window '%s' is dedicated"
             "Window '%s' is normal")
           (current-buffer)))


(global-set-key [f8] 'toggle-window-dedicated)

;; -----------------------------------------------------------------------------


;;; init.el ends here
