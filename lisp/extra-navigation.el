;;; extra-navigation.el --- navigating code and between a given signature

;;; Code:

(provide 'extra-navigation)

(defvar *todo* "TODO   ")

(defun insert-todo ()
  (interactive)
  (let ((col (current-column)))
      (beginning-of-line)
      (let ((cur (point)))
          (insert *todo*)
          (comment-region cur (point))
          (forward-line)
          (indent-for-tab-command)
          (move-to-column col))))

(defun search-todo-forward ()
  (interactive)
  (if (search-forward *todo* nil t)
      (forward-line 1)
      (message "Signature %s not found forward in buffer."
               *todo*)))

(defun search-todo-backward ()
  (interactive)
  (unless (search-backward *todo* nil t)
    (message "Signature %s not found backward in buffer."
             *todo*)))

;; from: https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

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

;;; extra-navigation.el ends here
