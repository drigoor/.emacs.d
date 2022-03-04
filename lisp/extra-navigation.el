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

;;; extra-navigation.el ends here
