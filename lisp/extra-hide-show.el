;;; extra-hide-show.el --- utilities for selectively hide/show code and comment blocks (based on hs-minor-mode)

;;; Code:

(provide 'extra-hide-show)

(setq hs-isearch-open 'code)

;; from: https://wiki/siscog/MetodosDeTrabalhoNaoOficiais#A.5BLBO.4020100209.5D_Hide.2BAC8-Show_Comments_.28and_Code.29
(defun hs-hide-all-comments ()
  "Adapted from `hs-hide-all'."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (unless hs-allow-nesting
       (hs-discard-overlays (point-min) (point-max)))
     (goto-char (point-min))
     (let ((spew (make-progress-reporter "Hiding all comments..."
                                         (point-min) (point-max)))
           (re (concat "\\(" hs-block-start-regexp "\\)"
                       "\\|\\(" hs-c-start-regexp "\\)")))
       (while (progn
                (unless hs-hide-comments-when-hiding-all
                  (forward-comment (point-max)))
                (re-search-forward re (point-max) t))
         (if (match-beginning 1)
             ;; we have found a block beginning, skip it
             (progn
               (goto-char (match-beginning 1))
               (forward-sexp 1))
           ;; found a comment, probably
           (let ((c-reg (hs-inside-comment-p)))
             (when (and c-reg (car c-reg))
               (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                   (hs-hide-block-at-point t c-reg)
                 (goto-char (nth 1 c-reg))))))
         (progress-reporter-update spew (point)))
       (progress-reporter-done spew)))
   (beginning-of-line)
   (run-hooks 'hs-hide-hook)))

(defvar hs-hide-all-comments-p nil)

(make-variable-buffer-local 'hs-all-comments-hidden-p)

(defun hs-toggle-all-comments ()
  (interactive)
  (setq hs-all-comments-hidden-p (not hs-all-comments-hidden-p))
  (if hs-all-comments-hidden-p
      (hs-hide-all-comments)
      (hs-show-all)))

;;; extra-hide-show.el ends here
