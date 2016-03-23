;;; keep-formation.el --- Make editing be aware of space tabulation
;;;
;;; Commentary:
;;;
;;; This Emacs mode is experimental.
;;;
;;; It's quite hacky and incomplete, but works for most scenarios right now.
;;;
;;; Here's an example, enabling it for haskell-mode:
;;;
;;; (defun my/haskell-mode-enable-keep-formation ()
;;;    (define-key haskell-mode-map (kbd "<delete>")    'keep-formation-delete-forward-char)
;;;    (define-key haskell-mode-map (kbd "<backspace>") 'keep-formation-delete-backward-char)
;;;    (keep-formation-mode)
;;; )
;;;
;;; (add-hook 'haskell-mode-hook 'my/haskell-mode-enable-keep-formation)
;;;
;;; Code:
;;;

(defun keep-formation-for-delete (offset)
  "Whitespace insertion from further down the line, depending on where you are."
  (let (eol nls orig)
    (progn
      (setq orig (point))

      (save-excursion (setq eol (search-forward "\n" nil t)))

      (if (not (eq eol nil))
	  (save-excursion (setq nls (search-forward "  " eol t))))

      (if (and eol nls (not (eq (+ orig offset) nls)))
	  (save-excursion
	    (goto-char nls)
	    (insert " ")
	    )
	)
      ))
  )

;;;###autoload
(define-minor-mode keep-formation-mode
  "Keep-formation editing mode"
  :lighter " KF"
  (if keep-formation-mode
      (progn
	(add-hook 'post-self-insert-hook 'keep-formation-post-self-insert-hook t))
      (progn
	(remove-hook 'post-self-insert-hook 'keep-formation-post-self-insert-hook))
      ))

(defun keep-formation-delete-forward-char ()
  "Delete but don't damage whitespace alignment."
  (interactive)

  (when keep-formation-mode
    (keep-formation-for-delete 2))
  (delete-char 1)
  )

(defun keep-formation-delete-backward-char ()
  "Delete backward but don't damage whitespace alignment."
  (interactive)

  (when keep-formation-mode
    (if (eq overwrite-mode nil)
	(keep-formation-for-delete 1)))
  (delete-char -1)
  )

(defun keep-formation-post-self-insert-hook ()
  "Run after a string is inserted."

  (if (eq overwrite-mode nil)
      (let (eol nls orig)
	(progn
	  ;; TODO: refactor common code with keep-formation-for-delete.
	  (setq orig (point))

	  (save-excursion (setq eol (search-forward "\n" nil t)))

	  (if (not (eq eol nil))
	      (save-excursion (setq nls (search-forward "  " eol t))))

	  (if (and eol nls)
	      (save-excursion
		(goto-char (- nls 1))
		(delete-char 1)
		)
	    )
	  ))
    )
  )

(provide 'keep-formation)
;;; keep-formation.el ends here
