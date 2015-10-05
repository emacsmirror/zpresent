;;; zpresent.el --- Simple presentation mode

;;; Commentary:

;;; Code:

(define-derived-mode zpresent-mode special-mode "zpresent-mode"
  (define-key zpresent-mode-map (kbd "n") #'zpresent-next-slide)
  (define-key zpresent-mode-map (kbd "C-n") #'zpresent-next-slide)
  (define-key zpresent-mode-map (kbd "p") #'zpresent-previous-slide)
  (define-key zpresent-mode-map (kbd "C-p") #'zpresent-previous-slide))


(defvar *zpresent-slides* nil
  "The slides for the current presentation.")

(defvar *zpresent-position* 0
  "The current slide position.")


(defun zpresent ()
  "Present the current .org file."
  (interactive)
  (setq *zpresent-slides* (zpresent-get-slides (buffer-substring 1 (buffer-size))))
  (switch-to-buffer "zpresentation")
  (zpresent-mode)
  (setq *zpresent-position* -1)
  (zpresent-next-slide))

(defun zpresent-get-slides (text)
  "Get slides from TEXT."
  '("first" "second" "third"))

(defun zpresent-next-slide ()
  "Move to the next slide."
  (interactive)
  (when (< *zpresent-position*
           (1- (length *zpresent-slides*)))
      (cl-incf *zpresent-position*)
      (zpresent-slide (elt *zpresent-slides* *zpresent-position*))))

(defun zpresent-previous-slide ()
  "Move to the previous slide."
  (interactive)
  (when (> *zpresent-position*
           0)
      (cl-decf *zpresent-position*)
      (zpresent-slide (elt *zpresent-slides* *zpresent-position*))))

(defun zpresent-slide (text)
  "Present TEXT as a slide."
  (interactive)
  (switch-to-buffer "zpresentation")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "\n")
    (insert text)))

(provide 'zpresent)

;;; zpresent.el ends here
