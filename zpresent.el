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
  (font-lock-mode 0)
  (zpresent-mode)
  (setq *zpresent-position* -1)
  (zpresent-next-slide))

(defface zpresent-h1 '((t . (:height 2.0))) "Face for titles.")

(defun zpresent-get-slides (text)
  "Get slides from TEXT."
  (let ((complete-slides (split-string text
                                       (regexp-quote "\n* ")
                                       t
                                       (regexp-quote "* ")))
        (slides nil))
    (dolist (slide complete-slides)
      (let* ((all-lines (split-string slide "\n"))
             (first-line (pop all-lines))
             (built-up-slide nil))
        (push first-line built-up-slide)
        (push first-line slides)

        (dolist (next-line all-lines)
          (push next-line
                built-up-slide)
          (push (string-join (reverse built-up-slide) "\n")
                slides))))
    (reverse slides)))

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
    (let* ((chars-in-line (/ (window-width)
                             (face-attribute 'zpresent-h1 :height)))
           (chars-in-text (length text))
           (chars-to-add (truncate (- chars-in-line chars-in-text)
                                   2)))
      (insert (propertize (format "%s%s" (make-string chars-to-add ?\s) text) 'face 'zpresent-h1)))))

(provide 'zpresent)

;;; zpresent.el ends here
