;;; zpresent.el --- Simple presentation mode

;;; Commentary:

;;; Code:

(define-derived-mode zpresent-mode special-mode "zpresent-mode"
  (define-key zpresent-mode-map (kbd "n") #'zpresent-next-slide)
  (define-key zpresent-mode-map (kbd "C-n") #'zpresent-next-slide)
  (define-key zpresent-mode-map (kbd "p") #'zpresent-previous-slide)
  (define-key zpresent-mode-map (kbd "C-p") #'zpresent-previous-slide)
  (define-key zpresent-mode-map (kbd "C-+") #'zpresent-increase-text-size)
  (define-key zpresent-mode-map (kbd "+") #'zpresent-increase-text-size)
  (define-key zpresent-mode-map (kbd "C-=") #'zpresent-increase-text-size)
  (define-key zpresent-mode-map (kbd "=") #'zpresent-increase-text-size)
  (define-key zpresent-mode-map (kbd "C--") #'zpresent-decrease-text-size)
  (define-key zpresent-mode-map (kbd "-") #'zpresent-decrease-text-size))


;;;; Requires:
(require 'subr-x)

;;;; Variables:
(defvar *zpresent-slides* nil
  "The slides for the current presentation.")

(defvar *zpresent-source-text* nil
  "The original text being presented.

This is used when we have to re-format everything.")

(defvar *zpresent-position* 0
  "The current slide position.")

(defconst *zpresent-increase-multiplier* 1.5
  "The amount to increase size when increasing size.")

(defconst *zpresent-decrease-multiplier* 0.66
  "The amount to decrease size when decreasing size.")


;;;; Faces:
(defface zpresent-h1 '((t . (:height 4.0))) "Face for titles." :group 'zpresent-faces)
(defface zpresent-body '((t . (:height 2.66))) "Face for the body." :group 'zpresent-faces)

(defvar *zpresent-faces* '(zpresent-h1 zpresent-body)
  "A list of all the faces for this group.
This should eventually be replaced by just getting the faces programatically.")

;;;; Actual code:
(defun zpresent ()
  "Present the current .org file."
  (interactive)

  (setq *zpresent-source-text* (buffer-substring 1 (buffer-size)))
  (setq *zpresent-position* 0)

  (zpresent-reformat)


  (switch-to-buffer "zpresentation")
  (font-lock-mode 0)
  (zpresent-mode)
  (setq *zpresent-position* -1)
  (zpresent-next-slide))

(defun zpresent-get-slides (text)
  "Get slides from TEXT."
  (let ((complete-slides (split-string text
                                       (regexp-quote "\n* ")
                                       t
                                       (regexp-quote "* ")))
        (slides nil))
    (dolist (slide complete-slides)
      (let* ((all-lines (split-string slide "\n"))
             (title (zpresent-pad-title (pop all-lines)))
             (built-up-slide nil))
        (push (format "%s%s"
                      (propertize title
                                  'face
                                  'zpresent-h1)
                      "\n\n")
              built-up-slide)

        (push (string-join (reverse built-up-slide) "\n")
              slides)

        (dolist (next-line all-lines)
          (push (propertize (format "  %s" next-line) 'face 'zpresent-body)
                built-up-slide)
          (push (format "%s\n"
                        (string-join (reverse built-up-slide) "\n"))
                slides))))
    (reverse slides)))

(defun zpresent-pad-title (title)
  "Pad TITLE with appropriate spaces."
  (let* ((chars-in-line (/ (window-width)
                           (face-attribute 'zpresent-h1 :height)))
         (chars-in-title (length title))
         (chars-to-add (max 0
                            (truncate (- chars-in-line chars-in-title)
                                      2))))
    (format "%s%s" (make-string chars-to-add ?\s) title)))

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

(defun zpresent-increase-text-size ()
  "Make everything bigger."
  (interactive)
  (dolist (face *zpresent-faces*)
    (set-face-attribute face
                        nil
                        :height
                        (* *zpresent-increase-multiplier*
                           (or (face-attribute face :height)
                               1))))
  (zpresent-reformat))

(defun zpresent-decrease-text-size ()
  "Make everything smaller."
  (interactive)
  (dolist (face *zpresent-faces*)
    (set-face-attribute face
                        nil
                        :height
                        (* *zpresent-decrease-multiplier*
                           (or (face-attribute face :height)
                               1))))
  (zpresent-reformat))

(defun zpresent-reformat ()
  "Reformat the presentation with the current window size, fonts, etc."
  (interactive)
  (setq *zpresent-slides* (zpresent-get-slides *zpresent-source-text*))
  (zpresent-slide (elt *zpresent-slides* *zpresent-position*)))

(provide 'zpresent)

;;; zpresent.el ends here
