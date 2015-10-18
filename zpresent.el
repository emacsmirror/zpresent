;;; zpresent.el --- Simple presentation mode

;;; Commentary:

;;; Code:

(define-derived-mode zpresent-mode special-mode "zpresent-mode"
  (define-key zpresent-mode-map (kbd "n") #'zpresent-next-slide)
  (define-key zpresent-mode-map (kbd "C-n") #'zpresent-next-slide)
  (define-key zpresent-mode-map (kbd "<right>") #'zpresent-next-slide)
  (define-key zpresent-mode-map (kbd "<down>") #'zpresent-next-slide)
  (define-key zpresent-mode-map (kbd "p") #'zpresent-previous-slide)
  (define-key zpresent-mode-map (kbd "C-p") #'zpresent-previous-slide)
  (define-key zpresent-mode-map (kbd "<left>") #'zpresent-previous-slide)
  (define-key zpresent-mode-map (kbd "<up>") #'zpresent-previous-slide)
  (define-key zpresent-mode-map (kbd "C-+") #'zpresent-increase-text-size)
  (define-key zpresent-mode-map (kbd "+") #'zpresent-increase-text-size)
  (define-key zpresent-mode-map (kbd "C-=") #'zpresent-increase-text-size)
  (define-key zpresent-mode-map (kbd "=") #'zpresent-increase-text-size)
  (define-key zpresent-mode-map (kbd "C--") #'zpresent-decrease-text-size)
  (define-key zpresent-mode-map (kbd "-") #'zpresent-decrease-text-size))


;;;; Requires:
(require 'org-element)
(require 'subr-x)

;;;; Variables:
(defvar *zpresent-slides* nil
  "The slides for the current presentation.")

(defvar *zpresent-source* nil
  "The original org structure for the presentation.")

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

  (setq *zpresent-source* (org-element-parse-buffer))
  (setq *zpresent-position* 0)
  (setq *zpresent-slides* (zpresent-format *zpresent-source*))

  (switch-to-buffer "zpresentation")
  (font-lock-mode 0)
  (zpresent-mode)

  (zpresent-redisplay))

(defun zpresent-format (org-data)
  "Convert ORG-DATA into the zpresentation list of slides."
  (cl-mapcan #'identity
             (org-element-map org-data
                 'headline
               #'zpresent-create-slides-from-block)))

(defun zpresent-create-slides-from-block (block)
  "Convert BLOCK into a list of slides."
  (let ((slides nil)
        (title (or (org-element-property :TITLE block)
                   (org-element-property :raw-value block))))
    (let ((cur-slide (make-hash-table)))
      (when title
        ;;zck do something for multi-line headlines
        ;;maybe automatically break it at 1/3 of width?
        ;;add template: http://orgmode.org/manual/Easy-templates.html#Easy-templates
        (puthash 'title title cur-slide))
      ;;zck add body of slide
      (push cur-slide slides))
    (reverse slides)))


(defun zpresent-format-title (title)
  "Format TITLE appropriately, including padding and applying the face."
  (let* ((chars-in-line (/ (window-width)
                           (face-attribute 'zpresent-h1 :height)))
         (chars-in-title (length title))
         (chars-to-add (max 0
                            (truncate (- chars-in-line chars-in-title)
                                      2))))
    (format "%s\n\n"
            (propertize (format "%s%s" (make-string chars-to-add ?\s) title)
                        'face
                        'zpresent-h1))))

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

(defun zpresent-slide (slide)
  "Present SLIDE."
  (interactive)
  (switch-to-buffer "zpresentation")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "\n")
    (when (gethash 'title slide)
      (insert (zpresent-format-title (gethash 'title slide))))))

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
  (zpresent-redisplay))

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
  (zpresent-redisplay))

(defun zpresent-redisplay ()
  "Redisplay the presentation at the current slide."
  (interactive)
  (zpresent-slide (elt *zpresent-slides* *zpresent-position*)))

(provide 'zpresent)

;;; zpresent.el ends here
