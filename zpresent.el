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

(defconst *zpresent-long-title-cutoff* 0.66
  "The fraction of the length of a line a title can be before it's considered long.")


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
        (puthash 'title (list title) cur-slide))
      ;;zck add body of slide
      (push cur-slide slides))
    (reverse slides)))


(defun zpresent-format-title (title &optional break-long-title)
  "Format TITLE appropriately, including padding and applying the face.

If BREAK-LONG-TITLE is t, and the title is more than
*zpresent-long-title-cutoff* of the line, break it there,
and print the rest of the title on the next line."
  (if (zpresent-title-should-be-split title break-long-title)
      (let* ((chars-in-line (/ (window-width)
                                 (face-attribute 'zpresent-h1 :height)))
             (chars-considered-long (truncate (* chars-in-line
                                                 *zpresent-long-title-cutoff*)))
             (title-lines (mapcar #'zpresent-format-title
                                  (zpresent-split-at-space title chars-considered-long))))
        (string-join title-lines))
    (zpresent-format-title-single-line title)))

(defun zpresent-title-should-be-split (title break-long-title)
  "Return t if TITLE should be split, nil otherwise.

BREAK-LONG-TITLE indicates whether we should split titles long enough."
  (let* ((chars-in-line (/ (window-width)
                           (face-attribute 'zpresent-h1 :height)))
         (chars-in-title (length title))
         (chars-considered-long (truncate (* chars-in-line
                                             *zpresent-long-title-cutoff*))))
    (and break-long-title
         (> chars-in-title
            chars-considered-long))))

(defun zpresent-format-title-single-line (title)
  "Format TITLE as a title.

Treat it as a single line, so won't try to break it for length."

  (let* ((chars-in-line (/ (window-width)
                           (face-attribute 'zpresent-h1 :height)))
         (chars-in-title (length title))
         (chars-considered-long (truncate (* chars-in-line
                                             *zpresent-long-title-cutoff*)))
         (chars-to-add (max 0
                            (truncate (- chars-in-line chars-in-title)
                                      2))))
        (format "%s\n"
                (propertize (format "%s%s" (make-string chars-to-add ?\s) title)
                            'face
                            'zpresent-h1))))

(defun zpresent-format-body (body-line)
  "Format BODY-LINE appropriately for the body."
  (propertize body-line
              'face
              'zpresent-body))


(defun zpresent-split-at-space (string max-length)
  "Split STRING at a space.  Each substring must be MAX-LENGTH or shorter.

If there's a single word of length MAX-LENGTH, that word will be on a line by itself."
  (if (<= (length string)
          max-length)
      (list (string-trim string))
    (let ((pos-to-split-at (position ?\s string :from-end t :end max-length)))
      (if pos-to-split-at
          (cons (string-trim (substring string 0 pos-to-split-at))
                (zpresent-split-at-space (string-trim (substring string pos-to-split-at)) max-length))
        (cons (string-trim (substring string 0 max-length))
              (zpresent-split-at-space (string-trim (substring string max-length)) max-length))))))

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
      (if (listp (gethash 'title slide))
          (dolist (title-line (gethash 'title slide))
            (insert (zpresent-format-title title-line)))
        (insert (zpresent-format-title (gethash 'title slide) t)))
      (insert "\n"))
    (when (gethash 'body slide)
      (dolist (body-line (gethash 'body slide))
        (insert (zpresent-format-body body-line))))))

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

;;the slide is stored as a hash. Key-value pairs are:
;; key: title
;; value: The title of the slide. If this is a string, automatically split it.
;;    If this is a list, assume it's been manually split by the user,
;;    so just use each line separately.
;; key: body
;; value: A list of the lines in the body of the slide.

(defun zpresent--test-presentation ()
  "Start a presentation with dummy data."
  (interactive)
  (setq *zpresent-position* 0)
  (setq *zpresent-slides*
        (list #s(hash-table data (title "one-line title" body ("body line 1" "body line 2")))
              #s(hash-table data (title ("title manually split" "onto three" "lines (this one is pretty gosh darn long, but it shouldn't be automatically split no matter how long it is.)")))
              #s(hash-table data (title "an automatically split really really really really really really really really really long title"))))

  (switch-to-buffer "zpresentation")
  (font-lock-mode 0)
  (zpresent-mode)

  (zpresent-redisplay))

(provide 'zpresent)

;;; zpresent.el ends here
