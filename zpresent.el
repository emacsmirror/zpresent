;;; zpresent.el --- Simple presentation mode
;; Copyright 2015 Zachary Kanfer <zkanfer@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

(define-derived-mode zpresent-mode special-mode "zpresent-mode"
  (define-key zpresent-mode-map (kbd "n") #'zpresent/next-slide)
  (define-key zpresent-mode-map (kbd "C-n") #'zpresent/next-slide)
  (define-key zpresent-mode-map (kbd "<right>") #'zpresent/next-slide)
  (define-key zpresent-mode-map (kbd "<down>") #'zpresent/next-slide)
  (define-key zpresent-mode-map (kbd "p") #'zpresent/previous-slide)
  (define-key zpresent-mode-map (kbd "C-p") #'zpresent/previous-slide)
  (define-key zpresent-mode-map (kbd "<left>") #'zpresent/previous-slide)
  (define-key zpresent-mode-map (kbd "<up>") #'zpresent/previous-slide)
  (define-key zpresent-mode-map (kbd "<home>") #'zpresent/first-slide)
  (define-key zpresent-mode-map (kbd "<end>") #'zpresent/last-slide)
  (define-key zpresent-mode-map (kbd "C-+") #'zpresent/increase-text-size)
  (define-key zpresent-mode-map (kbd "+") #'zpresent/increase-text-size)
  (define-key zpresent-mode-map (kbd "C-=") #'zpresent/increase-text-size)
  (define-key zpresent-mode-map (kbd "=") #'zpresent/increase-text-size)
  (define-key zpresent-mode-map (kbd "C--") #'zpresent/decrease-text-size)
  (define-key zpresent-mode-map (kbd "-") #'zpresent/decrease-text-size))


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

(defconst *zpresent-increase-multiplier* 1.25
  "The amount to increase size when increasing size.")

(defconst *zpresent-decrease-multiplier* 0.8
  "The amount to decrease size when decreasing size.")

(defconst *zpresent-long-title-cutoff* 0.66
  "The fraction of the length of a line a title can be before it's considered long.")

(defvar *zpresent-bullet* "▸")


;;;; Faces:
;;zck make scaling function just change zpresent-base, not the faces in *zpresent-faces*.
;;zck why doesn't this center properly anymore?
(defface zpresent-base '((t . (:height 4.0))) "The base face, so we can manage changing sizes only by changing this face." :group 'zpresent-faces)
(defface zpresent-h1 '((t . (:height 1.0 :inherit zpresent-base))) "Face for titles." :group 'zpresent-faces)
(defface zpresent-body '((t . (:height 0.66 :inherit zpresent-base))) "Face for the body." :group 'zpresent-faces)

;;;; Actual code:
(defun zpresent ()
  "Present the current .org file."
  (interactive)

  (setq *zpresent-source* (org-structure-buffer (current-buffer)))
  (setq *zpresent-position* 0)
  (setq *zpresent-slides* (zpresent/format *zpresent-source*))

  (switch-to-buffer "zpresentation")
  (font-lock-mode 0)
  (zpresent-mode)

  (visual-line-mode)

  (zpresent/redisplay))

(defun zpresent/format (structure-list)
  "Convert an STRUCTURE-LIST into a list of slides."
  (append (cl-mapcan #'zpresent/format-recursively
                     structure-list)))

(defun zpresent/format-recursively (structure)
  "Convert STRUCTURE into a list of slides."
  (let* ((slide-so-far (zpresent/make-top-level-slide structure))
         (slides-so-far (list slide-so-far)))
    (dolist (cur-child (gethash :children structure))
      (setq slides-so-far
            (append slides-so-far
                    (zpresent/format-recursively-helper cur-child slide-so-far 1)))
      (setq slide-so-far (car (last slides-so-far))))
    slides-so-far))

(defun zpresent/make-top-level-slide (structure)
  "Make a top level slide from STRUCTURE."
  (zpresent/make-slide (zpresent/extract-current-text structure)))

;;zck test how this interacts with indentation/centering, if it does
(defun zpresent/extract-current-text (structure)
  "Extracts the text that should go in the slide for STRUCTURE."
  (if (gethash :body structure)
       (append (list (gethash :text structure))
               (gethash :body structure))
     (gethash :text structure)))

(defun zpresent/format-recursively-helper (structure slide-so-far level)
  "Convert STRUCTURE into a list of slides.

SLIDE-SO-FAR is the built-up slide to append text in the body to.

STRUCTURE is at level LEVEL.  This is used for indentation.

Return the list of slides."
  (let* ((current-slide (zpresent/extend-slide slide-so-far structure level))
         (slides-so-far (list current-slide)))
    (dolist (cur-child (gethash :children structure))
      (setq slides-so-far
            (append slides-so-far
                    (zpresent/format-recursively-helper cur-child current-slide (1+ level))))
      (setq current-slide (car (last slides-so-far))))
    slides-so-far))

(defun zpresent/make-body-text (structure level)
  "Make the body text for STRUCTURE at level LEVEL."
  (cons (format " %s%s %s"
                (make-string (* (1- level) 2) ?\s)
                  (if (equal ?* (gethash :bullet-type structure))
                      *zpresent-bullet*
                    "")
                  (gethash :text structure))
        (when (gethash :body structure)
          (mapcar (lambda (line)
                    (format "%s%s%s"
                            (make-string (* level 2) ?\s)
                            (if (equal ?* (gethash :bullet-type structure))
                                " "
                               "")
                            line))
                  (split-string (gethash :body structure)
                                "\n")))))

(defun zpresent/make-slide (title &optional body)
  "Create the slide with title TITLE.

If BODY is present, add it as the body of the slide.  Otherwise, the
slide is created with an empty body."
  (let ((slide (make-hash-table)))
    (puthash 'title title slide)
    (puthash 'body (if body (list body) nil) slide)
    slide))

(defun zpresent/extend-slide (slide structure level)
   "Extend SLIDE with the contents of STRUCTURE, at level LEVEL."
  (let ((new-slide (copy-hash-table slide)))
    (puthash 'body
             (append (gethash 'body slide)
                     (zpresent/make-body-text structure level))
             new-slide)
    new-slide))

;;zck eventually keywords? Or symbols? Who knows.

(defun zpresent/format-title (title chars-in-line &optional break-long-title)
  "Format TITLE appropriately, including padding and applying the face.

Format the title for a line of CHARS-IN-LINE characters.
If BREAK-LONG-TITLE is t, and the title is more than
*zpresent-long-title-cutoff* of the line, break it there,
and print the rest of the title on the next line."
  (if (zpresent/title-should-be-split title chars-in-line break-long-title)
      (let* ((chars-considered-long (truncate (* chars-in-line
                                                 *zpresent-long-title-cutoff*)))
             (title-lines (mapcar (lambda (line) (zpresent/format-title line chars-in-line))
                                  (zpresent/split-at-space title chars-considered-long))))
        (string-join title-lines))
    (zpresent/format-title-single-line title chars-in-line)))

(defun zpresent/title-should-be-split (title chars-in-line break-long-title)
  "Return t if TITLE is too long for a line of length CHARS-IN-LINE, else nil.

BREAK-LONG-TITLE indicates whether we should split titles at all."
  (let* ((chars-in-title (length title))
         (chars-considered-long (truncate (* chars-in-line
                                             *zpresent-long-title-cutoff*))))
    (and break-long-title
         (> chars-in-title
            chars-considered-long))))

(defun zpresent/format-title-single-line (title chars-in-line)
  "Format TITLE as a title, with the max length being CHARS-IN-LINE.

Treat it as a single line, so won't try to break it for length."

  (let* ((chars-in-title (length title))
         (chars-to-add (max 0
                            (truncate (- chars-in-line chars-in-title)
                                      2))))
    (format "%s\n"
            (propertize (format "%s%s" (make-string chars-to-add ?\s) title)
                        'face
                        'zpresent-h1))))

(defun zpresent/format-body (body-line)
  "Format BODY-LINE appropriately for the body."
  (propertize (format "%s\n" body-line)
              'face
              'zpresent-body))


(defun zpresent/split-at-space (string max-length)
  "Split STRING at a space.  Each substring must be MAX-LENGTH or shorter.

If there's a single word of length MAX-LENGTH, that word will be on a line by itself."
  (if (<= (length string)
          max-length)
      (list (string-trim string))
    (let ((pos-to-split-at (position ?\s string :from-end t :end max-length)))
      (if pos-to-split-at
          (cons (string-trim (substring string 0 pos-to-split-at))
                (zpresent/split-at-space (string-trim (substring string pos-to-split-at)) max-length))
        (cons (string-trim (substring string 0 max-length))
              (zpresent/split-at-space (string-trim (substring string max-length)) max-length))))))

(defun zpresent/first-slide ()
  "Move to the first slide."
  (interactive)
  (setq *zpresent-position* 0)
  (zpresent/slide (elt *zpresent-slides* *zpresent-position*)))

(defun zpresent/last-slide ()
  "Move to the last slide."
  (interactive)
  (setq *zpresent-position* (1- (length *zpresent-slides*)))
  (zpresent/slide (elt *zpresent-slides* *zpresent-position*)))

(defun zpresent/next-slide ()
  "Move to the next slide."
  (interactive)
  (when (< *zpresent-position*
           (1- (length *zpresent-slides*)))
      (cl-incf *zpresent-position*)
      (zpresent/slide (elt *zpresent-slides* *zpresent-position*))))

(defun zpresent/previous-slide ()
  "Move to the previous slide."
  (interactive)
  (when (> *zpresent-position*
           0)
      (cl-decf *zpresent-position*)
      (zpresent/slide (elt *zpresent-slides* *zpresent-position*))))

(defun zpresent/slide (slide)
  "Present SLIDE."
  (interactive)
  (switch-to-buffer "zpresentation")
  (buffer-disable-undo "zpresentation")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "\n")
    (when (gethash 'title slide)
      (let ((chars-in-line (/ (window-width)
                              (face-attribute 'zpresent-h1 :height nil t))))
        (if (listp (gethash 'title slide))
            (dolist (title-line (gethash 'title slide))
              (insert (zpresent/format-title title-line chars-in-line)))
          (insert (zpresent/format-title (gethash 'title slide) chars-in-line t))))
      (insert "\n"))
    (when (gethash 'body slide)
      (dolist (body-line (gethash 'body slide))
        (insert (zpresent/format-body body-line))))))

(defun zpresent/increase-text-size ()
  "Make everything bigger."
  (interactive)
  (set-face-attribute 'zpresent-base
                      nil
                      :height
                      (* *zpresent-increase-multiplier*
                         (or (face-attribute 'zpresent-base :height)
                             1)))
  (zpresent/redisplay))

(defun zpresent/decrease-text-size ()
  "Make everything smaller."
  (interactive)
  (set-face-attribute 'zpresent-base
                      nil
                      :height
                      (* *zpresent-decrease-multiplier*
                         (or (face-attribute 'zpresent-base :height)
                             1)))
  (zpresent/redisplay))

(defun zpresent/redisplay ()
  "Redisplay the presentation at the current slide."
  (interactive)
  (zpresent/slide (elt *zpresent-slides* *zpresent-position*)))

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

  (zpresent/redisplay))

(provide 'zpresent)

;;; zpresent.el ends here
