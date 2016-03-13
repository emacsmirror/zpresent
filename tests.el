;;; tests.el -- tests for zpresent.el

;;; Commentary:

;;; Code:


;; (ert-deftest format-block-helper-single-block ()
;;   (should (org-structure/hash-tables-equal #s(hash-table data (title "a title" body ("one" "two")))
;;                                            (car (zpresent-format-block-helper (make-hash-table)
;;                                                                               (make-hash-table 'title "a title"
;;                                                                                                'body nil)
;;                                                                               nil)))))

(ert-deftest make-slide/title-only/check-title ()
  (should (equal "I'm the title!"
                 (gethash 'title (zpresent/make-slide "I'm the title!")))))

(ert-deftest make-slide/title-only/no-body ()
  (should-not (gethash 'body (zpresent/make-slide "I'm the title!"))))

(ert-deftest make-slide/title-only/proper-things-added ()
  (should (equal 2
                 (hash-table-count (zpresent/make-slide "I'm the title!")))))

(ert-deftest make-slide/body/no-body ()
  (should (equal "I'm a body"
                 (gethash 'body (zpresent/make-slide "I'm the title!" "I'm a body")))))

(ert-deftest make-slide/body/no-body ()
  (should (equal 2
                 (hash-table-count (zpresent/make-slide "I'm the title!" "I'm a body")))))


(ert-deftest extend-slide/original-slide-not-updated ()
  (let* ((original-slide (zpresent/make-slide "I'm the title!"))
         (new-slide (zpresent/extend-slide original-slide (car (org-structure "* New body text.")) 1)))
    (should (equal 2 (hash-table-count original-slide)))
    (should-not (gethash 'body original-slide))))

(ert-deftest extend-slide/check-title ()
  (let* ((original-slide (zpresent/make-slide "I'm the title!"))
         (new-slide (zpresent/extend-slide original-slide (car (org-structure "* New body text.")) 1)))
    (should (equal "I'm the title!"
                   (gethash 'title new-slide)))))

(ert-deftest extend-slide/check-new-body ()
  (let* ((original-slide (zpresent/make-slide "I'm the title!"))
         (new-slide (zpresent/extend-slide original-slide (car (org-structure "* New body text.")) 1)))
    (should (equal (list " ▸ New body text.")
                   (gethash 'body new-slide)))))

(ert-deftest extend-slide/check-added-body ()
  (let* ((original-slide (zpresent/make-slide "I'm the title!" "Initial body."))
         (new-slide (zpresent/extend-slide original-slide (car (org-structure "* New body text.")) 1)))
    (should (equal (list "Initial body." " ▸ New body text.")
                   (gethash 'body new-slide)))))


(ert-deftest extract-current-test/simple-headline ()
  (should (equal "simple headline"
                 (zpresent/extract-current-text (car (org-structure "* simple headline"))))))

(ert-deftest extract-current-test/nested-headline ()
  (should (equal "nested headline"
                 (zpresent/extract-current-text (car (org-structure "** nested headline"))))))

(ert-deftest extract-current-test/simple-headline-with-multiple-line-body ()
  (should (equal (list "nested headline" "with body" "over multiple lines")
                 (zpresent/extract-current-text (car (org-structure "** nested headline\nwith body\nover multiple lines"))))))

(ert-deftest extract-current-test/simple-plain-list ()
  (should (equal "simple plain list"
                 (zpresent/extract-current-text (car (org-structure "- simple plain list"))))))

(ert-deftest extract-current-test/nested-plain-list ()
  (should (equal "nested plain list"
                 (zpresent/extract-current-text (car (org-structure "  - nested plain list"))))))

(ert-deftest extract-current-test/simple-plain-list-with-multiple-line-body ()
  (should (equal (list "nested plain list" "with body" "over multiple lines")
                 (zpresent/extract-current-text (car (org-structure "  - nested plain list\nwith body\nover multiple lines"))))))



;;; tests.el ends here
