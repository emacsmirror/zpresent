;;; tests.el -- tests for zpresent.el

;;; Commentary:

;;; Code:


;; (ert-deftest format-block-helper-single-block ()
;;   (should (org-parser/hash-tables-equal #s(hash-table data (title "a title" body ("one" "two")))
;;                                            (car (zpresent-format-block-helper (make-hash-table)
;;                                                                               (make-hash-table 'title "a title"
;;                                                                                                :body nil)
;;                                                                               nil)))))


(defconst required-slide-keys '(:type :properties :body :title :checkpoint))


(ert-deftest make-slide/title-only/check-title ()
  (should (equal "I'm the title!"
                 (gethash :title (zpresent--make-slide "I'm the title!")))))

(ert-deftest make-slide/title-only/no-body ()
  (should-not (gethash :body (zpresent--make-slide "I'm the title!"))))

(ert-deftest make-slide/title-only/proper-things-added ()
  (let ((slide (zpresent--make-slide "I'm the title!")))
    (should (equal (length required-slide-keys)
                   (hash-table-count slide)))
    (dolist (key required-slide-keys)
      (should (zpresent--hash-contains? slide key)))))

(ert-deftest make-slide/no-body ()
  (should-not (gethash :body (zpresent--make-slide "I'm the title!"))))

(ert-deftest make-slide/body ()
    (should (equal '("I'm a body")
                   (gethash :body (zpresent--make-slide "I'm the title!" "I'm a body")))))


(ert-deftest make-top-level-slide/title-has-type-title ()
  (should (equal (gethash :type (zpresent--make-top-level-slide (car (gethash :content (org-parser-parse-string "* title\n:PROPERTIES:\n:type:     title\n:END:\n")))))
                 :title)))

(ert-deftest make-top-level-slide/title-has-type-normal ()
  (should (equal (gethash :type (zpresent--make-top-level-slide (car (gethash :content (org-parser-parse-string "* title\n:")))))
                 :normal)))


(ert-deftest make-following-slide/original-slide-not-updated ()
  (let* ((original-slide (zpresent--make-slide "I'm the title!"))
         (original-copy (copy-hash-table original-slide))
         (new-slide (zpresent--make-following-slide original-slide (car (gethash :content (org-parser-parse-string "* New body text."))) 1 0)))
    (dolist (key (hash-table-keys original-slide))
      (should (equal (gethash key original-slide)
                     (gethash key original-copy))))
    (should (equal (hash-table-count original-slide)
                   (hash-table-count original-copy)))))

(ert-deftest make-following-slide/check-title ()
  (let* ((original-slide (zpresent--make-slide "I'm the title!"))
         (new-slide (zpresent--make-following-slide original-slide (car (gethash :content (org-parser-parse-string "* New body text."))) 1 0)))
    (should (equal "I'm the title!"
                   (gethash :title new-slide)))))

(ert-deftest make-following-slide/check-new-body ()
  (let* ((original-slide (zpresent--make-slide "I'm the title!"))
         (new-slide (zpresent--make-following-slide original-slide (car (gethash :content (org-parser-parse-string "* New body text."))) 1 0)))
    (should (equal '((" ▸ " "New body text."))
                   (gethash :body new-slide)))))

(ert-deftest make-following-slide/check-added-body ()
  (let* ((original-slide (zpresent--make-slide "I'm the title!" "Initial body."))
         (new-slide (zpresent--make-following-slide original-slide (car (gethash :content (org-parser-parse-string "* New body text."))) 1 0)))
    (should (equal '("Initial body." (" ▸ " "New body text."))
                   (gethash :body new-slide)))))

(ert-deftest make-following-slide/with-explicit-parent ()
  (let* ((original-slide (zpresent--make-slide "I'm the title!" "Initial body."))
         (new-slide (zpresent--make-following-slide original-slide
                                                    (car (gethash :content (org-parser-parse-string "* New body text.")))
                                                    1
                                                    0
                                                    (car (gethash :content (org-parser-parse-string "* fake parent\n:PROPERTIES:\n:child-bullet-type: .\n:END:"))))))
    (should (equal '("Initial body." (" 1. " "New body text."))
                   (gethash :body new-slide)))))


(ert-deftest extract-current-text/simple-headline ()
  (should (equal '(("simple headline"))
                 (zpresent--extract-current-text (car (gethash :content (org-parser-parse-string "* simple headline")))))))

(ert-deftest extract-current-text/nested-headline ()
  (should (equal '(("nested headline"))
                 (zpresent--extract-current-text (car (gethash :content (org-parser-parse-string "** nested headline")))))))

(ert-deftest extract-current-text/simple-headline-with-multiple-line-body ()
  (should (equal '(("nested headline") ("with body") ("over multiple lines"))
                 (zpresent--extract-current-text (car (gethash :content (org-parser-parse-string "** nested headline\nwith body\nover multiple lines")))))))

(ert-deftest extract-current-text/simple-plain-list ()
  (should (equal '(("simple plain list"))
                 (zpresent--extract-current-text (car (gethash :content (org-parser-parse-string "- simple plain list")))))))

(ert-deftest extract-current-text/nested-plain-list ()
  (should (equal '(("nested plain list"))
                 (zpresent--extract-current-text (car (gethash :content (org-parser-parse-string "  - nested plain list")))))))

(ert-deftest extract-current-text/simple-plain-list-with-multiple-line-body ()
  (should (equal '(("nested plain list") ("with body") ("over multiple lines"))
                 (zpresent--extract-current-text (car (gethash :content (org-parser-parse-string "  - nested plain list\nwith body\nover multiple lines")))))))


(ert-deftest make-body/simple-headline ()
  (should (equal '((" ▸ " "headline"))
                 (zpresent--make-body (car (gethash :content (org-parser-parse-string "* headline"))) 1 0))))

(ert-deftest make-body/link-in-headline ()
  (let ((body (zpresent--make-body (car (gethash :content (org-parser-parse-string "* headline [[http://example.com][with link]] in it"))) 1 0)))
    (should (equal 1 (length body)))
    (let ((title-line (first body)))
      (should (equal 4 (length title-line)))
      (should (equal " ▸ " (first title-line)))
      (should (equal "headline " (second title-line)))
      (should (hash-table-p (third title-line)))
      (should (equal "http://example.com"
                     (gethash :target (third title-line))))
      (should (equal "with link"
                     (gethash :text (third title-line))))
      (should (equal " in it" (fourth title-line))))))

(ert-deftest make-body/link-in-headline-with-body ()
  (let ((body (zpresent--make-body (car (gethash :content (org-parser-parse-string "* headline [[http://example.com][with link]] in it\nand now a body [[http://example.com][with a link!]]\nwith two parts"))) 1 0)))
    (should (equal 3 (length body)))
    (cl-multiple-value-bind (first-line second-line third-line)
        body
      (should (equal 4 (length first-line)))
      (should (equal " ▸ " (first first-line)))
      (should (equal "headline " (second first-line)))
      (should (hash-table-p (third first-line)))
      (should (equal "http://example.com"
                     (gethash :target (third first-line))))
      (should (equal "with link"
                     (gethash :text (third first-line))))
      (should (equal " in it" (fourth first-line)))

      (should (equal 3 (length second-line)))
      (should (equal "   " (first second-line)))
      (should (equal "and now a body " (second second-line)))
      (should (hash-table-p (third second-line)))
      (should (equal "http://example.com"
                     (gethash :target (third second-line))))
      (should (equal "with a link!"
                     (gethash :text (third second-line))))

      (should (equal '("   " "with two parts")
                     third-line)))))

(ert-deftest make-body/indented-headline ()
  (should (equal '(("   ▸ " "my headline"))
                 (zpresent--make-body (car (gethash :content (org-parser-parse-string "** my headline"))) 2 0))))

(ert-deftest make-body/plain-list ()
  (should (equal '(("  " "a plain list"))
                 (zpresent--make-body (car (gethash :content (org-parser-parse-string "- a plain list"))) 1 0))))

(ert-deftest make-body/indented-plain-list ()
  (should (equal '(("    " "in too deep"))
                 (zpresent--make-body (car (gethash :content (org-parser-parse-string "  - in too deep"))) 2 0))))

(ert-deftest make-body/two-line-headline ()
  (should (equal '(("   ▸ " "top headline") ("     " "on two lines"))
                 (zpresent--make-body (car (gethash :content (org-parser-parse-string "* top headline\non two lines"))) 2 0))))

(ert-deftest make-body/ignores-children ()
  (should (equal '((" ▸ " "headline"))
                 (zpresent--make-body (car (gethash :content (org-parser-parse-string "* headline\n** I'm nested, you guys!"))) 1 0))))

(ert-deftest make-body/two-line-headline-not-at-top ()
  (should (equal '(("   ▸ " "headline") ("     " "on two lines"))
                 (zpresent--make-body (car (gethash :children (car (gethash :content (org-parser-parse-string "* top\n** headline\non two lines"))))) 2 0))))

(ert-deftest make-body/ordered-list-first-item ()
  (should (equal '((" 1. " "First stuff"))
                 (zpresent--make-body (first (gethash :children (car (gethash :content (org-parser-parse-string "* top\n1. First stuff\n2. Other stuff\n")))))
                                      1
                                      0))))

(ert-deftest make-body/ordered-list-because-of-parent-properties ()
  (should (equal '((" 2. " "Other stuff"))
                 (zpresent--make-body (car (gethash :content (org-parser-parse-string "* Other stuff")))
                                      1
                                      1
                                      (car (gethash :content (org-parser-parse-string "* whatever\n:PROPERTIES:\n:child-bullet-type: .\n:END:")))))))

(ert-deftest make-body/ordered-list-second-item ()
  (should (equal '((" 2. " "Other stuff"))
                 (zpresent--make-body (second (gethash :children (car (gethash :content (org-parser-parse-string "* top\n1. First stuff\n2. Other stuff\n")))))
                                      1
                                      1))))

(ert-deftest make-body/block ()
  (let ((body (zpresent--make-body (first (gethash :children (car (gethash :content (org-parser-parse-string "* whatever\n** nested -- this causes an error!\n#+BEGIN_SRC emacs-lisp\n  (format \"hi %s\"\n          \"mom\")\n#+END_SRC")))))
                                   2
                                   0)))
    (should (equal 2 (length body)))
    (should (equal '("   ▸ " "nested -- this causes an error!")
                   (first body)))
    (should (listp (second body)))
    (should (equal "     " (first (second body))))
    (let ((block (cdr (second body))))
      (should (hash-table-p block))
      (should (equal :block
                     (gethash :type block))))))



(ert-deftest get-bullet-type/regular-asterisk ()
  (should (equal ?*
                 (zpresent--get-bullet-type (car (gethash :content (org-parser-parse-string "* whatever")))
                                            (car (gethash :content (org-parser-parse-string "* whatever")))))))

(ert-deftest get-bullet-type/regular-paren ()
  (should (equal ?\)
                 (zpresent--get-bullet-type (car (gethash :content (org-parser-parse-string "1) whatever")))
                                            (car (gethash :content (org-parser-parse-string "* whatever")))))))

(ert-deftest get-bullet-type/regular-period ()
  (should (equal ?.
                 (zpresent--get-bullet-type (car (gethash :content (org-parser-parse-string "1. whatever")))
                                            (car (gethash :content (org-parser-parse-string "* whatever")))))))

(ert-deftest get-bullet-type/asterisk-property ()
  (should (equal ?*
                 (zpresent--get-bullet-type (car (gethash :content (org-parser-parse-string "* whatever")))
                                            (car (gethash :content (org-parser-parse-string "* whatever")))))))

(ert-deftest get-bullet-type/paren-property ()
  (should (equal ?\)
                 (zpresent--get-bullet-type (car (gethash :content (org-parser-parse-string "* top\n:PROPERTIES:\n:bullet-type: )\n:END:")))
                                            (car (gethash :content (org-parser-parse-string "* whatever")))))))

(ert-deftest get-bullet-type/period-property ()
  (should (equal ?.
                 (zpresent--get-bullet-type (car (gethash :content (org-parser-parse-string "* top\n:PROPERTIES:\n:bullet-type: .\n:END:")))
                                            (car (gethash :content (org-parser-parse-string "* whatever")))))))

(ert-deftest get-bullet-type/no-bullet-type-parent-with-child-type ()
  (should (equal ?.
                 (zpresent--get-bullet-type (car (gethash :content (org-parser-parse-string "* whatever")))
                                            (car (gethash :content (org-parser-parse-string "* top\n:PROPERTIES:\n:child-bullet-type: .\n:bullet-type: )\n:END:")))))))

(ert-deftest get-bullet-type/no-bullet-type-parent-with-no-child-type ()
  (should (equal ?*
                 (zpresent--get-bullet-type (car (gethash :content (org-parser-parse-string "* whatever")))
                                            (car (gethash :content (org-parser-parse-string "* top\n:PROPERTIES:\n:bullet-type: .\n:END:")))))))

(ert-deftest get-bullet-type/bullet-type-parent-with-child-type ()
  (should (equal ?.
                 (zpresent--get-bullet-type (car (gethash :content (org-parser-parse-string "* top\n:PROPERTIES:\n:bullet-type: .\n:child-bullet-type: )\n:END:")))
                                            (car (gethash :content (org-parser-parse-string "- top\n:PROPERTIES:\n:child-bullet-type: *\n:END:")))))))

(ert-deftest get-bullet-type/bullet-type-parent-with-no-child-type ()
  (should (equal ?\)
                 (zpresent--get-bullet-type (car (gethash :content (org-parser-parse-string "* top\n:PROPERTIES:\n:bullet-type: )\n:child-bullet-type: .\n:END:")))
                                            (car (gethash :content (org-parser-parse-string "- top\n:PROPERTIES:\n:bullet-type: .\n:END:")))))))



(ert-deftest format-bullet/headline ()
  (should (equal "▸"
                 (zpresent--format-bullet (car (gethash :content (org-parser-parse-string "* headline")))
                                          0))))

(ert-deftest format-bullet/paren-ordered-list ()
  (should (equal "1)"
                 (zpresent--format-bullet (car (gethash :content (org-parser-parse-string "12) headline")))
                                          0))))

(ert-deftest format-bullet/paren-ordered-list-set-by-parent ()
  (should (equal "12)"
                 (zpresent--format-bullet (car (gethash :content (org-parser-parse-string "* headline")))
                                          11
                                          (car (gethash :content (org-parser-parse-string "* headline\n:PROPERTIES:\n:child-bullet-type: )\n:END:")))))))

(ert-deftest format-bullet/dot-ordered-list ()
  (should (equal "12."
                 (zpresent--format-bullet (car (gethash :content (org-parser-parse-string "3. headline")))
                                          11))))

(ert-deftest format-bullet/headline-property ()
  (should (equal "▸"
                 (zpresent--format-bullet (car (gethash :content (org-parser-parse-string "* headline")))
                                          0))))

(ert-deftest format-bullet/paren-ordered-list-property ()
  (should (equal "4)"
                 (zpresent--format-bullet (car (gethash :content (org-parser-parse-string "* top\n:PROPERTIES:\n:bullet-type: )\n:END:")))
                                          3))))

(ert-deftest format-bullet/dot-ordered-list-property ()
  (should (equal "2."
                 (zpresent--format-bullet (car (gethash :content (org-parser-parse-string "* top\n:PROPERTIES:\n:bullet-type: .\n:END:")))
                                          1))))


(ert-deftest calculate-aligned-whitespace/single-string ()
  ;;3 on left; 3 on right
  (should (equal 3
                 (length (zpresent--calculate-aligned-whitespace '(("123"))
                                                                 9)))))

(ert-deftest calculate-aligned-whitespace/single-row ()
  (should (equal 4
                 (length (zpresent--calculate-aligned-whitespace '(("12" "34"))
                                                                 12)))))

(ert-deftest calculate-aligned-whitespace/three-rows ()
  (should (equal 4
                 (length (zpresent--calculate-aligned-whitespace '(("12" "3")
                                                                   ("1" "234")
                                                                   ("12"))
                                                                 12)))))

(ert-deftest calculate-aligned-whitespace/with-link ()
  (should (equal 3
                 (length (zpresent--calculate-aligned-whitespace (list (list (org-parser--make-link-hash "http://example.com" "1 3") "45")
                                                                       (list "123"))
                                                                 11)))))

(ert-deftest calculate-aligned-whitespace/uneven-padding ()
  ;;3 on left, 2 on right
  (should (equal 3
                 (length (zpresent--calculate-aligned-whitespace '(("123"))
                                                                 8)))))

(ert-deftest calculate-aligned-whitespace/too-long-line ()
  (should (string-empty-p (zpresent--calculate-aligned-whitespace '(("foo bar baz bif bof flam zip zap zop")) 12))))


(ert-deftest whitespace-for-centered-title-line/empty-string ()
  (should (equal 1
                 (length (zpresent--whitespace-for-centered-title-line '("")
                                                                       2)))))

(ert-deftest whitespace-for-centered-title-line/single-string ()
  (should (equal 2
                 (length (zpresent--whitespace-for-centered-title-line '("hi mom")
                                                                       10)))))

(ert-deftest whitespace-for-centered-title-line/multiple-strings ()
  ;; title is length 18.
  ;; 7 spaces of padding total, so 3 on the left.
  (should (equal 3
                 (length (zpresent--whitespace-for-centered-title-line '("hi mom" " I'm a title")
                                                                       25)))))

(ert-deftest whitespace-for-centered-title-line/with-link ()
  (should (equal 3
                 (length (zpresent--whitespace-for-centered-title-line `("hi mom "
                                                                         ,(org-parser--make-link-hash "http://example.com" "I'm a")
                                                                         " title")
                                                                       25)))))

(ert-deftest whitespace-for-centered-title-line/no-whitespace ()
  (should (equal 0
                 (length (zpresent--whitespace-for-centered-title-line '("hi mom" " I'm a title")
                                                                       18)))))

(ert-deftest whitespace-for-centered-title-line/title-too-long ()
  (should (equal 0
                 (length (zpresent--whitespace-for-centered-title-line '("hi mom" " I'm a title")
                                                                       10)))))

(ert-deftest whitespace-for-centered-title-line/tons-of-space ()
  ;; title is length 12
  ;; 18 spaces of padding total, so 9 on the left.
  (should (equal 9
                 (length (zpresent--whitespace-for-centered-title-line '("I'm a title!")
                                                                       30)))))



(ert-deftest break-title-into-lines/single-string ()
  (should (equal (list (list "I'm short enough"))
                 (zpresent--break-title-into-lines (list "I'm short enough") 50))))

(ert-deftest break-title-into-lines/sample-broken ()
  (should (equal (list (list "I'm long,")
                       (list "you guys!"))
                 (zpresent--break-title-into-lines (list "I'm long, you guys!") 10))))

(ert-deftest break-title-into-lines/with-unbroken-link ()
  (let ((result-list (zpresent--break-title-into-lines
                      (list "Here's a link: "
                            (org-parser--make-link-hash "my-target" "my-text"))
                      50)))
    (should (equal 1
                   (length result-list)))
    (should (equal 2
                   (length (first result-list))))
    (cl-multiple-value-bind (first-thing second-thing)
        (first result-list)
      (should (equal "Here's a link: "
                     first-thing))
      (should (hash-table-p second-thing))
      (should (equal "my-target"
                     (gethash :target second-thing)))
      (should (equal "my-text"
                     (gethash :text second-thing))))))

(ert-deftest break-title-into-lines/with-broken-link ()
  (let ((result-list (zpresent--break-title-into-lines
                      (list "123 "
                            (org-parser--make-link-hash "my-target" " 67 901 "))
                      10)))
    (should (equal 2
                   (length result-list)))
    (let ((first-sublist (first result-list))
          (second-sublist (second result-list)))
      (should (equal 2
                     (length first-sublist)))
      (should (equal "123 "
                     (first first-sublist)))
      (should (hash-table-p (second first-sublist)))
      (should (equal "my-target"
                     (gethash :target (second first-sublist))))
      (should (equal " 67"
                     (gethash :text (second first-sublist))))

      (should (equal 1
                     (length second-sublist)))
      (should (hash-table-p (first second-sublist)))
      (should (equal "my-target"
                     (gethash :target (first second-sublist))))
      (should (equal "901"
                     (gethash :text (first second-sublist)))))))

(ert-deftest break-title-into-lines/multiple-spaces-are-kept-when-not-broken ()
  (should (equal (list (list "Look at the    spaces!"))
                 (zpresent--break-title-into-lines (list "Look at the    spaces!") 50))))

(ert-deftest break-title-into-lines/multiple-spaces-are-kept-when-broken ()
  (should (equal (list (list "Look at all of")
                       (list "the    spaces!"))
                 (zpresent--break-title-into-lines (list "Look at all of the    spaces!") 15))))

(ert-deftest break-title-into-lines/multiple-spaces-in-link-text-are-kept ()
  (let ((result (zpresent--break-title-into-lines (list "Look at " (org-parser--make-link-hash "an invalid target" "some   spaces")) 50)))
    (should (equal 1
                   (length result)))
    (should (equal 2
                   (length (first result))))
    (should (equal "Look at "
                   (first (first result))))
    (let ((link-hash (second (first result))))
      (should (equal "an invalid target"
                     (gethash :target link-hash)))
      (should (equal "some   spaces"
                     (gethash :text link-hash))))))

(ert-deftest break-title-into-lines/with-text-after-unbroken-link ()
  (let ((result (zpresent--break-title-into-lines (list "Look at " (org-parser--make-link-hash "an invalid target" "this link") " and now more text")
                                                  50)))
    (should (equal 1
                   (length result)))
    (let ((sublist (first result)))
      (should (equal 3
                     (length sublist)))
      (should (equal "Look at "
                     (first sublist)))
      (should (hash-table-p (second sublist)))
      (should (equal "an invalid target"
                     (gethash :target (second sublist))))
      (should (equal "this link"
                     (gethash :text (second sublist))))
      (should (equal " and now more text"
                     (third sublist))))))

(ert-deftest break-title-into-lines/with-text-after-broken-link ()
  (let ((result (zpresent--break-title-into-lines (list "Here " (org-parser--make-link-hash "an invalid target" "this link") " is.")
                                                  9)))
    (should (equal 2
                   (length result)))
    (let ((first-sublist (first result))
          (second-sublist (second result)))

      (should (equal 2
                     (length first-sublist)))
      (should (equal "Here "
                     (first first-sublist)))
      (should (hash-table-p (second first-sublist)))
      (should (equal "an invalid target"
                     (gethash :target (second first-sublist))))
      (should (equal "this"
                     (gethash :text (second first-sublist))))

      (should (equal 2
                     (length second-sublist)))
      (should (hash-table-p (first second-sublist)))
      (should (equal "an invalid target"
                     (gethash :target (first second-sublist))))
      (should (equal "link"
                     (gethash :text (first second-sublist))))
      (should (equal " is."
                     (second second-sublist))))))

(ert-deftest break-title-into-lines/with-link-text-too-long ()
  (let ((result (zpresent--break-title-into-lines (list (org-parser--make-link-hash "an invalid target" "thistextissolongbutitcantbebroken!")) 10)))
    (should (equal 1 (length result)))
    (should (equal 1 (length (first result))))
    (let ((link-hash (first (first result))))
      (should (equal "an invalid target"
                     (gethash :target link-hash)))
      (should (equal "thistextissolongbutitcantbebroken!"
                     (gethash :text link-hash))))))

(ert-deftest break-title-into-lines/very-long-line ()
  (should (equal '(("arst tsra") ("arst tsra") ("arst tsra") ("arst tsra"))
                 (zpresent--break-title-into-lines '("arst tsra arst tsra arst tsra arst tsra") 10))))

(ert-deftest break-title-into-lines/non-integer-length ()
  (should (equal '(("Hi mom")
                   ("I'm cool"))
                 (zpresent--break-title-into-lines '("Hi mom I'm cool") 9.5))))



(ert-deftest newlines-for-vertical-centering/1-3 ()
  (should (equal 1
                 (zpresent--newlines-for-vertical-centering 1 3))))

(ert-deftest newlines-for-vertical-centering/2-3 ()
  (should (equal 0
                 (zpresent--newlines-for-vertical-centering 2 3))))

(ert-deftest newlines-for-vertical-centering/1-5 ()
  (should (equal 2
                 (zpresent--newlines-for-vertical-centering 1 5))))

(ert-deftest newlines-for-vertical-centering/2-5 ()
  (should (equal 1
                 (zpresent--newlines-for-vertical-centering 2 5))))

(ert-deftest newlines-for-vertical-centering/too-many-lines ()
  (should (equal 0
                 (zpresent--newlines-for-vertical-centering 4 1))))


(ert-deftest pull-single-title-line/single-string ()
  (should (equal (list (list "One line, you guys!") nil)
                 (zpresent--pull-single-title-line (list "One line, you guys!")
                                                   50))))

(ert-deftest pull-single-title-line/broken-right-at-whitespace-before-link ()
  (let ((result (zpresent--pull-single-title-line (list "A "
                                                        (org-parser--make-link-hash "http://example.com"
                                                                                    "whatever"))
                                                  1)))
    (should (equal (list "A")
                   (first result)))
    (should (equal 1
                   (length (second result))))
    (should (hash-table-p (first (second result))))
    (should (equal "whatever"
                   (gethash :text
                            (first (second result)))))))

(ert-deftest pull-single-title-line/pulls-in-link-only-if-it-has-to ()
  (let ((result (zpresent--pull-single-title-line (list "my "
                                                        (org-parser--make-link-hash "http://example.com"
                                                                                    "link"))
                                                  4)))
    (should (equal '("my")
                   (first result)))))

;;pull-single-title-line doesn't strip whitespace from the beginning if it doesn't break the item.

(ert-deftest pull-single-title-line/whitespae-between-words-not-stripped ()
  (let ((result (zpresent--pull-single-title-line (list (org-parser--make-link-hash "http://example.com"
                                                                                    "link")
                                                        "  here and more stuff")
                                                  10)))
    (should (hash-table-p (first (first result))))
    (should (equal "link"
                   (gethash :text (first (first result)))))

    (should (equal "  here"
                   (second (first result))))))


(ert-deftest pull-single-title-line/beginning-whitespace-stripped ()
  (should (equal '(("my") nil)
                 (zpresent--pull-single-title-line (list "  my ") 4))))

(ert-deftest pull-single-title-line/ending-whitespace-stripped ()
  (should (equal '(("my") nil)
                 (zpresent--pull-single-title-line (list "my   ") 4))))

(ert-deftest pull-single-title-line/both-whitespace-stripped ()
  (should (equal '(("my") nil)
                 (zpresent--pull-single-title-line (list "   my ") 4))))


(ert-deftest pull-single-title-line/strict-length-true ()
  (should (equal '(nil ("too-long"))
                 (zpresent--pull-single-title-line (list "too-long")
                                                   4
                                                   t))))

(ert-deftest pull-single-title-line/strict-length-false ()
  (should (equal '(("too-long") nil)
                 (zpresent--pull-single-title-line (list "too-long")
                                                   4
                                                   nil))))

(ert-deftest pull-single-title-line/whitespace-stripped-when-not-broken ()
  (should (equal (list (list "Nothing before or after") nil)
                 (zpresent--pull-single-title-line (list "  Nothing before or after  ")
                                                   50))))

(ert-deftest pull-single-title-line/whitespace-stripped-when-broken ()
  (should (equal (list (list "Spaces")
                       (list "here"))
                 (zpresent--pull-single-title-line (list "     Spaces here")
                                                   10))))

(ert-deftest pull-single-title-line/beginning-whitespace-doesnt-cause-break ()
  (should (equal (list (list "1234 6") nil)
                 (zpresent--pull-single-title-line (list "    1234 6  ")
                                                   6))))

(ert-deftest pull-single-title-line/single-string-broken ()
  (should (equal (list (list "One line,")
                       (list "you guys!"))
                 (zpresent--pull-single-title-line (list "One line, you guys!")
                                                   10))))

(ert-deftest pull-single-title-line/multiple-strings-in-one-line ()
  (should (equal (list (list "One line, you guys!") nil)
                 (zpresent--pull-single-title-line (list "One line," " you guys!")
                                                   50))))

(ert-deftest pull-single-title-line/second-string-broken ()
  (should (equal (list (list "One line, you")
                       (list "guys!"))
                 (zpresent--pull-single-title-line (list "One line," " you guys!")
                                                   15))))

(ert-deftest pull-single-title-line/second-string-broken-exactly-at-word-boundary ()
  (should (equal (list (list "One line, you")
                       (list "guys!"))
                 (zpresent--pull-single-title-line (list "One line," " you guys!")
                                                   13))))


(ert-deftest pull-single-title-line/single-link ()
  (let ((result (zpresent--pull-single-title-line (list (org-parser--make-link-hash "http://example.com" "Short link text"))
                                                  20)))
    (should (equal 2
                   (length result)))
    (should (equal 1 (length (first result))))
    (let ((link-hash (first (first result))))
      (should (equal "http://example.com"
                     (gethash :target link-hash)))
      (should (equal "Short link text"
                     (gethash :text link-hash))))

    (should-not (second result))))

(ert-deftest pull-single-title-line/single-link-broken ()
  (let ((result (zpresent--pull-single-title-line (list (org-parser--make-link-hash "http://example.com" "Long link text"))
                                                  10)))
    (should (equal 2
                   (length result)))
    (should (equal 1 (length (first result))))
    (let ((first-link-hash (first (first result))))
      (should (equal "http://example.com"
                     (gethash :target first-link-hash)))
      (should (equal "Long link"
                     (gethash :text first-link-hash))))

    (should (equal 1 (length (second result))))
    (let ((second-link-hash (first (second result))))
      (should (equal "http://example.com"
                     (gethash :target second-link-hash)))
      (should (equal "text"
                     (gethash :text second-link-hash))))))

(ert-deftest pull-single-title-line/multiple-links-in-one-line ()
  (let ((result (zpresent--pull-single-title-line (list (org-parser--make-link-hash "http://example.com" "One link")
                                                        " and "
                                                        (org-parser--make-link-hash "http://example.com" "another link!"))
                                                  50)))
    (should (equal 2
                   (length result)))
    (should (equal 3
                   (length (first result))))
    (should (hash-table-p (first (first result))))
    (should (equal "http://example.com"
                   (gethash :target (first (first result)))))
    (should (equal "One link"
                   (gethash :text (first (first result)))))

    (should (equal " and "
                   (second (first result))))
    (should (hash-table-p (third (first result))))
    (should (equal "http://example.com"
                   (gethash :target (third (first result)))))
    (should (equal "another link!"
                   (gethash :text (third (first result)))))

    (should-not (second result))))

(ert-deftest pull-single-title-line/second-link-broken ()
  (let ((result (zpresent--pull-single-title-line (list (org-parser--make-link-hash "http://example.com" "One link")
                                                        " and "
                                                        (org-parser--make-link-hash "http://example.com" "another link!"))
                                                  22)))
    (should (equal 2
                   (length result)))
    (should (equal 3
                   (length (first result))))
    (should (hash-table-p (first (first result))))
    (should (equal "http://example.com"
                   (gethash :target (first (first result)))))
    (should (equal "One link"
                   (gethash :text (first (first result)))))

    (should (equal " and "
                   (second (first result))))
    (should (hash-table-p (third (first result))))
    (should (equal "http://example.com"
                   (gethash :target (third (first result)))))
    (should (equal "another"
                   (gethash :text (third (first result)))))

    (should (equal 1
                   (length (second result))))
    (should (hash-table-p (first (second result))))
    (should (equal "http://example.com"
                   (gethash :target (first (second result)))))
    (should (equal "link!"
                   (gethash :text (first (second result)))))))

(ert-deftest pull-single-title-line/first-of-many-lines ()
  (let ((result (zpresent--pull-single-title-line (list "One line," " you guys!" " For this is the first" " of many lines. " "This is a true fact.")
                                                  13)))
    (should (equal 2
                   (length result)))
    (should (equal (list "One line, you")
                   (first result)))
    (should (equal (list  "guys! For this is the first of many lines. This is a true fact.")
                   (second result)))))



(ert-deftest trim-beginning-and-end-of-line/one-string-no-trimming ()
  (should (equal '("nothing here")
                 (zpresent--trim-beginning-and-end-of-line (list "nothing here")))))

(ert-deftest trim-beginning-and-end-of-line/one-link-no-trimming ()
  (let ((result (zpresent--trim-beginning-and-end-of-line (list (org-parser--make-link-hash "http://example.com"
                                                                                            "nor here")))))
    (should (equal 1
                   (length result)))
    (should (hash-table-p (first result)))
    (should (equal "nor here"
                   (gethash :text
                            (first result))))))

(ert-deftest trim-beginning-and-end-of-line/one-string-both-trimming ()
  (should (equal '("a string")
                 (zpresent--trim-beginning-and-end-of-line (list "   a string  ")))))

(ert-deftest trim-beginning-and-end-of-line/one-link-both-trimming ()
  (let ((result (zpresent--trim-beginning-and-end-of-line (list (org-parser--make-link-hash "http://example.com"
                                                                                            " trim both  ")))))
    (should (equal 1
                   (length result)))
    (should (hash-table-p (first result)))
    (should (equal "trim both"
                   (gethash :text
                            (first result))))))

(ert-deftest trim-beginning-and-end-of-line/multiple-strings-no-trimming ()
  (should (equal '("nothing here")
                 (zpresent--trim-beginning-and-end-of-line (list "nothing here")))))

(ert-deftest trim-beginning-and-end-of-line/multiple-strings-trim-all ()
  (should (equal '("there were   " "spaces " "but now there aren't")
                 (zpresent--trim-beginning-and-end-of-line (list "  there were   " "spaces " "but now there aren't   ")))))

(ert-deftest trim-beginning-and-end-of-line/strings-and-links-no-trimming ()
  (let ((result (zpresent--trim-beginning-and-end-of-line (list "nothing here "
                                                                (org-parser--make-link-hash "http://example.com"
                                                                                            " nor here ")
                                                                " and definitely not here."))))
    (should (equal 3
                   (length result)))
    (should (equal "nothing here "
                   (first result)))
    (should (equal " nor here "
                   (gethash :text
                            (second result))))
    (should (equal " and definitely not here."
                   (third result)))))

(ert-deftest trim-beginning-and-end-of-line/strings-and-links-with-trimming ()
  (let ((result (zpresent--trim-beginning-and-end-of-line (list "   some stuff here "
                                                                (org-parser--make-link-hash "http://example.com"
                                                                                            " but not here ")
                                                                " and definitely not here."
                                                                (org-parser--make-link-hash "http://example.com"
                                                                                            " but here I guess  ")))))
    (should (equal 4
                   (length result)))
    (should (equal "some stuff here "
                   (first result)))
    (should (hash-table-p (second result)))
    (should (equal " but not here "
                   (gethash :text
                            (second result))))
    (should (equal " and definitely not here."
                   (third result)))
    (should (hash-table-p (fourth result)))
    (should (equal " but here I guess"
                   (gethash :text
                            (fourth result))))))


(ert-deftest trim-item-left/string ()
  (should (equal "whatever this thing is "
                 (zpresent--trim-item-left "  whatever this thing is "))))

(ert-deftest trim-item-left/link ()
  (let ((result-hash (zpresent--trim-item-left (org-parser--make-link-hash "http://example.com"
                                                                           "  whatever this is "))))
    (should (hash-table-p result-hash))
    (should (equal "http://example.com"
                   (gethash :target result-hash)))
    (should (equal "whatever this is "
                   (gethash :text result-hash)))))

(ert-deftest trim-item-left/untrimmed-string ()
  (should (equal "whatever this thing is "
                 (zpresent--trim-item-left "whatever this thing is "))))

(ert-deftest trim-item-left/untrimmed-link ()
  (let ((result-hash (zpresent--trim-item-left (org-parser--make-link-hash "http://example.com"
                                                                           "whatever this is "))))
    (should (hash-table-p result-hash))
    (should (equal "http://example.com"
                   (gethash :target result-hash)))
    (should (equal "whatever this is "
                   (gethash :text result-hash)))))

(ert-deftest trim-item-right/string ()
  (should (equal "  whatever this thing is"
                 (zpresent--trim-item-right "  whatever this thing is   "))))

(ert-deftest trim-item-right/link ()
  (let ((result-hash (zpresent--trim-item-right (org-parser--make-link-hash "http://example.com"
                                                                            "  whatever this is "))))
    (should (hash-table-p result-hash))
    (should (equal "http://example.com"
                   (gethash :target result-hash)))
    (should (equal "  whatever this is"
                   (gethash :text result-hash)))))

(ert-deftest trim-item-right/untrimmed-string ()
  (should (equal " whatever this thing is"
                 (zpresent--trim-item-right " whatever this thing is "))))

(ert-deftest trim-item-right/untrimmed-link ()
  (let ((result-hash (zpresent--trim-item-right (org-parser--make-link-hash "http://example.com"
                                                                            "  whatever this is"))))
    (should (hash-table-p result-hash))
    (should (equal "http://example.com"
                   (gethash :target result-hash)))
    (should (equal "  whatever this is"
                   (gethash :text result-hash)))))


(ert-deftest combine-consecutive-strings-in-list/one-string ()
  (should (equal '("I'm one string")
                 (zpresent--combine-consecutive-strings-in-list '("I'm one string")))))

(ert-deftest combine-consecutive-strings-in-list/two-strings ()
  (should (equal '("I'm two strings")
                 (zpresent--combine-consecutive-strings-in-list '("I'm two" " strings")))))

(ert-deftest combine-consecutive-strings-in-list/three-strings ()
  (should (equal '("I'm three strings")
                 (zpresent--combine-consecutive-strings-in-list '("I'm" " three " "strings")))))

(ert-deftest combine-consecutive-strings-in-list/link ()
  (let ((result (zpresent--combine-consecutive-strings-in-list (list (org-parser--make-link-hash "http://example.com" "my target")))))
    (should (equal 1
                   (length result)))
    (should (hash-table-p (first result)))))

(ert-deftest combine-consecutive-strings-in-list/link-and-two-strings ()
  (let ((result (zpresent--combine-consecutive-strings-in-list (list (org-parser--make-link-hash "http://example.com" "my target")
                                                                     "Two "
                                                                     "strings"))))
    (should (equal 2
                   (length result)))
    (should (hash-table-p (first result)))
    (should (equal "Two strings"
                   (second result)))))


(ert-deftest combine-consecutive-strings-in-list/whitespaces-kept ()
  (should (equal '("Here are four spaces:    and now more text")
                 (zpresent--combine-consecutive-strings-in-list '("Here are four spaces:  " "  and now more text")))))



(ert-deftest line-length/a-few-strings ()
  (should (equal 32
                 (zpresent--line-length (list "This is " "some text" " with no links.")))))

(ert-deftest line-length/single-link ()
  (should (equal 14
                 (zpresent--line-length (list (org-parser--make-link-hash "target" "link text here"))))))

(ert-deftest line-length/link-and-strings ()
  (should (equal 14
                 (zpresent--line-length (list "Here's "
                                              (org-parser--make-link-hash "target" "a link!"))))))


(ert-deftest item-length/string ()
  (should (equal 17
                 (zpresent--item-length "This is a string."))))

(ert-deftest item-length/link ()
  (should (equal 35
                 (zpresent--item-length (org-parser--make-link-hash "http://ignore.example.com" "This is what matters for this test.")))))

(ert-deftest item-length/image-link ()
  ;;zck should this be 1? Should it look at the actual width?
  ;; It definitely shouldn't be the length of "zp-image".
  :expected-result :failed
  (should (equal -1
                 (zpresent--item-length (org-parser--make-link-hash "file:favicon.ico" "zp-image")))))


(ert-deftest item-is-image/string-isnt ()
  (should-not (zpresent--item-is-image "I'm not an image, silly!")))

(ert-deftest item-is-image/zp-image-string-isnt ()
  (should-not (zpresent--item-is-image "zp-image")))

(ert-deftest item-is-image/image-is ()
  (should (zpresent--item-is-image (org-parser--make-link-hash "http://example.com/image.jpg" "zp-image"))))

(ert-deftest item-is-image/non-image-link-isnt ()
  (should-not (zpresent--item-is-image (org-parser--make-link-hash "http://example.com/image.jpg" "I'm not an image, silly!"))))



(ert-deftest break-item/unbroken-string ()
  (should (equal (list "I'm short enough!" nil)
                 (zpresent--break-item "I'm short enough!" 20))))

(ert-deftest break-item/broken-string ()
  (should (equal (list "I'm too" "long!")
                 (zpresent--break-item "I'm too long!" 10))))

(ert-deftest break-item/too-long-string ()
  (should (equal (list "I'mTooLongToBeBroken" nil)
                 (zpresent--break-item "I'mTooLongToBeBroken" 10))))

(ert-deftest break-item/unbroken-link ()
  (cl-multiple-value-bind (before-break after-break)
      (zpresent--break-item (org-parser--make-link-hash "http://example.com"
                                                        "Still short")
                            20)
    (should (hash-table-p before-break))
    (should (equal "http://example.com"
                   (gethash :target before-break)))
    (should (equal "Still short"
                   (gethash :text before-break)))

    (should-not after-break)))

(ert-deftest break-item/unbroken-link ()
  (cl-multiple-value-bind (before-break after-break)
      (zpresent--break-item (org-parser--make-link-hash "http://example.com"
                                                        "Long enough to break somewhere in the line")
                            20)
    (should (hash-table-p before-break))
    (should (equal "http://example.com"
                   (gethash :target before-break)))
    (should (equal "Long enough to break"
                   (gethash :text before-break)))

    (should (hash-table-p after-break))
    (should (equal "http://example.com"
                   (gethash :target after-break)))
    (should (equal "somewhere in the line"
                   (gethash :text after-break)))))

(ert-deftest break-item/too-long-link ()
  (cl-multiple-value-bind (before-break after-break)
      (zpresent--break-item (org-parser--make-link-hash "http://example.com"
                                                        "I'mWayTooLongToBreak but you can break me")
                            10)
    (should (hash-table-p before-break))
    (should (equal "http://example.com"
                   (gethash :target before-break)))
    (should (equal "I'mWayTooLongToBreak"
                   (gethash :text before-break)))

    (should (hash-table-p after-break))
    (should (equal "http://example.com"
                   (gethash :target after-break)))
    (should (equal "but you can break me"
                   (gethash :text after-break)))))

(ert-deftest break-item/string-strict-length-t ()
  (should (equal (list nil "too-long-johnny")
                 (zpresent--break-item "too-long-johnny"
                                       5
                                       t))))

(ert-deftest break-item/link-strict-length-t ()
  (let ((result (zpresent--break-item (org-parser--make-link-hash "http://example.com"
                                                                  "too-long-johnny")
                                      5
                                      t)))
    (should-not (first result))
    (should (hash-table-p (second result)))))

(ert-deftest break-item/link-strict-length-nil ()
  (let ((result (zpresent--break-item (org-parser--make-link-hash "http://example.com"
                                                                  "too-long-johnny")
                                      5
                                      nil)))
    (should (hash-table-p (first result)))
    (should-not (second result))))

(ert-deftest break-item/string-strict-length-nil ()
  (should (equal (list "too-long-johnny" nil)
                 (zpresent--break-item "too-long-johnny"
                                       5
                                       nil))))

(ert-deftest break-item/beginning-whitespace-kept ()
  (should (equal (list "  text" "here")
                 (zpresent--break-item "  text here"
                                       6))))

(ert-deftest break-item/trailing-whitespace-kept ()
  (should (equal (list "text" "here  ")
                 (zpresent--break-item "text here  "
                                       6))))

(ert-deftest break-item/middle-whitespace-kept ()
  (should (equal (list "a  b" "c d")
                 (zpresent--break-item "a  b c d"
                                       4))))


(ert-deftest split-at-space/unsplit ()
  (should (equal '("I'm short")
                 (zpresent--split-at-space "I'm short"
                                           10))))

(ert-deftest split-at-space/single-split ()
  (should (equal '("Split" "me!")
                 (zpresent--split-at-space "Split me!"
                                           7))))

(ert-deftest split-at-space/multiple-splits ()
  (should (equal '("I could" "be split" "a whole" "bunch of" "times, as" "I'm long" "enough")
                 (zpresent--split-at-space "I could be split a whole bunch of times, as I'm long enough"
                                           10))))

(ert-deftest split-at-space/multiple-spaces-kept ()
  (should (equal '("I have a bunch of spaces after" "this word       and before the" "next one.")
                 (zpresent--split-at-space "I have a bunch of spaces after this word       and before the next one."
                                           33))))

(ert-deftest split-once-at-space/unsplit ()
  (should (equal '("I'm short" nil)
                 (zpresent--split-once-at-space "I'm short"
                                                10))))

(ert-deftest split-once-at-space/single-split ()
  (should (equal '("Split" "me!")
                 (zpresent--split-once-at-space "Split me!"
                                                7))))

(ert-deftest split-once-at-space/multiple-splits ()
  (should (equal '("I could be" "split a whole bunch of times, as I'm long enough")
                 (zpresent--split-once-at-space "I could be split a whole bunch of times, as I'm long enough"
                                                10))))

(ert-deftest split-once-at-space/multiple-spaces-before-split-kept ()
  (should (equal '("I have a bunch of spaces after" "this word       and before the next one.")
                 (zpresent--split-once-at-space "I have a bunch of spaces after this word       and before the next one."
                                                33))))

(ert-deftest split-once-at-space/multiple-spaces-after-split-kept ()
  (should (equal '("I have a bunch of spaces after this word       and" "before the next one.")
                 (zpresent--split-once-at-space "I have a bunch of spaces after this word       and before the next one."
                                                50))))

(ert-deftest split-once-at-space/first-word-is-too-long ()
  (should (equal '("longest" "thing")
                 (zpresent--split-once-at-space "longest thing"
                                                5))))

(ert-deftest split-once-at-space/first-word-is-too-long-strict-length-t ()
  (should (equal '(nil "longest thing")
                 (zpresent--split-once-at-space "longest thing"
                                                5
                                                t))))

(ert-deftest split-once-at-space/leading-spaces-are-kept ()
  (should (equal '("  before" "after")
                 (zpresent--split-once-at-space "  before after"
                                                10))))

(ert-deftest split-once-at-space/trailing-spaces-are-kept ()
  (should (equal '("before" "after  ")
                 (zpresent--split-once-at-space "before after  "
                                                6))))

(ert-deftest split-once-at-space/non-integer-length ()
  (should (equal '("arst" "tsra")
                 (zpresent--split-once-at-space "arst tsra" 2.5))))


(ert-deftest format/ordered-lists-start-at-1 ()
  (should (equal '(" 1. " "first child.")
                 (first (gethash :body (first (zpresent--format (gethash :content (org-parser-parse-string "* top\n1. first child.\n2. second child.")))))))))

(ert-deftest format/ordered-list-second-item-is-2 ()
  (should (equal '(" 2. " "second child.")
                 (second (gethash :body (first (zpresent--format (gethash :content (org-parser-parse-string "* top\n1. first child.\n2. second child.")))))))))

(ert-deftest format/nested-ordered-lists-start-at-1 ()
  (should (equal '("   1. " "first double-nested child.")
                 (second (gethash :body (first (zpresent--format (gethash :content (org-parser-parse-string "* top\n1. first nested list.\n   1. first double-nested child.\n   2. second double-nested child.")))))))))

(ert-deftest format/nested-ordered-lists-second-item-is-2 ()
  (should (equal '("   2. " "second double-nested child.")
                 (third (gethash :body (fourth (zpresent--format (gethash :content (org-parser-parse-string "* top    :slide:\n1. first nested list.    :slide:\n   1. first double-nested child.    :slide:\n   2. second double-nested child.")))))))))



(ert-deftest format-structure/single-headline ()
  (should (equal '(("my headline"))
                 (gethash :title (first (zpresent--format-structure (car (gethash :content (org-parser-parse-string "* my headline")))))))))

(ert-deftest format-structure/single-body ()
  (should (equal '((" ▸ " "the body here"))
                 (gethash :body (second (zpresent--format-structure (car (gethash :content (org-parser-parse-string "* my headline :slide:\n** the body here")))))))))

(ert-deftest format-structure/goes-until-slide ()
  (should (equal 2
                 (length (zpresent--format-structure (car (gethash :content (org-parser-parse-string "* headline\n** second\n** third   :slide:\n** fourth\n** fifth"))))))))

(ert-deftest format-structure/single-headline-is-checkpoint ()
  (should (gethash :checkpoint
                   (first (zpresent--format-structure (car (gethash :content (org-parser-parse-string "* headline\n"))))))))

(ert-deftest format-structure/headline-with-children-is-checkpoint ()
  (should (gethash :checkpoint
                   (first (zpresent--format-structure (car (gethash :content (org-parser-parse-string "* headline\n** nested but that shouldn't matter"))))))))

(ert-deftest format-structure/second-slide-is-not-checkpoint ()
  (should-not (gethash :checkpoint
                       (second (zpresent--format-structure (car (gethash :content (org-parser-parse-string "* headline    :slide:\n** nested and I'm not a checkpoint"))))))))


(ert-deftest get-last-descendant/no-children ()
  (should (equal '("top")
                 (gethash :text
                          (zpresent--get-last-descendant (car (gethash :content (org-parser-parse-string "* top"))))))))

(ert-deftest get-last-descendant/many-children-no-grandchildren ()
  (should (equal '("child3")
                 (gethash :text
                          (zpresent--get-last-descendant (car (gethash :content (org-parser-parse-string "* top\n** child1\n** child2\n** child3"))))))))

(ert-deftest get-last-descendant/many-grandchildren ()
  (should (equal '("grandchild33")
                 (gethash :text
                          (zpresent--get-last-descendant (car (gethash :content (org-parser-parse-string "* top\n** child1\n*** grandchild11\n** child2\n*** grandchild21\n*** grandchild22\n** child3\n*** grandchild31\n*** grandchild32\n*** grandchild33"))))))))



(ert-deftest find-forwards/empty-list ()
  (should-not (zpresent--find-forwards #'identity nil 0)))

(ert-deftest find-forwards/start-at-zero-find-match ()
  (should (equal 3
                 (zpresent--find-forwards #'identity
                                          '(nil nil nil t t nil t 3)
                                          0))))

(ert-deftest find-forwards/start-at-zero-no-match ()
  (should-not (zpresent--find-forwards #'cl-oddp
                                       '(2 8 12 -6)
                                       0)))

(ert-deftest find-forwards/start-in-list-no-match ()
  (should-not (zpresent--find-forwards #'cl-evenp
                                       '(0 2 4 7 9 11 313)
                                       3)))

(ert-deftest find-forwards/match-at-starting-point ()
  (should (equal 3
                 (zpresent--find-forwards #'cl-evenp
                                          '(0 2 4 6 8 10)
                                          3))))

(ert-deftest find-forwards/start-in-list-find-match-at-starting-point ()
  (should (equal 5
                 (zpresent--find-forwards #'cl-evenp
                                          '(2 0 -12 28 14 42 16 9 10000004)
                                          5))))

(ert-deftest find-forwards/start-in-list-find-match-after-starting-point ()
  (should (equal 6
                 (zpresent--find-forwards #'cl-evenp
                                          '(2 0 -12 28 14 41 1024 4 10000004)
                                          5))))

(ert-deftest find-forwards/start-in-list-find-match-long-after-starting-point ()
  (should (equal 13
                 (zpresent--find-forwards #'cl-evenp
                                          '(2 0 -12 27 41 1 7 -12345 1 1 1 1 1 1024 4 10000004)
                                          3))))

(ert-deftest find-forwards/no-starting-point-and-found-immediately ()
  (should (equal 0
                 (zpresent--find-forwards #'cl-evenp
                                          '(2 4 6 8)))))

(ert-deftest find-forwards/no-starting-point-and-found-eventually ()
  (should (equal 3
                 (zpresent--find-forwards #'cl-evenp
                                          '(1 3 5 6 7 9)))))

(ert-deftest find-forwards/no-starting-point-and-not-found ()
  (should-not (zpresent--find-forwards #'cl-evenp
                                       '(1 3 5 7 9))))

(ert-deftest find-forwards/starting-point-longer-than-list-not-found ()
  (should-not (zpresent--find-forwards #'identity
                                       '(0 1 2)
                                       14)))


(ert-deftest find-backwards/empty-list ()
  (should-not (zpresent--find-backwards #'identity
                                        nil
                                        0)))

(ert-deftest find-backwards/start-at-end-find-match ()
  (should (equal 3
                 (zpresent--find-backwards #'cl-evenp
                                           '(0 2 4 6 7 9 11)
                                           6))))

(ert-deftest find-backwards/start-at-end-no-match ()
  (should-not (zpresent--find-backwards #'cl-evenp
                                        '(1 3 5 7 9 11 13)
                                        6)))

(ert-deftest find-backwards/dont-match-after-starting-point ()
  (should-not (zpresent--find-backwards #'cl-evenp
                                        '(1 3 5 7 9 10 12 14 16)
                                        4)))

(ert-deftest find-backwards/match-at-starting-point ()
  (should (equal 3
                 (zpresent--find-backwards #'cl-evenp
                                           '(0 1 2 4 6)
                                           3))))

(ert-deftest find-backwards/no-starting-point-but-found ()
  (should (equal 2
                 (zpresent--find-backwards #'cl-evenp
                                           '(0 2 4 5)))))

(ert-deftest find-backwards/no-starting-point-but-not-found ()
  (should-not (zpresent--find-backwards #'cl-evenp
                                        '(1 3 5 7))))

(ert-deftest find-backwards/match-before-starting-point ()
  (should (equal 4
                 (zpresent--find-backwards #'cl-evenp
                                           '(1 3 5 7 8 9 11)
                                           5))))

(ert-deftest find-backwards/match-far-before-starting-point ()
  (should (equal 3
                 (zpresent--find-backwards #'cl-evenp
                                           '(1 3 5 6 7 9 11 13 15 17 19 21 23 25 27 29 31 33)
                                           15))))

(ert-deftest find-backwards/starting-point-beyond-end-of-list ()
  (should (equal 2
                 (zpresent--find-backwards #'cl-evenp
                                           '(1 3 4 7)
                                           104))))



(ert-deftest hash-contains/empty-hash ()
  (should-not (zpresent--hash-contains? (make-hash-table) "foo")))

(ert-deftest hash-contains/containing-hash ()
  (let ((hash (make-hash-table)))
    (puthash 'foo 'bar hash)
    (should (zpresent--hash-contains? hash 'foo))))

(ert-deftest hash-contains/not-containing-hash ()
  (let ((hash (make-hash-table)))
    (puthash 'foo 'bar hash)
    (should-not (zpresent--hash-contains? hash 'nope))))



(ert-deftest make-keyword/returns-proper-keyword ()
  (should (equal (zpresent--make-keyword "pants")
                 :pants)))



;;; tests.el ends here
