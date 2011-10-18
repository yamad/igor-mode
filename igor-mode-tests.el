;; Automatic tests for igor-mode functions (using ert)
(require 'igor-mode)

;; Tests
(ert-deftest append-to-pair-test ()
  (should
   (equal
    (igor-append-to-pair
     '(1 . 2)
     '((1 . 3) (4 . 5)))
    '(1 3 2)))
  (should
   (equal
    (igor-append-to-pair
     '("if" . "elseif")
     '(("if" "endif" "else")
       ("try" "catch")))
    '("if" "endif" "else" "elseif")))
  (should
   (equal
    (igor-append-to-pair
     '("if" "elseif")
     '(("if" . "endif")
       ("other" . "gone")))
    '("if" "endif" "elseif")))
  (should
   (equal
    (igor-append-to-pair
     '("if" "elseif")
     '(("notif" "dont add")
       ("alsonotif" "dont add")))
    '("if" "elseif"))))

(ert-deftest append-to-alist-test ()
  (should
   (equal
    (igor-append-to-alist
     '(("if" "elseif")
      ("try" "catch"))
     '(("if" "endif")
       ("other" "gone")) t)
    '(("if" "endif" "elseif")
      ("try" "catch"))))
  (should
   (equal
    (igor-append-to-alist
     '((1 . 2) (2 . 2))
     '((1 . 3) (1 . 4) (2 . 4) (3 . 5) (3 . 6)))
    '((1 3 4 2) (2 4 2) (3 5 6))))
  (should
   (equal
    (igor-append-to-alist
     '((1 . 2) (2 . 2))
     '((1 . 3) (1 . 4) (2 . 4) (3 . 5) (3 . 6))
     t)
    '((1 3 4 2) (2 4 2))))
  (should
   (equal
    (igor-append-to-alist
     '((1 . 2) (2 . 2) (4 . 7) (4 . 8))
     '((1 . 3) (1 . 4) (2 . 4) (3 . 5) (3 . 6))
     t)
    '((1 3 4 2) (2 4 2) (4 7) (4 8))))
  (should
   (equal
    (igor-append-to-alist
     (igor-compress-alist-keys
      '((1 . 2) (2 . 2) (4 . 7) (4 . 8)))
     '((1 . 3) (1 . 4) (2 . 4) (3 . 5) (3 . 6))
     t)
    '((1 3 4 2) (2 4 2) (4 7 8))))
)

(ert-deftest convert-to-list-test ()
  (should
   (equal
    (igor-convert-to-list
     '(1 2 3))
    '(1 2 3)))
  (should
   (equal
    (igor-convert-to-list
     '(1 2 . 3))
    '(1 2 3)))
  (should
   (equal
    (igor-convert-to-list
     "NotAList")
    '("NotAList")))
  (should
   (equal
    (igor-convert-to-list 2)
    '(2)))
  (should
   (equal
    (igor-convert-to-list '(2))
    '(2 . nil)))
)

(ert-deftest alist-all-assoc ()
  (should
   (equal
    (igor-alist-all-assoc
     1
     '((2 . 2) (1 . 3) (3 . 1) (5 . 1) (1 . 4)))
    '((1 . 3) (1 . 4)))))

(ert-deftest compress-alist-key ()
  (should
   (equal
    (igor-compress-alist-key
     1
     '((1 2) (1 3) (5 6)))
    '(1 2 3)))
  (should
   (equal
    (igor-compress-alist-key
     1
     '((1 . 2) (1 . 3) (5 . 6)))
    '(1 2 3))))

(ert-deftest compress-alist-keys ()
  (should
   (equal
    (igor-compress-alist-keys
     '((1 2) (5 6) (1 3) (2 4) (5 7)))
    '((1 2 3) (5 6 7) (2 4)))))

(ert-deftest test-cons-list ()
  "Test to show the difference between a cons cell and a list.

A 'proper' list always has a last element that points to nil. A
cons cell does not automatically have a final nil and so is only
a list if the nil is explicitly added. A cons cell without a
final nil is indicated by a dotted notation. This is a feature of
lisp."
  (should
   (equal
    (cons 1 (cons 2 nil))
    (list 1 2))))


(provide 'igor-mode-tests)
;;; igor-mode-tests.el ends here
