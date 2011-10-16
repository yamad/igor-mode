;; Automatic tests for igor-mode functions (using ert)

;; Tests
(ert-deftest append-to-alist-test ()
  (should
   (equal
    (igor-append-to-alist
     (cons 1 2)
     (list (cons 1 3) (cons 4 5)))
    '(1 2 3)))
  (should
   (equal
    (igor-append-to-alist
     (cons "if" "elseif")
     (list (list "if" "endif" "else")
           (cons "try" "catch")))
    '("if" "elseif" "endif" "else"))))

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
