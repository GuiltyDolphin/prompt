;;; prompt-tests.el
;;; Code:

(require 'ert)
(require 'prompt)

(ert-deftest prompt-test-make-prompt ()
  "Tests for `prompt--make-prompt'."
  (let ((cases '(("Name?" ("?") nil "Name?")
                 ("Name?" ("?") "Jeff" "Name (default Jeff)?")
                 ("Name:" ("?") "Jeff" "Name: (default Jeff)")
                 ("Name:" ("?" ":") "Jeff" "Name (default Jeff):")
                 ("Name:" nil "Jeff" "Name: (default Jeff)")
                 ("Name:  " nil "Jeff" "Name: (default Jeff)")
                 ("Name:  " nil nil "Name:"))))
    (dolist (case cases)
      (-let (((prompt suffixes default expected) case))
        (should (equal expected (prompt--make-prompt prompt suffixes default)))))))

(provide 'prompt-tests)
;;; prompt-tests.el ends here
