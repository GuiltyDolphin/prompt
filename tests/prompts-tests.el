;;; prompts-tests.el
;;; Code:

(require 'ert)
(require 'prompts)

(ert-deftest prompts-test-make-prompt ()
  "Tests for `prompts--make-prompt'."
  (let ((cases '(("Name?" ("?") nil "Name? ")
                 ("Name?" ("?") "Jeff" "Name (default Jeff)? ")
                 ("Name:" ("?") "Jeff" "Name: (default Jeff) ")
                 ("Name:" ("?" ":") "Jeff" "Name (default Jeff): ")
                 ("Name:" nil "Jeff" "Name: (default Jeff) ")
                 ("Name:  " nil "Jeff" "Name: (default Jeff) ")
                 ("Name:  " nil nil "Name: "))))
    (dolist (case cases)
      (-let (((prompt suffixes default expected) case))
        (should (equal expected (prompts--make-prompt prompt suffixes default)))))))

(provide 'prompts-tests)
;;; prompts-tests.el ends here
