(require 'org-site)

(ert-deftest test1 ()
  (should (= (+ 1 2) 3)))

(ert-deftest org-mode-version-test ()
  (should (version<= "8.2.0" org-version)))
