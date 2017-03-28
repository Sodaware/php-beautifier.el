;;; php-beautifier-test.el --- Tests for php-beautifier.el

;; Indentation method checks

(ert-deftest php-beautifier-test/adds-space-indent-method-correctly ()
  (let ((php-beautifier-indent-method "spaces"))
    (should (string= "php_beautifier --indent_spaces"
                     (php-beautifier--create-shell-command)))))

(ert-deftest php-beautifier-test/adds-tab-indent-method-correctly ()
  (let ((php-beautifier-indent-method "tabs"))
    (should (string= "php_beautifier --indent_tabs"
                     (php-beautifier--create-shell-command)))))

(ert-deftest php-beautifier-test/defaults-to-tab-indentation-method()
  (let ((php-beautifier-indent-method nil))
    (should (string= "php_beautifier --indent_tabs"
                     (php-beautifier--create-shell-command)))))

;;; php-beautifier-test.el ends here
