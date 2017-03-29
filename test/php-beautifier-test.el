;;; php-beautifier-test.el --- Tests for php-beautifier.el

;; Execution tests

;; Test that the entire buffer is sent (will be point 1 => 1)
(ert-deftest php-beautifier-tests/sends-entire-buffer-when-empty ()
  (with-mock
   (mock (shell-command-on-region 1 1 * * t * t) => 0)
   (php-beautifier-format-buffer)))

;; Test that the entire buffer is sent when there is content. Inserts 10 chars
;; into the scratch buffer and checks it is sent.
(ert-deftest php-beautifier-tests/sends-entire-buffer-when-full ()
  (with-mock
   (switch-to-buffer "*scratch*")
   (insert "1234567890")
   (mock (shell-command-on-region 1 11 * * t * t) => 0)
   (php-beautifier-format-buffer)))


;; phpcbf standard checks

(ert-deftest php-beautifier-test/can-fetch-all-phpcbf-standards ()
  (with-mock
   (stub php-beautifier--phpcbf-fetch-standards => "The installed coding standards are MySource, PEAR and PHPCS\n")
   (let ((result (php-beautifier-phpcbf-standards)))
     (should (listp result)))))

(ert-deftest php-beautifier-test/can-parse-valid-phpcbf-standards-list ()
  (let ((standards "The installed coding standards are MySource, PEAR and PHPCS\n"))
    (should (listp (php-beautifier--phpcbf-parse-standards standards)))))

(ert-deftest php-beautifier-test/valid-standard-returns-t-when-valid ()
  (with-mock
   (stub php-beautifier--phpcbf-fetch-standards => "The installed coding standards are MySource, PEAR and PHPCS\n")
   (should (php-beautifier-phpcbf-valid-standard-p "MySource"))
   (should (php-beautifier-phpcbf-valid-standard-p "PEAR"))
   (should (php-beautifier-phpcbf-valid-standard-p "PHPCS"))))

(ert-deftest php-beautifier-test/valid-standard-returns-nil-when-invalid ()
  (with-mock
   (stub php-beautifier--phpcbf-fetch-standards => "The installed coding standards are MySource, PEAR and PHPCS\n")
   (should-not (php-beautifier-phpcbf-valid-standard-p "SomeOtherStandard"))))

(ert-deftest php-beautifier-test/valid-standard-returns-nil-when-empty-or-nil ()
  (with-mock
   (stub php-beautifier--phpcbf-fetch-standards => (error "Should not reach this far"))
   (should-not (php-beautifier-phpcbf-valid-standard-p nil))
   (should-not (php-beautifier-phpcbf-valid-standard-p ""))))


;; Indentation method parameter checks

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
