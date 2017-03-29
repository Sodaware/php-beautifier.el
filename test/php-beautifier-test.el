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

(ert-deftest php-beautifier-tests/exec-checks-for-0-result-when-no-phpcbf ()
  ;; CBF not used but 1 returned - should return nil
  (with-mock
   (mock (shell-command-on-region 1 1 * * t * t) => 1)
   (stub php-beautifier-phpcbf-can-use-p => nil)
   (should-not (php-beautifier--exec nil 1 1 nil)))
  ;; CBF not used and 0 returned - should return true
  (with-mock
   (mock (shell-command-on-region 1 1 * * t * t) => 0)
   (stub php-beautifier-phpcbf-can-use-p => nil)
   (should (php-beautifier--exec nil 1 1 nil))))

(ert-deftest php-beautifier-tests/exec-checks-for-1-result-when-using-phpcbf ()
  ;; CBF used but 0 returned - should return nil
  (with-mock
   (mock (shell-command-on-region 1 1 * * t * t) => 0)
   (stub php-beautifier-phpcbf-can-use-p => t)
   (should-not (php-beautifier--exec nil 1 1 nil)))
  ;; CBF used and 1 returned - should return true
  (with-mock
   (mock (shell-command-on-region 1 1 * * t * t) => 1)
   (stub php-beautifier-phpcbf-can-use-p => t)
   (should (php-beautifier--exec nil 1 1 nil))))


;; phpcbf integration

(ert-deftest php-beautifier-test/phpcbf-installed-p-returns-t-when-installed ()
  (with-mock
   (stub executable-find => "/path/to/phpcbf")
   (should (php-beautifier-phpcbf-installed-p))))

(ert-deftest php-beautifier-test/phpcbf-installed-p-returns-nil-when-not-installed ()
  (with-mock
   (stub executable-find => nil)
   (should-not (php-beautifier-phpcbf-installed-p))))

(ert-deftest php-beautifier-test/phpcbf-can-use-p-returns-t-when-installed-and-valid-standard ()
  (with-mock
   (stub executable-find => "/path/to/phpcbf")
   (stub php-beautifier--phpcbf-fetch-standards => "The installed coding standards are MySource, PEAR and PHPCS\n")
   (let ((php-beautifier-phpcbf-standard "MySource"))
     (should (php-beautifier-phpcbf-can-use-p)))))

(ert-deftest php-beautifier-test/phpcbf-can-use-p-returns-nil-when-no-phpcbf ()
  (with-mock
   (stub executable-find => nil)
   (stub php-beautifier-phpcbf-valid-standard-p => t)
   (should-not (php-beautifier-phpcbf-can-use-p))))

(ert-deftest php-beautifier-test/phpcbf-can-use-p-returns-nil-when-no-valid-standard ()
  (with-mock
   (stub executable-find => "/path/to/phpcbf")
   (stub php-beautifier-phpcbf-valid-standard-p => nil)
   (should-not (php-beautifier-phpcbf-can-use-p))))

(ert-deftest php-beautifier-test/can-creates-phpcbf-command-line ()
  (let ((php-beautifier-phpcbf-path "/path/to/phpcbf")
        (php-beautifier-phpcbf-standard "Testing"))
    (should (string= "/path/to/phpcbf --standard=Testing"
                     (php-beautifier--create-phpcbf-shell-command)))))

(ert-deftest php-beautifier-test/adds-phpcbf-command-line-when-setup ()
  (with-mock
   (stub executable-find => "/path/to/phpcbf")
   (stub php-beautifier-phpcbf-standards => (list "Testing"))
   (let ((php-beautifier-executable-path "/path/to/php_beautifier")
         (php-beautifier-phpcbf-path "/path/to/phpcbf")
         (php-beautifier-phpcbf-standard "Testing"))
     (should (string= "/path/to/php_beautifier --indent_spaces | /path/to/phpcbf --standard=Testing"
                      (php-beautifier--create-shell-command))))))

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
