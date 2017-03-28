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
