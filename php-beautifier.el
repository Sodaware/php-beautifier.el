;;; php-beautifier.el --- Use PHP_Beautifier to format an Emacs buffer or region

;; Copyright (C) 2017 Phil Newton <phil@sodaware.net>

;; Author: Phil Newton<phil@sodaware.net>
;; Version: 0.2.0
;; URL: https://github.com/sodaware/php-beautifier.el
;; Keywords: php beautifier

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Integrates `PHP_Beautifier` with Emacs and allows it to be called on a buffer
;; or a selected region.  PHP_Beautifier must be installed for this extension to
;; work - see https://pear.php.net/package/PHP_Beautifier/

;; Once PHP_Beautifier is installed you may need to configure the executable
;; path.  The indentation style can also be switched between `tabs` and
;; `spaces`.

;; The customisation options are:

;;   - `php-beautifier-executable-path` - The full path to the `PHP_Beautifier`
;;      executable.  This is `php_beautifier` by default.

;;   - `php-beautifier-indent-method` - The indentation method to use.  Either
;;      "spaces" or "tabs".  The default value is "spaces".

;; As well as `PHP_Beautifier`, `phpcbf` is also supported.  `phpcbf` is a tool
;; for fixing coding standard errors in PHP code.  If `phpcbf` is installed and
;; configured the beautified will be passed through it before insertion.  By
;; default it is disabled.

;; `phpcbf` is part of PHP_CodeSniffer:
;; https://github.com/squizlabs/PHP_CodeSniffer

;; The customisation options for `phpcbf` are:

;;   - `php-beautifier-phpcbf-path` - The full path to the `phpcbf` executable.
;;     This is `phpcbf` by default.

;;   - `php-beautifier-phpcbf` - The coding standard to use with `phpcbf`.  Must
;;      be a valid and installed standard.  The default value is nil, which means
;;      phpcbf will not be run.

;; Parts of this extension are based on `web-beautify`
;; see: https://github.com/yasuyk/web-beautify

;;; Code:

;; Configuration

(defgroup php-beautifier nil
  "Emacs interface for PHP_Beautifier"
  :group 'convenience
  :prefix "php-beautifier-")

(defcustom php-beautifier-executable-path "php_beautifier"
  "The full path to the PHP_Beautifier executable."
  :group 'php-beautifier
  :type '(file))

(defcustom php-beautifier-indent-method "spaces"
  "The full path to the PHP_Beautifier executable."
  :group 'php-beautifier
  :options '("tabs" "spaces")
  :type '(string))

(defcustom php-beautifier-phpcbf-path "phpcbf"
  "The full path to the `phpcbf` executable."
  :group 'php-beautifier
  :type '(file))

(defcustom php-beautifier-phpcbf-standard nil
  "The coding standard to use when calling phpcbf."
  :group 'php-beautifier
  :type '(string))


;; PHPCBF integration

(defun php-beautifier-phpcbf-installed-p ()
  "Check if phpcbf is installed and configured correctly."
  (executable-find php-beautifier-phpcbf-path))

(defun php-beautifier-phpcbf-can-use-p ()
  "Check if phpcbf is installed and a valid standard is set."
  (and (php-beautifier-phpcbf-installed-p)
       (php-beautifier-phpcbf-valid-standard-p php-beautifier-phpcbf-standard)))

(defun php-beautifier--create-phpcbf-shell-command ()
  "Create the shell command to call phpcbf."
  (format "%s --standard=%s"
          php-beautifier-phpcbf-path
          php-beautifier-phpcbf-standard))

;; PHPCBF standards helpers

(defun php-beautifier-phpcbf-valid-standard-p (standard-name)
  "Check STANDARD-NAME is registered with phpcbf."
  (when (> (length standard-name) 0)
    (member standard-name (php-beautifier-phpcbf-standards))))

(defun php-beautifier-phpcbf-standards ()
  "Fetch a list of all standards registered with phpcbf."
  (php-beautifier--phpcbf-parse-standards (php-beautifier--phpcbf-fetch-standards)))

(defun php-beautifier--phpcbf-fetch-standards ()
  "Call the phpcbf executable and return the standards it lists."
  (shell-command-to-string
   (format "%s -i" php-beautifier-phpcbf-path)))

(defun php-beautifier--phpcbf-parse-standards (standards)
  "Parse a list of STANDARDS and return as a list of names."
  ;; TODO: Remove the magic number `35` here - it's the length of:
  ;; "The installed coding standards are MySource, PEAR and PHPCS\n"
  ;; which isn't useful for none-English installs.
  ;; Should probably remove the `and` as well.
  (mapcar 'php-beautifier--trim-standard-name
          (delete "and" (split-string (substring standards 35)))))

(defun php-beautifier--trim-standard-name (standard)
  "Trim trailing spaces and commas from STANDARD name."
  (if (string-match "[\ ,]*$" standard)
      (replace-match "" nil nil standard)
      standard))

;; Code formatting functions

(defun php-beautifier--create-shell-command ()
  "Create the shell command to call PHP_Beautifier."
  (format "%s %s%s"
          php-beautifier-executable-path
          (if (string= "spaces" php-beautifier-indent-method)
              "--indent_spaces"
              "--indent_tabs")
          (if (php-beautifier-phpcbf-can-use-p)
              (format " | %s" (php-beautifier--create-phpcbf-shell-command))
              "")))

(defun php-beautifier--exec (input-buffer start-point end-point output-buffer)
  "Execute the beautifier on a region.

Call the beautifier backend on INPUT-BUFFER between START-POINT and END-POINT
points and place the result into OUTPUT-BUFFER.

Returns `t` if the process executed correctly or `NIL` if it failed."
  (let ((result (shell-command-on-region
                 start-point end-point
                 (php-beautifier--create-shell-command)
                 input-buffer t output-buffer t)))
    (if (php-beautifier-phpcbf-can-use-p)
        (= 1 result)
        (zerop result))))

(defun php-beautifier--format-region (start end)
  "Replace a region from START to END with content formatted by PHP_Beautifier.

Note that the region must start with `<?php` in order for the beautifier to
recognize it as PHP code."
  (let* ((output-buffer-name "*PHP_Beautifier error messages*")
         (output-buffer (get-buffer-create output-buffer-name))
         (previous-point (point))
         (previous-window-start (window-start)))
    (if (php-beautifier--exec (current-buffer) start end output-buffer)
        (php-beautifier--on-success previous-point
                                    previous-window-start
                                    output-buffer)
        (php-beautifier--on-failure))))

(defun php-beautifier--on-success (previous-point previous-window-start output-buffer)
  "Called after PHP_Beautifier has run successfully.

Moves the point back to PREVIOUS-POINT, moves the window start
back to PREVIOUS-WINDOW-START and closes OUTPUT-BUFFER."
  (goto-char previous-point)
  (set-window-start nil previous-window-start)
  (message "Region formatted")
  (kill-buffer output-buffer))

(defun php-beautifier--on-failure ()
  "Called after PHP_Beautifier has been run but failed."
  (undo)
  (error "PHP Beautification failed"))


;; Interactive functions

;;;###autoload
(defun php-beautifier-format-region ()
  "Reformat the current region using PHP_Beautifier."
  (interactive)
  (php-beautifier--format-region (region-beginning) (region-end)))

;;;###autoload
(defun php-beautifier-format-buffer ()
  "Reformat the current buffer using PHP_Beautifier."
  (interactive)
  (mark-whole-buffer)
  (php-beautifier--format-region (point-min) (point-max)))

(provide 'php-beautifier)
;;; php-beautifier.el ends here
