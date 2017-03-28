;;; php-beautifier.el --- Use PHP_Beautifier to format an Emacs buffer or region

;; Copyright (C) 2017 Phil Newton <phil@sodaware.net>

;; Author: Phil Newton<phil@sodaware.net>
;; Version: 0.1.0
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

;; Integrates `PHP_Beautifier` with Emacs, allowing it to be called on a buffer
;; or a selected region.

;; To install PHP_Beautifier visit https://pear.php.net/package/PHP_Beautifier/

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


;; Code formatting functions

(defun php-beautifier--create-shell-command ()
  "Create the shell command to call PHP_Beautifier."
  (format "%s %s"
          php-beautifier-executable-path
          (if (string= "spaces" php-beautifier-indent-method)
              "--indent_spaces"
              "--indent_tabs")))

(defun php-beautifier--format-region (start end)
  "Replace a region from START to END with content formatted by PHP_Beautifier.

Note that the region must start with `<?php` in order for the beautifier to
recognize it as PHP code."
  (let* ((output-buffer-name "*PHP_Beautifier error messages*")
         (output-buffer (get-buffer-create output-buffer-name))
         (previous-point (point))
         (previous-window-start (window-start))
         (shell-command (php-beautifier--create-shell-command)))
    (if (zerop (shell-command-on-region start end shell-command (current-buffer) t output-buffer t))
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
