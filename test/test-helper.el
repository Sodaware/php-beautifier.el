;; Load mock libraries
(require 'el-mock)
(eval-when-compile
  (require 'cl))

;; Set directory for the module
(setq module-directory
      (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(add-to-list 'load-path module-directory)

(require 'php-beautifier)
