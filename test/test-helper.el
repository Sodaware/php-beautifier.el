;; Add library path
(add-to-list 'load-path
             (file-name-directory (directory-file-name (file-name-directory load-file-name))))

(require 'php-beautifier)
