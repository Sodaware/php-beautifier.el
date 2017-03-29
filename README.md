# php-beautifier.el

*php-beautifier.el* is a lightweight extension that integrates PHP source code
beautification into Emacs using the
[PHP_Beautifier](https://pear.php.net/package/PHP_Beautifier/) tool.


## Installation

All Emacs installs are a little different, but the basic outline is this:

  - Download the source code and put it somewhere Emacs can find it (probably
    `~/.emacs.d/`)
  - Add that directory to your `load-path` if it's not yet there: `(add-to-list
    'load-path "/path/to/dir")`
  - Add `(require 'php-beautifier)` somewhere in your `~/.emacs.d/init.el`


## Configuration

*php-beautifier* supplies the following customisation options:

  - `php-beautifier-executable-path` - The full path to the `PHP_Beautifier`
    executable. This is `php_beautifier` by default.
  - `php-beautifier-indent-method` - The indentation method to use. Either
    `spaces` or `tabs`. Default is `spaces`.

An example configuration with a full path and tab indentation:

```emacs-lisp
(setq php-beautifier-executable-path "/usr/bin/php_beautifier")
(setq php-beautifier-indent-method "tabs")
```


## Usage

*php-beautifier* can either format an entire buffer or a single region. The
`php_beautifier` tool will only format code contained in a `<?php` block, so
formatting a region that doesn't contain one will not work.

**Interactive Commands**

  - `php-beautifier-format-region` - Format the current region and replace it
    with formatted code.
  - `php-beautifier-format-buffer` - Format the current buffer and replace its
    contents with formatted code.


## Running Tests

To run unit tests you'll need [Cask](https://github.com/cask/cask) installed.

The following command will run all tests:

```bash
cask exec ert-runner
```


## Licence

Released under the GPLv3. See `COPYING` for the full licence.
