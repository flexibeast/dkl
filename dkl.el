;;; dkl.el --- Display keyboard layout.

;; Copyright (C) 2016  Alexis <flexibeast@gmail.com>

;; Author: Alexis <flexibeast@gmail.com>
;; Maintainer: Alexis <flexibeast@gmail.com>
;; Created: 2016-09-29
;; URL: https://github.com/flexibeast/dkl
;; Keywords: input, keyboard, layout

;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Commentary:

;; `dkl' provides an ASCII-art representation of a keyboard layout, within an Emacs buffer.

;; ## Table of Contents

;; - [Installation](#installation)
;; - [Usage](#usage)
;; - [Layout file format](#layout)
;; - [TODO](#todo)
;; - [Issues](#issues)
;; - [License](#license)

;; ## Installation

;; Install [dkl from MELPA](http://melpa.org/#/ewmctrl), or put the `dkl' directory in your load-path and do a `(require 'display-keyboard-layout)'.

;; ## Usage

;; Create an `dkl-layout' buffer with `M-x dkl-display'.

;; Within the `dkl-layout' buffer, the default keybindings are:

;; * s - Toggle display of shifted and unshifted layouts.

;; * q - Close the `dkl-layout' buffer and window.

;; Customisation options, including how `dkl' highlights typed keys, are available via the `dkl' customize-group.

;; <a name="layout"></a>

;; ## Layout file format

;; A layout file contains Emacs Lisp which ensures the layout is used with the correct keyboard, followed by the setting of the `dkl--current-layout` variable:

;; ```elisp
;; (if (not (string= dkl-keyboard-name "standard"))
;;     (user-error "Layout `qwerty-us' must be used with `dkl-keyboard-name' set to \"standard\"")
;;   (setq dkl--current-layout
;;         ;; Top row
;;         '((57 . ("`" "~"))
;;         ...
;; ```

;; The layout data structure consists of an alist, where the `car` of each entry indicates a character position in the relevant keyboard file, and the `cdr` contains a list of the unshifted and shifted glyphs to display at that position in a `*dkl-layout*` buffer.

;; ## TODO

;; * `devanagari-inscript` layout:

;;   * Fix failure to highlight certain keys during composition.

;; ## Issues / bugs

;; If you discover an issue or bug in `dkl' not already noted:

;; * as a TODO item, or

;; * in [the project's "Issues" section on GitHub](https://github.com/flexibeast/dkl/issues),

;; please create a new issue with as much detail as possible, including:

;; * which version of Emacs you're running on which operating system, and

;; * how you installed `dkl'.

;; ## License

;; [GNU General Public License version 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any later version.

;;; Code:


(require 'cl-lib)

(defvar dkl-mode-map (make-sparse-keymap)
  "Keymap for `dkl'.")

(define-derived-mode dkl-mode special-mode "dkl"
  "Major mode for displaying keyboard layout."
  (read-only-mode)
  (define-key dkl-mode-map (kbd "q") 'dkl-close)
  (define-key dkl-mode-map (kbd "s") 'dkl-shift-toggle))


;;
;; Internal variables.
;;

(defconst dkl--elisp-dir (file-name-directory load-file-name)
  "Absolute path of the directory containing dkl.el.")

(defvar dkl--current-layout '()
  "The keyboard layout currently in effect.

The value of this variable is initialised by `dkl--refresh'.")

(defvar dkl--reverse-current-layout-shifted '()
  "An alist mapping shifted glyphs to keyboard layout positions.

Derived from the value of `dkl--current-layout' by `dkl--refresh'.")

(defvar dkl--reverse-current-layout-unshifted '()
  "An alist mapping unshifted glyphs to keyboard layout positions.

Derived from the value of `dkl--current-layout' by `dkl--refresh'.")

(defvar dkl--shifted nil
  "Whether or not shifted layout should be displayed.")


;;
;; Customisable variables.
;;

(defgroup dkl nil
  "Display a keyboard layout."
  :group 'convenience)

(defcustom dkl-highlight-duration 0.5
  "Number of seconds to highlight a typed key for."
  :type 'number
  :group 'dkl)

(defcustom dkl-keyboard-name "standard"
  "Keyboard to use, as named in dkl's `keyboards' directory."
  :type `(radio ,@(mapcar #'(lambda (entry)
                              `(const :tag ,entry ,entry))
                          (remove ".." (remove "." (directory-files (concat dkl--elisp-dir "keyboards/"))))))
  :group 'dkl)

(defcustom dkl-layout-name "qwerty-us"
  "Keyboard layout to use, as named in dkl's `layouts' directory."
  :type `(radio ,@(mapcar #'(lambda (entry)
                              `(const :tag ,entry ,entry))
                          (remove ".." (remove "." (directory-files (concat dkl--elisp-dir "layouts/"))))))
  :group 'dkl)

(defface dkl-glyph-highlight-face
  '((((background light)) :foreground "green"))
  "Face to use for highlighting typed glyphs."
  :group 'dkl)


;;
;; Internal functions.
;;

(defun dkl--highlight-typed-glyph ()
  "Highlight the last-typed glyph in the *dkl-layout* buffer."
  (let ((glyph (key-description (this-command-keys-vector)))
        (match nil))
    (if (and (not dkl--shifted)
             (assoc glyph dkl--reverse-current-layout-unshifted))
        (setq match 'unshifted))
    (if (and dkl--shifted
             (assoc glyph dkl--reverse-current-layout-shifted))
        (setq match 'shifted))
    (if match
        (let* ((pos (if (eq match 'unshifted)
                        (cdr (assoc glyph dkl--reverse-current-layout-unshifted))
                      (cdr (assoc glyph dkl--reverse-current-layout-shifted)))))
          (with-current-buffer "*dkl-layout*"
            (let ((inhibit-read-only t))
              (add-text-properties (- pos 1) (+ pos 2) (list 'face 'dkl-glyph-highlight-face))
              (sit-for dkl-highlight-duration)
              (remove-text-properties (- pos 1) (+ pos 2) (list 'face))))))))

(defun dkl--refresh ()
  "Refresh contents of *dkl-layout* buffer."
  (let ((bfr (get-buffer-create "*dkl-layout*"))
        (inhibit-read-only t))
    (load (concat dkl--elisp-dir "layouts/" dkl-layout-name))
    (setq dkl--reverse-current-layout-unshifted
          (append '()
                  (mapcar (lambda (entry)
                            (cons (cadr entry) (car entry)))
                          dkl--current-layout)))
    (setq dkl--reverse-current-layout-shifted
          (append '()
                  (mapcar (lambda (entry)
                            (cons (cl-caddr entry) (car entry)))
                          dkl--current-layout)))
    (with-current-buffer "*dkl-layout*"
      (erase-buffer)
      (insert-file-contents-literally (concat dkl--elisp-dir "keyboards/" dkl-keyboard-name))
      (dolist (entry dkl--current-layout)
        (let ((glyph (if (not dkl--shifted)
                         (cadr entry)
                       (cl-caddr entry))))
          (goto-char (car entry))
          (delete-char 1)
          (insert glyph)))
      (dkl-mode))
    (if (get-buffer-window "*dkl-layout*")
        (with-selected-window (get-buffer-window "*dkl-layout*")
          (fit-window-to-buffer))
      (progn
        (switch-to-buffer "*dkl-layout*")
        (fit-window-to-buffer)))))

(defun dkl--remove-hooks ()
  "Remove dkl-related functions from various hooks."
  (if (string= (buffer-name) "*dkl-layout*")
      (progn
        (remove-hook 'post-self-insert-hook 'dkl--highlight-typed-glyph)
        (remove-hook 'kill-buffer-hook 'dkl--remove-hooks))))


;;
;; User-facing functions.
;;

(defun dkl-change-current-layout (layout)
  "Change the layout displayed for this session."
  (interactive
   (list
    (completing-read "Layout: "
                     (remove ".."(remove "." (directory-files (concat dkl--elisp-dir "layouts/")))))))
  (setq dkl-layout-name layout)
  (if (get-buffer "*dkl-layout*")
      (with-current-buffer "*dkl-layout*"
        (dkl--refresh))))

(defun dkl-close ()
  "Close the *dkl-layout* buffer."
  (interactive)
  (if (kill-buffer "*dkl-layout*")
      (unless (one-window-p)
        (delete-window)))
  (dkl--remove-hooks))

;;;###autoload
(defun dkl-display ()
  "Create and populate a new *dkl-layout* buffer."
  (interactive)
  (save-selected-window
    (select-window (split-window-vertically))
    (dkl--refresh))
  (add-hook 'post-self-insert-hook 'dkl--highlight-typed-glyph)
  (add-hook 'kill-buffer-hook 'dkl--remove-hooks))

(defun dkl-shift-toggle ()
  "Toggle display of shifted layout."
  (interactive)
  (setq dkl--shifted (not dkl--shifted))
  (dkl--refresh))


;; --

(provide 'dkl)

;;; dkl.el ends here
