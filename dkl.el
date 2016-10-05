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

;; <img src="screencap.png">

;; ## Table of Contents

;; - [Installation](#installation)
;; - [Usage](#usage)
;; - [Layout file format](#layout)
;; - [TODO](#todo)
;; - [Issues](#issues)
;; - [License](#license)

;; ## Installation

;; Install [dkl from MELPA](https://melpa.org/#/dkl), or put the `dkl' directory in your load-path and do a `(require 'dkl)'.

;; ## Usage

;; Create an `dkl-layout' buffer with `M-x dkl-display'.

;; Within the `dkl-layout' buffer, the default keybindings are:

;; * l - Set the layout to use (`dkl-set-current-layout`).

;; * q - Close the `dkl-layout' buffer and window (`dkl-close`).

;; * s - Toggle display of shifted and unshifted layouts (`dkl-shift-toggle`).

;; Customisation options, including how `dkl' highlights typed keys, are available via the `dkl' customize-group.

;; <a name="layout"></a>

;; ## Layout file format

;; A layout file contains Emacs Lisp which:

;; * ensures the layout is used with the correct keyboard;

;; * specifies the directionality of the script used in the layout; and

;; * sets the `dkl--current-layout` variable.

;; For example:

;; ```elisp
;; (if (not (string= dkl-keyboard-name "standard"))
;;     (user-error "Layout `qwerty-us' must be used with `dkl-keyboard-name' set to \"standard\"")
;;   (progn
;;     (setq dkl--current-layout-script-direction 'left-to-right)
;;     (setq dkl--current-layout
;;           '(;; Top row
;;             (60 . ((0 . ("`" "~"))
;;                    (4 . ("1" "!"))
;;             ...
;; ```

;; The layout data structure is an alist. Each entry in the alist represents a keyboard row:

;; * The `car` of the entry indicates the character position for the first glyph in that row.

;; * The `cdr` of the entry is itself an alist, where:

;;   * the `car` of each entry is an offset, in characters, from the first glyph in that row;

;;   * the `cdr` of each entry is a list of the unshifted and shifted glyphs to display.

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


(defvar dkl-mode-map (make-sparse-keymap)
  "Keymap for `dkl'.")

(define-derived-mode dkl-mode special-mode "dkl"
  "Major mode for displaying keyboard layout."
  (read-only-mode)
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local cursor-type nil)
  (define-key dkl-mode-map (kbd "l") 'dkl-set-current-layout)
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

(defvar dkl--current-layout-script-direction 'left-to-right
  "The directionality of the script in the current layout.

Possible values are the symbols `left-to-right' or `right-to-left'.")

(defvar dkl--position-adjustment 0
  "Number of characters by which to adjust glyph positions.

The value of this variable will be 0 for left-to-right scripts,
and a positive number for right-to-left scripts. The latter
is used to ensure that the displayed layout is flush-right in
the *dkl-layout* window.")

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

(defun dkl--get-keyboard-width ()
  "Return width, in characters, of current keyboard."
  (with-temp-buffer
    (insert-file-contents (concat dkl--elisp-dir "keyboards/" dkl-keyboard-name))
    (goto-char (point-min))
    (let ((width (- (line-end-position) (line-beginning-position))))
      (while (not (eobp))
        (progn
          (forward-line)
          (let ((current-width (- (line-end-position) (line-beginning-position))))
            (if (> current-width width)
                (setq width current-width)))))
      width)))

(defun dkl--highlight-typed-glyph ()
  "Highlight the last-typed glyph in the *dkl-layout* buffer."
  (let ((glyph (key-description (this-command-keys-vector))))
    (with-current-buffer "*dkl-layout*"
      (goto-char (point-min))
      (if (search-forward glyph (point-max) t)
          (let ((pos (match-beginning 0))
                (inhibit-read-only t))
            (add-text-properties (- pos 1) (+ pos 2) (list 'face 'dkl-glyph-highlight-face))
            (sit-for dkl-highlight-duration)
            (remove-text-properties (- pos 1) (+ pos 2) (list 'face)))))))

(defun dkl--refresh ()
  "Refresh contents of *dkl-layout* buffer."
  (let ((bfr (get-buffer-create "*dkl-layout*"))
        (inhibit-read-only t))
    (load (concat dkl--elisp-dir "layouts/" dkl-layout-name))
    (with-current-buffer "*dkl-layout*"
      (erase-buffer)
      (insert-file-contents (concat dkl--elisp-dir "keyboards/" dkl-keyboard-name))
      (if (eq dkl--current-layout-script-direction 'right-to-left)
          (setq dkl--position-adjustment (1+ (- (window-width) (dkl--get-keyboard-width))))
        (setq dkl--position-adjustment 0))
      (set-left-margin (point-min) (point-max) dkl--position-adjustment)
      (let ((row-count 1))
        (dolist (layout-entry dkl--current-layout)
          (let ((row (cdr (assoc (car layout-entry) dkl--current-layout)))
                (row-position (car layout-entry)))
            (dolist (row-entry row)
              (let* ((glyph (if (not dkl--shifted)
                                (cadr row-entry)
                              (car (cddr row-entry))))
                     (position (car row-entry))
                     (adjusted-position (+ (* (* 2 row-count)
                                              dkl--position-adjustment)
                                           row-position
                                           position)))
                (goto-char adjusted-position)
                (delete-char 1)
                (insert glyph)))
            (setq row-count (1+ row-count)))))
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

(defun dkl-set-current-layout (layout)
  "Set the layout displayed in this session."
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
