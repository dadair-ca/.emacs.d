;;; init-funs.el --- Initialize custom functions.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 David Adair

;; Author: David Adair <adair.david@gmail.com>
;; URL: https://github.com/adairdavid/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Initialize custom functions.
;;

;;; Code:

(defun da--kill-and-echo (X)
  "Copy `X' into the `kill-ring' and echo to the minibuffer."
  (kill-new X)
  (message X))

(defun da/path ()
  "Echo file name to minibuffer and copy to kill ring.

Will remove project prefix if inside a project."
  (interactive)
  (let ((buf (buffer-file-name))
        (proj (car (nthcdr 2 (project-current nil)))))
    (if proj
        (let* ((proj (expand-file-name proj))
               (splits (split-string buf proj)))
          (da--kill-and-echo (car (cdr splits))))
      (da--kill-and-echo (buffer-file-name)))))

(defun da/surround (BEGIN END OPEN CLOSE)
  "Put OPEN at BEGIN and CLOSE at END of the selected region."
  (interactive "r\nsStart: \nsEnd: ")
  (save-excursion
    (goto-char END)
    (insert (if (string= CLOSE "") OPEN CLOSE))
    (goto-char BEGIN)
    (insert OPEN)))

(defun da/emacs-somewhere ()
  "Open a new buffer and paste clipboard contents."
  (interactive)
  (scroll-bar-mode -1)
  (pop-to-buffer-same-window (get-buffer-create "*emacs-somewhere*"))
  (yank))

(use-package 0xc :ensure t)

(defun da/combine-3des (comp1 comp2 comp3)
  "Combine key components COMP1, COMP2, and COMP3 into a 3DES key.

Components should be hex-strings.

Requires `0xc' installed from MELPA."
  (let* ((dec1 (string-to-number (0xc-convert 10 comp1)))
         (dec2 (string-to-number (0xc-convert 10 comp2)))
         (dec3 (string-to-number (0xc-convert 10 comp3))))
    (0xc-convert 16 (number-to-string (logxor dec1 dec2 dec3)))))

(defun da--utf8-to-hex (s)
  "Convert a set of utf8 encoded characters S to hex."
  (mapconcat (lambda (c) (format "%02X" c))
             (encode-coding-string s 'utf-8) ""))

(defun da/region-utf8-to-hex (start end)
  "Print the marked region between START and END, interpreted as utf8, as hex."
  (interactive "*r")
  (da--utf8-to-hex (buffer-substring start end))
  (deactivate-mark))

(defun da/disable-scroll-bars (frame)
  "Disable scroll bars in FRAME."
  (modify-frame-parameters
   frame
   '((vertical-scroll-bars . nil)
     (horizontal-scroll-bars . nil))))

(provide 'init-funs)

;;; init-funs.el ends here

