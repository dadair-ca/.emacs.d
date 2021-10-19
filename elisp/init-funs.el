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
        (proj (project-current nil)))
    (if proj
        (let* ((proj (expand-file-name (cdr proj)))
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

(provide 'init-funs)

;;; init-funs.el ends here
