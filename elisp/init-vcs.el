;;; init-vcs.el --- Version control configuration.	-*- lexical-binding: t -*-

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
;; Version control configuration.
;;

;;; Code:

(defvar ediff-window-setup-functin 'ediff-setup-windows-plain)

(use-package magit
  :bind ("<f2>" . magit-status))

(use-package forge :after magit)

(use-package code-review
  :custom
  (code-review-fill-column 80)
  (code-review-download-dir "/tmp/code-review/")
  (code-review-ath-login-marker 'forge))

(provide 'init-vcs)

;;; init-vcs.el ends here
