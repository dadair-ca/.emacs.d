;;; init.el --- Emacs configurations.	-*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2019 David Adair

;; Author: David Adair <adair.david@gmail.com>
;; URL: https://github.com/adairdavid/.emacs.d
;; Version: 0.0.1
;; Keywords: .emacs.d

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
;; My Emacs configuration.
;;

;;; Code:

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold
                             normal-gc-cons-threshold))))

(use-package beancount
  :vc (:url "https://github.com/beancount/beancount-mode")
  :demand t
  :mode ("\\.beancount\\'" . beancount-mode))

(global-auto-revert-mode t)

;;; init.el ends here
