;;; init-ui.el --- UI setup -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2025-2026, David Adair <adair.david@gmail.com>
;;
;; Author: David Adair <adair.david@gmail.com>
;; Maintainer: David Adair <adair.david@gmail.com>
;;
;; Created: 20 Dec 2025
;;
;; URL: https://github.com/dadair-ca/.emacs.d/tree/master
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Configures the Emacs user interface
;;
;;; Code:

(set-face-attribute 'default nil :height 130)
;;(setq default-frame-alist '((width . 165) (height . 60) (top . 125) (left . 310)))
(setq inhibit-startup-screen t)
(pixel-scroll-precision-mode)
(load-theme 'wombat)

(setopt use-short-answers t)

(provide 'init-ui)
;;; init-ui.el ends here
