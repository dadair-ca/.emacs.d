;;; init-dired.el --- dired setup -*- lexical-binding: t; -*-
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
;; Configures dired
;;
;;; Code:

(use-package dired
  :hook (dired-mode-hook . dired-hide-details-mode))

(provide 'init-dired)
;;; init-dired.el ends here
