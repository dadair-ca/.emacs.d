;;; init-ui.el --- UI configuration.	-*- lexical-binding: t -*-

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
;; UI configuration.
;;

;;; Code:

(use-package fireplace
  :config
  (run-with-idle-timer 300 nil #'fireplace))

;; (use-package hl-line
;;   :ensure nil
;;   :hook (after-init . global-hl-line-mode))

(use-package show-paren
  :ensure nil
  :hook (after-init . show-paren-mode))

(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package doom-themes
  :disabled
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-all))

(use-package which-key
  :defer 5
  :diminish
  :commands (which-key-mode)
  :config
  (which-key-mode))

(load-theme 'tango)

(provide 'init-ui)

;;; init-ui.el ends here
