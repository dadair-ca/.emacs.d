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

;; (use-package fireplace
;;   :config
;;   (run-with-idle-timer 300 nil #'fireplace))

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

(use-package which-key
  :defer 5
  :diminish
  :commands (which-key-mode)
  :config
  (which-key-mode))

(add-hook 'after-make-frame-functions 'da/disable-scroll-bars)

(setq compilation-scroll-output 'first-error)

(setq display-line-numbers-type t)
(setq display-line-numbers-major-tick 0)
(setq display-line-numbers-minor-tick 0)
(setq display-line-numbers-widen t)
(global-display-line-numbers-mode)

(use-package lin
  :ensure t
  :custom
  (lin-mode-hooks
   '(dired-mode-hook
     ibuffer-mode-hook
     log-view-mode-hook
     magit-log-mode-hook
     occur-mode-hook
     org-agenda-mode-hook
     ;;notmuch-search-mode-hook
     ;;notmuch-tree-mode-hook
     ))
  :config
  (lin-global-mode 1)
  (global-hl-line-mode))

(global-visual-line-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tab-bar

(setq tab-bar-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline

(setq display-time-format "%a %e %b, %H:%M")
(setq display-time-interval 60)
(setq display-time-default-load-average nil)
(display-time-mode)

(use-package minions
  :config (minions-mode 1))

(provide 'init-ui)

(add-to-list 'default-frame-alist
             '(font . "Menlo-14"))

;;; init-ui.el ends here
