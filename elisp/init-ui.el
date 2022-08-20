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

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   (load-theme 'doom-one t)
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-neotree-config)
;;   ;(doom-themes-org-config)
;;   )

;; (use-package modus-vivendi :ensure)
;; (use-package modus-operandi :ensure)

;; (use-package modus-vivendi-theme :ensure)
;; (use-package modus-operandi-theme :ensure)

;; (use-package modus-themes
;;   :ensure
;;   :init
;;   (setq modus-themes-slanted-constructs t
;;         modus-themes-bold-constructs nil)
;;   (modus-themes-load-themes)
;;   :config
;;   ;; (modus-themes-load-operandi)
;;   (modus-themes-load-vivendi)
;;   )

(defun da/load-random-theme ()
  "Load a random theme based on system settings."
  (if (string-match-p
     "dark"
     (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme"))
    (ef-themes-load-random 'dark)
  (ef-themes-load-random 'light)))

(use-package ef-themes
  :ensure
  :custom
  (ef-themes-height-1 1.0)
  (ef-themes-height-2 1.0)
  (ef-themes-height-3 1.0)
  (ef-themes-height-4 1.0)
  (ef-themes-height-5 1.0)
  (ef-themes-height-6 1.0)
  (ef-themes-height-7 1.0)
  (ef-themes-height-8 1.0)
  :init
  (mapc #'disable-theme custom-enabled-themes)
  :config
  (da/load-random-theme))

;; (use-package doom-modeline
;;   :hook (after-init . doom-modeline-mode)
;;   :config
;;   (setq doom-modeline-buffer-file-name-style 'truncate-all))

(use-package which-key
  :defer 5
  :diminish
  :commands (which-key-mode)
  :config
  (which-key-mode))

;; (load-theme 'tango)

;; (global-display-fill-column-indicator-mode)

(setq default-frame-alist '(;(undecorated . t)
                            (font . "Source Code Pro-11")))

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
     org-agenda-mode-hook))
  :config
  (lin-global-mode 1)
  (global-hl-line-mode))

(provide 'init-ui)

;;; init-ui.el ends here
