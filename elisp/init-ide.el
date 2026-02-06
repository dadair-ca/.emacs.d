;;; init-ide.el --- IDE-like features -*- lexical-binding: t; -*-
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
;; Installs IDE-like features (corfu, cape), etc.
;;
;;; Code:

(require 'config-path)

(use-package vertico
  :ensure t
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :demand t
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
	 ("C-*" . consult-outline)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode t))

(recentf-mode 1)

(provide 'init-ide)
;;; init-ide.el ends here
