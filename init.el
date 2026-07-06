;;; init.el --- Emacs configuration                  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  David Adair

;; Author: David Adair <david.adair@MPro0147>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;; EXEC path from shell

(use-package exec-path-from-shell
  :vc (:url "https://github.com/purcell/exec-path-from-shell")
  :init
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

;;; General Emacs Configs

(use-package emacs
  :ensure nil
  :bind
  (("M-o" . other-window)
   ("M-z" . nil)
   ("C-x C-b" . ibuffer)
   ("C-M-z" . delete-pair)
   ("C-;" . duplicate-dwim)
   )
  :config
  (setq
   create-lockfiles nil
   make-backup-files nil
   pixel-scroll-precision-mode t
   read-answer-short t
   ;;recentf-mode 1
   savehist-mode 1))

;;; ABBREV

(use-package abbrev
  :ensure nil
  :config
  (setq save-abbrevs nil)

  (define-abbrev-table 'global-abbrev-table
    '(;; Arrows
      ("ra" "→")
      ("la" "←")
      ("ua" "↑")
      ("da" "↓")
      ("hc" "✔")
      ("hx" "✘")
      ("fl" "⚑")
      ("qq" "⁇"))))

;;; AUTO-REVERT

(use-package autorevert
  :ensure nil
  :hook (emacs-startup-hook . global-auto-revert-mode)
  :config
  (setq
   auto-revert-verbose t
   global-auto-revert-non-file-buffers t))

;;; VERTICO

(use-package vertico
  :ensure t
  :config
  (setq vertico-resize t)
  (setq vertico-cycle t)
  :init
  (vertico-mode))

;;; ORDERLESS

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;; CONSULT

(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)
   ("C-*" . consult-outline)))

;;; MARGINALIA

(use-package marginalia
  :ensure t
  :demand t
  :bind
  (:map minibuffer-local-map
	("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;; DIRED

(use-package dired
  :ensure nil
  :config
  (setq
   dired-use-ls-dired nil
   dired-auto-revert-buffer t
   dired-dwim-target t
   dired-kill-when-opening-new-dired-buffer t))

;;; ESHELL

(use-package eshell
  :ensure nil
  :bind (("C-c e" . eshell))
  :defer t
  :config
  (setq eshell-history-size 100000))

;;; ISEARCH

(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq search-whitespace-regexp ".*?"))

;;; VC

(use-package vc
  :ensure nil
  :defer t
  :config
  (setopt
   vc-auto-revert-mode t))

;;; HOWM

(use-package howm
  :vc (:url "https://github.com/kaorahi/howm")
  :ensure t
  :config
  (setq howm-directory "~/git/howm")
  (setq howm-follow-theme t))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(keymap-global-set "M-Q" 'unfill-paragraph)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(consult consult-denote denote denote-journal exec-path-from-shell
	     howm marginalia orderless vertico))
 '(package-vc-selected-packages
   '((exec-path-from-shell :url
			   "https://github.com/purcell/exec-path-from-shell")
     (howm :url "https://github.com/kaorahi/howm"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
