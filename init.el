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

;;;; Preamble

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defun update-load-path (&rest _)
  "Update LOAD-PATH."
  (push (expand-file-name "elisp" user-emacs-directory) load-path))

(advice-add #'package-initialize :after #'update-load-path)

(update-load-path)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq-default create-lockfiles nil)

(require 'init-org)
(require 'init-ledger)

(use-package uniline :ensure t)

(use-package vertico
  :ensure t
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
	 ("C-*" . consult-outline)))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n o" . denote-open-or-create)
   ("C-c n l" . denote-link-or-create)
   ("C-c n b" . denote-backlinks))
  :init
  (setq denote-directory (expand-file-name "~/denote"))
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-infer-keywords nil)
  (setq
   denote-known-keywords
   '("computer"
     "emacs"
     "finances"
     "health"
     "home"
     "journal"
     "personal"
     "pets"
     "vehicle"
     "pm" ;; General product management
     "nl" ;; Neo lending
     "nc" ;; Neo card
     ))
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :ensure t
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :init
  (consult-denote-mode 1))

(use-package denote-journal
  :ensure t
  :hook (calendar-mode . denote-journal-calendar-mode)
  :bind
  (("C-c n j o" . denote-journal-new-or-existing-entry)
   ("C-c n j l" . denote-journal-link-or-create-entry))
  :config
  (setq denote-journal-directory (expand-file-name "journal" denote-directory))
  (setq denote-journal-keyword "journal")
  (setq denote-journal-title-format 'day-date-month-year))

(global-set-key (kbd "C-x c") 'calendar)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(pixel-scroll-precision-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(recentf-mode 1)

;;; init.el ends here

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(emoji ((t (:height 110 :family "Apple Color Emoji"))))
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company ledger-mode homw hledger-mode org-habit org-habit-stats gptel consult-denote magit consult marginalia orderless vertico orgalist uniline denote-journal denote which-key)))
