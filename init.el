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

(setq-default user-full-name "David Adair"
	      user-mail-address "adair.david@gmail.com")

(require 'config-path)
(require 'init-elpa)

(setq custom-file (concat user-emacs-directory "custom.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'init-ai)
(require 'init-ide)
(require 'init-ui)

(require 'savehist)
(setq savehist-file (locate-user-emacs-file "savehist"))
(setq history-length 500)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(add-hook 'after-init-hook #'savehist-mode)

(require 'org-id)

(defun my/org-capture-add-default-properties ()
  (org-entry-put nil "ID" (org-id-get-create))
  (org-id-copy))

(use-package org
  :demand t
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (setq
   org-directory "~/git/org"
   org-agenda-files '("~/git/org")
   org-default-notes-file "~/git/org/inbox.org"
   org-modules '(ol-doi
		 ol-w3m
		 ol-bbdb
		 ol-bibtex
		 ol-docview
		 ol-gnus
		 ol-info
		 ol-irc
		 ol-mhe
		 ol-rmail
		 ol-eww
		 org-habit)
   org-capture-templates
   '(("i" "Inbox" entry (file "inbox.org")
      "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
%a"
      :before-finalize my/org-capture-add-default-properties))))

(use-package denote
  :ensure (:host github :repo "protesilaos/denote")
  :demand t
  :bind (("C-c n o" . denote-open-or-create)
	 ("C-c n l" . denote-link-or-create))
  :hook ((dired-mode . denote-dired-mode)
	 (text-mode . denote-fontify-links-mode))
  :config
  (setq denote-directory "~/git/org")
  (setq denote-file-type 'text)
  (denote-rename-buffer-mode 1)
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-prompts '(title keywords file-type)))

(use-package denote-journal
  :ensure (:host github :repo "protesilaos/denote-journal")
  :commands (denote-journal-new-entry
	     denote-journal-new-or-existing-entry
	     denote-journal-link-or-create-entry)
  :hook (calendar-mode . denote-journal-calendar-mode)
  :bind (("C-c n j o" . denote-journal-new-or-existing-entry)
	 ("C-c n j l" . denote-journal-link-or-create-entry)
	 ("C-x c" . calendar))
  :config
  (setq
   denote-journal-directory denote-directory
   denote-journal-keyword "journal"
   denote-journal-title-format 'day-date-month-year))

(use-package consult-denote
  :ensure (:host github :repo "protesilaos/consult-denote")
  :demand t
  :bind (("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

(use-package dired
  :hook ((dired-mode . dired-hide-details-mode))
  :config
  (when (string= system-type "darwin")       
    (setq dired-use-ls-dired nil)))

(use-package beancount
  :ensure (:host github :repo "beancount/beancount-mode")
  :demand t
  :mode ("\\.beancount\\'" . beancount-mode))

(use-package ledger-mode
  :custom
  ((ledger-binary-path "hledger")
   (ledger-mode-should-check-version nil)
   (ledger-report-auto-width nil)
   (ledger-report-links-in-register nil)
   (ledger-report-native-highlighting-arguments '("--color=always")))
  :mode ("\\.hledger\\'" "\\.ledger\\'"))

(use-package markdown-mode
  :ensure t)

(global-auto-revert-mode t)

;;; init.el ends here
