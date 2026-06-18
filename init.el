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

;;; Cache Paths

(defvar cache-directory
  (expand-file-name "cache/" user-emacs-directory)
  "Base directory for cache files.")

;;; General Emacs Configs

(use-package emacs
  :ensure nil
  :bind
  (("M-o" . other-window)
   ("M-z" . nil)
   ("C-x C-b" . ibuffer)
   ("C-M-z" . delete-pair)
   )
  :config
  (setq
   create-lockfiles nil
   make-backup-files nil
   pixel-scroll-precision-mode t
   read-answer-short t
   recentf-mode 1
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
      ("da" "↓"))))

;;; AUTO-REVERT

(use-package autorevert
  :ensure nil
  :hook (emacs-startup-hook . global-auto-revert-mode)
  :config
  (setq
   auto-revert-verbose t
   global-auto-revert-non-file-buffers t))

;;; ICOMPLETE

(use-package icomplete
  :ensure nil
  :bind (:map icomplete-minibuffer-map
	      ("C-n" . icomplete-forward-completions)
	      ("C-p" . icomplete-backward-completions)
	      ("RET" . icomplete-force-complete-and-exit)
	      ("C-j" . exit-minibuffer))
  :hook (after-init-hook . icomplete-vertical-mode)
  :config
  (setq
   icomplete-delay-completions-threshold 0
   icomplete-compute-delay 0
   icomplete-show-matches-on-no-input t
   icomplete-hide-common-prefix nil
   icomplete-with-completion-tables t
   icomplete-in-buffer t
   icomplete-max-delay-chars 0
   icomplete-scroll t))

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

;;; Org

(use-package org
  :ensure nil
  :defer t
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda))
  :mode ("\\.org\\'" . org-mode)
  :config
  (setopt org-export-backends '(ascii html icalendar latex odt md))
  (setq
   org-startup-folded t
   org-startup-indented t
   org-auto-align-tags nil
   org-tags-column 0

   org-agenda-tags-column 0
   org-agenda-block-separator ?-)
  (setq org-ellipsis " ↴ ")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "|" "DONE(d!)")
	  (sequence "HOLD(h@/!)" "WAITING(w@/!)" "|" "CANCELLED(c@)" "MEETING")))

  (setq org-todo-keyword-faces
	'(("TODO" :foreground "red" :weight bold)
	  ("NEXT" :foreground "blue" :weight bold)
	  ("DONE" :foreground "forest green" :weight bold)
	  ("WAITING" :foreground "orange" :weight bold)
	  ("HOLD" :foreground "magenta" :weight bold)
	  ("CANCELLED" :foreground "forest green" :weight bold)))  

  (setq org-log-done 'time)

  (setq org-agenda-files '("~/git/org/"))
  (setq org-default-notes-file "~/git/org/refile.org")

  (setq org-id-link-to-org-use-id t)

  (setq org-capture-templates
	`(("t" "todo" entry (file org-default-notes-file)
	   "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
- >>> :: %a"))))

;;; init.el ends here
