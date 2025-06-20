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

(use-package gptel
  :ensure t
  :bind (("C-c g RET" . gptel-send)
	 ("C-c g /" . gptel))
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model 'gpt-4o)
  (setq gptel-include-reasoning "*Reasoning*")
  (setq
   gptel-directives
   '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
     (product . "You are a very senior product manager at a Canadian D2C fintech.  Respond concisely.")
     (strategy . "You are an expert product strategist with deep knowledge of Canadian D2C fintechs and Canadian provincial and federal regulations and laws.  Challenge my assumptions and ask probing questions to refine strategy.  Repond concidely.")
     (writing . "You are a large language model and a writing assistant. Respond concisely.")
     )))

(use-package org
  :ensure t
  :defer t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
	 ("C-c l" . org-store-link))
  :config
  (set-face-attribute 'org-tag nil :foreground "Grey" :weight 'thin) ;; subdue tags
  :init
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org"))
  (setq org-default-notes-file "~/org/inbox-MPro0147.org")
  (setq org-tags-column 0) ;; place tags immediately after headline text
  (setq org-log-into-drawer t)
  (setq org-id-link-to-org-use-id t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-include-diary nil) ;; directly integrated in diary.org file
  (setq
   org-refile-targets
   '((org-agenda-files :maxlevel . 2)
     (nil :maxlevel . 2)))
  (setq
   org-todo-keywords
   (quote ((sequence "TODO(t)" "|" "DONE(d)")
	   (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)"))))
  (setq
   org-todo-keyword-faces
   (quote (("TODO" :foreground "red" :weight bold)
	   ("WAITING" :foreground "orange" :weight bold)
	   ("DONE" :foreground "green" :weight bold)
	   ("CANCELLED" :foreground "green" :weight bold))))
  (setq
   org-tag-alist
   (quote (
	   ("emacs" . ?e)
	   ("family" . ?f)
	   ("health" . ?l)
	   ("home" . ?h)
	   ("money" . ?m)
	   ("pets" . ?c)
	   ("product" . ?p)
	   ("tasks" . ?t)
	   ("vehicle" . ?v)
	   )))
  (setq
   org-agenda-custom-commands
   '((" " "Default"
      ((agenda "" ((org-agenda-span 1)))
       (todo "WAITING")
       (todo "TODO")))))
  (setq
   org-capture-templates
   '(("t" "Task" entry
      (file "~/org/inbox-MPro0147.org")
      "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
%a")
     ("r" "Task (from region)" entry
      (file "~/org/inbox-MPro0147.org")
      "* TODO %i%?
:PROPERTIES:
:CREATED: %U
:END:
%a")
     ("h" "Habit" entry
      (file "~/org/inbox-MPro0147.org")
      "* TODO %?
SCHEDULED: <%<%Y-%m-%d %a .+1d>>
:PROPERTIES:
:CREATED: %U
:STYLE: habit
:END:"))))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-agenda-show-habits t)
(setq org-habit-show-habits-only-for-today t)

(use-package org-habit-stats
  :ensure t
  :bind (:map org-mode-map
	      ("C-c h" . org-habit-stats-view-habit-at-point)
	      :map org-agenda-mode-map
	      ("C-c H" . org-habit-stats-view-habit-at-point-agenda)))

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

;;; init.el ends here

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(emoji ((t (:height 110 :family "Apple Color Emoji")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-habit-stats gptel consult-denote magit consult marginalia orderless vertico orgalist uniline denote-journal denote which-key howm)))
