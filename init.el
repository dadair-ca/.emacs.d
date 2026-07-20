;;; init.el --- Personal emacs configuration         -*- lexical-binding: t; -*-

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

;; Personal emacs configuration

;;; Code:

;;; BASE

(use-package exec-path-from-shell
  :vc (:url "https://github.com/purcell/exec-path-from-shell")
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package emacs
  :ensure nil
  :hook
  ((text-mode . turn-on-auto-fill))
  :bind
  (("C-;" . duplicate-dwim)
   ("C-x M-e" . eval-buffer))
  :config
  (setq duplicate-line-final-position -1)
  (setq adaptive-fill-mode t)
  (setq make-backup-files nil)
  (setq create-lockfiles nil)
  (setq which-key-mode 1))

;;; CALENDAR

(use-package calendar
  :ensure nil
  :bind
  (("C-x c" . calendar))
  :config
  (setq calendar-mode-line-format nil)
  (setq calendar-time-display-form
	'(24-hours ":" minutes
		   (when time-zone (format "(%s)" time-zone))))
  (setq calendar-week-start-day 1)
  (setq calendar-date-style 'iso)
  (setq calendar-time-zone-style 'numeric)

  (require 'cal-dst)
  (setq calendar-standard-time-zone-name "-0700")
  (setq calendar-daylight-time-zone-name "-0600"))

;;; CONSULT, ORDERLESS, VERTICO, MARGINALIA

(use-package consult
  :vc (:url "https://github.com/minad/consult")
  :bind
  (("C-x b" . consult-buffer)
   ("C-x r b" . consult-bookmark)
   ("M-g o" . consult-outline)
   ("M-s g" . consult-grep)
   ("M-s l" . consult-line)))

(use-package orderless
  :vc (:url "https://github.com/oantolin/orderless")
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :vc (:url "https://github.com/minad/vertico")
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 10)
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :vc (:url "https://github.com/minad/marginalia")
  :bind
  (:map minibuffer-local-map
	("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package savehist
  :config
  (setq history-length 1000)
  :init
  (savehist-mode 1))

;;; DENOTE

(use-package denote
  :vc (:url "https://github.com/protesilaos/denote")
  :hook
  ((text-mode . denote-fontify-links-mode)
   (dired-mode . denote-dired-mode))
  :bind
  (("C-c n n" . denote)
   ("C-c n N" . denote-type)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep)
   ("C-c n r" . denote-rename-file)
   :map text-mode-map
   ("C-c n i" . denote-link)
   ("C-c n I" . denote-add-links)
   ("C-c n b" . denote-backlinks)
   ("C-c n R" . denote-rename-file-using-front-matter)
   :map dired-mode-map
   ("C-c C-d C-i" . denote-dired-link-marked-notes)
   ("C-c C-d C-r" . denote-dired-rename-marked-files)
   ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
   ("C-c C-d C-f" . denote-dired-rename-marked-files-using-front-matter))
  :config
  (setq denote-directory (expand-file-name "~/Documents/notes"))
  (setq denote-file-type 'org)
  (setq denote-known-keywords '("emacs"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-buffer-name-prefix "[D] ")
  (setq denote-rename-buffer-format "%D")
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :vc (:url "https://github.com/protesilaos/consult-denote")
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

(defun da--insert-journal-log-header ()
  "Insert a log header into buffer at point."
  (interactive)
  (let* ((stamp (concat "--" (format-time-string "T%H%M%S")))
	 (l (length stamp))
	 (p (- 70 l)))
    (insert stamp (make-string p ?-))))

(use-package denote-journal
  :vc (:url "https://github.com/protesilaos/denote-journal")
  :hook
  ((calendar-mode . denote-journal-calendar-mode))
  :bind
  (("C-c n j" . denote-journal-new-or-existing-entry)
   :map text-mode-map
   ("C-c j l" . da--insert-journal-log-header)
   :map denote-journal-calendar-mode-map
   ("F" . denote-journal-calendar-fine-file)
   ("N" . denote-journal-calendar-new-or-existing))
  :config
  (setq denote-journal-directory (expand-file-name "journal" denote-directory))
  (setq denote-journal-keyword "journal")
  (setq denote-journal-title-format 'day-date-month-year))

;;; DIRED

(use-package dired
  :ensure nil
  :config
  (setq dired-use-ls-dired nil)
  (setq dired-dwim-target t))

;;; EMBARK

(use-package embark
  :vc (:url "https://github.com/oantolin/embark")
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t)

;;; ORG

(require 'org-habit)

(use-package org
  :ensure nil
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c o" . org-open-at-point-global)
   ("C-c a" . org-agenda)
   :map org-mode-map
   ("C-c M-l" . org-insert-last-stored-link)
   ("C-c C-M-l" . org-toggle-link-display))
  :config
  (setq org-modules '(org-habit))
  (setq org-directory "~/Documents/org/")
  (setq org-imenu-depth 7)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-hide-emphasis-markers nil)
  (setq org-hide-macro-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-cycle-separator-lines 0)
  (setq org-fold-catch-invisible-edits 'show)
  (setq org-return-follows-link t)
  (setq org-use-sub-superscripts '{})
  (setq org-insert-heading-respect-content t)
  (setq org-read-date-prefer-future 'time)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-block-delimiter-line t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?C)
  (setq org-priority-faces nil)

  (setq org-indent-mode-turns-on-hiding-stars nil)

  (setq org-refile-targets
	'((org-agenda-files . (:maxlevel . 2))
	  (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-reverse-note-order t)

  (setq org-tag-persistent-alist (quote ((:startgroup)
					 ("@errand" . ?e)
					 ("@office" . ?o)
					 ("@home" . ?H)
					 ("@call" . ?C)
					 (:endgroup)
					 ("WAITING" . ?w)
					 ("HOLD" . ?h)
					 ("NOTE" . ?n)
					 ("CANCELLED" . ?c)
					 ("LINK" . ?l)
					 ("FLAGGED" . ??))))

  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "HABIT(b)" "|" "DONE(d)")
		(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
	      ("HABIT" :foreground "purple" :weight bold))))

  (setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

  (setq org-tag-faces
	'(("inbox" . (:background "magenta1" :foreground "white" :weight bold))))
  
  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-fontify-whole-heading-line nil)

  (setq org-tag-alist nil)
  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)

  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-note-clock-out nil)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)

  (setq org-link-context-for-files t)
  (setq org-link-keep-stored-after-insertion nil)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (defun da--before-finalize-function ()
    "Add 'ID' & 'CREATED' to captured entry, and add 'LINK' tag if 'SLACK' or
'URL' properties are non-nil."
    (org-id-get-create)
    (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))
    (let ((slack (org-entry-get nil "SLACK"))
	  (url (org-entry-get nil "URL")))
      ;; If either property exists and is not an empty string
      (when (or (and slack (not (string= slack "")))
                (and url (not (string= url ""))))
        ;; Add the "LINK" tag without prompting
        (org-toggle-tag "LINK" 'on))))

  (setq org-property-format "%-1s %s")

  (setq org-capture-templates
	`(("t" "Task" entry (file "inbox.org")
	   "* TODO %?\n%a"
	   :before-finalize da--before-finalize-function)
	  ("n" "Note" entry (file "inbox.org")
	   "* %? :NOTE:\n%a"
	   :before-finalize da--before-finalize-function)
	  ("m" "Meeting" entry (file "inbox.org")
	   "* MEETING \"%?\""
	   :before-finalize da--before-finalize-function)
	  ("p" "Phone call" entry (file "inbox.org")
	   "* PHONE \"%?\""
	   :before-finalize da--before-finalize-function)
	  ("h" "Habit" entry (file "inbox.org")
	   ,(concat "* HABIT %?
SCHEDULED: "
		    (format-time-string "<%Y-%m-%d %a .+1d/3d>")
		    "\n"
		    ":PROPERTIES:
:STYLE: habit
:REPEAT_TO_STATE: HABIT
:END:
")
	   :before-finalize da--before-finalize-function)
	  ("R" "Product review" entry (file "inbox.org")
	   ,(concat "* TODO (%^{For Who}) Review: %? :LINK:
SCHEDULED:"
		    (format-time-string "<%Y-%m-%d %a>")
		    "\n"
		    ":PROPERTIES:
:URL: %^{Document link}
:SLACK: %^{Slack thread}
:END:
")
	   :before-finalize da--before-finalize-function)))

  (setq org-agenda-custom-commands
	`(("N" "Notes" tags "NOTE")
	  ("h" "Habits" tags-todo "STYLE=\"habit\"")
	  (" " "Agenda"
	   ((tags-todo "PRIORITY=\"A\"")
	    (agenda ""
		    ((org-agenda-span 3)
		     (org-agenda-start-day "0d")))
	    (tags "inbox")
	    (tags-todo "-CANCELLED+WAITING|HOLD/!")
	    (tags "CLOSED>=\"<today>\"")))))

  (setq org-habit-preceding-days 63)

  (setq org-agenda-include-diary t)

  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)

  (setq org-agenda-files (list org-directory))
  (setq org-agenda-span 'day)
  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-window-setup 'current-window)

  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-todo-list-sublevels t)

  (setq org-deadline-warning-days 35)

  (setq org-agenda-show-inherited-tags t)
  (setq org-agenda-use-tag-inheritance
        '(todo search agenda))
  (setq org-agenda-hide-tags-regexp nil)
  (setq org-agenda-tags-column 1)
  )

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(consult-denote denote denote-journal embark embark-consult
		    exec-path-from-shell marginalia orderless vertico))
 '(package-vc-selected-packages
   '((exec-path-from-shell :url
			   "https://github.com/purcell/exec-path-from-shell")
     (embark :url "https://github.com/oantolin/embark")
     (marginalia :url "https://github.com/minad/marginalia")
     (vertico :url "https://github.com/minad/vertico")
     (orderless :url "https://github.com/oantolin/orderless")
     (denote-journal :url
		     "https://github.com/protesilaos/denote-journal")
     (consult-denote :url
		     "https://github.com/protesilaos/consult-denote")
     (denote :url "https://github.com/protesilaos/denote")))
 '(safe-local-variable-values
   '((calendar-latitude . 51.04) (calendar-longitude . -114.03))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
