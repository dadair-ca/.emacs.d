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

(use-package emacs
  :hook
  ((text-mode . turn-on-auto-fill))
  :custom
  (adaptive-fill-mode t)
  (make-backup-files nil)
  (create-lockfiles nil))

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
  (setq denote-file-type 'text)
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

(use-package denote-journal
  :vc (:url "https://github.com/protesilaos/denote-journal")
  :hook
  ((calendar-mode . denote-journal-calendar-mode))
  :bind
  (("C-c n j" . denote-journal-new-or-existing-entry))
  :config
  (setq denote-journal-directory (expand-file-name "journal" denote-directory))
  (setq denote-journal-keyword "journal")
  (setq denote-journal-title-format 'day-date-month-year))

;;; DIRED

(use-package dired
  :custom
  (dired-use-ls-dired nil))

;;; ORG

(use-package org
  :hook
  ((org-mode . org-indent-mode))
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c o" . org-open-at-point-global)
   ("C-c A" . org-agenda)
   ("C-c a" . (lambda ()
		"Call `org-agenda' with custom configuration."
		(interactive)
		(org-agenda nil "A"))))
  :config
  (setq org-directory "~/Documents/org/")
  (setq org-imenu-depth 7)
  (setq org-ellipsis " ▼")
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
  (setq org-indent-indentation-per-level 2)
  (setq org-startup-folded 'content)

  (setq org-refile-targets
	'((org-agenda-files . (:maxlevel . 2))
	  (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-reverse-note-order nil)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "MAYBE(m)" "|" "CANCELLED(c@)" "DONE(d!)")))

  (defface prot/org-todo-alternative
    '((t :inherit (italic org-todo)))
    "Face for alternative TODO-type Org keywords.")

  (defface prot/org-done-alternative
    '((t :inherit (italic org-done)))
    "Face for alternative DONE-type Org keywords.")

  (defface prot/org-tag-personal
    '((default :inherit unspecified :weight regular :slant normal)
      (((class color) (min-colors 88) (background light))
       :foreground "#004476")
      (((class color) (min-colors 88) (background dark))
       :foreground "#c0d0ef")
      (t :foreground "cyan"))
    "Face for personal Org tag.")

  (defface prot/org-tag-neo
    '((default :inherit unspecified :weight regular :slant normal)
      (((class color) (min-colors 88) (background light))
       :foreground "#603f00")
      (((class color) (min-colors 88) (background dark))
       :foreground "#deba66")
      (t :foreground "yellow"))
    "Face for neo Org tag.")

  (setq org-tag-faces
	'(("personal" . prot/org-tag-personal)
	  ("neo" . prot/org-tag-neo)))

  (setq org-todo-keyword-faces
	'(("MAYBE" . prot/org-todo-alternative)
	  ("CANCELLED" . prot/org-done-alternative)))

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

  (setq org-capture-templates
	`(("t" "Task to do (Personal)" entry
	   (file+headline "gtd.org" "All tasks")
	   "* TODO %^{Title} %^g\n")
	  ("T" "Task to do (Work)" entry
	   (file+headline "neo.org" "All tasks")
	   "* TODO %^{Title} %^g\n")
	  ("r" "Routine task (Personal)" entry
	   (file+headline "gtd.org" "Routine")
	   "* TODO %^{Title} %^g\n")
	  ("R" "Routine task (Work)" entry
	   (file+headline "neo.org" "Routine")
	   "* TODO %^{Title} %^g\n")))

  (setq org-agenda-custom-commands
	`(("A" "Daily agenda and top-priority tasks"
	   ((agenda "" ((org-agenda-overriding-header "\nPending scheduled tasks")
			(org-agenda-span 1)
			(org-agenda-show-all-dates nil)
			(org-scheduled-past-days 365)
			(org-scheduled-delays-days 1)
			(org-agenda-time-grid nil)
			(org-agenda-block-separator nil)
			(org-agenda-entry-types '(:scheduled))
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
			(org-agenda-day-face-function (lambda (date) 'org-agenda-date))
			(org-agenda-format-date "")
			))
	    (agenda "" ((org-agenda-overriding-header "\nToday's agenda\n")
			(org-agenda-span 1)
			(org-deadline-warning-days 0)
			(org-agenda-block-separator nil)
			(org-scheduled-past-days 0)
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "ROUTINE"))
			;; We don't need the `org-agenda-date-today'
			;; highlight because that only has a practical
			;; utility in multi-day views.
			(org-agenda-day-face-function (lambda (date) 'org-agenda-date))
			(org-agenda-format-date "%A %-e %B %Y")))
	    (agenda "" ((org-agenda-overriding-header "\nNext three days\n")
			(org-agenda-start-on-weekday nil)
			(org-agenda-start-day nil)
			(org-agenda-start-day "+1d")
			(org-agenda-span 3)
			(org-deadline-warning-days 0)
			(org-agenda-block-separator nil)
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
	    (agenda "" ((org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")
			(org-agenda-time-grid nil)
			(org-agenda-start-on-weekday nil)
			;; We don't want to replicate the previous section's
			;; three days, so we start counting from the day after.
			(org-agenda-start-day "+4d")
			(org-agenda-span 14)
			(org-agenda-show-all-dates nil)
			(org-deadline-warning-days 0)
			(org-agenda-block-separator nil)
			(org-agenda-entry-types '(:deadline))
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)))))
	   )))

  (setq org-default-notes-file (make-temp-file "emacs-org-notes-")) ; send it to oblivion
  (setq org-agenda-files (list org-directory))
  (setq org-agenda-span 'week)
  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-skip-comment-trees t)
  (setq org-agenda-menu-show-matcher t)
  (setq org-agenda-menu-two-columns nil)
  (setq org-agenda-sticky nil)
  (setq org-agenda-custom-commands-contexts nil)
  (setq org-agenda-max-entries nil)
  (setq org-agenda-max-todos nil)
  (setq org-agenda-max-tags nil)
  (setq org-agenda-max-effort nil)

  (setq org-agenda-prefix-format "%c	 %t %s")
  (setq org-agenda-sorting-strategy
        '(((agenda habit-down time-up priority-down category-keep)
           (todo priority-down category-keep)
           (tags priority-down category-keep)
           (search category-keep))))
  (setq org-agenda-breadcrumbs-separator "->")
  (setq org-agenda-todo-keyword-format "%-1s")
  (setq org-agenda-fontify-priorities 'cookies)
  (setq org-agenda-category-icon-alist nil)
  (setq org-agenda-remove-times-when-in-prefix nil)
  (setq org-agenda-remove-timeranges-from-blocks nil)
  (setq org-agenda-compact-blocks nil)
  (setq org-agenda-block-separator ?—)

  (setq org-agenda-bulk-mark-char "#")
  (setq org-agenda-persistent-marks nil)

  (setq diary-file (make-temp-file "emacs-diary-"))

  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-todo-list-sublevels t)

  (setq org-agenda-include-deadlines t)
  (setq org-deadline-warning-days 0)
  (setq org-agenda-skip-scheduled-if-done nil)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-if-done nil)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (setq org-agenda-skip-scheduled-delay-if-deadline nil)
  (setq org-agenda-skip-additional-timestamps-same-entry nil)
  (setq org-agenda-skip-timestamp-if-done nil)
  (setq org-agenda-search-headline-for-time nil)
  (setq org-scheduled-past-days 365)
  (setq org-deadline-past-days 365)
  (setq org-agenda-move-date-from-past-immediately-to-today t)
  (setq org-agenda-show-future-repeats t)
  (setq org-agenda-prefer-last-repeat nil)
  (setq org-agenda-timerange-leaders
        '("" "(%d/%d): "))
  (setq org-agenda-scheduled-leaders
        '("Scheduled: " "Sched.%2dx: "))
  (setq org-agenda-inactive-leader "[")
  (setq org-agenda-deadline-leaders
        '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
  ;; Time grid
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-timegrid-use-ampm nil)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-show-current-time-in-grid t)
  (setq org-agenda-current-time-string (concat "Now " (make-string 70 ?.)))
  (setq org-agenda-time-grid
        '((daily today require-timed)
          ( 0600 0700 0800 0900 1000 1100
            1200 1300 1400 1500 1600 1700
            1800 1900 2000 2100 2200 2300)
          "" ""))
  (setq org-agenda-default-appointment-duration nil)

  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-todo-ignore-timestamp t)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-time-comparison-use-seconds t)
  (setq org-agenda-tags-todo-honor-ignore-options nil)

  (setq org-agenda-show-inherited-tags t)
  (setq org-agenda-use-tag-inheritance
        '(todo search agenda))
  (setq org-agenda-hide-tags-regexp nil)
  (setq org-agenda-remove-tags nil)
  (setq org-agenda-tags-column 1)
  )

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(consult-denote denote denote-journal))
 '(package-vc-selected-packages
   '((denote-journal :url "https://github.com/protesilaos/denote-journal")
     (consult-denote :url
		     "https://github.com/protesilaos/consult-denote")
     (denote :url "https://github.com/protesilaos/denote"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
