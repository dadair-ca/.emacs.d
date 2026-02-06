;;; init-vulpea.el --- Vulpea setup -*- lexical-binding: t; -*-
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
;; Configuration for Vulpea to set up my PMK and GTD systems.
;;
;;; Code:

(require 'config-path)

(defvar vulpea-directory
  (expand-file-name "vulpea" path-git-dir)
  "Directory containing notes.")

(defvar vulpea-capture-inbox-file
  (format "inbox-%s.org" (system-name))
  "The path to the inbox file.

It is relative to `org-directory', unless it is absolute.")

(defun directory-subdirs (directory &optional rec)
  "Return subdirs or files of DIRECTORY.

If REC is non-nil then do recursive search."
  (let ((res
         (seq-map
          #'file-name-as-directory
          (seq-remove
           (lambda (file)
             (or (string-match "\\`\\."
                               (file-name-nondirectory file))
                 (string-match "\\`#.*#\\'"
                               (file-name-nondirectory file))
                 (string-match "~\\'"
                               (file-name-nondirectory file))
                 (not (file-directory-p file))))
           (directory-files directory t)))))
    (if rec
        (apply
         #'append
         (seq-map (lambda (p) (cons p (directory-subdirs p)))
                  res))
      res)))

(defun vulpea-subdir-select ()
  "Select notes subdirectory."
  (interactive)
  (let ((dirs (cons
	       "."
	       (seq-map
		(lambda (p)
		  (string-remove-prefix vulpea-directory p))
		(directory-subdirs vulpea-directory 'recursive)))))
    (completing-read "Subdir: " dirs nil t)))

(use-package vulpea
  :ensure t
  :demand t
  :init
  (setq-default
   vulpea-db-sync-directories (list vulpea-directory)
   vulpea-default-notes-directory vulpea-directory
   vulpea-db-location (expand-file-name "vulpea.db" path-cache-dir)

   vulpea-db-parse-method 'single-temp-buffer
   vulpea-db-index-heading-level t

   vulpea-db-sync-scan-on-enable 'async
   
   vulpea-create-default-template
   '(:file-name "%(vulpea-subdir-select)/${timestamp}-${slug}.org"
		:properties (("CREATED" . "%<[%Y-%m-%d %H:%M]>"))))
  (vulpea-db-autosync-mode +1)
  :config
  (add-hook 'vulpea-insert-handle-function #'vulpea-insert-handle))

(use-package vui
  :ensure (:host github :repo "d12frosted/vui.el")
  :demand t)

(use-package vulpea-ui
  :ensure (:host github :repo "d12frosted/vulpea-ui")
  :after (vui)
  ;; :hook (org-mode . vulpea-ui-sidebar-open)
  :defer t
  ;; :config
  ;; ;; Completely disable sidebar - prevent it from opening
  ;; (advice-add 'vulpea-ui-sidebar-open :override #'ignore)
  ;; (advice-add 'vulpea-ui-sidebar-toggle :override #'ignore)
  )

(use-package vulpea-journal
  :ensure (:host github :repo "d12frosted/vulpea-journal")
  :after (vulpea vulpea-ui)
  :defer t
  :bind (("C-c j" . vulpea-journal)
	 ("C-c n d d" . vulpea-journal-date)
	 ("C-c n d t" . vulpea-journal-today)
	 ("C-c n d [" . vulpea-journal-previous)
	 ("C-c n d ]" . vulpea-journal-next))
  :init
  (setq-default
   vulpea-journal-ui-previous-years-count 10)
  :config
  (vulpea-journal-setup))

(use-package org
  :ensure t
  :hook ((org-mode . visual-line-mode)
	 (org-mode . org-indent-mode))
  :commands (org-check-agenda-file
	     org-link-set-parameters)
  :init
  (setq org-directory vulpea-directory)
  (setq org-modules '(org-id org-attach org-habits))

  (setq-default
   org-adapt-indentation nil
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars t
   org-image-actual-width '(512)
   org-imenu-depth 1
   org-pretty-entities nil
   org-startup-folded 'show2levels
   org-blank-before-new-entry '((heading . t)
				(plain-list-item . nil))
   org-cycle-separator-lines 1)
  (setq-default org-fold-catch-invisible-edits 'smart)

  (setq org-property-format "%-24s %s")

  (setq
   org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d!)")
     (sequence
      "WAITING(w@/!)"
      "HOLD(h@/!)"
      "|"
      "CANCELLED(c@/!)"
      "MEETING"))

   org-use-fast-tag-selection t
   org-enforce-todo-dependencies t
   org-treat-S-cursor-todo-selection-as-state-change nil

   org-todo-state-tags-triggers
   '(("CANCELLED" ("CANCELLED" . t))
     ("WAITING" ("WAITING" . t))
     ("HOLD" ("WAITING") ("HOLD" . t))
     (done ("WAITING") ("HOLD") ("FOCUS"))
     ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
     ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))

   org-log-into-drawer t)

  (setq
   org-tag-persistent-alist '(("FOCUS" . ?f)
			      ("PROJECT" . ?p))
   org-use-tag-inheritance t
   org-tags-exclude-from-inheritance '("project" "area" "FOCUS" "people"))

  ;;(advice-add #'org-check-agenda-file :around #'vulpea-check-agenda-file)
  :config
  (add-to-list 'org-file-apps '(directory . emacs))
  (setq org-indirect-buffer-display 'current-window)
  (setq org-return-follows-link t))

(use-package org-refile
  :ensure nil
  :after org
  :init
  (setq
   org-outline-path-complete-in-steps nil
   org-refile-targets '((nil :maxlevel . 4)
			(org-agenda-files :maxlevel . 4))
   org-refile-use-outline-path t
   org-refile-allow-creating-parent-nodes nil
   org-refile-target-verify-function #'vulpea-refile-verify-target))

(use-package org-id
  :ensure nil
  :after org
  :hook ((before-save . vulpea-id-auto-assign))
  :config
  (setq
   org-id-track-globally t
   org-id-extra-files
   (list (expand-file-name ".archive/archive" org-directory)
	 (expand-file-name ".archive/archive.org" org-directory))
   org-id-link-to-org-use-id t
   org-id-locations-file (expand-file-name "org-id-locations" path-cache-dir)))

(defun vulpea-capture-setup ()
  "Wire all bits for capturing."
  (dolist (var '(vulpea-capture-inbox-file))
    (set var (expand-file-name (symbol-value var) vulpea-directory)))
  (unless org-default-notes-file
    (setq org-default-notes-file vulpea-capture-inbox-file))
  (setq
   org-capture-templates
   '(("t" "todo" entry (file vulpea-capture-inbox-file)
      "* TODO %?\n%U\n"
      :clock-in t :clock-resume t)
     ("m" "meeting" entry (file vulpea-capture-inbox-file)
      "* MEETING [%<%Y-%m-%d %a>] :MEETING:\n%U\n\n%?"
      :clock-in t :clock-resumt t))))

(defun vulpea-capture-task ()
  "Capture a task."
  (interactive)
  (org-capture nil "t"))

(defun vulpea-capture-meeting ()
  "Capture a meeting."
  (interactive)
  (org-capture nil "m"))

(use-package org-capture
  :ensure nil
  :after org
  :bind (("C-c c X" . org-capture)
	 ("C-c c t" . vulpea-capture-task)
	 ("C-c c m" . vulpea-capture-meeting)
	 ("C-c c l" . org-store-link))
  :init
  (setq-default org-capture-bookmark nil)
  (vulpea-capture-setup))

(defconst vulpea-agenda-cmd-refile
  '(tags
    "REFILE"
    ((org-agenda-overriding-header "To refile")
     (org-tags-match-list-sublevels t))))

(defconst vulpea-agenda-cmd-today
  '(agenda
    ""
    ((org-agenda-span 'day)
     (org-agenda-skip-deadline-prewarning-if-scheduled t)
     (org-agenda-sorting-strategy '(habit-down
				    time-up
				    category-keep
				    todo-state-down
				    priority-down)))))

(defconst vulpea-agenda-cmd-focus
  '(tags-todo
    "FOCUS"
    ((org-agenda-overriding-header "To focus on")
     (org-tags-match-list-sublevels t)
     (org-agenda-sorting-strategy '(todo-state-down
				    org-priority-down
				    effort-up
				    category-keep)))))

(defconst vulpea-agenda-cmd-waiting
  '(tags-todo
    "-CANCELLED+WAITING-FOCUS|+HOLD/!"
    ((org-agenda-overriding-header "Waiting and postponed tasks")
     (org-tags-match-list-sublevels nil))))

(defun vulpea-agenda-category (&optional len)
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        \\='((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (if (eq major-mode 'org-mode)
      (let* ((file-name (when buffer-file-name
                          (file-name-sans-extension
                           (file-name-nondirectory buffer-file-name))))
             (title (or (vulpea-buffer-meta-get! (vulpea-buffer-meta) "short name")
                        (vulpea-buffer-prop-get "title")))
             (category (org-get-category))
             (result
              (or (if (and
                       title
                       (string-equal category file-name))
                      title
                    category)
                  "")))
        (if (numberp len)
            (s-truncate len (s-pad-right len " " result))
          result))
    (s-repeat (or len 0) " ")))

(use-package org-agenda
  :ensure nil
  :after org
  :bind (("C-c a" . org-agenda))
  :config
  ;; TODO starting with a naive full recursive directory
  ;;(advice-add 'org-agenda :before #'vulpea-agenda-files-update)
  (setq
   org-agenda-files (list vulpea-directory)
   org-agenda-dim-blocked-tasks nil
   org-agenda-inhibit-startup nil
   org-agenda-log-mode-items '(closed clock state)
   org-agenda-show-inherited-tags nil

   org-agenda-prefix-format
   '((agenda . " %(vulpea-agenda-category 12) %?-12t %12s")
     (todo . " %(vulpea-agenda-category 12) ")
     (tags . " %(vulpea-agenda-category 12) ")
     (search . " %(vulpea-agenda-category 12) "))
   org-agenda-todo-keyword-format "%-1s"
   org-agenda-tags-column 0
   org-agenda-window-setup 'current-window
   org-agenda-hide-tags-regexp "REFILE\\|FOCUS\\|MEETING"
   org-agenda-custom-commands
   `((" " "Agenda"
      (,vulpea-agenda-cmd-refile
       ,vulpea-agenda-cmd-today
       ,vulpea-agenda-cmd-focus
       ,vulpea-agenda-cmd-waiting)))))

(provide 'init-vulpea)
;;; init-vulpea.el ends here
