;;; init-org.el --- Orgmode configuration.	-*- lexical-binding: t -*-

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
;; Orgmode configuration.
;;

;;; Code:

(use-package org
  :ensure org-plus-contrib)

(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '("~/Dropbox/org/contacts.org")))

(use-package org-noter
  :after (org-mode))

(use-package org-ref
  :ensure t
  :config
  (setq org-ref-bibliography-notes "~/Dropbox/Documents/Medical-School/Research/Bibliography/notes.org")
  (setq org-ref-default-bibliography '("~/Dropbox/Documents/Medical-School/Research/Bibliography/master.bib"))
  (setq org-ref-pdf-directory "~/Dropbox/Documents/Medical-School/Research/Bibliography/PDFs"))

(setq org-directory "~/Dropbox/org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/refile-mobileorg.org")
(setq org-default-notes-file "~/Dropbox/org/notes.org")

(setq org-agenda-files '(;"~/Dropbox/org/gtd.org"
                         "~/Dropbox/org/gtd2.org"
                         "~/Dropbox/org/refile.org"
                         "~/Dropbox/org/refile-mobileorg.org"
                         "~/Dropbox/org/someday.org"))

(setq org-log-into-drawer t)
(setq org-clock-into-drawer t)

(setq org-habit-graph-column 70)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "firebrick1" :weight normal)
              ("NEXT" :foreground "deep sky blue" :weight normal)
              ("DONE" :foreground "spring green" :weight normal)
              ("WAITING" :foreground "tomato" :weight normal)
              ("HOLD" :foreground "burlywood" :weight normal)
              ("CANCELLED" :foreground "spring green" :weight normal))))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-switchb)

(setq org-capture-templates
      (quote (("t" "Todo" entry (file "~/Dropbox/org/refile.org")
               "* TODO %?\n%U\n%a\n"
               :empty-lines 1)
              ("n" "Note" entry (file "~/Dropbox/org/notes.org")
               "* %? :NOTE:\n%U\n%a\n"
               :empty-lines 1)
              ("h" "Habit" entry (file "~/Dropbox/org/refile.org")
               "* TODO %?\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: TODO\n:END:\n%U\n%a\n"
               :empty-lines 1)
              ("p" "Project" entry (file "~/Dropbox/org/refile.org")
               "\n* TODO %? :PROJECT:\n%U\n\n** Outcome\n\n** NEXT\n"
               :empty-lines 1)
              ("c" "Contact" entry (file+headline "~/Dropbox/org/contacts.org" "Uncategorized")
               "\n* %(org-contacts-template-name)\n:PROPERTIES:\n:EMAIL: %(org-contacts-template-email)\n:PHONE:\n:ALIAS:\n:NOTE:\n:ADDRESS:\n:END:"
               :empty-lines 1))))

(setq org-refile-targets (quote (;("~/Dropbox/org/gtd.org" :maxlevel . 3)
                                 ("~/Dropbox/org/gtd2.org" :maxlevel . 3)
                                 ("~/Dropbox/org/someday.org" :level . 1)
                                 ("~/Dropbox/org/notes.org" :maxlevel . 2))))

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-startup-indented t)

(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-span 7)
(setq org-reverse-note-order t)
(setq org-deadline-warning-days 21)
(setq org-agenda-show-all-dates t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (latex . t)
   (ledger . t)
   (python . t)
   (shell . t)
   (clojure . t)))

(setq org-tags-exclude-from-inheritance '("PROJECT"))

(defun da/org-skip-subtree-if-priority (priority)
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun da/is-project-p ()
  "Action if the :PROJECT: tag."
  (save-restriction
    (widen)
    (member "PROJECT" (org-get-tags-at))))

(defun da/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (da/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil))
        next-headline))))

(setq org-agenda-custom-commands
      '((" " "Agenda"
         ((agenda "" nil)
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          (tags-todo "-CANCELLED/!NEXT"
                     ((org-agenda-overriding-header "Next Tasks")
                      (org-tags-match-list-sublevels t)))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting On")))))

        ("F" "Focus"
         ((tags-todo "PRIORITY=\"A\"/-WAITING"
                ((org-agenda-overriding-header "High-priority incomplete tasks:")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'TODO 'DONE 'CANCELLED))))
          (agenda ""
                  ((org-agenda-span 1)))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting on:")))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next actions:")
                 (org-agenda-skip-function '(da/org-skip-subtree-if-priority ?A))))))

        ("W" "Weekly Review"
         ((tags-todo "PRIORITY=\"A\"/-WAITING"
                     ((org-agenda-overriding-header "High-priority incomplete tasks:")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'TODO 'DONE 'CANCELLED))))
          (agenda "" ((org-agenda-span 7)))
          (tags "REFILE"
                ((org-agenda-overriding-header "Inbox:")
                 (org-tags-match-list-sublevels nil)))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting on:")))
          (tags-todo "+PROJECT/-DONE-CANCELLED"
                     ((org-agenda-overriding-header "Active projects:")
                      (org-tags-match-list-sublevels nil)))
          (tags-todo "+PROJECT/-DONE-CANCELLED"
                     ((org-agenda-overriding-header "Stuck projects:")
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-skip-function 'da/skip-non-stuck-projects)
                      (org-agenda-sorting-strategy '(category-keep))))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next actions:")
                 (org-agenda-skip-function '(da/org-skip-subtree-if-priority ?A))))))))

(setq org-confirm-babel-evaluate nil)

(defun da/org-agenda-save-on-quit ()
  (interactive)
  (org-save-all-org-buffers)
  (org-agenda-quit))

(eval-after-load 'org-agenda
  '(define-key org-agenda-mode-map (kbd "q") 'da/org-agenda-save-on-quit))

(provide 'init-org)

;;; init-org.el ends here
