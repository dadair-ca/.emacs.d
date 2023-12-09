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

(defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result)
    (cond
                                        ; time specific items are already sorted first by org-agenda-sorting-strategy

                                        ; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

                                        ; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

                                        ; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '> a b))

                                        ; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

                                        ; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

                                        ; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

                                        ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
    ; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
    ; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
    ; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ; if none match leave them unsorted
    (t nil)))

(defmacro bh/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun bh/is-not-scheduled-or-deadline (date-str)
  (and (not (bh/is-deadline date-str))
       (not (bh/is-scheduled date-str))))

(defun bh/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun bh/is-late-deadline (date-str)
  (string-match "\\([0-9]*\\) d\. ago:" date-str))

(defun bh/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun bh/is-deadline (date-str)
  (or (bh/is-due-deadline date-str)
      (bh/is-late-deadline date-str)
      (bh/is-pending-deadline date-str)))

(defun bh/is-scheduled (date-str)
  (or (bh/is-scheduled-today date-str)
      (bh/is-scheduled-late date-str)))

(defun bh/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun bh/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :ensure t
  :config
  (add-to-list 'org-modules 'org-habit t)
  (setq org-stuck-projects '("" nil nil ""))
  (setq org-habit-show-all-today t)
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars nil)
  (setq org-M-RET-may-split-line '((headline . nil) (default . t)))
  (setq org-agenda-fontify-priorities t)
  (setq org-agenda-tags-column -102)
  (setq org-agenda-prefix-format
        '((agenda . "  %-11c%?-12t% s")
          (timeline . "  % s")
          (todo . "  %-11c%5(org-todo-age) [%e] ")
          (tags . "  %-11c%5(org-todo-age) [%e] ")
          (search . "  %-11c%5(org-todo-age) [%e] ")))
  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/Dropbox/org/refile.org")
           "* TODO %?
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:END:
%a"
           :prepend t)
          ("n" "Note" entry (file "~/Dropbox/org/refile.org")
           "* %? :NOTE:
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:END:"
           :prepend t)
          ("m" "Meeting" entry (file "~/Dropbox/org/refile.org")
           "* %U Meeting on \"%^{Subject}\" :NOTE:
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:END:
- Attendees
  - [X] David Adair
- Agenda
- Notes"
           :prepend t)
          ("h" "Habit" entry (file "~/Dropbox/org/refile.org")
           "* TODO %?
SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:STYLE: habit
:REPEAT_TO_STATE: TODO
:END:"
           :prepend t)
          ("p" "Project" entry (file "~/Dropbox/org/refile.org")
           "* %^{Project} [/] :prj:%^{tag}:
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:CATEGORY: %^{category}
:VISIBILITY: folded
:COOKIE_DATA: recursive todo
:END:
** Information
:PROPERTIES:
:VISIBILITY: folded
:END:
** Notes
:PROPERTIES:
:VISIBILITY: folded
:END:
** Tasks
:PROPERTIES:
:VISIBILITY: content
:END:"
           :prepend t)
          ("a" "Area" entry (file "~/Dropbox/org/refile.org")
           "* %^{Area} [/] :%^{tag}:
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:CATEGORY: %^{category}
:VISIBILITY: folded
:COOKIE_DATA: recursive todo
:END:
** Information
:PROPERTIES:
:VISIBILITY: folded
:END:
** Notes
:PROPERTIES:
:VISIBILITY: folded 
:END:
** Tasks
:PROPERTIES:
:VISIBILITY: content
:END:"
           :prepend t)))
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)
  (setq org-clock-into-drawer t)
  (setq org-habit-graph-column 60)
  (setq org-adapt-indentation nil)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-deadline-warning-days 14)
  (setq org-agenda-ndays 1)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-block-separator nil)
  (setq org-directory "~/Dropbox/org")
  (setq org-agenda-files '("~/Dropbox/org/gtd.org"
                           "~/Dropbox/org/refile.org"
                           "~/Dropbox/org/neo.org"))
  (setq org-refile-targets '(("~/Dropbox/org/gtd.org" :regexp . "\\(?:\\(?:Area\\|Note\\|Project\\|Task\\)s\\)")
                             ("~/Dropbox/org/neo.org" :regexp . "\\(?:\\(?:Area\\|Note\\|Project\\|Task\\)s\\)")
                             ("~/Dropbox/org/refile.org" :level . 0)))
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
  (setq org-todo-keyword-faces '(("TODO" . lin-yellow)
                                 ("NEXT" . lin-blue)
                                 ("DONE" . lin-green)
                                 ("WAITING" . lin-red)
                                 ("HOLD" . lin-magenta)
                                 ("CANCELLED" . lin-green)))
  (setq org-tag-persistent-alist '(("@home" . ?h) ("@office" . ?o)))
  (setq org-tags-exclude-from-inheritance '("prj"))
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-agenda-repeating-timestamp-show-all t)
  (setq org-agenda-cmp-user-defined 'bh/agenda-sort)
  (setq org-agenda-sorting-strategy
        (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
                (todo category-up effort-up)
                (tags category-up effort-up)
                (search category-up))))
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-span 'day)
  (setq org-agenda-hide-tags-regexp "NEO\\|PERSONAL\\|REFILE\\|prj")
  (setq org-agenda-custom-commands
        '(("g" "Get Things Done (GTD)"
           ((agenda ""
                    ((org-agenda-use-time-grid t)
                     (org-agenda-time-grid
                      (quote ((daily today require-timed)
                              (0800 1000 1200 1400 1600 1800)
                              "......" "----------------")))
                     (org-deadline-warning-days 14)))
            (todo "WAITING"
                  ((org-agenda-overriding-header "\n🤝️ Waiting On")
                   (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "+PRIORITY=\"A\""
                       ((org-agenda-overriding-header "\n❗ High Priority")))
            (tags-todo "-PRIORITY=\"A\"/NEXT"
                  ((org-agenda-overriding-header "\n🔎 Focus Tasks")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   ;;(org-agenda-prefix-format "  %i %-12c [%e] ")
                   ))
            (tags "REFILE"
                  ((org-agenda-overriding-header "\n📥 Inbox")
                   (org-agenda-prefix-format "  %?-12t% s")
                   (org-tags-match-list-sublevels nil)))
            ;; WIP - want to focus to some weeks ahead
            ;; (tags "DEADLINE>=\"<today>\""
            ;;       ((org-agenda-overriding-header "\n Upcoming Deadlines")))
            ;;
            (tags "prj"
                  ((org-agenda-overriding-header "\n📁 Projects")
                   (org-agenda-prefix-format "  %?-12t% s")))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\n✅ Completed Today")))))))
  (setq org-confirm-babel-evaluate nil)
  (setq org-startup-indented t)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-switchb)

(defun org-todo-age-time (&optional pos)
  (let ((stamp (org-entry-get (or pos (point)) "CREATED" t)))
    (when stamp
      (time-subtract (current-time)
                     (org-time-string-to-time
                      (org-entry-get (or pos (point)) "CREATED" t))))))

(defun org-todo-age (&optional pos)
  (let ((days (time-to-number-of-days (org-todo-age-time pos))))
    (cond
     ((< days 1)   "today")
     ((< days 7)   (format "%dd" days))
     ((< days 30)  (format "%.1fw" (/ days 7.0)))
     ((< days 358) (format "%.1fM" (/ days 30.0)))
     (t            (format "%.1fY" (/ days 365.0))))))

(defun da/org-skip-subtree-if-priority (priority)
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun da/org-agenda-save-on-quit ()
  (interactive)
  (org-save-all-org-buffers)
  (org-agenda-quit))

(defadvice org-agenda (around fit-windows-for-agenda activate)
  "Slurps Drafts App notes saved in Dropbox into TODO task for refiling."
  (let ((notes
         (ignore-errors
           (directory-files
            "~/Dropbox"
            t "[0-9].*\\.txt\\'" nil))))
    (when notes
      (with-current-buffer (find-file-noselect "~/Dropbox/org/refile.org")
        (save-excursion
          (goto-char (point-min))
          (forward-line 2)
          (dolist (note notes)
            (insert
             "* "
             (with-temp-buffer
               (insert-file-contents note)
               (goto-char (point-min))
               (forward-line)
               (unless (bolp))
               (insert ?\n)
               (goto-char (point-max))
               (unless (bolp)
                 (insert ?\n))
               (let ((uuid (substring (shell-command-to-string "uuidgen") 0 -1))
                     (file (file-name-nondirectory note)))
                 (string-match
                  (concat "\\`\\([0-9]\\{4\\}\\)"
                          "-\\([0-9]\\{2\\}\\)"
                          "-\\([0-9]\\{2\\}\\)"
                          "-\\([0-9]\\{2\\}\\)"
                          "-\\([0-9]\\{2\\}\\)"
                          "-\\([0-9]\\{2\\}\\)"
                          "\\.txt\\'") file)
                 (let* ((year (string-to-number (match-string 1 file)))
                        (mon (string-to-number (match-string 2 file)))
                        (day (string-to-number (match-string 3 file)))
                        (hour (string-to-number (match-string 4 file)))
                        (min (string-to-number (match-string 5 file)))
                        (sec (string-to-number (match-string 6 file)))
                        (date (format "%04d-%02d-%02d %s"
                                      year mon day
                                      (calendar-day-name (list mon day year) t))))
                   (insert (format (concat ":PROPERTIES:\n"
                                           ":ID: %s\n"
                                           ":CREATED: ")
                                   uuid))
                   (insert (format "[%s %02d:%02d]\n:END:\n" date hour min))))
               (buffer-string)))
            (delete-file note t)))
        (when (buffer-modified-p)
          (save-buffer)))))
  ad-do-it)

(defun my-org-reset-checkbox-state-maybe ()
  "Reset all checkboxes in an entry if the `RESET_CHECK_BOXES' property is set"
  (interactive "*")
  (if (org-entry-get (point) "RESET_CHECK_BOXES")
      (org-reset-checkbox-state-subtree)))

(defun my-org-reset-checkbox-when-done ()
  (when (member org-state org-done-keywords) ;; org-state dynamically bound in org.el/org-todo
    (my-org-reset-checkbox-state-maybe)))

(add-hook 'org-after-todo-state-change-hook 'my-org-reset-checkbox-when-done)

(eval-after-load 'org-agenda
  '(define-key org-agenda-mode-map (kbd "q") 'da/org-agenda-save-on-quit))

(use-package org-noter
  :ensure t)

(require 'org-protocol)

(provide 'init-org)

;;; init-org.el ends here
