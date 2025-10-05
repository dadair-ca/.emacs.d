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
  :demand t
  :commands org-resolve-clocks
  :bind* (("C-c S" . org-store-link)
          ("C-c l" . org-insert-link))
  :bind (:map
         org-mode-map
         ([return] . (lambda () (interactive) (org-return t)))
         ([(control return)])
         ([(control meta return)] . org-insert-heading-after-current))
  :hook
  (org-mode             . turn-on-auto-fill)
  (org-log-buffer-setup . turn-on-auto-fill)
  :custom
  (org-id-link-to-org-use-id t)
  (org-stuck-projects '("" nil nil ""))
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars nil)
  (org-M-RET-may-split-line '((headline . nil) (default . t)))
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-clock-into-drawer t)
  (org-adapt-indentation nil)
  (org-deadline-warning-days 14)
  (org-extend-today-until 4)
  (org-global-properties
   '(("Effort_ALL" . "0:05 0:15 0:30 1:00 2:00 3:00 4:00 6:00 8:00")))
  (org-directory "~/git/org")
  (org-refile-targets '(("~/git/org/gtd.org" :maxlevel . 3) ("~/git/org/neo.org" :maxlevel . 3) ("~/git/org/refile.org" :level . 0)))
  (org-todo-keywords '((sequence
                        "TODO(t)"
                        "|"
                        "DONE(d!)")
                       (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
  (org-todo-keyword-faces '(("TODO" . diff-refine-changed)
                            ("DONE" . diff-refine-added)
                            ("WAITING" . diff-refine-removed)
                            ("HOLD" . diff-nonexistent)
                            ("CANCELLED" . diff-refine-added)))
  (org-tag-persistent-alist
   '(("Call" . 99)
     ("Errand" . 101)
     ("Home" . 104)))
  (org-tags-column -97)
  (org-use-fast-todo-selection t)
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-capture-templates
   '(("t" "Todo" entry
      (file "~/git/org/refile.org")
      "* TODO %?"
      :prepend t)
     ("h" "Habit" entry
      (file "~/git/org/refile.org")
      "* TODO %?
SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")
:PROPERTIES:
:STYLE: habit
:REPEAT_TO_STATE: TODO
:LOG_INTO_DRAWER: t
:END:"
      :prepend t)))

  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %U %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))
     ("m" "meeting" entry
      (file "~/git/org/template/meeting.org")
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n")
      :jump-to-captured t)))
  (org-confirm-babel-evaluate nil)
  (org-startup-indented t)
  (org-startup-folded 'show3levels)
  :preface
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
       ((< days 365) (format "%.1fM" (/ days 30.0)))
       (t            (format "%.1fY" (/ days 365.0)))))))

(use-package org-agenda
  :commands org-agenda-list
  :bind* ("C-c a" . org-agenda)
  :custom
  (org-agenda-custom-commands
   '((" " "Get Things Done (GTD)"
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
       (tags-todo "+PRIORITY=\"A\"-TODO=\"WAITING\"" ((org-agenda-overriding-header "\n❗ Now")))
       (tags "REFILE"
             ((org-agenda-overriding-header "\n📥 Inbox")
              (org-tags-match-list-sublevels nil)))
       (tags "CLOSED>=\"<today>\""
             ((org-agenda-overriding-header "\n✅ Completed Today")))))))
  (org-agenda-fontify-priorities t)
  (org-agenda-prefix-format
   '((agenda   . "%-10c%-5e%?-12t% s")
     (timeline . "% s")
     (todo     . "%-10c%5(org-todo-age) ")
     (tags     . "%-10c")))
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-slip-timestamp-if-done t)
  (org-agenda-skip-unavailable-files t)
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-sorting-strategy
   '((agenda habit-down time-up todo-state-up priority-down)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep)))
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday nil)
  (org-agenda-ndays 1)
  (org-agenda-tags-column -100)
  (org-agenda-show-all-dates t)
  (org-agenda-block-separator nil)
  (org-agenda-files '("~/git/org/gtd.org" "~/git/org/neo.org" "~/git/org/refile.org"))
  (org-agenda-repeating-timestamp-show-all t)
  (org-agenda-use-time-grid t)
  (org-agenda-hide-tags-regexp "REFILE"))

(use-package org-appear
  :ensure t
  :after org
  :hook (org-mode . org-appear-mode))

(use-package org-capture
  :after org
  :demand t
  :preface
  (defun my/org-basic-properties (&optional arg)
    (interactive "P")
    (save-excursion
      (org-id-get-create arg)
      (unless (org-entry-get (point) "CREATED")
        (org-entry-put (point) "CREATED"
                       (format-time-string (org-time-stamp-format t t))))))
  :config
  (add-hook 'org-capture-before-finalize-hook #'my/org-basic-properties))

(use-package org-habit
  :after org-agenda
  :custom
  (org-habit-preceding-days 42)
  (org-habit-today-glyph 45)
  (org-habit-graph-column 44))

(use-package ob
  :after org
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t))))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)

(defun da/org-agenda-save-on-quit ()
  (interactive)
  (org-save-all-org-buffers)
  (org-agenda-quit))

(defadvice org-agenda (around slurp-drafts activate)
  "Slurps Drafts App notes saved in Google Drive into TODO task for refiling."
  (let ((notes
         (ignore-errors
           (directory-files
            "~/Library/CloudStorage/GoogleDrive-adair.david@gmail.com/My Drive"
            t "[0-9].*\\.txt\\'" nil))))
    (when notes
      (with-current-buffer (find-file-noselect "~/git/org/refile.org")
        (save-excursion
          (goto-char (point-min))
          (forward-line 1)
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

(require 'org-protocol)

(use-package org-download :ensure t)

(provide 'init-org)

;;; init-org.el ends here
