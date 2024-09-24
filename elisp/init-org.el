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
  :ensure t
  :config
  (add-to-list 'org-modules 'org-habit t)
  (setq org-id-link-to-org-use-id t)
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
          (todo . "  %-11c%?-12t% s")
          (tags . "  %-11c%?-12t% s")
          (search . "  %-11c%?-12t% s")))
  (setq
   org-capture-templates
   '(("n" "New" entry (file "~/org/refile.org")
      "* %?
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:END:
%a"
      :prepend t)
     ("h" "Habit" entry (file "~/org/refile.org")
      "* TODO %?
SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:STYLE: habit
:REPEAT_TO_STATE: TODO
:END:"
      :prepend t))

   ;; org-roam-capture-templates
   ;; '(("o" "1:1" entry
   ;;    (file "~/org/templates/o3.org")
   ;;    :target (file+datetree nil 'day)
   ;;    :jump-to-captured t))

   org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %U %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))
     ("m" "meeting" entry
      (file "~/org/templates/meeting.org")
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n")
      :jump-to-captured t)))
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
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org/gtd.org" "~/org/neo.org" "~/org/refile.org"))
  (setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 3) ("~/org/neo.org" :maxlevel . 3) ("~/org/refile.org" :level . 0)))
  (setq org-todo-keywords '((sequence
                             "TODO(t)"
                             "|"
                             "DONE(d!)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
  (setq org-todo-keyword-faces '(("TODO" . lin-yellow)
                                 ("DONE" . lin-green)
                                 ("WAITING" . lin-red)
                                 ("HOLD" . lin-magenta)
                                 ("CANCELLED" . lin-green)))
  (setq org-tag-persistent-alist '(("FLAGGED" . ?f)))
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-agenda-repeating-timestamp-show-all t)
  (setq org-agenda-sorting-strategy
        (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
                (todo category-up effort-up)
                (tags category-up effort-up)
                (search category-up))))
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-span 'day)
  (setq org-agenda-hide-tags-regexp "REFILE")
  (setq org-default-priority ?C)
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
            (tags-todo "+PRIORITY=\"A\"-TODO=\"WAITING\"" ((org-agenda-overriding-header "\n❗ Now")))
            (tags "REFILE"
                  ((org-agenda-overriding-header "\n📥 Inbox")
                   (org-tags-match-list-sublevels nil)))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\n✅ Completed Today")))))))
  (setq org-confirm-babel-evaluate nil)
  (setq org-startup-indented t)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-switchb)

(defun da/org-agenda-save-on-quit ()
  (interactive)
  (org-save-all-org-buffers)
  (org-agenda-quit))

(defadvice org-agenda (around fit-windows-for-agenda activate)
  "Slurps Drafts App notes saved in Google Drive into TODO task for refiling."
  (let ((notes
         (ignore-errors
           (directory-files
            "~/Library/CloudStorage/GoogleDrive-adair.david@gmail.com/My Drive"
            t "[0-9].*\\.txt\\'" nil))))
    (when notes
      (with-current-buffer (find-file-noselect "~/org/refile.org")
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

(require 'org-protocol)

(use-package org-download :ensure t)

(provide 'init-org)

;;; init-org.el ends here
