2;;; init-org.el --- Orgmode configuration.	-*- lexical-binding: t -*-

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
  (load "org-settings"))

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/notes.org")
(setq org-agenda-files
      (quote ("~/Dropbox/org/gtd.org"
              "~/Dropbox/org/refile.org"
              "~/Dropbox/org/neo.org.gpg"
              "~/Dropbox/org/notes.org")))
(setq org-refile-targets
      '(("~/Dropbox/org/gtd.org" :maxlevel . 3)
        ("~/Dropbox/org/neo.org.gpg" :maxlevel . 3)
        ("~/Dropbox/org/refile.org" :level . 1)
        ("~/Dropbox/org/notes.org" :maxlevel . 2)))

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

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (latex . t)
   (ledger . t)
   (python . t)
   (shell . t)))

(setq org-tags-exclude-from-inheritance '("PROJECT"))

(defun my-org-startup ()
  (org-agenda-list))

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

(defun da/is-project-p ()
  "Action if the :PROJECT: tag."
  (save-restriction
    (widen)
    (member "PROJECT" (org-get-tags))))

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
                (unless (member "WAITING" (org-get-tags))
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
         ((agenda ""
                  ((org-agenda-span 1)))
          (tags-todo "PRIORITY=\"A\"/-WAITING"
                     ((org-agenda-overriding-header "High-priority incomplete tasks:")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'TODO 'DONE 'CANCELLED))))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting on:")))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next actions:")
                 (org-agenda-skip-function '(da/org-skip-subtree-if-priority ?A))))))

        ("W" "Weekly Review"
         ((agenda "" ((org-agenda-span 7)))
          (tags-todo "PRIORITY=\"A\"/-WAITING"
                     ((org-agenda-overriding-header "High-priority incomplete tasks:")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'TODO 'DONE 'CANCELLED))))
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

(defadvice org-agenda (around fit-windows-for-agenda activate)
  "Fit the Org Agenda to its buffer."
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
             "* TODO "
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
                   (insert (format (concat "SCHEDULED: <%s>\n"
                                           ":PROPERTIES:\n"
                                           ":ID: %s\n"
                                           ":CREATED: ")
                                   date uuid))
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

(provide 'init-org)

;;; init-org.el ends here
