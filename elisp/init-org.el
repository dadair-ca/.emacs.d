;;; init-org.el --- Orgmode configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2025 David Adair

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
  :defer t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
	 ("C-c l" . org-store-link))
  :config
  (set-face-attribute 'org-tag nil :foreground "Grey" :weight 'thin) ;; subdue tags
  :init
  (setq org-directory "~/git/org")
  (setq org-agenda-files '("~/git/org/todo.org" "~/git/org/refile.org" "~/git/org/neo.org"))
  (setq org-default-notes-file "~/git/org/refile.org")
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
      (file "~/git/org/refile.org")
      "* TODO %?
%a")
     ("r" "Task (from region)" entry
      (file "~/git/org/refile.org")
      "* TODO %i%?
%a")
     ("h" "Habit" entry
      (file "~/git/org/refile.org")
      "* TODO %? :habit:
SCHEDULED: <%<%Y-%m-%d %a .+1d>>
:PROPERTIES:
:STYLE: habit
:END:"))))

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

(defadvice org-agenda (around fit-windows-for-agenda activate)
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

(provide 'init-org)
