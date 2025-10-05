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
      (file "~/org/refile.org")
      "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
%a")
     ("r" "Task (from region)" entry
      (file "~/org/refile.org")
      "* TODO %i%?
:PROPERTIES:
:CREATED: %U
:END:
%a")
     ("h" "Habit" entry
      (file "~/org/refile.org")
      "* TODO %? :habit:
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


(provide 'init-org)
