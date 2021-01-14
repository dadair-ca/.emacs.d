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
  :ensure org-plus-contrib
  :bind ("<f5>" . org-agenda)
  :config
  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-modules 'org-checklist t)
  (setq org-directory "~/Dropbox/org")
  (setq org-agenda-files '("~/Dropbox/org/gtd.org"))
  (setq org-refile-targets org-agenda-files)
  (setq org-log-into-drawer t)
  (setq org-adapt-indentation nil
        org-hide-leading-starts nil
        org-odd-levels-only nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t))))

(provide 'init-org)

;;; init-org.el ends here
