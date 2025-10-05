;;; init-org-roam.el --- Org Roam configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2024 David Adair

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
;; Org Roam configuration.
;;

;;; Code:

(use-package org-roam
  :ensure t
  :config
  (setq org-roam-directory (file-truename "~/git/org"))
  (org-roam-db-autosync-mode))

(use-package org-roam-dailies
  :after org-roam
  :demand t
  :bind
  (("C-c n D" . org-roam-dailies-capture-today)
   ("C-c n d" . org-roam-dailies-map)
   :map
   org-roam-dailies-map
   ("Y" . org-roam-dailies-capture-yesterday)
   ("T" . org-roam-dailies-capture-tomorrow))
  :custom
  (org-roam-dailies-directory "daily/")
  :functions
  (org-roam-dailies-capture-tomorrow
   org-roam-dailies-capture-yesterday
   org-roam-dailies-capture-today
   org-roam-dailies--capture))

(use-package org-roam-ui :ensure t)

(provide 'init-org-roam)

;;; init-org-roam.el ends here
