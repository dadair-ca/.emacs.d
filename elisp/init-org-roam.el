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
  (setq org-roam-directory (file-truename "~/org/roam"))
  (org-roam-db-autosync-mode))

(use-package org-roam-ui :ensure t)

(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n l") 'org-roam-node-insert)
(global-set-key (kbd "C-c n l") 'org-roam-capture)
(global-set-key (kbd "C-c n u") 'org-roam-ui-open)

(global-set-key (kbd "C-c n d t") (lambda () (interactive) (org-roam-dailies-capture-today nil "d")))
(global-set-key (kbd "C-c n d y") (lambda () (interactive) (org-roam-dailies-capture-yesterday 1 "d")))
(global-set-key (kbd "C-c n d m") (lambda () (interactive) (org-roam-dailies-capture-tomorrow 1 "d")))
(global-set-key (kbd "C-c n d d") (lambda () (interactive) (org-roam-dailies-capture-date t "d")))

(provide 'init-org-roam)

;;; init-org-roam.el ends here
