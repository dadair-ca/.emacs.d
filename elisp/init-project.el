;;; init-project.el --- Configure project.el.	-*- lexical-binding: t -*-

;; Copyright (C) 2021 David Adair

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
;; Configure project.el.
;;

;;; Code:

;;;###autoload
(defun da/project-magit-status ()
  "Run `magit-status' on current project."
  (interactive)
  (let* ((pr (nthcdr 2 (project-current t)))
         (dir (car pr)))
    (magit-status (directory-file-name dir))))

(use-package project
  :config
  (setq project-switch-commands
        '((?f "File" project-find-file)
          (?g "Grep" project-find-regexp)
          (?d "Dired" project-dired)
          (?b "Buffer" project-switch-to-buffer)
          (?r "Query replace" project-query-replace-regexp)
          (?m "Magit" da/project-magit-status)
          (?v "VC" project-vc-dir)
          (?e "Eshell" project-eshell)))
  (define-key global-map (kbd "C-x p m") #'da/project-magit-status))

(provide 'init-project)

;;; init-project.el ends here
