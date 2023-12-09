;;; init-denote.el --- Denote configuration.	-*- lexical-binding: t -*-

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
;; Denote configuration.
;;

;;; Code:

(use-package denote
  :ensure t
  :config
  (setq denote-directory "~/Dropbox/wiki")
  (setq denote-date-prompt-use-org-read-date t)
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (setq denote-prompts '(subdirectory title keywords))

  (defun journal ()
    "Create an entry tagged 'journal' with the date as its title.
If a journal for the date already exists, visit it.  If multiple
entries exist, prompt with completion for a choice between them."
    (interactive)
    (let* ((today (format-time-string "%A %e %B %Y"))
           (string (denote-sluggify today))
           (files (denote-directory-files-matching-regexp string)))
      (cond
       ((> (length files) 1)
        (find-file (completing-read "Select file: " files nil :require-match)))
       (files
        (find-file (car files)))
       (t
        (denote
         today
         '("journal")))))))

(provide 'init-denote)

;;; init-denote.el ends here
