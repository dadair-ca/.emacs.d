;;; config-path.el --- Path constants -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2025-2026, David Adair <adair.david@gmail.com>
;;
;; Author: David Adair <adair.david@gmail.com>
;; Maintainer: David Adair <adair.david@gmail.com>
;;
;; Created: 20 Dec 2025
;;
;; URL: https://github.com/dadair-ca/.emacs.d/tree/master
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Defines path constants for Emacs directories (config, cache, packages)
;; and external locations (home, cloud storage, projects). These paths are
;; used throughout the configuration to ensure consistent file placement.
;;
;;; Code:

(defconst path-home-dir (file-name-as-directory (getenv "HOME"))
  "Path to user home directory; i.e., $HOME.")

(defconst path-local-dir
  (concat (file-name-as-directory (concat path-home-dir ".cache")) "emacs/")
  "The root directory for local emacs files.

Use this as permanent storage for files that are safe to share
 across systems.")

(defconst path-cache-dir (concat path-local-dir "cache/")
  "Directory for volatile storage.

Use this for files that change often, like cache files.")

(defconst path-backup-dir (concat path-cache-dir "backups/")
  "Directory for backup files.")

;; Create backup directory if it doesn't exist
(unless (file-exists-p path-backup-dir)
  (make-directory path-backup-dir t))

;; Store all backup files in the cache backup directory
(setq backup-directory-alist `(("." . ,path-backup-dir)))

;; Also move auto-save files to the cache directory
(setq auto-save-file-name-transforms
      `((".*" ,path-backup-dir t)))

(defconst path-git-dir
  (file-name-as-directory (expand-file-name "git" path-home-dir))
  "Path to user git directory; i.e., $HOME/git/.")

(provide 'config-path)
;;; config-path.el ends here
