;;; init.el --- Emacs configurations.	-*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2019 David Adair

;; Author: David Adair <adair.david@gmail.com>
;; URL: https://github.com/adairdavid/.emacs.d
;; Version: 0.0.1
;; Keywords: .emacs.d

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
;; My Emacs configuration.
;;

;;; Code:

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

;; Speed up startup
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (if (boundp 'after-focus-change-function)
                 (add-function :after after-focus-change-function
                               (lambda ()
                                 (unless (frame-focus-state)
                                   (garbage-collect))))
               (add-hook 'focus-out-hook 'garbage-collect))))

;; Load path
(defun update-load-path (&rest _)
  "Update LOAD-PATH."
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "elisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to the LOAD-PATH."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

(setq comp-async-report-warnings-errors nil)

(setq inhibit-splash-screen t)

(require 'init-package)
(require 'init-custom)
(require 'init-pass)

(require 'init-alert)
(require 'init-basic)

(use-package company :ensure t)

;; (use-package treemacs :ensure t)

(require 'init-ace)
;; (require 'init-clojure)
(require 'init-dashboard)
(require 'init-devops)
;; (require 'init-diary)
(require 'init-dired)
(require 'init-ediff)
(require 'init-editing)
(require 'init-elfeed)
(require 'init-email)
(require 'init-funs)
(require 'init-keycast)
(require 'init-lsp)
;; (require 'init-mct)
(require 'init-mongosh)
(require 'init-org)
(require 'init-org-roam)
(require 'init-path)
(require 'init-prog)
(require 'init-project)
;; (require 'init-python)
(require 'init-search)
(require 'init-sql)
(require 'init-ui)
(require 'init-vcs)
(require 'init-web)

(when (locate-library "init-neo")
  (message "Loading Neo configuration...")
  (require 'init-neo))

(defun da/idle-function ()
  "My idle timer function."
  (interactive)
  (da/load-random-theme)
  (org-agenda nil "N"))

(run-with-idle-timer (* 5 60) nil 'da/idle-function)

;;; init.el ends here
