;;; init-basic.el --- Initialize basic configuration.	-*- lexical-binding: t -*-

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
;; Initialize basic configuration.
;;

;;; Code:

(setq user-full-name "David Adair")
(setq user-mail-address "adair.david@gmail.com")

(defvar epa-pinentry-mode 'loopback)

(eval-and-compile
  (defun lookup-password (host user port)
    (require 'auth-source)
    (require 'auth-source-pass)
    (let ((auth (auth-source-search :host host :user user :port port)))
      (if auth
          (let ((secretf (plist-get (car auth) :secret)))
            (if secretf
                (funcall secretf)
              (error "Auth entry for %s@%s:%s has no secret!"
                     user host port)))
        (error "No auth entry found for %s@%s:%s" user host port)))))

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
    (setq exec-path (append exec-path '("/usr/local/bin")))
    (exec-path-from-shell-initialize)))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package saveplace
  :hook (after-init . save-place-mode)
  :init
  (defvar save-place-file (concat user-emacs-directory "places")))

(use-package recentf
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          ".elfeed"
                          "bookmarks"
                          "cache"
                          "persp-confs"
                          "recentf"
                          "url"
                          "COMMIT_EDITMSG\\'")))

(setq ring-bell-function 'ignore)

(defvar x-select-enable-clipboard t)
(defvar x-select-enable-primary t)

(setq save-interprogram-paste-before-kill t)

(defvar apropos-do-all t)

(setq mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(setenv "GPG_AGENT_INFO" nil)

(setq disabled-command-function nil)

(global-set-key (kbd "s-m") nil)
(global-set-key (kbd "s-k") nil)

(setq ns-function-modifier 'hyper)

(use-package abbrev
  :ensure f
  :defer 5
  :diminish
  :hook ((text-mode prog-mode erc-mode) . abbrev-mode)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(use-package ag)

(use-package avy
  :bind (("M-g g" . avy-goto-char)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

(use-package ledger-mode
  :mode "\\.ledger\\'")

(provide 'init-basic)

;;; init-basic.el ends here
