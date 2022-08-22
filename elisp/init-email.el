;;; init-email.el --- Email setup.	-*- lexical-binding: t -*-

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
;; Email setup.
;;

;;; Code:

(use-package smtpmail
  :ensure t
  :config
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-stream-type 'ssl)
  (setq smtpmail-queue-mail nil)
  (setq send-mail-function 'smtpmail-send-it)
  (setq smtpmail-debug-info t))

(setq da/unread-mail-indicator nil)

(defun da/get-unread-mail ()
  "Get unread mail."
  (string-trim
   (shell-command-to-string
    "notmuch count tag:unread")))

(defun da/set-unread-mail-indicator ()
  "Show unread mail in the mode line."
  (let* ((count (da/get-unread-mail))
         (indicator (format "@%s " count))
         (old-indicator da/unread-mail-indicator))
    (when old-indicator
      (setq global-mode-string (delete old-indicator global-mode-string)))
    (cond
     ((>= (string-to-number count) 1)
      (setq global-mode-string (push indicator global-mode-string))
      (setq da/unread-mail-indicator indicator))
     (t
      (setq da/unread-mail-indicator nil)))))

(use-package notmuch
  :ensure t
  :custom
  (mail-user-agent 'message-user-agent)
  (message-mail-user-agent t)
  (message-directory "~/Mail")
  ;; Search
  (notmuch-saved-searches
   `(( :name "inbox"
       :query "tag:inbox"
       :sort-order newest-first
       :key ,(kbd "i"))
     ( :name "unread (all)"
       :query "tag:unread not tag:archived"
       :sort-order newest-first
       :key ,(kbd "U"))
     ( :name "unread (inbox)"
       :query "tag:unread and tag:inbox"
       :sort-order newest-first
       :key ,(kbd "u"))
     ( :name "github"
       :query "tag:github not tag:archived"
       :sort-order newest-first
       :key ,(kbd "G"))))
  (notmuch-search-oldest-first nil)
  (notmuch-search-result-format
   '(("date" . "%12s  ")
     ("count" . "%-7s  ")
     ("authors" . "%-20s  ")
     ("subject" . "%-80s  ")
     ("tags" . "(%s)")))
  ;; UI
  (notmuch-show-relative-dates t)
  (notmuch-show-logo nil)
  (notmuch-column-control 1.0)
  (notmuch-hello-auto-refresh t)
  (notmuch-hello-recent-searches-max 20)
  ;; Reading
  (notmuch-message-headers-visible t)
  (notmuch-message-headers '("To" "Cc" "Subject" "Date"))
  ;; Composing
  (notmuch-mua-compose-in 'current-window)
  (notmuch-always-prompt-for-sender t)
  ;; Tags
  (notmuch-archive-tags '("-inbox" "-unread" "+archived"))
  (notmuch-message-replied-tags '("+replied"))
  (notmuch-message-forwarded-tags '("+forwarded"))
  (notmuch-show-mark-read-tags '("-unread"))
  (notmuch-draft-tags '("+draft"))
  (notmuch-draft-folder "drafts")
  (notmuch-fcc-dirs '()) ;; appended to from init-neo.el
  :config
  (global-unset-key (kbd "C-c m"))
  (global-set-key (kbd "C-c m c") 'notmuch-mua-new-mail)
  (global-set-key (kbd "C-c m m") 'notmuch)
  (run-at-time t 60 #'da/set-unread-mail-indicator))

(provide 'init-email)

;;; init-email.el ends here
