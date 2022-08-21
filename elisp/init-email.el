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

(use-package notmuch
  :ensure t
  :custom
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
  
  :config
  (global-unset-key (kbd "C-c m"))
  (global-set-key (kbd "C-c m c") 'notmuch-mua-new-mail)
  (global-set-key (kbd "C-c m m") 'notmuch))

(provide 'init-email)

;;; init-email.el ends here
