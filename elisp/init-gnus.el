;;; init-gnus.el --- GNUs configuration.	-*- lexical-binding: t -*-

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
;; GNUs configuration.
;;

;;; Code:

(use-package bbdb
  :config
  (setq bbdb-file "~/Dropbox/Documents/Relationships/bbdb")
  :init
  (calendar-set-date-style 'iso)
  (bbdb-initialize 'gnus 'message 'anniv)
  (bbdb-mua-auto-update-init 'message)
  (setq bbdb-mua-auto-update-p 'query)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))

(use-package gnus
  :config
  (global-set-key (kbd "<f6>") 'gnus)
  (setq gnus-select-method '(nnml ""))
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (setq gnus-always-read-dribble-file t)
  (setq gnus-asynchronous t)
  (setq gnus-use-cache t)
  (setq gnus-secondary-select-methods '((nntp "news.gmane.org")
                                        (nntp "news.gwene.org")))
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "personal"
                        (nnimap-address "imap.gmail.com")
                        (nnimap-server-port "imaps")
                        (nnimap-stream ssl)
                        (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                        (nnmail-expiry-wait immediate)))
  (setq gnus-posting-styles '((".*" (signature (string-join '("David Adair" "adair.david@gmail.com") "\n")))))
  (setq gnus-message-archive-group nil)
  (setq smtpmail-smtp-service 587
        smtpmail-smtp-user "adair.david@gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        send-mail-function 'smtpmail-send-it))

(provide 'init-gnus)

;;; init-gnus.el ends here
