;;; init-lsp.el --- Initialize LSP package and features.	-*- lexical-binding: t -*-

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
;; Initialize LSP package and features.
;;

;;; Code:

(use-package lsp-mode
  :hook ((typescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :custom
  (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
  (lsp-response-timeout 30)
  (lsp-enable-file-watchers nil)
  :config
  ;(setq read-process-output-max (* 1024 1024))
  (global-unset-key (kbd "C-x l"))
  (define-key global-map (kbd "C-x l r") 'lsp-find-references)
  (define-key global-map (kbd "C-x l R") 'lsp-ui-peek-find-references)
  (define-key global-map (kbd "C-x l d") 'lsp-find-definition)
  (define-key global-map (kbd "C-x l D") 'lsp-ui-peek-find-definitions)
  (define-key global-map (kbd "C-x l i") 'lsp-find-implementation)
  (define-key global-map (kbd "C-x l I") 'lsp-ui-peek-find-implementation)
  (define-key global-map (kbd "C-x l e") 'lsp-ui-flycheck-list)
  )

(use-package lsp-ui
  :commands lsp-ui-mode)

(provide 'init-lsp)

;;; init-lsp.el ends here
