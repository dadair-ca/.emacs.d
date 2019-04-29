;;; init-editing.el --- Editing configuration.	-*- lexical-binding: t -*-

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
;; General editing configuration.
;;

;;; Code:

(setq my-column-limit 100)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(setq require-final-newline t)
(delete-selection-mode t)

(setq tab-always-indent 'complete)

(setq-default fill-column my-column-limit)
(auto-fill-mode t)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "C-c s") 'sort-lines)

(global-auto-revert-mode t)

(use-package guru-mode
  :diminish guru-mode
  :init (add-hook 'prog-mode-hook 'guru-mode))

(setq whitespace-line-column my-column-limit
      whitespace-style '(face line-tail))

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :hook (after-init-hook . global-whitespace-mode)
  :commands (whitespace-buffer.
             whitespace-cleanup
             whitespace-mode))

(provide 'init-editing)

;;; init-editing.el ends here
