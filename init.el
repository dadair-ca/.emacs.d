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

;;;; Preamble

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defun update-load-path (&rest _)
  "Update LOAD-PATH."
  (push (expand-file-name "elisp" user-emacs-directory) load-path))

(advice-add #'package-initialize :after #'update-load-path)

(update-load-path)

(use-package gptel
  :bind (("C-c g RET" . gptel-send)
	 ("C-c g /" . gptel))
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model 'gpt-4o)
  (setq gptel-include-reasoning "*Reasoning*")
  (setq
   gptel-directives
   '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
     (product . "You are a very senior product manager at a Canadian D2C fintech.  Respond concisely.")
     (strategy . "You are an expert product strategist with deep knowledge of Canadian D2C fintechs and Canadian provincial and federal regulations and laws.  Challenge my assumptions and ask probing questions to refine strategy.  Repond concidely.")
     (writing . "You are a large language model and a writing assistant. Respond concisely.")
     )))

(use-package howm
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(pixel-scroll-precision-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 140 :width normal :foundry "nil" :family "Menlo")))))

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(which-key howm)))
