;;; init-clojure.el --- Clojure configuration.	-*- lexical-binding: t -*-

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
;; Clojure configuration.
;;

;;; Code:

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package clojure-mode
  :mode ("\\.clj[scx]?\\'" "\\.edn\\'"))

(use-package cider
  :after (clojure-mode)
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (setq cider-font-lock-dynamically '(macro core function var))
  (setq cider-auto-test-mode t))

(use-package clj-refactor
  :after (clojure-mode)
  :bind (:map clojure-mode-map
              ("C-c o" . cljr-clean-ns))
  :hook (clojure-mode . my-clojure-mode-hook)
  :config
  (setq cljr-favor-prefix-notation nil))

(use-package flycheck-clj-kondo
  :mode ("\\.clj[scx]?\\'"))

(provide 'init-clojure)

;;; init-clojure.el ends here
