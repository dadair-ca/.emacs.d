;;; init-web.el --- Web development configuration.	-*- lexical-binding: t -*-

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
;; Web development configuration.
;;

;;; Code:

(use-package emmet-mode
  :bind (("C-c x" . emmet-expand-line))
  :hook ((typescript-mode . emmet-mode)))

(use-package prettier-js
  :hook (typescript-mode . prettier-js-mode))

(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :config
  (setq typescript-indent-level 2)
  (flycheck-add-mode 'typescript-tslint 'typescript-mode)
  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled)))

(use-package tide
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

(use-package web-mode
  :hook ((typescript-mode . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
        web-mode-attr-indent-offset 2

        web-mode-enable-current-column-highlight t
        web-mode-enable-auto-quoting nil
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t))

(provide 'init-web)

;;; init-web.el ends here
