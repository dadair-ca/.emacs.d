;;; init-elfeed.el --- Elfeed configuration.	-*- lexical-binding: t -*-

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
;; Elfeed configuration.
;;

;;; Code:

(use-package elfeed
  :ensure t
  :custom
  (elfeed-feeds
   '("https://planet.emacslife.com/atom.xml"
     "https://ag91.github.io/rss.xml"))
  :config
  (global-set-key (kbd "C-x w") 'elfeed))

(provide 'init-elfeed)

;;; init-elfeed.el ends here
