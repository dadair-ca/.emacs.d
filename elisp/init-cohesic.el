;;; init-cohesic.el --- Initialize Cohesic-specific configuration.	-*- lexical-binding: t -*-

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
;; Cohesic-specific Emacs configuration.
;;

;;; Code:

(defvar sql-postgres-login-params
  '((user :default "acuity_user")
    (database :default "appserver")
    (server :default "localhost")
    (port :default 5432)))

(defun acuity-workspace ()
  "Load the Acuity workspace, with preconfigured windows and terminals."
  (interactive)
  (delete-other-windows)
  (find-file "~/git/cohesic/acuity")
  (split-window-horizontally)
  (split-window-below)
  (other-window 2)
  (split-window-below)
  (other-window 2)

  ;; SQL
  (multi-term)
  (rename-buffer "*term-sql*")
  (comint-send-string "*term-sql*" "./scripts/start-services.sh sql\n")

  ;; WEB
  (other-window 1)
  (multi-term)
  (rename-buffer "*term-web*")
  (comint-send-string "*term-web*" "./scripts/start-services.sh web\n")

  ;; API
  (other-window 1)
  (multi-term)
  (rename-buffer "*term-api*")
  (comint-send-string "*term-api*" "./scripts/start-services.sh api\n")

  ;; Appserver
  (other-window 1)
  (find-file "~/git/cohesic/acuity/appserver/resources/appserver/config.edn")
  (cider-jack-in '(:project-dir "~/git/cohesic/acuity/appserver")))

(provide 'init-cohesic)

;;; init-cohesic.el ends here
