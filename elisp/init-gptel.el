;;; init-gptel.el --- gptel configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2025 David Adair

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
;; gptel configuration.
;;

;;; Code:

(use-package gptel
  :ensure t
  :bind
  (("C-c u" . gptel-menu)
   :prefix-map
   gptel-cmd-map
   :prefix "C-x c"
   ("x" . gptel-abort)
   ("RET" . gptel-send)
   ("<return>" . gptel-send))
  :hook
  (gptel-mode . turn-off-auto-fill)
  :custom
  (gptel-cache t)
  (gptel-default-mode 'org-mode)
  (gptel-log-level 'info)
  (gptel-use-header-line t)
  ;;(gptel-org-branching-context t) ;; Requires
  (gptel-prompt-prefix-alist
   '((org-mode . "*Prompt*: ")))
  (gptel-response-prefix-alist
   '((org-mode . "*Response*:\n")))
  (gptel-directives
   '((default . "You are a large language model living in Emacs and a helpful assistant.  Respond concisely.")
     (product . "You are an expert product management assistant specializing in fintech, with in-depth knowledge of behavioral economics, cognitive biases, and the Canadian fintech landscape, as well as insights into global players like Nubank, Monzo, and Revolut. Assist me in optimizing product strategy, user experience, and market positioning, while offering data-driven recommendations and addressing potential consumer biases.  Respond concisely."))))

(use-package gptel-transient
  :after (gptel)
  :config
  ;; Patch the transient menu so it accept <return> instead of RET.
  (when (ignore-errors (transient-get-suffix 'gptel-menu "RET"))
    (transient-suffix-put 'gptel-menu "RET" :key "<return>"))
  (when (ignore-errors (transient-get-suffix 'gptel-tools "RET"))
    (transient-suffix-put 'gptel-tools "RET" :key "<return>")))

;; TODO
;; (use-package gptel-prompts
;;   :after (gptel)
;;   :ensure t
;;   :demand t
;;   :config
;;   (gptel-prompts-update)
;;   (gptel-prompts-add-update-watchers))

(provide 'init-gptel)

;;; init-gptel.el ends here
