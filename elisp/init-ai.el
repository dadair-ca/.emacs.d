;;; init-ai.el --- AI packages & configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2025-2026, David Adair <adair.david@gmail.com>
;;
;; Author: David Adair <adair.david@gmail.com>
;; Maintainer: David Adair <adair.david@gmail.com>
;;
;; Created: 20 Dec 2025
;;
;; URL: https://github.com/dadair-ca/.emacs.d/tree/master
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Installs and configures AI tools.
;;
;;; Code:

(require 'config-path)

;; Set Netskope CA certificates for API requests
(let ((ca-bundle (expand-file-name "~/.neonetskope/nscacert_combined.pem")))
  (when (file-exists-p ca-bundle)
    (setenv "CURL_CA_BUNDLE" ca-bundle)
    (setenv "SSL_CERT_FILE" ca-bundle)
    (setenv "REQUESTS_CA_BUNDLE" ca-bundle)
    (setenv "NODE_EXTRA_CA_CERTS" ca-bundle)))

(defvar my/litellm-api-key-cache nil
  "Cached LiteLLM API key from 1Password.")

(defun my/litellm-api-key ()
  "Get LiteLLM API key from 1Password, with caching."
  (or my/litellm-api-key-cache
      (setq my/litellm-api-key-cache
            (string-trim
             (shell-command-to-string
              "op read 'op://Employee/LiteLLM API Key/password' 2>/dev/null")))))

(use-package transient :ensure t :demand t)

(use-package gptel
  :ensure t
  :demand t
  :config
  (setq gptel-backend
        (gptel-make-openai "LiteLLM"
          :host "litellm.techops.integration.neotools.ca"
          :protocol "https"
          :stream t
          :key #'my/litellm-api-key
          :models '((claude-4-opus
                     :description "Most capable Claude model"
                     :capabilities (media tool json))
                    (claude-4-sonnet
                     :description "Balanced performance and speed"
                     :capabilities (media tool json))
                    (claude-4-haiku
                     :description "Fast and efficient"
                     :capabilities (media tool json))))
        gptel-model 'claude-4-opus
        gptel-prompt-prefix-alist '((org-mode . "*Prompt*: "))
        gptel-response-prefix-alist '((org-mode . "*Response*:\n")))
  (setq-default gptel-default-mode 'org-mode))

(use-package eat :ensure t :demand t)

(provide 'init-ai)
;;; init-ai.el ends here
