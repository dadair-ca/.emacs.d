;;; init-mongosh.el --- Configuration for mongosh.	-*- lexical-binding: t -*-

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
;; Configuration for mongosh.
;;

;;; Code:

(require 'comint)

(defconst mongosh-keywords
  '("use" "show" "exit" "Mongo" "connect" "it" "version" "load"))

(defconst mongosh-font-lock-keywords
  (list
   `(,(concat "\\_<" (regexp-opt mongosh-keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlint in `mongosh-mode'.")

(defvar mongosh-file-path "/usr/bin/mongosh"
  "File path for the `mongosh' executable.")

(defvar mongosh-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `mongosh'.")

(defvar mongosh-prompt-regexp "^\\(> \\)+"
  "Regexp matching the expected `mongosh' REPL prompt.")

(defun mongosh (SECRET)
  "Run an inferior instance of `mongosh' against the connection url contained in `pass' SECRET."
  (interactive "sSecret: ")
  (let* ((mongosh-program mongosh-file-path)
         (buffer (comint-check-proc "Mongosh")))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'mongosh-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Mongosh*"))
       (current-buffer)))
    (unless buffer
      (apply 'make-comint-in-buffer
             "Mongosh"
             buffer
             mongosh-program
             nil
             (list (car (split-string (shell-command-to-string (concat "pass" " " SECRET)) "\n"))))
      (mongosh-mode))))

(defun mongosh--initialize ()
  "Helper function to initialize `mongosh'."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode mongosh-mode comint-mode "Mongosh"
  "Major mode for `mongosh'.

\\<mongosh-mode-map>"
  nil "Mongosh"
  (setq comint-prompt-regexp mongosh-prompt-regexp)
  (setq comint-prompt-read-only t)
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(mongosh-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) mongosh-prompt-regexp))

(add-hook 'mongosh-mode-hook 'mongosh--initialize)

(provide 'init-mongosh)

;;; init-mongosh.el ends here
