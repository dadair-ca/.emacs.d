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

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold
                             normal-gc-cons-threshold))))

(setq-default user-full-name "David Adair"
	      user-mail-address "adair.david@gmail.com")

(require 'config-path)
(require 'init-elpa)

(setq custom-file (concat user-emacs-directory "custom.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'init-ai)
(require 'init-ide)
(require 'init-dired)
(require 'init-ui)

(require 'savehist)
(setq savehist-file (locate-user-emacs-file "savehist"))
(setq history-length 500)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(add-hook 'after-init-hook #'savehist-mode)

(defun my/goto-agenda-file ()
  (interactive)
  "Jump to agenda file."
  (find-file "~/git/brain/agenda.txt"))

(defun agenda-move-line-up ()
  "Move current line up one line, swapping with the line above."
  (interactive)
  (when (> (line-number-at-pos) 1)
    (let ((col (current-column)))
      (transpose-lines 1)
      (forward-line -2)
      (move-to-column col))))

(defun agenda-move-line-down ()
  "Move current line down one line, swapping with the line below."
  (interactive)
  (unless (= (line-end-position) (point-max))
    (let ((col (current-column)))
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1)
      (move-to-column col))))

(defun agenda-sort-entries ()
  "Sort agenda entries by date, preserving the preamble."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Skip preamble (;;; lines)
    (while (and (not (eobp))
                (looking-at "^;;;"))
      (forward-line 1))
    ;; Skip blank lines after preamble
    (while (and (not (eobp))
                (looking-at "^$"))
      (forward-line 1))
    (let ((sort-start (point)))
      (sort-lines nil sort-start (point-max)))))

(defun agenda-cancel-entry ()
  "Toggle +CANCEL tag on current agenda entry."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
      (if (re-search-forward " \\+CANCEL" (line-end-position) t)
          (replace-match "")
        (end-of-line)
        (if (re-search-backward "\\[" (line-beginning-position) t)
            (progn (skip-chars-backward " ")
                   (insert " +CANCEL"))
          (insert " +CANCEL"))))))

(defvar-local agenda-preamble-overlay nil
  "Overlay for hiding agenda preamble.")

(defun agenda-preamble-bounds ()
  "Return (BEG . END) of preamble, or nil if none."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "^;;;")
      (let ((beg (point)))
        (while (and (not (eobp))
                    (looking-at "^;;;"))
          (forward-line 1))
        (cons beg (point))))))

(defun agenda-preamble-line-p ()
  "Return t if point is on a preamble line."
  (save-excursion
    (beginning-of-line)
    (looking-at "^;;;")))

(defun agenda-toggle-preamble ()
  "Toggle visibility of preamble."
  (interactive)
  (when-let ((bounds (agenda-preamble-bounds)))
    (if (and agenda-preamble-overlay
             (overlay-buffer agenda-preamble-overlay))
        (progn
          (delete-overlay agenda-preamble-overlay)
          (setq agenda-preamble-overlay nil))
      (setq agenda-preamble-overlay
            (make-overlay (car bounds) (cdr bounds)))
      (overlay-put agenda-preamble-overlay 'display
                   (propertize ";;; ...\n" 'face 'shadow)))))

(defun agenda-collapse-preamble ()
  "Collapse preamble if not already collapsed."
  (unless (and agenda-preamble-overlay
               (overlay-buffer agenda-preamble-overlay))
    (agenda-toggle-preamble)))

(defun agenda-cycle-at-point ()
  "Cycle visibility: preamble if on ;;; line, else default outline."
  (interactive)
  (if (agenda-preamble-line-p)
      (agenda-toggle-preamble)
    (agenda-outline-cycle)))

(use-package agenda
  :ensure (:host github :repo "rougier/agenda")
  :bind (("M-a" . my/goto-agenda-file)
	 ("C-c a c" . agenda-capture)
	 ("C-c a d" . agenda-view-day)
	 ("C-c a w" . agenda-view-week)
	 ("C-c a m" . agenda-view-month-1)
	 :map agenda-edit-mode-map
	 ("C-c a" . nil)
	 ("s-<up>" . agenda-move-line-up)
	 ("s-<down>" . agenda-move-line-down)
	 ("C-c s" . agenda-sort-entries)
	 ("C-c k" . agenda-cancel-entry)
	 ("TAB" . agenda-cycle-at-point))
  :demand t
  :config
  (setq agenda-file "~/git/brain/agenda.txt")
  (add-hook 'agenda-edit-mode-hook #'agenda-collapse-preamble))

(use-package denote
  :ensure (:host github :repo "protesilaos/denote")
  :bind (("C-c n c" . denote)
	 ("C-c n l" . denote-link))
  :config
  (setq denote-directory "~/git/brain")
  (setq denote-file-type 'text)
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :ensure (:host github :repo "protesilaos/consult-denote")
  :after (consult denote)
  :bind (("C-c n f" . consult-denote-find)
	 ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

(global-auto-revert-mode t)

;;; init.el ends here
