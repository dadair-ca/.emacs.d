;; Connect to Melpa package system
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Add elisp folder for custom 'libraries'
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; Load custom libraries
(load-library "style")
(load-library "backups")
(load-library "keys")
(load-library "editing")

;; Keep Emacs custom-variables in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; ---------- Configure Packages

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char-2)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

(use-package cider
  :ensure t)

(use-package clj-refactor
  :ensure t
  :init
  (load-library "config-cljrefactor"))

(use-package clojure-mode
  :ensure t)

(use-package company
  :ensure t
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (load-library "system"))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package guru-mode
  :ensure t
  :init (add-hook 'prog-mode-hook 'guru-mode))

(use-package helm
  :ensure t
  :config
  (helm-mode t))

(use-package lua-mode
  :ensure t)

(use-package magit
  :ensure t
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind ("s-m m" . magit-status))

(use-package markdown-mode
  :ensure t)

(use-package mu4e
  :load-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu4e"
  :config
  (load-library "config-mu4e"))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(use-package nyan-mode
  :ensure t
  :init (nyan-mode))

(use-package org)

(use-package paredit
  :ensure t
  :init
  (load-library "config-paredit"))

(use-package projectile
  :ensure t
  :init (projectile-global-mode))

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package restclient
  :ensure t)

(use-package yasnippet
  :ensure t
  :config (load-library "config-yasnippet"))

(use-package yesql-ghosts
  :ensure t)
