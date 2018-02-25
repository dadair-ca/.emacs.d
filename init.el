;; Connect to Melpa package system
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

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
  :bind (("M-g g" . avy-goto-char)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

(use-package ace-window
  :ensure t
  :bind ("M-g i" . ace-window)
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ace-link
  :ensure t
  :init (ace-link-setup-default))

(use-package ag :ensure t)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4)
  (auto-package-update-maybe))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

(use-package clj-refactor
  :ensure t
  :init
  (load-library "config-cljrefactor")
  :config
  (setq cljr-favor-prefix-notation t))

(use-package company
  :ensure t
  :diminish company-mode
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package dash-at-point :ensure t)

(use-package diminish :ensure t)

(use-package dockerfile-mode :ensure t)

(use-package editorconfig
  :ensure t
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1)))

(use-package ensime :defer t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (load-library "system"))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package fill-column-indicator
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'turn-on-fci-mode)
  (setq fci-rule-color "#47422A")
  (setq fci-rule-width 2))

(use-package guru-mode
  :ensure t
  :diminish guru-mode
  :init (add-hook 'prog-mode-hook 'guru-mode))

(use-package magit
  :ensure t
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind ("s-m m" . magit-status))

(use-package markdown-mode :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package projectile
  :ensure t
  :init (projectile-global-mode))

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package restclient :ensure t)

(use-package scala-mode :ensure t)

(use-package smex
  :ensure t
  :bind (("M-x" . smex)))

(use-package typescript-mode :ensure t)

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  :config
  (setq-default
   ;; js2-mode
   js2-basic-offset 2
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)
  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (load-library "config-yasnippet"))

(use-package yasnippet-snippets
  :ensure t)

(use-package yaml-mode :ensure t)

(use-package which-key
  :ensure t
  :init (which-key-mode))
