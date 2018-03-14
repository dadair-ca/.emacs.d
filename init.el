(defconst emacs-start-time (current-time))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect))
          t)

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
(load-library "config-org")

;; Keep Emacs custom-variables in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; ---------- Libraries

(use-package diminish :demand t)

;; ---------- Packages

(use-package avy
  :bind (("M-g g" . avy-goto-char)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

(use-package ace-window
  :bind ("M-g i" . ace-window)
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ag
  :commands (ag ag-dired ag-files))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4)
  (auto-package-update-maybe))

(use-package clojure-mode
  :mode "\\.clj[scx]?\\'")

(use-package cider
  :after (clojure-mode)
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

(use-package clj-refactor
  :after (clojure-mode)
  :init
  (load-library "config-cljrefactor")
  :config
  (setq cljr-favor-prefix-notation t))

(use-package company
  :defer 5
  :diminish
  :commands (company-mode company-indent-or-complete-common)
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  clojure-mode-hook))
    (add-hook hook
              #'(lambda ()
                  (local-set-key (kbd "<tab>")
                                 #'company-indent-or-complete-common))))
  :config
  (global-company-mode 1))

(use-package editorconfig
  :diminish
  :hook ((prog-mode text-mode) . editorconfig-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (load-library "system"))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(defun on-off-fci-before-company(command)
  (when (string= "show" command)
    (turn-off-fci-mode))
  (when (string= "hide" command)
    (turn-on-fci-mode)))

(use-package fill-column-indicator
  :ensure t
  :init
  ;; https://github.com/alpaker/Fill-Column-Indicator/issues/54
  (advice-add 'company-call-frontends :before #'on-off-fci-before-company)
  (add-hook 'prog-mode-hook #'turn-on-fci-mode)
  (setq fci-rule-color "#47422A")
  (setq fci-rule-width 2))

(use-package guru-mode
  :ensure t
  :diminish guru-mode
  :init (add-hook 'prog-mode-hook 'guru-mode))

(use-package ledger-mode
  :mode "\\.ledger\\'")

(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind ("s-m m" . magit-status))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(use-package paredit
  :diminish
  :hook ((lisp-mode emacs-lisp-mode clojure-mode) . paredit-mode)
  :config
  (require 'eldoc)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

(use-package projectile
  :demand
  :diminish
  :config
  (projectile-global-mode))

(use-package restclient
  :mode "\\.https?\\'")

(use-package smex
  :bind (("M-x" . smex)))

(use-package typescript-mode
  :mode "\\.tsx?\\'")

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (load-library "config-yasnippet"))

(use-package yasnippet-snippets
  :ensure t)

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package which-key
  :defer 5
  :diminish
  :commands (which-key-mode)
  :config
  (which-key-mode))

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode))

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed)))
          t)
