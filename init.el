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
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
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

;; SQL config only exists on work laptop
(let ((cohesic-sql-config "config-sql"))
  (when (locate-library cohesic-sql-config)
      (load-library "config-sql")))

;; Keep Emacs custom-variables in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq use-package-always-ensure t)

;; ---------- Libraries

(use-package diminish)

;; ---------- Packages

(use-package avy
  :bind (("M-g g" . avy-goto-char)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

(use-package ace-window
  :bind (("M-g i" . ace-window)
         ("C-M-g i" . ace-swap-window))
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ace-link
  :bind (("M-g o" . ace-link-org)))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4
        auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package clojure-mode
  :mode ("\\.clj[scx]?\\'" "\\.edn\\'"))

(use-package cider
  :after (clojure-mode)
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (setq cider-font-lock-dynamically '(macro core function var)))

(use-package clj-refactor
  :after (clojure-mode)
  :init
  (load-library "config-cljrefactor")
  :config
  (setq cljr-favor-prefix-notation nil))

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

(use-package ensime
  :mode "\\.scala\\'"
  :config (setq ensime-startup-notification nil))

(use-package scala-mode
  :mode "\\.scala\\'")

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :config
  (load-library "system"))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package guru-mode
  :ensure t
  :diminish guru-mode
  :init (add-hook 'prog-mode-hook 'guru-mode))

(use-package helm
  :ensure t
  :diminish
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-c h o") 'helm-occur)
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)))

(use-package helm-ag
  :after (helm-mode))

(use-package helm-bibtex
  :after (helm-mode)
  :config
  (setq bibtex-completion-bibliography "~/Dropbox/Documents/Medical-School/Research/Bibliography/master.bib"
        bibtex-completion-library-path "~/Dropbox/Documents/Medical-School/Research/Bibliography/PDFs"
        bibtex-completion-notes-path "~/Dropbox/Documents/Medical-School/Research/Bibliography/notes.org"))

(use-package helm-descbinds
  :after (helm-mode))

(use-package helm-org-rifle
  :after (org-mode))

(use-package helm-projectile
  :ensure t
  :diminish
  :config (helm-projectile-on))

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

(use-package org-noter
  :after (org-mode))

(use-package org-ref
  :ensure t
  :config
  (setq org-ref-bibliography-notes "~/Dropbox/Documents/Medical-School/Research/Bibliography/notes.org")
  (setq org-ref-default-bibliography '("~/Dropbox/Documents/Medical-School/Research/Bibliography/master.bib"))
  (setq org-ref-pdf-directory "~/Dropbox/Documents/Medical-School/Research/Bibliography/PDFs/"))

(use-package paredit
  :ensure t
  :diminish
  :config
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (require 'eldoc)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

(use-package prettier-js
  :pin melpa
  :ensure t
  :config
  (add-hook 'web-mode-hook #'prettier-js-mode)
  (add-hook 'tide-mode-hook #'prettier-js-mode)
  (add-hook 'typescript-mode-hook #'prettier-js-mode))

(use-package projectile
  :ensure t
  :diminish
  :config
  (projectile-mode))

(use-package restclient
  :mode "\\.https?\\'")

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :mode ("\\.tsx\\'" "\\.ts\\'")
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (load-library "config-yasnippet"))

(use-package yasnippet-snippets
  :ensure t)

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package web-mode
  :mode "\\.tsx?\\'"
  :config
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-indentation nil)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  (flycheck-add-mode 'typescript-tslint 'web-mode))

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
  :commands (whitespace-buffer.
             whitespace-cleanup
             whitespace-mode))

(put 'downcase-region 'disabled nil)

(provide 'init.el)
;;; init.el ends here
