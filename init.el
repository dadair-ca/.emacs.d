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
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(setq user-full-name "David Adair"
      user-mail-address "adair.david@gmail.com")

(setq epa-pinentry-mode 'loopback)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(eval-and-compile
  (defun lookup-password (host user port)
    (require 'auth-source)
    (require 'auth-source-pass)
    (let ((auth (auth-source-search :host host :user user :port port)))
      (if auth
          (let ((secretf (plist-get (car auth) :secret)))
            (if secretf
                (funcall secretf)
              (error "Auth entry for %s@%s:%s has no secret!"
                     user host port)))
        (error "No auth entry found for %s@%s:%s" user host port)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Libraries

(use-package diminish)

(use-package abbrev
  :ensure f
  :defer 5
  :diminish
  :hook ((text-mode prog-mode erc-mode) . abbrev-mode)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

(use-package all-the-icons
  :ensure t)

(use-package ag)

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

(use-package bbdb
  :config
  (setq bbdb-file "~/Dropbox/Documents/Relationships/bbdb")
  :init
  (calendar-set-date-style 'iso)
  (bbdb-initialize 'gnus 'message 'anniv)
  (bbdb-mua-auto-update-init 'message)
  (setq bbdb-mua-auto-update-p 'query)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))

(use-package clojure-mode
  :mode ("\\.clj[scx]?\\'" "\\.edn\\'"))

(use-package cider
  :after (clojure-mode)
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (setq cider-font-lock-dynamically '(macro core function var))
  (setq cider-auto-test-mode t))

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
                  clojure-mode-hook
                  typescript-mode-hook))
    (add-hook hook
              #'(lambda ()
                  (local-set-key (kbd "<tab>")
                                 #'company-indent-or-complete-common))))
  :config
  (global-company-mode 1))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 5))))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

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

(use-package flycheck-joker
  :ensure t)

(use-package gnus
  :config
  (global-set-key (kbd "<f6>") 'gnus)
  (setq gnus-select-method '(nnml ""))
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (setq gnus-always-read-dribble-file t)
  (setq gnus-asynchronous t)
  (setq gnus-use-cache t)
  (setq gnus-secondary-select-methods '((nntp "news.gmane.org")
                                        (nntp "news.gwene.org")))
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "personal"
                        (nnimap-address "imap.gmail.com")
                        (nnimap-server-port "imaps")
                        (nnimap-stream ssl)
                        (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                        (nnmail-expiry-wait immediate)))
  (setq gnus-posting-styles '((".*" (signature (string-join '("David Adair" "adair.david@gmail.com") "\n")))))
  (setq gnus-message-archive-group nil)
  (setq smtpmail-smtp-service 587
        smtpmail-smtp-user "adair.david@gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        send-mail-function 'smtpmail-send-it))

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

(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t))

(use-package org
  :ensure org-plus-contrib)

(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '("~/Dropbox/org/contacts.org")))

(use-package org-noter
  :after (org-mode))

(use-package org-ref
  :ensure t
  :config
  (setq org-ref-bibliography-notes "~/Dropbox/Documents/Medical-School/Research/Bibliography/notes.org")
  (setq org-ref-default-bibliography '("~/Dropbox/Documents/Medical-School/Research/Bibliography/master.bib"))
  (setq org-ref-pdf-directory "~/Dropbox/Documents/Medical-School/Research/Bibliography/PDFs"))

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
  :ensure t
  :hook (typescript-mode . prettier-js-mode))

(use-package projectile
  :ensure t
  :diminish
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode))

(use-package restclient
  :mode ("\\.https?\\'" . restclient-mode))

(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :config
  (flycheck-add-mode 'typescript-tslint 'typescript-mode)
  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled)))

(use-package tide
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-enable-auto-quoting nil))

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
(put 'narrow-to-region 'disabled nil)
