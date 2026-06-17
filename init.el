(add-to-list 'load-path (expand-file-name "user-lisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-hl-line-mode t)
(display-battery-mode 1)
(display-time-mode 1)
(global-display-fill-column-indicator-mode 1)
(setq inhibit-startup-screen t)

(setq make-backup-files nil)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(load-theme 'modus-operandi-tinted)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
	 ("C-*" . consult-outline)))

(use-package vertico
  :ensure t
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles
					  partial-completion)))))

(use-package marginalia
  :ensure t
  :demand t
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(keymap-global-set "M-Q" 'unfill-paragraph)

(put 'narrow-to-region 'disabled nil)

;;; Vulpea & Org

(setq org-directory "~/git/vulpea")

(defun org-directory-buffer-p ()
  "Return non-nil if the currently visited buffer is an org directory file."
  (and buffer-file-name
       (string-prefix-p
	(expand-file-name (file-name-as-directory org-directory))
	(file-name-directory buffer-file-name))))

(defun my/agenda-note-p ()
  "Return non-nil if current buffer has any non-completed todo entries."
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (eq (org-element-property :todo-type h)
	  'todo))
    nil 'first-match))

(use-package vulpea
  :ensure t
  :bind (("C-c n f" . vulpea-find)
	 ("C-c n i" . vulpea-insert))
  :config
  (setq vulpea-db-sync-directories (list org-directory))
  (setq vulpea-default-notes-directory org-directory)
  (setq vulpea-db-location (expand-file-name "vulpea.db" user-emacs-directory))
  (vulpea-db-autosync-mode +1))

(use-package consult-vulpea
  :ensure t
  :after vulpea
  :config
  (consult-vulpea-mode 1))

(defun my/tag-agenda-notes ()
  "Update the `agenda' tag in the current org buffer."
  (when (and (not (active-minibuffer-window))
	     (org-directory-buffer-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (vulpea-buffer-tags-get))
	     (original-tags tags))
	(if (my/agenda-note-p)
	    (setq tags (cons "agenda" tags))
	  (setq tags (remove "agenda" tags)))
	(setq tags (seq-uniq tags))
	(when (or (seq-difference tags original-tags)
		  (seq-difference original-tags tags))
	  (apply #'vulpea-buffer-tags-set tags))))))

(add-hook 'find-file-hook #'my/tag-agenda-notes)
(add-hook 'before-save-hook #'my/tag-agenda-notes)

(defun my/org-inbox-file-path ()
  "Resolves the default org inbox file path, relative to org-directory."
  (format "inbox-%s.org" (system-name)))

(defun my/agenda-notes ()
  "Return a list of notes tagged with `agenda'."
  (seq-uniq
   (seq-map
    #'vulpea-note-path
    (vulpea-db-query-by-tags-some '("agenda")))))

(defun my/resolve-org-agenda-files (&rest _)
  "Dynamically resolve the value of `org-agenda-files'."
  (setq org-agenda-files (my/agenda-notes)))

(defun my/org-capture-add-id ()
  "Add an ID property with a newly generated ID to the current node."
  (org-id-get-create))

(use-package org
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda))
  :config
  (setq org-id-link-to-org-use-id t)
  (setq org-capture-templates
	`(("t" "todo" plain (file ,(my/org-inbox-file-path))
	   "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
- >>> :: %a"
	   :prepare-finalize my/org-capture-add-id)))
  (advice-add 'org-agenda :before #'my/resolve-org-agenda-files)
  (advice-add 'org-todo-list :before #'my/resolve-org-agenda-files))

(use-package vulpea-journal
  :ensure t
  :bind (("C-c n j o" . vulpea-journal))
  :config
  (vulpea-journal-setup))
