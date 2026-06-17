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

(use-package org
  :ensure nil
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda))
  :config
  (setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-")
                                            ("A)" . "-")
                                            ("B)" . "-")
                                            ("a)" . "-")
                                            ("b)" . "-")
                                            ("A." . "-")
                                            ("B." . "-")
                                            ("a." . "-")
                                            ("b." . "-"))))
  (setq org-modules '(org-id org-habit))
  (setq org-startup-indented t)
  (setq org-insert-heading-respect-content nil)
  (setq org-reverse-note-order nil)
  (setq org-id-method 'uuidgen)
  (setq org-deadline-warning-days 30)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-state-notes-insert-after-drawers nil)
  (setq org-directory "~/git/org")
  (setq org-agenda-files '("~/git/org"))
  (setq org-default-notes-file "~/git/org/refile.org")
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
  
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "HOLD(h@/!)" "WAITING(w@/!)" "|" "CANCELLED(c@)" "MEETING")))
  (setq org-todo-keyword-faces
	'(("TODO" :foreground "red" :weight bold)
	  ("NEXT" :foreground "blue" :weight bold)
	  ("DONE" :foreground "forest green" :weight bold)
	  ("WAITING" :foreground "orange" :weight bold)
	  ("HOLD" :foreground "magenta" :weight bold)
	  ("CANCELLED" :foreground "forest green" :weight bold)))
  (setq org-todo-state-tags-triggers
	'(("CANCELLED" ("CANCELLED" . t))
	  ("WAITING" ("WAITING" . t))
	  ("HOLD" ("WAITING") ("HOLD" . t))
	  (done ("WAITING") ("HOLD"))
	  ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
	  ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
	  ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
  (setq org-capture-templates
	'(("t" "todo" entry (file "~/git/org/refile.org")
	   "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
	  ("n" "note" entry (file "~/git/org/refile.org")
	   "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
	  ("m" "Meeting" entry (file "~/git/org/refile.org")
	   "* MEETING \"%?\" :MEETING:\n%U" :clock-in t :clock-resume t)
	  ("h" "Habit" entry (file "~/git/org/refile.org")
	   "* NEXT %?
SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")
:PROPERTIES:
:STYLE: habit
:REPEAT_TO_STATE: NEXT
:END:
%U
%a
")))
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-span 'day)
  (setq org-agenda-custom-commands
	'(("N" "Notes" tags "NOTE"
	   ((org-agenda-overriding-header "Notes")
	    (org-tags-match-list-sublevels t)))
	  ("h" "Habits" tags-todo "STYLE=\"habit\""
	   ((org-agenda-overriding-header "Habits")
	    (org-agenda-sorting-strategy
	     '(todo-state-down effort-up category-keep))))
	  (" " "Agenda"
	   ((agenda "" nil)
	    (tags "REFILE"
		  ((org-agenda-overriding-header "Tasks to Refile")
		   (org-tags-match-list-sublevels nil)))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")
		   (org-tags-match-list-sublevels nil)))
	    (tags-todo "-CANCELLED+WAITING|HOLD/!"
		       ((org-agenda-overriding-header "Waiting and Postponed Tasks")
			(org-tags-match-list-sublevels nil)))
	    (tags "CLOSED>=\"<today>\""
		  ((org-agenda-overriding-header "Completed Today")
		   (org-tags-match-list-sublevels nil)))))))
  (run-at-time "00:59" 3600 'org-save-all-org-buffers))

(use-package marginalia
  :ensure t
  :demand t
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; (use-package denote
;;   :vc (:url "https://github.com/protesilaos/denote")
;;   :ensure t
;;   :demand t
;;   ;; :bind (("C-c n o" . denote-open-or-create)
;;   ;; 	 ("C-c n l" . denote-link-or-create))
;;   :hook ((dired-mode . denote-dired-mode)
;; 	 (text-mode . denote-fontify-links-mode))
;;   :config
;;   (setq denote-directory "~/git/org")
;;   (setq denote-file-type 'text)
;;   (denote-rename-buffer-mode 1)
;;   (setq denote-date-prompt-use-org-read-date t)
;;   (setq denote-prompts '(title keywords file-type)))

;; (use-package denote-journal
;;   :vc (:url "https://github.com/protesilaos/denote-journal")
;;   :ensure t
;;   :commands (denote-journal-new-entry
;; 	     denote-journal-new-or-existing-entry
;; 	     denote-journal-link-or-create-entry)
;;   :hook (calendar-mode . denote-journal-calendar-mode)
;;   :bind (("C-c n j o" . denote-journal-new-or-existing-entry)
;; 	 ("C-c n j l" . denote-journal-link-or-create-entry)
;; 	 ("C-x c" . calendar))
;;   :config
;;   (setq
;;    denote-journal-directory denote-directory
;;    denote-journal-keyword "journal"
;;    denote-journal-title-format 'day-date-month-year))

;; (use-package consult-denote
;;   :vc (:url "https://github.com/protesilaos/consult-denote")
;;   :ensure t
;;   :demand t
;;   :bind (("C-c n g" . consult-denote-grep))
;;   :config
;;   (consult-denote-mode 1))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(keymap-global-set "M-Q" 'unfill-paragraph)

(use-package beancount
  :vc (:url "https://github.com/beancount/beancount-mode")
  :ensure t
  :mode ("\\.beancount\\'"))

(put 'narrow-to-region 'disabled nil)
