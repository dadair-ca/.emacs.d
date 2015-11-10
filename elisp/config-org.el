;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG Files

(setq org-directory "~/org"
      org-agenda-files (quote ("~/org/todos.org"))
      org-default-notes-file "~/org/notes.org"
      org-refile-targets (quote ((org-agenda-files :maxlevel . 2))))

(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg"
      org-mobile-inbox-for-pull "~/org/mobile.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO States, Tags, Priorities

(setq org-todo-keywords
      (quote ((sequence "TODO(t)"
                        "STARTED(s!)"
                        "DELEGATED(l@)"
                        "BLOCKED(b@)"
                        "|"
                        "CANCELLED(c!)"
                        "DEFERRED(f@)"
                        "DONE(d!)"))))

(setq org-todo-keyword-faces
      '(("TODO"      . (:foreground "white"))
        ("STARTED"   . (:foreground "blue"))
        ("DELEGATED" . (:foreground "grey" :weight bold))
        ("BLOCKED"   . (:foreground "red" :weight bold))
        ("CANCELLED" . (:foreground "green"))
        ("DEFERRED"  . (:foreground "green"))
        ("DONE"      . (:foreground "green"))))

(setq org-use-property-inheritance t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capturing + Refilling

;; Use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      (quote (("t" "todo" entry (file+headline "~/org/todos.org" "Inbox")
               "* TODO %?\n"))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes (quote confirm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usability

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t
      ido-everywhere t
      ido-max-directory-size 100000)

(ido-mode (quote both))

; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window)

; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

(provide 'config-org)
