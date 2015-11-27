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
                        "NEXT(n)"
                        "WAITING(w@/!)"
                        "HOLD(h@/!)"
                        "|"
                        "CANCELLED(c@/!)"
                        "DONE(d)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red")
              ("NEXT" :foreground "cyan")
              ("DONE" :foreground "green")
              ("WAITING" :foreground "orange")
              ("HOLD" :foreground "magenta")
              ("CANCELLED" :foreground "green"))))

(setq org-use-property-inheritance t)

; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-enforce-todo-dependencies t
      org-hide-leading-stars t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda Views

(setq org-agenda-compact-blocks t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capturing + Refilling

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/inbox.org")
               "* TODO %?"))))

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
