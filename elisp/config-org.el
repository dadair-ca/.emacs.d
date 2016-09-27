;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG Files

(setq org-directory "~/org"
      org-agenda-files '("~/org/todos.org" "~/.diary")
      org-default-notes-file "~/org/notes.org"
      org-refile-targets (quote ((org-agenda-files :maxlevel . 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO States, Tags, Priorities

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "")
              ("STARTED" :foreground "violet")
              ("DONE" :foreground "green")
              ("WAITING" :foreground "orange")
              ("HOLD" :foreground "orange")
              ("CANCELLED" :foreground "red"))))

(setq org-use-property-inheritance t)

(setq org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-hide-leading-stars t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda Views

(setq org-agenda-compact-blocks nil)

(setq org-agenda-custom-commands
      '((" " "Agenda"
         ((agenda "" ((org-agenda-ndays 3)))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "Urgent Tasks")
                      (org-agenda-sorting-strategy '(priority-up effort-down))))
          (todo "TODO"
                ((org-agenda-overriding-header "Todo Items")
                 (org-agenda-sorting-strategy '(priority-up effort-down))))
          ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capturing + Refilling

(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/org/todos.org" "Inbox")
         "* TODO %?\n")))

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
