;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG Files

(setq org-directory "~/org"
      org-agenda-files '("~/org/todos.org" "~/org/inbox.org" "~/org/mobile.org" "~/diary")
      org-default-notes-file "~/org/notes.org"
      org-refile-targets (quote ((org-agenda-files :maxlevel . 2))))

(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg"
      org-mobile-inbox-for-pull "~/org/mobile.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO States, Tags, Priorities

(setq org-todo-keywords
      (quote ((sequence "PROJECT(p)" "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("PROJECT" :foreground "")
              ("TODO" :foreground "")
              ("NEXT" :foreground "")
              ("DONE" :foreground "")
              ("WAITING" :foreground "")
              ("HOLD" :foreground "")
              ("CANCELLED" :foreground ""))))

(setq org-use-property-inheritance t)

; Tags with fast selection keys
(setq org-tag-alist (quote (("TWO" . ?2) ("FLAGGED" . ??))))

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
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")))
          (tags-todo "+TWO"
                     ((org-agenda-overriding-header "Two-minute Tasks")
                      (org-agenda-sorting-strategy '(priority-up effort-down))))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")
                 (org-agenda-sorting-strategy '(priority-up effort-down))))
          (todo "TODO"
                ((org-agenda-overriding-header "Todo Items")
                 (org-agenda-sorting-strategy '(priority-up effort-down))))
          ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effort Estimation and Clocking

(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"
      org-global-properties '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                              ("STYLE_ALL" . "habit")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capturing + Refilling

(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/inbox.org")
               "* TODO %?\n%a\n"))))

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
