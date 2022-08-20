(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(find-sibling-rules
   '(("src/\\(.*\\).ts" "test/\\1.test.ts")
     ("test/\\(.*\\).test.ts" "src/\\1.ts")))
 '(custom-safe-themes
   '("f21756050d9a6cd931517b54356ffbce5a51e0cd15454199bf408254d6364963" "f22bedc06862db3b29bde2c94960f83c96cfd7ccbccd163a2cb2c1dd076bab38" "d2db4af7153c5d44cb7a67318891e2692b8bf5ddd70f47ee7a1b2d03ad25fcd9" "a10ca93d065921865932b9d7afae98362ce3c347f43cb0266d025d70bec57af1" default))
 '(elpy-formatter 'black)
 '(js-indent-level 2)
 '(org-M-RET-may-split-line '((headline) (default . t)))
 '(org-adapt-indentation nil)
 '(org-agenda-block-separator 45)
 '(org-agenda-custom-commands
   '((" " "Agenda"
      ((agenda "" nil)
       (tags "REFILE"
             ((org-agenda-overriding-header "Tasks to Refile")
              (org-tags-match-list-sublevels nil)))
       (tags-todo "-CANCELLED/!NEXT"
                  ((org-agenda-overriding-header "Next Tasks")
                   (org-tags-match-list-sublevels t)))
       (todo "WAITING"
             ((org-agenda-overriding-header "Waiting On")))))
     ("N" "Neo"
      ((agenda ""
               ((org-agenda-span 3)
                (org-agenda-time-grid nil)
                (org-agenda-tag-filter-preset
                 '("CATEGORY=\"NEO\""))))
       (tags-todo "+CATEGORY=\"Q\""
                  ((org-agenda-overriding-header "Questions")))
       (tags-todo "+CATEGORY=\"NEO\"+PRIORITY=\"A\"-WAITING"
                  ((org-agenda-overriding-header "High-priority incomplete tasks:")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'TODO 'DONE 'CANCELLED))))
       (tags "REFILE"
             ((org-agenda-overriding-header "Refile")
              (org-tags-match-list-sublevels nil)))
       (tags-todo "+CATEGORY=\"READ\""
                  ((org-agenda-overriding-header "Readings")))))
     ("F" "Focus"
      ((agenda ""
               ((org-agenda-span 1)))
       (tags-todo "PRIORITY=\"A\"/-WAITING"
                  ((org-agenda-overriding-header "High-priority incomplete tasks:")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'TODO 'DONE 'CANCELLED))))
       (todo "WAITING"
             ((org-agenda-overriding-header "Waiting on:")))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next actions:")
              (org-agenda-skip-function
               '(da/org-skip-subtree-if-priority 65))))))
     ("W" "Weekly Review"
      ((agenda ""
               ((org-agenda-span 7)))
       (tags-todo "PRIORITY=\"A\"/-WAITING"
                  ((org-agenda-overriding-header "High-priority incomplete tasks:")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'TODO 'DONE 'CANCELLED))))
       (tags "REFILE"
             ((org-agenda-overriding-header "Inbox:")
              (org-tags-match-list-sublevels nil)))
       (todo "WAITING"
             ((org-agenda-overriding-header "Waiting on:")))
       (tags-todo "+PROJECT/-DONE-CANCELLED"
                  ((org-agenda-overriding-header "Active projects:")
                   (org-tags-match-list-sublevels nil)))
       (tags-todo "+PROJECT/-DONE-CANCELLED"
                  ((org-agenda-overriding-header "Stuck projects:")
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-skip-function 'da/skip-non-stuck-projects)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next actions:")
              (org-agenda-skip-function
               '(da/org-skip-subtree-if-priority 65))))))))
 '(org-agenda-deadline-leaders '("!D!: " "D%02d: " "-D%02d: "))
 '(org-agenda-files
   '("~/Dropbox/org/gtd.org" "~/Dropbox/org/refile.org" "~/Dropbox/org/neo.org.gpg" "~/Dropbox/org/notes.org"))
 '(org-agenda-fontify-priorities t)
 '(org-agenda-include-diary t)
 '(org-agenda-ndays 1)
 '(org-agenda-prefix-format
   '((agenda . "  %-11c%?-12t% s")
     (timeline . "  % s")
     (todo . "  %-11c%5(org-todo-age) ")
     (tags . "  %-11c")))
 '(org-agenda-scheduled-leaders '("" "S%d: "))
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-capture-templates
   '(("t" "Todo" entry
      (file "~/Dropbox/org/refile.org")
      "* TODO %?\12:PROPERTIES:\12:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U\12:END:" :prepend t)
     ("n" "Note" entry
      (file "~/Dropbox/org/notes.org")
      "* %? :NOTE:\12:PROPERTIES:\12:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U\12:END:" :prepend t)
     ("h" "Habit" entry
      (file "~/Dropbox/org/refile.org")
      "* TODO %?\12SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\12:PROPERTIES:\12:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U\12:STYLE: habit\12:REPEAT_TO_STATE: TODO\12:END:")
     ("p" "Project" entry
      (file "~/Dropbox/org/refile.org")
      "* TODO %? :PROJECT:\12:PROPERTIES:\12:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U\12:END:" :prepend t)))
 '(org-clock-into-drawer t)
 '(org-confirm-babel-evaluate nil)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/Dropbox/org/notes.org")
 '(org-directory "~/Dropbox/org")
 '(org-habit-graph-column 70)
 '(org-habit-show-all-today t)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-log-into-drawer t)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-refile-targets
   '(("~/Dropbox/org/gtd.org" :maxlevel . 3)
     ("~/Dropbox/org/neo.org.gpg" :maxlevel . 3)
     ("~/Dropbox/org/refile.org" :level . 1)
     ("~/Dropbox/org/notes.org" :maxlevel . 2)))
 '(org-refile-use-outline-path t)
 '(org-reverse-note-order t)
 '(org-tags-exclude-from-inheritance '("PROJECT"))
 '(org-todo-keyword-faces
   '(("TODO" :foreground "firebrick1" :weight normal)
     ("NEXT" :foreground "deep sky blue" :weight normal)
     ("DONE" :foreground "spring green" :weight normal)
     ("WAITING" :foreground "tomato" :weight normal)
     ("HOLD" :foreground "burlywood" :weight normal)
     ("CANCELLED" :foreground "spring green" :weight normal)))
 '(org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
 '(org-treat-S-cursor-todo-selection-as-state-change nil)
 '(org-use-fast-todo-selection t)
 '(package-selected-packages
   '(ef-themes toggle-test org-noter wordel org-roam js-mode mct keycast dired-git-info command-log-mode emacs-everywhere pass forge treemacs lsp-metals lsp-python-ms elpy company company-mode slack alert modus-vivendi modus-operandi-theme modus-vivendi-theme theme-changer modus-themes auto-package-update selectrum-prescient prescient marginalia selectrum counsel-projectile councel-projectile counsel ivy org-babel web-mode tide typescript-mode prettier emmet-mode magit which-key doom-modeline diminish use-package))
 '(send-mail-function 'mailclient-send-it)
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
