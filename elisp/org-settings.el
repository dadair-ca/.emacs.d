(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-habit-show-all-today t)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-M-RET-may-split-line '((headline . nil) (default . t)))
 '(org-agenda-deadline-leaders '("!D!: " "D%02d: " "-D%02d: "))
 '(org-agenda-scheduled-leaders '("" "S%d: "))
 '(org-agenda-fontify-priorities t)
 '(org-agenda-prefix-format
   '((agenda . "  %-11c%?-12t% s")
     (timeline . "  % s")
     (todo . "  %-11c%5(org-todo-age) ")
     (tags . "  %-11c")))
 '(org-capture-templates
   '(("t" "Todo" entry (file "~/Dropbox/org/refile.org")
      "* TODO %?
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:END:"
      :prepend t)
     ("n" "Note" entry (file "~/Dropbox/org/notes.org")
      "* %? :NOTE:
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:END:"
      :prepend t)
     ("h" "Habit" entry (file "~/Dropbox/org/refile.org")
      "* TODO %?
SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:STYLE: habit
:REPEAT_TO_STATE: TODO
:END:")
     ("p" "Project" entry (file "~/Dropbox/org/refile.org")
      "* TODO %? :PROJECT:
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:END:"
      :prepend t)))
 '(org-log-into-drawer t)
 '(org-clock-into-drawer t)
 '(org-habit-graph-column 70)
 '(org-adapt-indentation nil)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-deadline-warning-days 14)
 '(org-agenda-ndays 1)
 '(org-agenda-show-all-dates t)
 '(org-agenda-block-separator ?-)
 '(org-directory "~/Dropbox/org")
 '(org-default-notes-file "~/Dropbox/org/notes.org")
 '(org-agenda-files '("~/Dropbox/org/gtd.org"
                      "~/Dropbox/org/refile.org"
                      "~/Dropbox/org/neo.org.gpg"
                      "~/Dropbox/org/notes.org"))
 '(org-refile-targets '(("~/Dropbox/org/gtd.org" :maxlevel . 3)
                        ("~/Dropbox/org/neo.org.gpg" :maxlevel . 3)
                        ("~/Dropbox/org/refile.org" :level . 1)
                        ("~/Dropbox/org/notes.org" :maxlevel . 2)))
 '(org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                       (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
 '(org-todo-keyword-faces '(("TODO" :foreground "firebrick1" :weight normal)
                            ("NEXT" :foreground "deep sky blue" :weight normal)
                            ("DONE" :foreground "spring green" :weight normal)
                            ("WAITING" :foreground "tomato" :weight normal)
                            ("HOLD" :foreground "burlywood" :weight normal)
                            ("CANCELLED" :foreground "spring green" :weight normal)))
 '(org-use-fast-todo-selection t)
 '(org-treat-S-cursor-todo-selection-as-state-change nil)
 '(org-refile-use-outline-path t)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-tags-exclude-from-inheritance '("PROJECT"))
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
                (org-agenda-tag-filter-preset '("CATEGORY=\"NEO\""))))
       (tags-todo "+CATEGORY=\"Q\"" ((org-agenda-overriding-header "Questions")))
       (tags-todo "+WAITING" ((org-agenda-overriding-header "Waiting")))
       (tags-todo "+CATEGORY=\"NEO\"+PRIORITY=\"A\"-WAITING"
                  ((org-agenda-overriding-header "High-priority incomplete tasks:")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'TODO 'DONE 'CANCELLED))))
       (tags "REFILE"
             ((org-agenda-overriding-header "Refile")
              (org-tags-match-list-sublevels nil)))
       (tags-todo "+CATEGORY=\"READ\"" ((org-agenda-overriding-header "Readings")))))

     ("F" "Focus"
      ((agenda ""
               ((org-agenda-span 1)))
       (tags-todo "PRIORITY=\"A\"/-WAITING"
                  ((org-agenda-overriding-header "High-priority incomplete tasks:")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'TODO 'DONE 'CANCELLED))))
       (todo "WAITING"
             ((org-agenda-overriding-header "Waiting on:")))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next actions:")
              (org-agenda-skip-function '(da/org-skip-subtree-if-priority ?A))))))

     ("W" "Weekly Review"
      ((agenda "" ((org-agenda-span 7)))
       (tags-todo "PRIORITY=\"A\"/-WAITING"
                  ((org-agenda-overriding-header "High-priority incomplete tasks:")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'TODO 'DONE 'CANCELLED))))
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
                   (org-agenda-sorting-strategy '(category-keep))))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next actions:")
              (org-agenda-skip-function '(da/org-skip-subtree-if-priority ?A))))))))
 '(org-confirm-babel-evaluate nil))
