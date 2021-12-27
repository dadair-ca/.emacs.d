(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-M-RET-may-split-line '((headline . nil) (default . t)))
 '(org-agenda-deadline-leaders '("!D!: " "D%02d: " "-D%02d: "))
 '(org-agenda-scheduled-leaders '("" "S%d: "))
 '(org-agenda-fontify-priorities t)
 '(org-agenda-include-diary t)
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
 '(org-agenda-show-all-dates t))
