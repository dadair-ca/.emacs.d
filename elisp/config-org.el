(setq org-directory "~/Dropbox/org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/refile-mobileorg.org")
(setq org-default-notes-file "~/Dropbox/org/notes.org")
(setq org-agenda-files '("~/Dropbox/org/gtd.org"
                         "~/Dropbox/org/refile.org"
                         "~/Dropbox/org/refile-mobileorg.org"
                         "~/Dropbox/org/someday.org"))

(setq org-habit-graph-column 70)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "firebrick1" :weight normal)
              ("NEXT" :foreground "deep sky blue" :weight normal)
              ("DONE" :foreground "spring green" :weight normal)
              ("WAITING" :foreground "tomato" :weight normal)
              ("HOLD" :foreground "burlywood" :weight normal)
              ("CANCELLED" :foreground "spring green" :weight normal))))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-switchb)

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Dropbox/org/refile.org")
               "* TODO %?\n%U\n%a\n")
              ("n" "note" entry (file "~/Dropbox/org/notes.org")
               "* %? :NOTE:\n%U\n%a\n")
              ("h" "Habit" entry (file "~/Dropbox/org/refile.org")
               "* TODO %?\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: TODO\n:END:\n%U\n%a\n")
              ("g" "Goal" entry (file "~/Dropbox/org/refile.org")
               "* TODO %? :GOAL:\n%U\n%a\n"))))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-startup-indented t)

(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-span 7)
(setq org-reverse-note-order t)
(setq org-deadline-warning-days 21)
(setq org-agenda-show-all-dates t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (latex . t)
   (ledger . t)
   (python . t)
   (shell . t)
   (clojure . t)))

(setq org-agenda-custom-commands
      (quote ((" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header "Next Tasks")
                            (org-tags-match-list-sublevels t)))
                (todo "WAITING"
                           ((org-agenda-overriding-header "Waiting On"))))))))

(setq org-confirm-babel-evaluate nil)

(provide 'config-org)
