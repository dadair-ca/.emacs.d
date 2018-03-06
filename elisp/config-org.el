(setq org-agenda-files (quote ("~/Dropbox/org/todo.org"
                               "~/Dropbox/org/cohesic.org")))

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/refile.org")

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Dropbox/org/refile.org")
               "* TODO %?\n%U\n%a\n")
              ("n" "note" entry (file "~/Dropbox/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n")
              ("h" "Habit" entry (file "~/Dropbox/org/refile.org")
               "* TODO %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: TODO\n:END:\n")
              ("g" "Goal" entry (file "~/Dropbox/org/refile.org")
               "* TODO %? :GOAL:\n%U\n%a\n"))))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

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
   (sh . t)))

(provide 'config-org)
