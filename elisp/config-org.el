(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/refile.org")
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Dropbox/org/refile.org")
               "* TODO %?\n%U\n%a\n")
              ("n" "note" entry (file "~/Dropbox/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n"))))

(provide 'config-org)
