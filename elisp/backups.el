;; Write backups files to their own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;; make backups, even when version-controlled
(setq vc-make-backup-files t)

;; Provide the library to Emacs
(provide 'backups)
