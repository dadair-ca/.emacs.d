(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")

(require 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Maildir"))
(setq mu4e-get-mail-command "mbsync work")
(setq mu4e-change-filenames-when-moving t)

(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder "/[Gmail].Sent Mail")
(setq mu4e-trash-folder "/[Gmail].Trash")

(setq mu4e-sent-messages-behavior 'delete)

(setq mu4e-maildir-shortcuts
      '(("/INBOX" . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash" . ?t)
        ("/[Gmail].All Mail" . ?a)))

(setq mu4e-get-mail-command "mbsync work")

(setq user-mail-address "david@cohesic.com"
      user-full-name "David Adair")

(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq message-kill-buffer-on-exit t)


(setq smtpmail-queue-mail nil
      smtpmail-queue-dir "~/Maildir/queue/cur")

(provide 'config-mu4e)
