(add-to-list 'load-path (expand-file-name "user-lisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-hl-line-mode t)
(display-battery-mode 1)
(display-time-mode 1)
(global-display-fill-column-indicator-mode 1)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(load-theme 'modus-operandi-tinted)

(use-package howm
  :vc (:url "https://github.com/kaorahi/howm")
  :ensure t
  :init
  (setq howm-directory "~/git/howm"))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(keymap-global-set "M-Q" 'unfill-paragraph)
