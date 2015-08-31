(require 'paredit)

(add-hook 'prog-mode-hook #'enable-paredit-mode)

(defun conditionally-enable-paredit-mode ()
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(provide 'config-paredit)
