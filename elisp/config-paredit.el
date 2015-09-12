(require 'paredit)

(add-hook 'prog-mode-hook #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook #'enable-paredit-mode)

(provide 'config-paredit)
