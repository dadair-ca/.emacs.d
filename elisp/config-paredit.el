(require 'paredit)

(add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)

(provide 'config-paredit)
