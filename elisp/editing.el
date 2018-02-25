(setq-default indent-tabs-mode nil) ;; don't use tabs
(setq-default tab-width 2) ;; tabs look like 2 spaces

(setq require-final-newline t) ;; always require final newline
(delete-selection-mode t) ;; delete entire selection

(setq tab-always-indent 'complete) ;; smart tabbing

(setq-default fill-column 80)
(auto-fill-mode t) ;; always use auto-fill mode

(provide 'editing)
