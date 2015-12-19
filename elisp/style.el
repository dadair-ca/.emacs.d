;; Turn off mouse interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; Highlight current line
(global-hl-line-mode 1)

;; Highlight matching parentheses when the point is on them
(show-paren-mode 1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/tomorrow-theme")
(add-to-list 'load-path "~/.emacs.d/themes/tomorrow-theme")

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-init-hook 'global-whitespace-mode)

(setq whitespace-style (list 'face 'trailing))

(add-hook 'prog-mode-hook 'prettify-symbols-mode)

(provide 'style)
