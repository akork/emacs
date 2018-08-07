(load-theme 'dracula t)
(menu-bar-mode -1)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq visible-bell 1)
(setq scroll-margin 8)
(setq scroll-conservatively 10000)
(save-place-mode 1)

(setq whitespace-style (quote (lines-tail))
  whitespace-line-column 80)
(global-whitespace-mode 1)
