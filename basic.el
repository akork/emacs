(package-initialize)

(add-to-list 'load-path "~/.emacs.d/use-package")
(require 'use-package)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(load-theme 'manoj-dark)

(setq scroll-margin 8)
(setq scroll-conservatively 10000)

(defun ak/yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

(defun ak/half-page-down ()
  (interactive)
  (next-line 20))

(defun ak/half-page-up ()
  (interactive)
  (previous-line 20))


(global-set-key "\M-Y" 'ak/yank-pop-forwards)
(global-set-key (kbd "<next>") 'ak/half-page-down)
(global-set-key (kbd "<prior>") 'ak/half-page-up)

(defvar ak/keymap-mode-map (make-sparse-keymap)
  "Keymap for `ak/keymap-mode'.")

;;;###autoload
(define-minor-mode ak/keymap-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-ak/keymap-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " ak/keymap-mode"
  :keymap ak/keymap-mode-map)

;;;###autoload
(define-globalized-minor-mode global-ak/keymap-mode ak/keymap-mode ak/keymap-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'

                                        ;(delight 'ak/keymap-mode nil ak/keymap-mode) ; do not display mode name in mode line

(add-to-list 'emulation-mode-map-alists `((ak/keymap-mode . ,ak/keymap-mode-map)))

(add-to-list 'load-path "~/.emacs.d/evil")
(use-package evil
  :config
  (evil-make-overriding-map ak/keymap-mode-map)
  (evil-mode 1))

(add-to-list 'load-path "~/.emacs.d/general")
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer bmap)
  (general-create-definer gmap :keymaps 'ak/keymap-mode-map)

  (defalias 'gkd 'general-key-dispatch)
  (defalias 'gsk 'general-simulate-keys)

  (gmap :keymaps 'dired-mode-map
        :states '(normal visual motion operator insert emacs hybrid)
        "o" 'dired-mark
        "a" 'dired-unmark)

  (gmap :keymaps 'latex-mode-map
        "SPC" 'aking/yas-expand-or-self-insert 
        "q" 'aking/project-sq)

  (gmap :states '(normal visual motion operator insert emacs hybrid)
        "s-<return>" 'ak/make
        "M-s-g" 'ak/generate-makefile)

  (gmap :states '(normal visual motion operator insert emacs hybrid)
        :predicate '(not (derived-mode-p 'term-mode))
        "M-<right>" 'forward-word
        "M-<left>" 'evil-backward-word-begin)

  (gmap :states '(normal visual motion operator insert emacs hybrid)
        "s-<right>" 'move-end-of-line
        "s-<left>" 'back-to-indentation)
  
  (gmap :states '(normal visual motion)
        :keymaps 'magit-status-mode-map
        "k" 'magit-commit-popup
        "j" 'magit-rebase-popup
        "c" 'evil-next-line
        "r" 'evil-previous-line)

  (gmap :states '(normal visual motion operator)
        :predicate '(not (derived-mode-p 'magit-status-mode))

        "C-M-\\" 'spacemacs/indent-region-or-buffer

        "n" 'evil-forward-word-begin
        "N" 'evil-forward-WORD-begin
        "z" 'evil-ex-search-next
        "Z" 'evil-ex-search-previous

        "_" 'evil-first-non-blank
        "s" 'evil-end-of-line

        "j" 'evil-forward-word-end
        "'" 'evil-join
        "J" (gsk "a <return>")

        "z" 'evil-jump-item
        "e" 'evil-delete
        "d" 'evil-find-char-to
        "t" 'evil-forward-char
        "m" 'evil-backward-char
        "c" 'evil-next-line
        "r" 'evil-previous-line

        "/" 'evil-ex-search-forward
        "?" 'evil-ex-search-backward

        "C-M-S-t" 'mode-line-other-buffer
        "TAB" 'evil-visual-line
        "d" 'evil-visual-line

        "C-d" 'duplicate-line

        "f" (gsk "20r")
        "g" (gsk "20c")

        "h" 'evil-find-char-to)

  (gmap :states '('normal 'visual)
        :predicate '(not (derived-mode-p 'magit-status-mode))
        "k" 'evil-delete-char
        "K" 'evil-delete-backward-char
        "l" 'evil-change

        "(" (gsk "C-o")
        ")" '(lambda () (interactive) (evil-first-non-blank) (evil-previous-open-brace)))

  (gmap :states '(insert hybrid)
        :predicate '(not (string= (buffer-name) "*terminal*"))
        "C-e" 'move-end-of-line
        "C-a" 'evil-first-non-blank)
)
