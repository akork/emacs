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


(defun ak/duplicate ()
  (interactive)
  (cond
   ((string= evil-state "normal")
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank))
   ((string= evil-state "visual")
    (setq size (- (region-end) (region-beginning)))
    ;; (evil-delete (region-beginning) (region-end))
    (kill-ring-save (region-beginning) (region-end))
    (yank)
    ;; (yank)
    ;; (forw-char)
    )))

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
  :lighter " gmap"
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
  (setq evil-cross-lines t)
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
    "C-M-S-t" 'mode-line-other-buffer
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
    "t" 'evil-forward-char
    "m" 'evil-backward-char
    "v" 'evil-forward-word-end
    "V" 'evil-backward-word-end
    "n" 'evil-forward-word-begin
    "N" 'evil-forward-WORD-begin
    "_" 'evil-first-non-blank
    "z" 'evil-first-non-blank
    "s" 'evil-end-of-line
    "c" 'evil-next-line
    "r" 'evil-previous-line
    "f" 'ak/half-page-up
    "g" 'ak/half-page-down

    "w" 'evil-ex-search-next
    "W" 'evil-ex-search-previous
    "h" 'evil-find-char-to
					;"z" 'evil-jump-item
    "(" (gsk "C-o")
    ")" '(lambda () (interactive) (evil-first-non-blank) (evil-previous-open-brace))

    "d" 'evil-visual-line
    "D" 'evil-visual-char
    
    "e" 'evil-delete
    "l" 'evil-change
    "k" 'evil-delete-char
    "K" 'evil-delete-backward-char
    "'" 'evil-join
    "J" (gsk "a <return>")
    "C-d" 'ak/duplicate

    "C-e" 'move-end-of-line
    "C-a" 'evil-first-non-blank)

  (gmap :states '(normal)
    :predicate '(not (derived-mode-p 'magit-status-mode))
    "C" (gsk "0 D c s")
    "R" (gsk "s D r")
    "G" (gsk "D r s o")
    "F" (gsk "D s o r"))

  (gmap :states '(visual)
    :predicate '(not (derived-mode-p 'magit-status-mode))
    "C" (gsk "c")
    "R" (gsk "r"))
  
  (gmap :states '(insert hybrid)
    :predicate '(not (string= (buffer-name) "*terminal*"))
    "C-e" 'move-end-of-line
    "C-a" 'evil-first-non-blank))
