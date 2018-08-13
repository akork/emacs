;;; folding {{{

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local origami-fold-style 'triple-braces)
	    (origami-mode 1)
	    (run-at-time "0.1 sec" nil `(lambda ()
					  (origami-show-only-node (current-buffer) (point))))))

;; }}}
;;; look {{{

(load-theme 'dracula t)
(menu-bar-mode -1)

;; (setq display-line-numbers-type 'relative)
;; (global-display-line-numbers-mode)

;; }}}
;;; feel {{{
(setq visible-bell 1)
(setq scroll-margin 8)
(setq scroll-conservatively 10000)
(save-place-mode 1)
;; (vimish-fold-global-mode 1)

;; only for non-terminal:
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; }}}
;;; overriding keymap {{{
(defvar ak-keymap-mode-map (make-sparse-keymap)
  "Keymap for `ak-kkeymap-mode'.")

	      ;;;###autoload
(define-minor-mode ak-keymap-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-ak-keymap-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " gmap"
  :keymap ak-keymap-mode-map)

	      ;;;###autoload
(define-globalized-minor-mode global-ak-keymap-mode ak-keymap-mode 
  (lambda ()
    (when (not (derived-mode-p
		'dired-mode 'org-mode))
      (ak-keymap-mode)))
  )

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'

					;(delight 'ak-keymap-mode nil ak-keymap-mode) ; do not display mode name in mode line

;; (add-to-list 'emulation-mode-map-alists `((ak-keymap-mode . ,ak-keymap-mode-map)))

(defun turn-off-my-mode ()
  "Turn off my-mode."
  (ak-keymap-mode -1))

(add-hook 'dired-mode-hook #'turn-off-my-mode)
;; }}}
;;; evil setup {{{
(add-to-list 'load-path "~/.emacs.d/evil")
(use-package evil
  :config
  (setq evil-cross-lines nil)
  ;; (evil-make-overriding-map ak-keymap-mode-map)
  (evil-mode 1))
;; }}}
;;; funcs {{{
(defun ak-half-page-down ()
  (interactive)
  (next-line 20))

(defun ak-half-page-up ()
  (interactive)
  (previous-line 20))

(global-set-key (kbd "<next>") 'ak-half-page-down)
(global-set-key (kbd "<prior>") 'ak-half-page-up)

(defun ak-paste-appending-nl ()
  (interactive)
  (evil-paste-before 1)
  (forward-char)
  (insert "\n")
  (indent-for-tab-command))

(defun ak-paste-prepending-nl ()
  (interactive)
  ;; (set-mark-command 4)
  (set-mark-command nil)
  (insert "\n")
  (save-excursion
    (yank)
    (indent-region (mark) (point))
    (deactivate-mark))
  (indent-for-tab-command))

(defun ak-eval ()
  (interactive)
  (if (use-region-p)
      (eval-region)
    (eval-last-sexp (point))))

(defun ak-indent-buffer ()
  (interactive)
  (save-excursion
    (save-restriction
      (mark-whole-buffer)
      (indent-region (region-beginning) (region-end))
      ;; (setq transient-mark-mode nil)
      (keyboard-quit))))

(defun ak-yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

(defun ak-duplicate ()
  (interactive)
  (
   cond
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
    ;; (setq size (- (region-end) (region-beginning)))
    ;; (evil-delete (region-beginning) (region-end))
    (kill-ring-save (region-beginning) (region-end))
    ;; (forward-char 1)
    (ak-paste-prepending-nl))))

(global-set-key "\M-Y" 'ak-yank-pop-forwards)

(defun ak-org-edit-src ()
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-edit-special)
    (org-edit-src-exit)))

(defun ak-previous-open-brace ()
  (interactive)
  (previous-line)
  (end-of-line)
  (evil-previous-open-brace))

(evil-define-command ak-current-file-name ()
  "Copy the current buffer-file-name to the clipboard."
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when filename
      (setq select-enable-clipboard t)
      (kill-new filename)
      (setq select-enable-clipboard nil)
      (message "'%s' to the clipboard." filename)
      filename)))

(evil-define-command ak-current-file-dir ()
  "Copy the current file-name-directory to the clipboard."
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when filename
      (setq select-enable-clipboard t)
      (kill-new (file-name-directory filename))
      (setq select-enable-clipboard nil)
      (message "'%s' to the clipboard." (file-name-directory filename))
      (file-name-directory filename))))

(evil-define-command ak-current-mode ()
  (setq select-enable-clipboard t)
  (kill-new major-mode)
  (setq select-enable-clipboard nil)
  (message "%s" major-mode))
;; }}}
;;; keymaps {{{
(add-to-list 'load-path "~/.emacs.d/general")

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer gdk-ov :keymaps 'ak-keymap-mode-map)
  (general-create-definer gdk)

  (defalias 'gkd 'general-key-dispatch)
  (defalias 'gsk 'general-simulate-keys)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; global
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (gdk :states '(motion normal visual operator insert emacs)
    "C-k" nil ; kill-line restore
    "C-a" nil
    "C-e" nil
    "C-M-b" 'buffer-menu
    "C-M-S-t" 'mode-line-other-buffer
    "s-<return>" 'ak-make
    "M-s-g" 'ak-generate-makefile
    "C-M-i" 'evil-jump-item
    "C-w" 'backward-char
    ;;    :predicate '(not (derived-mode-p 'term-mode))
    "M-<right>" 'forward-word
    "M-<left>" 'evil-backward-word-begin
    "s-<right>" 'move-end-of-line
    "s-<left>" 'back-to-indentation
    "M-s-g" 'ak-generate-makefile
    "C-M-i" 'evil-jump-item
    "C-w" 'backward-char
    "M-s-g" 'ak-generate-makefile
    "C-M-i" 'evil-jump-item
    "C-w" 'backward-char
    "C-M-e" 'er/expand-region
    "C-M-i" 'evil-jump-item
    "C-w" 'backward-char
    "C-M-e" 'er/expand-region)

  (gdk :states '(motion normal visual operator)
    ;; basic movement:
    "t" 'evil-forward-char
    "m" 'evil-backward-char
    "n" 'evil-forward-word-begin
    "N" 'evil-forward-WORD-begin
    "d" 'evil-first-non-blank
    "C-a" 'evil-first-non-blank
    "s" 'evil-end-of-line
    "C-e" 'move-end-of-line
    "c" 'evil-next-line
    "r" 'evil-previous-line
    "f" 'ak-half-page-up
    "g" 'ak-half-page-down
    ;; advanced movement:
    "w" 'evil-ex-search-next
    "W" 'evil-ex-search-previous
    "h" (gkd 'evil-search-forward :timeout 0.5
	     "h" 'ak-org-edit-src)
    "_" 'evil-find-char
    "z" 'evil-jump-item
    ")" (gsk "C-o")
	;;; bug in evil-previous-open-brace : evil-first-non-blank needed
    "(" '(lambda () (interactive) (evil-first-non-blank) (evil-previous-open-brace))
    ;; state changing:
    "D" 'evil-visual-char
    "v" 'evil-visual-char
    ;; delete/change:
    "e" 'evil-delete
    "l" 'evil-change
    "k" 'evil-delete-char
    "K" 'evil-delete-backward-char
    ;; misc:
    "'" 'evil-join
    "J" (gsk "a <return>")
    "C-d" 'ak-duplicate
    "DEL" 'projectile-find-file
    "x" nil
    "x r" 'outline-show-all
    "x a" 'outline-show-subtree
    "TAB" 'outline-toggle-children
    "x m" 'outline-hide-body
    "x r" 'vimish-fold-unfold-all
    "x a" 'vimish-fold-unfold
    "TAB" 'vimish-fold-toggle
    "x m" 'vimish-fold-refold-all
    "x a" 'origami-toggle-node
    "TAB" 'origami-recursively-toggle-node
    "x r" 'origami-open-all-nodes
    "x o" 'origami-open-node
    "x m" 'origami-close-all-nodes)

  (gdk :states '(motion normal)
    "Z" (gsk "D %")
    "C" (gsk "0 D c s")
    "R" (gsk "s D r")
    "G" (gsk "D r s o s m")
    "F" (gsk "D s o r"))

  (gdk :states '(visual)
    "TAB" 'ak-duplicate
    "Z" (gsk "D %")
    "C" (gsk "0 D c s")
    "R" (gsk "s D r")
    "G" (gsk "c s m")
    "F" (gsk "D s o r"))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; mode specific
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (gdk :keymaps 'org-mode-map
    :states '(motion normal visual)
    "TAB" 'org-cycle
    "." 'org-cycle
    "(" 'outline-up-heading)

  (gdk :keymaps 'dired-mode-map
    :states '(motion normal visual)
    "c" 'dired-next-line
    "r" 'dired-previous-line
    "g" 'dired-next-dirline
    "f" 'dired-prev-dirline
    "m" 'dired-mark
    "d" 'dired-flag-file-deletion
    "C" 'dired-copy-file
    "R" 'dired-do-rename
    "z" 'dired-up-directory)

  ;; (gdk-ov :states '(motion normal visual)
  ;;   :predicate '(derived-mode-p 'magit-status-mode)
  ;;   "k" 'magit-commit-popup
  ;;   "j" 'magit-rebase-popup
  ;;   "c" 'evil-next-line
  ;;   "r" 'evil-previous-line)

  (gdk :keymaps 'latex-mode-map
    "SPC" 'aking/yas-expand-or-self-insert 
    "q" 'aking/project-sq))
;; }}}
;;; leader-map {{{
(gdk :states '(emacs motion normal visual)
  "q"
  (gkd 'evil-record-macro :timeout 1
       "a" 'origami-recursively-toggle-node
       "r" 'origami-open-all-nodes
       "o" 'origami-recursively-toggle-node
       "m" 'origami-close-all-nodes))

(gdk :states '(emacs motion normal visual)
  ;; :keymaps 'doc-view-mode-map
  "SPC"
  (gkd 'helm-projectile-switch-project :timeout 1
       "r" 'evil-goto-first-line
       "c" 'evil-goto-line
       "e"
       (gkd 'ak-eval :timeout 0.5
	    "e" 'eval-buffer)
       "u" 'undo-tree-redo
       "m" 'aking/latex-convert-to-big
       "p"
       (gkd 'ak-current-file-name :timeout 0.5
	    "p" 'ak-current-file-dir
	    "m" 'ak-current-mode)
       "d"
       (gkd 'edit-config-keymap :timeout 0.5
	    "h" 'aking/conf-hammerspoon
	    "q" 'aking/conf-spacemacs-quail
	    "b" 'aking/conf-bash
	    "d"
	    (gkd 'edit-config :timeout 0.5
		 "d" 'spacemacs/find-dotfile))

       "k" 'aking/conf-karabiner

       "s"
       (gkd 'aking/save :timeout 0.5
	    "a" 'aking/save-all
	    "/" 'evil-save-and-quit)
       "i" 'save-reload-init
       "f"
       (gkd  '(lambda () (interactive)
		;; (aking/view-pdf)
		(aking/compile-project)
		)
	 :timeout 0.5
	 "c" '(lambda () (interactive)
		(aking/view-pdf)
		(aking/compile-project)
		(preview-buffer)))
       ;; "g" 'helm-projectile-grep
       "g" 'magit-status
       ;; "g" 'preview-buffer
       ;; "w" 'aking/test
       "w" 'ak/view-pdf
       "v" 'aking/view-pdf
       "b" 'aking/latex-build
       "h" 'avy-goto-word-1
       "n" 'avy-goto-line
       "o"
       (gkd 'aking/dired-home :timeout 0.5
	    "m" 'aking/dired-math
	    "f" 'aking/dired-file
	    "c" 'aking/dired-cs
	    "d" 'aking/dired-dot
	    "p" 'projectile-dired)
       "t"
       (gkd 'aking/latex-template :timeout 0.5
	    "d" 'aking/test
	    "n" 'aking/latex-new
	    "t" 'aking/latex-template
	    "s"
	    (gkd  'aking/latex-upsync-default :timeout 0.5
		  "s" 'aking/latex-upsync))
       "l"
       (gkd 'aking/latex-new :timeout 0.5
	    "c" (gsk "; u C-c C-e")
	    "l" (gsk "C-c C-l")
	    "e" (gsk "C-c `")
	    "n" (gsk "C-c C-e")
	    "s"
	    (gkd 'aking/yas-latex :timeout 0.5
		 "s" 'aking/yas-latex-script)
	    "r" 'aking/yas-reload)
       "y"
       (gkd 'helm-yas-visit-snippet-file :timeout 0.5
	    "n" 'yas-new-snippet
	    "r" 'aking/yas-reload
	    ;; "l" 'aking/yas-latex
	    ;; "t" (gkd 'aking/yas-latex :timeout 0.5
	    ;;          "s" 'aking/yas-latex-script))
	    )))
;; }}}
