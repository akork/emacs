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

(defun ak-indent-buffer ()
  (interactive)
  (save-excursion
    (save-restriction
      (mark-whole-buffer)
      (indent-region (region-beginning) (region-end))
      (setq transient-mark-mode nil))))

(defun ak-yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

(defun ak-half-page-down ()
  (interactive)
  (next-line 20))

(defun ak-half-page-up ()
  (interactive)
  (previous-line 20))

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
    (setq size (- (region-end) (region-beginning)))
    ;; (evil-delete (region-beginning) (region-end))
    (kill-ring-save (region-beginning) (region-end))
    (yank)
    ;; (yank)
    ;; (forw-char)
    )))

(global-set-key "\M-Y" 'ak-yank-pop-forwards)
(global-set-key (kbd "<next>") 'ak-half-page-down)
(global-set-key (kbd "<prior>") 'ak-half-page-up)

(defvar ak-keymap-mode-map (make-sparse-keymap)
  "Keymap for `ak-keymap-mode'.")

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
(define-globalized-minor-mode global-ak-keymap-mode ak-keymap-mode ak-keymap-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'

					;(delight 'ak-keymap-mode nil ak-keymap-mode) ; do not display mode name in mode line

(add-to-list 'emulation-mode-map-alists `((ak-keymap-mode . ,ak-keymap-mode-map)))

(add-to-list 'load-path "~/.emacs.d/evil")
(use-package evil
  :config
  (setq evil-cross-lines t)
  (evil-make-overriding-map ak-keymap-mode-map)
  (evil-mode 1))

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

(add-to-list 'load-path "~/.emacs.d/general")

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer bmap)
  (general-create-definer gmap :keymaps 'ak-keymap-mode-map)

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
    "C-M-b" 'buffer-menu
    "C-M-S-t" 'mode-line-other-buffer
    "s-<return>" 'ak-make
    "M-s-g" 'ak-generate-makefile)

  (gmap :states '(normal visual motion operator insert emacs hybrid)
    :predicate '(not (derived-mode-p 'term-mode))
    "M-<right>" 'forward-word
    "M-<left>" 'evil-backward-word-begin)

  (gmap :states '(normal visual motion operator insert emacs hybrid)
    "s-<right>" 'move-end-of-line
    "s-<left>" 'back-to-indentation)

  (gmap :states '(normal visual motion operator)
    ;; :predicate '(not (derived-mode-p 'magit-status-mode))
    "t" 'evil-forward-char
    "m" 'evil-backward-char
    "v" 'evil-forward-word-end
    "V" 'evil-backward-word-end
    "n" 'evil-forward-word-begin
    "N" 'evil-forward-WORD-begin
    "d" 'evil-first-non-blank
    "s" 'evil-end-of-line
    "c" 'evil-next-line
    "r" 'evil-previous-line
    "f" 'ak-half-page-up
    "g" 'ak-half-page-down

    "w" 'evil-ex-search-next
    "W" 'evil-ex-search-previous
    "h" (gkd 'evil-find-char-to :timeout 0.5
	     "h" 'ak-org-edit-src)
    "_" 'evil-find-char
    "z" 'evil-jump-item
    ")" (gsk "C-o")
    ;; bug in evil-previous-open-brace : evil-first-non-blank needed
    "(" '(lambda () (interactive) (evil-first-non-blank) (evil-previous-open-brace))

    "D" 'evil-visual-char

    "e" 'evil-delete
    "l" 'evil-change
    "k" 'evil-delete-char
    "K" 'evil-delete-backward-char
    "'" 'evil-join
    "J" (gsk "a <return>")
    "C-d" 'ak-duplicate

    "C-e" 'move-end-of-line
    "C-a" 'evil-first-non-blank
    "DEL" 'projectile-find-file)

  (gmap :states '(normal visual motion)
    :predicate '(derived-mode-p 'org-mode)
    "TAB" 'org-cycle
    "(" 'outline-up-heading)

  ;; (gmap :states '(normal visual motion)
  ;;   :predicate '(derived-mode-p 'magit-status-mode)
  ;;   "k" 'magit-commit-popup
  ;;   "j" 'magit-rebase-popup
  ;;   "c" 'evil-next-line
  ;;   "r" 'evil-previous-line)

  (gmap :states '(normal)
    :predicate '(not (derived-mode-p 'magit-status-mode))
    "C" (gsk "0 D c s")
    "R" (gsk "s D r")
    "G" (gsk "D r s o")
    "F" (gsk "D s o r"))

  (gmap :states '(visual)
    :predicate '(not (derived-mode-p 'magit-status-mode))
    "C" 'evil-next-line
    "G" 'evil-next-line
    "R" 'evil-previous-line
    "F" 'evil-previous-line)

  (gmap :states '(insert hybrid)
    :predicate '(not (string= (buffer-name) "*terminal*"))
    "C-e" 'move-end-of-line
    "C-a" 'evil-first-non-blank))

(gmap :states '(emacs motion normal visual)
      ;; :keymaps 'doc-view-mode-map
      "SPC"
      (gkd 'helm-projectile-switch-project :timeout 1
	   "r" 'evil-goto-first-line
	   "c" 'evil-goto-line
	   "e" 'duplicate-line
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
