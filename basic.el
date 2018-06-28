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

(defvar my-mode-map (make-sparse-keymap)
  "Keymap for `my-mode'.")

;;;###autoload
(define-minor-mode my-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-my-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " my-mode"
  :keymap my-mode-map)

;;;###autoload
(define-globalized-minor-mode global-my-mode my-mode my-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'

					;(delight 'my-mode nil my-mode) ; do not display mode name in mode line

(add-to-list 'emulation-mode-map-alists `((my-mode . ,my-mode-map)))

(add-to-list 'load-path "~/.emacs.d/evil")
(use-package evil
  :config
  (evil-make-overriding-map my-mode-map)
  (evil-mode 1))

(add-to-list 'load-path "~/.emacs.d/general")
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer bmap)
  (general-create-definer gmap :keymaps 'my-mode-map)

  (defun not-dired ()
    (and (not (string= (message "%s" major-mode) "image-dired-thumbnail-mode"))
	 (not (string= (message "%s" major-mode) "sx-question-list-mode"))
	 (not (string= (message "%s" major-mode) "sx-inbox-mode"))
	 (not (string= (message "%s" major-mode) "dired-mode"))))

  (defalias 'gkd 'general-key-dispatch)
  (defalias 'gsk 'general-simulate-keys)

  (defalias 'helm-M-x-2 'helm-M-x)

  (defalias 'self-insert-command-2 'self-insert-command)
  (defalias 'self-insert-command-3 'self-insert-command)
  (defalias 'self-insert-command-4 'self-insert-command)
  (defalias 'russian-input-method-2 'russian-input-method)

  (gmap
    :keymaps 'dired-mode-map
    :states '(normal visual motion operator insert emacs)
    "o" 'dired-mark
    "a" 'dired-unmark)

  (gmap
    :keymaps 'doc-view-mode-map
    :states '(normal visual motion operator insert emacs)
    "m" 'doc-view-next-line-or-next-page
    "t" 'doc-view-previous-line-or-previous-page
    "a" 'doc-view-next-page
    "p" 'doc-view-previous-page)

  (gmap  :states '(insert emacs)
    :keymaps 'terminal-mode-map
    "C-r" 'term-send-raw
    "C-a" 'term-send-raw
    "C-w" 'term-send-raw
    "C-e" 'term-send-raw
    "M-b" (lambda () (interactive) (term-send-raw-meta))
    "M-f" (lambda () (interactive) (term-send-raw-meta))

    "M-<left>" (gsk "M-b")
    "M-<right>" (gsk "M-f")
    "M-<backspace>" (gsk "C-w")

    "M-t" (lambda () (/Us/Users/Aleksey/Dropbox/Settings/snippets/ers/Aleksey/Dropbox/Settings/snippets/interactive) (term-send-raw-string "\M-t"))
    "C-M-t" (lambda () (interactive) (term-send-raw-string "\M-t"))
    "C-c C-c" (lambda () (interactive) (term-send-raw-string "\C-c"))
    "C-h k" 'describe-key
    "C-h v" 'describe-variable
    "C-x o" 'other-window)

  (gmap :keymaps 'latex-mode-map
    "q" 'aking/project-sq)

  (bmap :keymaps 'helm-color-map
    "RET" 'helm-color-run-insert-rgb
    "C-c C-r" 'helm-color-run-insert-rgb)

  ;; (gmap :keymaps 'isearch-mode-map
  ;;       "C-%" 'aking/isearch-cancel
  ;;       "<escape>" 'aking/isearch-cancel)

  ;; }}}

;;;; normal visual motion operator insert emacs {{{

  (gmap :states '(normal visual motion operator insert emacs hybrid)
    "s-p" 'helm-buffers-list
    ;; "M-x" 'helm-M-x
    "M-^" 'aking/reset-input-method
    "M-%" 'save-reload-init

    "C-s-r" 'aking/compile-project
    "s-<return>" 'ak/make
    "M-s-g" 'ak/generate-makefile

    ;; "C-x o" 'other-window

    "C-q <right>" 'evil-window-right
    "C-q <left>" 'evil-window-left
    "C-q <up>" 'evil-window-up
    "C-q <down>" 'evil-window-down
    "C-q C-w" 'evil-window-next
    "C-q W" 'evil-window-prev
    "C-q C-d" 'kill-this-buffer
    "C-q q" 'evil-window-delete
    "C-q v" 'evil-window-vsplit
    "C-q s" 'evil-window-split
    "C-q C-t" 'ak/term
    "C-q C-q C-t" 'ak/term-cd
    "C-q C-r" 'aking/compile-project

    ;; "C-c C-i" 'toggle-input-method
    "C-c C-i" 'aking/toggle-input-method
    "M-c" 'self-insert-command
    "C-c $" 'aking/TeX-insert-dollar)

  (gmap :states '(normal visual motion operator insert emacs)
    :predicate '(not (derived-mode-p 'term-mode))
    "M-s" 'cpp-terminate-line
    "C-M-b" 'backward-char
    "C-M-n" 'forward-char
    "M-t" 'evil-delete-backward-word  ; C-M-Del
    "C-M-t" 'backward-delete-char     ; C-M-Del
    "C-M-h" 'describe-key
    "M-<right>" 'forward-word
    "M-<left>" 'evil-backward-word-begin
    "C-c C-d" 'LaTeX-delete-environment
    "C-c C-o C-p" 'preview-buffer)

  ;; }}}

;;;; normal visual motion operator insert       {{{
  (gmap :states '(normal visual motion operator insert)
    "s-<right>" 'move-end-of-line
    "s-<left>" 'back-to-indentation
    )

  ;; }}}

;;;; normal visual motion                 emacs {{{

  (gmap :states '(motion normal visual emacs)
    ;; :keymaps 'doc-view-mode-map
    "SPC"
    (gkd 'helm-projectile-switch-project :timeout 1
         "r" 'evil-goto-first-line
         "c" 'evil-goto-line
         "e" 'duplicate-line
         "u" 'undo-tree-redo
         "m" 'aking/latex-convert-to-big
         "p"
         (gkd 'current-file-name :timeout 0.5
              "p" 'current-file-dir
              ;; "m" 'current-mode
              )
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

;;;; normal visual motion                       {{{

  (gmap :states '('normal visual motion)
    :keymaps 'magit-status-mode-map
    "k" 'magit-commit-popup
    "j" 'magit-rebase-popup
    "c" 'evil-next-line
    "r" 'evil-previous-line
    )

  (gmap :states '('normal visual motion)
    :predicate '(not (derived-mode-p 'magit-status-mode))
    ;; "g TAB" 'evil-visual-restore
    ;; "x" (gsk "z")
    ;; "|" 'aking/evil-insert-ru
    ;; "i" 'aking/evil-insert-en
    ;; ;; "." 'aking/evil-append-ru
    ;; "a" 'aking/evil-append-en
    ;; "o" 'aking/evil-open-below-en
    ;; "O" 'aking/evil-open-above-en

    ;; "v" 'evil-visual-line
    ;; "V" 'evil-visual-char
    ;; "w" 'evil-visual-char

    "C-/" 'helm-swoop

    ;; "T" 'evil-scroll-down
    ;; "M" 'evil-scroll-up

    "R" (gsk "20r")
    "C" (gsk "20c")


    "C-q C-l" 'helm-buffers-list
    "C-q C-p" 'helm-projectile-switch-project

    "C-c p" 'helm-projectile-find-file
    "C-c C-p" 'helm-projectile-switch-project
    "C-c b" 'helm-buffers-list
    )


  ;; }}}
;;;; normal visual motion operator              {{{

  (defalias 'helm-projectile-find-file-2 'helm-projectile-find-file)

  (gmap :states '(normal visual motion operator)
    :predicate '(and (not (string= (buffer-name) "*terminal*"))
                     (not
                      (derived-mode-p 'dired-mode)))
    "RET" 'evil-ex-search-next
    "S-RET" 'evil-ex-search-previous
    )

  (gmap :states '(normal visual motion operator)
    :predicate '(not (derived-mode-p 'magit-status-mode))

    "C-M-\\" 'spacemacs/indent-region-or-buffer

    "n" 'evil-forward-word-begin
    "N" 'evil-forward-WORD-begin
    "z" 'evil-ex-search-next
    "Z" 'evil-ex-search-previous

    "_" 'evil-first-non-blank
    "s" 'evil-end-of-line

    "f" (gsk "20r")
    "g" (gsk "20c")

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

    "h"
    (gkd 'evil-find-char-to :timeout 0.2
         ;; "r" 'aking/evil-end-of-line
         ;; "h" 'evil-first-non-blank
         "r" 'evil-goto-first-line
         "c" 'evil-goto-line
         ;; "n" 'aking/evil-end-of-line
         ;; "b" 'evil-first-non-blank
         ;; "s" 'evil-find-char-to
         ;; "g" 'evil-find-char-to-backward

         ;; "o" 'evil-open-above
         "y" 'duplicate-line
         "u" 'undo-tree-redo
         ))

  ;; }}}
;;;; normal visual                              {{{
  (gmap :states '('normal 'visual)
    :predicate '(not (derived-mode-p 'magit-status-mode))
    "k" 'evil-delete-char
    "K" 'evil-delete-backward-char
    "l" 'evil-change

    "(" (gsk "C-o")
    ")" '(lambda () (interactive) (evil-first-non-blank) (evil-previous-open-brace))
    )

  ;; }}}

;;;;                               insert       {{{

  ;; (doc-view-set-slice )

  ;; (gmap :states 'insert
  ;;       "л"
  ;;       (gkd 'self-insert-command-4
  ;;         :timeout 0.25
  ;;         "s" 'reset-input-method)
  ;;       ;; "e"
  ;;       ;; (gkd 'self-insert-command-2
  ;;       ;;   :timeout 0.25
  ;;       ;;   "a" 'russian-input-method-2)
  ;;       ;; "h"
  ;;       ;; (gkd 'self-insert-command-3
  ;;       ;;   :timeout 0.5
  ;;       ;;   "l" 'evil-normal-state)
  ;;       ;; "u"
  ;;       ;; (gkd 'self-insert-command
  ;;       ;;   :timeout 0.25
  ;;       ;;   "h" '(lambda () (interactive) (yas-expand-snippet (yas-lookup-snippet "un"))))
  ;;       ;; "к"
  ;;       ;; (gkd 'self-insert-command-2
  ;;       ;;   :timeout 0.5
  ;;       ;;   "l" 'evil-normal-state)
  ;;       )

  ;; (key-chord-define evil-insert-state-map "hl" 'evil-normal-state)
  ;; (key-chord-define evil-insert-state-map "eu" 'russian-input-method)

  (gmap :states '('insert 'hybrid)
    :predicate '(not (string= (buffer-name) "*terminal*"))
    "C-M-S-u" 'upcase-dwim
    "SPC" 'aking/yas-expand-or-self-insert
    "C-c C-h" '(lambda () (interactive) (yas-expand-snippet (yas-lookup-snippet "un")))

    ;; "e u" 'russian-input-method
    ;; "t n" 'reset-input-method
    "," 'self-insert-command
    "C-e" 'move-end-of-line
    "C-a" 'evil-first-non-blank
    ;; move-end-of-line
    ;; move-beginning-of-line
    "C-t" 'yas-next-field-or-maybe-expand
    "C-c C-l" 'helm-colors
    ;; "C-j" (general-simulate-keys "C-q C-j")
    "C-c C-n" 'aking/yas-next-field
    ;; "q" 'aking/self-insert-or-expand
    ;; "t" 'aking/self-insert-or-expand
    ;; "b" 'aking/self-insert-or-expand
    ;; "r" 'aking/self-insert-or-expand
    ;; "n" 'aking/self-insert-or-expand
    ;; "i" 'aking/self-insert-or-expand
    )

  ;; }}}

;;;;                      operator              {{{
  (gmap  :states 'operator
    ;; "r" 'evil-ex-search-forward
    ;; "h" 'evil-ex-search-backward
    ;; "n" 'evil-end-of-line
    ;; "b" 'evil-first-non-blank
    ;; "s" 'evil-find-char-to
    ;; "g" 'evil-find-char-to-backward
    ;; "$" (gsk "C-q C-j")
    )



  ;; }}}

;;;; PREFIX MAP {{{


  ;; (gmap :states '(normal)
  ;;       "u"
  ;;       (gkd 'evil-forward-word-end :timeout 0.3
  ;;            "c"
  ;;            (gkd 'current-file-name :timeout 0.5
  ;;                 "s" (gsk "c f")
  ;;                 "h" (gsk "c F"))
  ;;            ))

  (gmap :states '(motion normal visual insert)
    "C-M-c"
    (gkd (gsk "C-c C-e") :timeout 0.3
         "b" (gsk "C-c C-f C-b")
         ;; "m" (gsk "C-c C-f C-t")
         "t" (gsk "C-c C-f C-i")
         ))
  )
