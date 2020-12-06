;;; -*- lexical-binding: t -*-

;;; help {{{

;; minor (bound-and-true-p yas-minor-mode) or major-mode-list variable
;; major (derived-mode-p 'json-mode 'emacs-lisp-) or major-mode variable

;; (string= evil-state "normal")
;; (local-set-key "\C-w" 'ak-time) ; map <buffer> vim analogue
;; S - shift, s - super
;; m-o action selection in counsel-mx
;; git rebase -i HEAD~2
;; M-. lispy-goto-symbol

;; evil-define-interactive evil-operator-range evil-read-motion evil-motion-range

;; C-x C-h - show all keybindings starting with C-x

;;  (funcall 'f args) == (eval '(f args))
;; ?_ -- symbol "_"

;;; }}}
;;; PATH set {{{

(setenv "SHELL" "/usr/local/bin/bash")
(setenv "LANG" "") 						;for lualatex
(setenv "SHELL_LOG" "")

;;; }}}
;;; before init {{{

;; (setq lexical-binding t)
(setq enable-recursive-minibuffers t)
(setq ak-home "/Users/AK/")
(setq-default explicit-shell-file-name "/usr/local/bin/bash")

(setq prev-time (current-time))

(defun ak-log (mes)
  (interactive)
  (message (concat (format "%-20s" mes) " : " (format-time-string "%H:%M:%S.%N") " delta= "
				   (format-time-string "%S.%N" (time-subtract (current-time) prev-time))))
  (setq prev-time (current-time)))

(defun ak-log-symbol (mes &rest args)
  (interactive)
  (message (concat (format "%-20s" mes) " : " (format-time-string "%H:%M:%S.%N") " delta= "
				   (format-time-string "%S.%N" (time-subtract (current-time) prev-time))))
  (setq prev-time (current-time)))

;;; }}}
;;; straight {{{
(ak-log "straight")

;; (require 'package)

                                        ; list the packages you want
;; (setq package-list '(use-package smex counsel projectile))

                                        ; list the repositories containing them
;; (setq package-archives '(("melpa" . "http://melpa.org/packages/")
;;                           ("elpa" . "http://tromey.com/elpa/")))

(defvar bootstrap-version)

(ak-log "straight bootstrap begin")
(let ((bootstrap-file
	   (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	  (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
	(message "loading bootstrap-file")
    (with-current-buffer
		(url-retrieve-synchronously
		 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
		 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (ak-log "load bootstrap")
  (load bootstrap-file nil 'nomessage))
(ak-log "straight bootstrap end")

(straight-use-package 'use-package)
(setq straight-use-package-by-default 101)

;;; }}}
;;; packages {{{
(ak-log "packages")

;; (use-package exec-path-from-shell
;;   :config
;;   (exec-path-from-shell-initialize))

(advice-add 'use-package :before 'ak-log-symbol)

(use-package counsel)                   ; counsel-mxOA
(use-package lispy)
(use-package smex)
(use-package evil-numbers)
(use-package move-text)
(use-package persistent-overlays
  :config
  (setq persistent-overlays-directory "~/.emacs.d/.emacs-persistent-overlays/"))
(use-package magit
  :commands (magit-status))
(use-package yasnippet)
(use-package yasnippet-snippets
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package lua-mode)
(use-package yaml-mode)
(use-package typescript-mode)

;; (use-package flymake-json)

;; (use-package dired-du)
;; (use-package dired-quick-sort)

(use-package undo-tree)

;; (use-package rtags)
;; (use-package flycheck-rtags)
;; (setq rtags-socket-address "10.0.1.33:8989")
;; (setq rtags-tramp-enabled t)
;; (setq rtags-autostart-diagnostics t)
;; (setq password-cache-expiry nil)
;; (setq rtags-rc-log-enabled t)

;; (use-package elpy)


;; (use-package eglot)
;; (add-to-list 'eglot-server-programs
             ;; `(python-mode . ("pyls" "-v" "--tcp" "--host"
;; "localhost" "--port" :autoport)))

(use-package lsp-mode)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

;; (use-package ein)

;;; }}}
;;; folding {{{
(ak-log "folding")

;; (defun ak-origami-triple-braces ()
;;   (interactive)
;;   (setq-local origami-fold-style 'triple-braces)
;;   (origami-mode 1)
;;   (run-at-time "0.1 sec" nil `(lambda ()
;;                                 (origami-show-only-node (current-buffer) (point)))))

(defun ak-set-foldmarker (fmr)
  "Set Vim-type foldmarkers for the current buffer"
  (interactive "sSet local Vim foldmarker: ")
  (if (equal fmr "")
	  (message "Abort")
    (setq fmr (regexp-quote fmr))
    (set (make-local-variable 'outline-regexp)
		 (concat ".*" fmr "\\([0-9]+\\)?"))
    (set (make-local-variable 'outline-level)
		 `(lambda ()
			(save-excursion
			  (re-search-forward
			   ,(concat fmr "\\([0-9]+\\)") nil t)
			  (if (match-string 1)
				  (string-to-number (match-string 1))
				(string-to-number "0")))))))

(defun ak-set-vim-foldmarker-and-hide-except ()
  (interactive)
  (outline-minor-mode 42)
  (ak-set-foldmarker (concat "{{" "{"))
  ;; (run-at-time "1 sec" nil
  ;;   `(lambda ()
  ;;      (outline-hide-body)
  ;;      (outline-toggle-children)))
  (persistent-overlays-minor-mode)
  )

(add-hook 'emacs-lisp-mode-hook 'ak-set-vim-foldmarker-and-hide-except)
(add-hook 'sh-mode-hook 'ak-set-vim-foldmarker-and-hide-except)
(add-hook 'vimrc-mode-hook 'ak-set-vim-foldmarker-and-hide-except)
;; (add-hook 'find-file-hook 'ak-set-vim-foldmarker-and-hide-except)

(setq ak-test-variable '((lambda () (ak-test)) (lambda () ak-home)))

;; }}}
;;; projectile {{{
;; (ak-log "projectile")

;; (use-package projectile
;;   :commands (projectile-switch-project projectile-find-file ak-set-main-project ak-projectile-set-persistent-project)
;;   :config
;;   (defcustom ak-projectile-project-name
;;     :type 'string)
;;   (setq ak-projectile-current-project nil)

;;   ;; preserve projectile-project-root before advicing
;;   (unless (and (boundp 'ak-projectile-project-root--original)
;; 			   ak-projectile-project-root--original)
;;     (fset 'ak-projectile-project-root--original (symbol-function 'projectile-project-root)))

;;   (defun ak-projectile-set-persistent-project (&optional dir)
;;     "Set the projectile main project based on the current buffer.
;;    When called with argument DIR, make that main project instead."
;;     (interactive)
;;     (unless dir
;;       (setq dir (ak-projectile-project-root--original)))
;;     (message (concat "dir = " dir))
;;     (setq ak-projectile-current-project dir)
;;     (projectile-add-known-project dir)
;;     (projectile-switch-project-by-name dir))

;;   (defun ak-projectile-project-root--advice (oldfun &rest args)
;;     "Skip calling `projectile-project-root' when there is a main project defined."

;;     (if (and (boundp 'ak-projectile-current-project)
;; 			 ak-projectile-current-project)
;; 		(progn
;; 		  (setq ak-projectile-project-name ak-projectile-current-project)
;; 		  ak-projectile-current-project)
;;       (funcall oldfun)))

;;   (defun ak-projectile-project-name--advice (oldfun &rest args)
;;     (if (and nil (boundp 'ak-projectile-project-name)
;; 			 ak-projectile-project-name)
;; 		ak-projectile-project-name
;;       (funcall oldfun)))

;;   (advice-add #'projectile-project-root :around #'ak-projectile-project-root--advice)
;;   (advice-add #'projectile-project-name :around #'ak-projectile-project-name--advice)

;;   (projectile-load-known-projects)
;;   (setq projectile-generic-command "find -L . -type f -print0"))

;;; }}}
;;; feel {{{
(ak-log "feel")

;; (electric-pair-mode 1)
(setq backup-directory-alist `(("" . ,(concat ak-home ".emacs.d/backup"))))
(global-auto-revert-mode t)
(global-undo-tree-mode)
(setq visible-bell 1)
(setq scroll-margin 8)
(setq scroll-conservatively 10000)
(save-place-mode 1)
(ido-mode)
;; (vimish-fold-global-mode 1)

;; only for non-terminal:
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(pixel-scroll-mode)

(let ((inhibit-message t))
  (recentf-mode 1))

(setq compilation-scroll-output 'first-error)

(defun slick-cut (beg end)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-region :before #'slick-cut)

(defun slick-copy (beg end)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-ring-save :before #'slick-copy)

(setq subword-mode t)

;; }}}
;;; hooks {{{

(add-hook 'ibuffer-mode-hook
		  (lambda ()
			(setq ibuffer-expert t)
			(ibuffer-auto-mode 1)
			(setq ibuffer-show-empty-filter-groups nil)
			(ibuffer-switch-to-saved-filter-groups "default")))

;; (use-package ibuffer-git)

(setq ibuffer-formats
	  '((
		 mark modified read-only locked " "
		 (name 22 18 :left :elide)
		 " " (size 9 -1 :right)
		 " " (mode 20 16 :left :elide)
		 ;; " " (git-status 8 8 :left)
		 " " filename-and-process)))


;; (setq ibuffer-hook nil)
;; (add-hook 'ibuffer-hook
;; 		  (lambda ()
;; 			(setq-default ibuffer-saved-filter-groups
;; 						  (list (cons "default"
;; 									  (append
;; 									   '(("Help" (or (name . "\*Help\*")
;; 													 (name . "\*Apropos\*")
;; 													 (name . "\*info\*"))))
;; 									   '(("Dired" (mode . dired-mode)))
;; 									   '(("Temporary" (name . "\*.*\*")))
;; 									   (ibuffer-vc-generate-filter-groups-by-vc-root)
;; 									   '(("Magit" (or (mode . magit-status-mode)
;; 													  (mode . magit-commit))))))))
;; 			;; (ibuffer-vc-set-filter-groups-by-vc-root)
;; 			(ibuffer-switch-to-saved-filter-groups "default")
;; 			(ibuffer-jump-to-buffer (buffer-name (cadr (buffer-list))))))

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

(add-hook 'after-init-hook (lambda () (counsel-mode 1)))

;;; }}}

;;; dired {{{

(defun ak-dired-find-file ()
  (interactive)
  (let ((file-path (dired-get-file-for-visit)))
    (message file-path)
    (if (file-directory-p file-path)
		(dired-find-file)
      (shell-command (concat "open \"" file-path "\"")))))

(defun ak-dired-opener (&optional path samewindow interactive)
  (interactive)
  (lambda () (interactive)
    (unless path
      (setq ppath default-directory))
	(unless samewindow
      (when (= (count-windows) 1)
		(split-window-right))
      (other-window 1))
    (if interactive
		(let ((default-directory ppath))
		  (call-interactively 'dired))
      (dired ppath))))

;; (use-package all-the-icons-dired
  ;; :after all-the-icons
  ;; :hook (dired-mode . all-the-icons-dired-mode))

(defun ak-dired (&optional dir file)
  (interactive)
  (if dir
	  (progn
		(if file
			(if (string-match "^[~|/]" file)
				(find-file file)
			  (find-file (concat dir "/" file)))
		  (dired dir)))
	(let ((default-directory "~/yd/"))
      (call-interactively 'dired))))


;;; }}}
;;; funcs {{{

(defun ak-eval (&optional save-exc)
  (interactive)
  (if (use-region-p)
	  (eval-region)
	(let ((p (point)))
	  (lispy-right 10)
	  (eval-last-sexp '-)
	  (unless save-exc
		(goto-char p)))))

(defun ak-eval-forward ()
  (interactive)
  (if (use-region-p)
	  (eval-region)
	(ak-eval t)
	(lispy-down 1)
	(lispy-left 1)))

(defun ak-shell-command-on-buffer (shell-command-text)
  (interactive "MShell command:")
  (shell-command (format shell-command-text (shell-quote-argument buffer-file-name))))

(defun ak-time ()
  (interactive)
  (message (current-time-string)))

(defun ak-test ()
  (interactive)
  (funcall (intern "ak-time"))
  (message (point)))

(advice-add #'other-window
			:after (lambda (a &optional b) (interactive "p") (ignore-errors (abort-recursive-edit))))

(defun ak-find-file-home (file)
  (interactive)
  (lambda () (interactive) (message file) (find-file (expand-file-name (concat ak-home file)))))


(defun ak-half-page-down ()
  (interactive)
  (next-line 20))

(defun ak-half-page-up ()
  (interactive)
  (previous-line 20))

(global-set-key (kbd "<next>") 'ak-half-page-down)
(global-set-key (kbd "<prior>") 'ak-half-page-up)

(defun ak-indent-buffer ()
  (interactive)
  (save-excursion
    (save-restriction
      (mark-whole-buffer)
      (indent-region (region-beginning) (region-end))
      ;; (setq transient-mark-mode nil)
      (keyboard-quit))))

(defun ak-select-from-lb ()
  (interactive)
  (set-mark (line-beginning-position))
  (let ((le (if (eq (line-beginning-position) (line-end-position))
				(line-end-position)
              (- (line-end-position) 1))))
    (goto-char le)))

(defun ak-newline ()
  (interactive)
  (if (and (boundp 'evil-state)
		   (eq evil-state 'normal)
		   (not (= (line-beginning-position) (point))))
	  (forward-char 1))
  (newline))


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

(defun ak-transmission-find-file-dir ()
  ;;(transmission-files-file-at-point)
  (interactive)
  (let ((filePath

		 (replace-regexp-in-string "\\/\\(?:.\\(?!\\/\\)\\)+$" "iiiiiiiiii" (transmission-files-file-at-point))))
    (shell-command (concat "osascript ~/yd/cfg/scripts/open.scpt \"" filePath "\""))))

(defun ak-transmission-find-file ()
  ;;(transmission-files-file-at-point)
  (interactive)
  ;; (message res)
  (let
	  ;; ((filePath (shell-command-to-string (concat "LANG=''; ~/yd/cfg/scripts/open-file-dir.pl \"" (transmission-files-file-at-point) "\""))))
	  ((filePath (transmission-files-file-at-point)))
    (setq filePath (transmission-files-file-at-point))
    (message (concat "path is " filePath))
    (dired (file-name-directory filePath))
    (revert-buffer)
    (goto-char 1)
    (search-forward (file-name-nondirectory filePath))
    ;; (find-file filePath)
    ))

(defun ak-toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
	  (let* ((this-win-buffer (window-buffer))
			 (next-win-buffer (window-buffer (next-window)))
			 (this-win-edges (window-edges (selected-window)))
			 (next-win-edges (window-edges (next-window)))
			 (this-win-2nd (not (and (<= (car this-win-edges)
										 (car next-win-edges))
									 (<= (cadr this-win-edges)
										 (cadr next-win-edges)))))
			 (splitter
              (if (= (car this-win-edges)
					 (car (window-edges (next-window))))
				  'split-window-horizontally
                'split-window-vertically)))
		(delete-other-windows)
		(let ((first-win (selected-window)))
		  (funcall splitter)
		  (if this-win-2nd (other-window 1))
		  (set-window-buffer (selected-window) this-win-buffer)
		  (set-window-buffer (next-window) next-win-buffer)
		  (select-window first-win)
		  (if this-win-2nd (other-window 1))))))

(defun ak-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (let* ((ob (other-buffer (current-buffer)))
		 (ob (if (string= (buffer-name ob) "*Ibuffer*")
				 (other-buffer ob)
			   ob)))
    ;;(message (buffer-name ob))
    ;;(message (buffer-name obn))
    (switch-to-buffer ob)
    ))

(defun ak-save-all ()
  (interactive)
  (save-some-buffers 'no-confirm (lambda () t)))

(defun ak-term ()
  (interactive)
  (term explicit-shell-file-name))

(defun ak-buffer-file-name ()
  (interactive)
  (kill-ring-save buffer-file-name))

(defun ak-eval-buffer ()
  (interactive)
  ;; (evil-normal-state)
  (eval-buffer))

(setq ak-eshell-window-configuration nil)
(setq ak-eshell-default-directory nil)

(defun ak-eshell-other-window ()
  (interactive)
  (if (string= (buffer-name) "*eshell*")
	  (progn (set-window-configuration
			  ak-eshell-window-configuration)
			 (save-selected-window
			   (other-window 1)
			   (when (string= (buffer-name) "*eshell*")
				 (ak-switch-to-previous-buffer)))
			 (setq ak-eshell-window-configuration nil))
    (setq ak-eshell-window-configuration
		  (current-window-configuration))
    (setq ak-eshell-default-directory default-directory)
    (if (= (count-windows) 1)
		(split-window-right))
    (other-window 1)
    (eshell)))

(defun ak-eshell-cd ()
  (interactive)
  (let ((dir ak-eshell-default-directory))
    (kill-new (concat "cd " dir))
    (eshell-bol)
    (if (not (eq (point) (line-end-position)))
		(delete-region (point) (line-end-position)))
    (yank)
    (eshell-send-input)))

(defun ak-query-replace-regexp-or-joker ()
  "In special buffers, where replace doesn't have sense, do some other usefull stuff."
  (interactive)
  (if (string= (buffer-name) "*eshell*")
	  (ak-eshell-cd)
    (call-interactively 'query-replace-regexp)))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; (eshell-send-input)
    ))

(defun ak-buffer-file-name ()
  (interactive)
  (let ((buf (buffer-base-buffer)))
    (unless buf (setq buf (current-buffer)))
    (kill-new (buffer-file-name buf))))

(defun ak-default-directory ()
  "Copies current working directory to kill-ring."
  (interactive)
  (kill-new default-directory))

(defun ak-jump-item ()
  (interactive)
  (if (save-excursion (ignore-errors (evil-forward-char)) (nth 4 (syntax-ppss)))
	  (progn (search-forward (concat "}}" "}"))
			 (previous-line))
    (evil-jump-item 1)))

(defun ak-outline-next-heading ()
  (interactive)
  (let ((point-before (point))
		(backward-move (lambda () (previous-line 3)))
		(point-after (progn (outline-next-heading) (point))))
    (if (= (point) (save-excursion (funcall backward-move) (outline-next-heading) (point)))
		(funcall backward-move))
    (when (<= (point) point-before)
      (outline-next-heading))))

(defun ak-outline-show-all--wrapper (fun)
  (interactive)
  (save-restriction
    (let ((tbuf nil))
      (setq tbuf (persistent-overlays-get-existing-overlays))
      (outline-show-all)
      (call-interactively fun)
      (eval-buffer tbuf)
      (kill-buffer tbuf))))

(defun ak-kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun ak-out-forward-and-eval ()
  (interactive)
  (save-excursion
    (lispy--out-forward 1)
    (eval-last-sexp nil)))

(defun ak-stage-and-commit ()
  (interactive)
  (save-buffer)
  (magit-stage-modified)
  (magit-commit-create))

(defun start-named-server (name)
  (let ((server-name name))
    (server-start)))

(defun ak-kill-line-0 ()
  (interactive)
  (kill-line 0)
  (backward-delete-char 1))

(defun ak-evil-yank ()
  (interactive)
  (save-excursion
	(call-interactively 'evil-yank)))

(defun ak-format-buffer ()
  "Format code with clang-format."
  (interactive)
  (let ((p (point)))
	(let (beg end)
      (if (region-active-p)
          (setq beg (region-beginning)
				end (region-end))
		(setq beg (point-min)
              end (point-max)))
      (shell-command-on-region
       beg end
	   ;; "astyle --style=otbs --indent=spaces=4 -U --pad-oper --add-brackets -k3"
	   "clang-format"
       nil t)
	  (if (< (point-max) p)
		  (goto-char (point-max))
		(goto-char p)))))

(defun ak-upcase-previous-WORD ()
  (interactive)
  (set-mark (point))
  (forward-whitespace -1)
  (call-interactively
   'upcase-region)
  (setq mark-active t)
  (exchange-point-and-mark)
  (setq deactivate-mark nil)
  (run-with-timer 0.1 nil (lambda () (setq mark-active nil))))

(defun ak-select-line ()
  (interactive)
  (if (and (= (point) (line-end-position))
		   mark-active)
	  (end-of-line 2)
	(end-of-line)
	(set-mark (line-beginning-position))))

(defun ak-evil-change (motion &optional count type)
  (let ((motion (lambda () (interactive) (eval motion))))
	(lambda () (interactive) (apply #'evil-change (evil-motion-range motion count type)))))

(defun ak-shell-command-on-region (str)
  (interactive "sShell command on region: ")
  (shell-command-on-region (region-beginning) (region-end) str nil t))

;; (load "server")
;; (unless (server-running-p) (start-named-server "main"))

;; (advice-add 'comment-line :around 'ak-outline-show-all--wrapper)

;;; }}}


;;; yas functions {{{

(defun aking/yas-expand-or-self-insert ()
  "Try to expand a snippet at a key before point.
Otherwise insert space"
  (interactive)
  (if (bound-and-true-p yas-minor-mode)
      (progn (if yas-triggers-in-field
                 (let ((yas-fallback-behavior 'return-nil)
                       )
                   (unless (yas-expand)
                     (progn (message (current-time-string))
                            (self-insert-command 1))))
               ))
    (self-insert-command 1)
    )
  )

(defun aking/yas-reload ()
  (interactive)
  (evil-write-all nil)
  (shell-command (concat "cd " dot-path "/snippets/ && ./yas-latex.pl"))
  (yas-reload-all))

(defun aking/yas-latex ()
  (interactive)
  (find-file (concat dot-path "/snippets/snip-latex")))

(defun aking/yas-latex-script ()
  (interactive)
  (find-file (concat dot-path "/snippets/yas-latex.pl")))

(defun aking/LaTeX-environment ()
  (interactive)
  (let ((environment (LaTeX-current-environment 1)))
    (message environment)))


;; +++++++++++++++++++++++++++++++++++++++++++
(defun aking/test ()
  (interactive)
  (save-excursion
    (if  (search-backward "test" (- (point) (length "test")) t)
        (message "found")
      (message "not found"))))

(setq aking/latex-snippets '("ar" "un" "ob" "on" "sq" "ti" "hi"))

(defun aking/expand (string)
  (let ((len (length string)))
    (if (save-excursion (search-backward string (- (point) len) t))
        (progn (delete-backward-char len)
               (yas-expand-snippet (yas-lookup-snippet (concat string "_t"))))
      nil)))

(defun aking/expand-all (list)
  (loop for string in list do
        (when (aking/expand string)
          (return))))

(defun aking/self-insert-or-expand ()
  (interactive)
  (self-insert-command 1)
  (and (derived-mode-p 'latex-mode)
       (texmathp)
       (aking/expand-all aking/latex-snippets)))

(defun aking/yas-next-field ()
  (interactive)
  (yas-next-field))

(defun aking/latex-math-mode ()
  "key: m"
  (interactive)
  (unless (texmathp)
    (progn  (yas-expand-snippet (yas-lookup-snippet "mathm"))
            (aking//toggle-off-input-method))))

(defun aking/LaTeX-new-equation ()
  (interactive)
  (if (or (string= (LaTeX-current-environment 1) "equation*") (string= (LaTeX-current-environment 1) "equation"))
      (progn (preview-environment 0)
             (LaTeX-find-matching-end)
             (newline)
             (yas-expand-snippet (yas-lookup-snippet "ens")))
    (progn (LaTeX-indent-line)
		   ;; (reindent-then-newline-and-indent)
		   (yas-expand-snippet (yas-lookup-snippet "ens")))))

(defun aking/latex-convert-to-big ()
  (interactive)
  (re-search-backward "\\\\(")
  (kill-region (point) (+ (point) 2))
  (insert "\n\\begin{equation*}\n")
  (re-search-forward "\\\\)")
  (kill-region (point) (+ (point) -2))
  (insert "\n\\end{equation*}\n"))

;; +++++++++++++++++++++++++++++++++++++++++++



(defun aking/match-latex-math-end ()
  (interactive)
  (re-search-forward "\\\\)"))

(defun LaTeX-delete-environment ()
  (interactive)
  (when (LaTeX-current-environment)
    (save-excursion
      (let* ((begin-start (save-excursion
                            (LaTeX-find-matching-begin)
                            (point)))
             (begin-end (save-excursion
                          (goto-char begin-start)
                          (search-forward-regexp "begin{.*?}")))
             (end-end (save-excursion
                        (LaTeX-find-matching-end)
                        (point)))
             (end-start (save-excursion
                          (goto-char end-end)
                          (1- (search-backward-regexp "\\end")))))
        ;; delete end first since if we delete begin first it shifts the
        ;; location of end
        (delete-region end-start end-end)
        (delete-region begin-start begin-end)))))

;; }}}

;;; copy-paste {{{

(defun ak-yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

(defun ak-duplicate ()
  (interactive)
  (if (region-active-p)
	  (progn
		(kill-ring-save (region-beginning) (region-end))
		(ak-paste-prepending-nl))
	(progn
	  (move-beginning-of-line 1)
	  (kill-line)
	  (yank)
	  (open-line 1)
	  (next-line 1)
	  (yank))))

(defun ak-yank ()
  (interactive)
  (kill-ring-save (region-beginning) (region-end)))

(defun ak-paste-before-appending-nl ()
  (interactive)
  ;; (backward-char 1)
  (insert "\n")
  (save-excursion
	(backward-char 1)
	(yank)
	;; (evil-paste-before 1)
	(indent-region (region-beginning) (+ (region-end) 2)))
  (back-to-indentation))

(defun ak-paste-before-appending-2nl ()
  (interactive)
  (insert "\n\n")
  (save-excursion
	(backward-char 2)
	(yank)
	;; (evil-paste-before 1)
	(indent-region (region-beginning) (region-end)))
  (back-to-indentation))

(defun ak-paste-after-prepending-nl ()
  (interactive)
  (if (and (boundp 'evil-state)
		   (eq evil-state 'normal))
	  (forward-char 1))
  (insert "\n")
  (save-excursion
	(yank)
	;; (evil-paste-before 1)
	(indent-region (region-beginning) (region-end)))
  (back-to-indentation))

(defun ak-paste-after-prepending-2nl ()
  (interactive)
  (if (and (boundp 'evil-state)
		   (eq evil-state 'normal))
	  (forward-char 1))
  (insert "\n\n")
  (save-excursion
	(yank)
	;; (evil-paste-before 1)
	(indent-region (region-beginning) (region-end)))
  (back-to-indentation))

(defun ak-duplicate-region-after ()
  (interactive)
  (if (eq evil-state 'normal)
	  (kill-ring-save (region-beginning) (1+ (region-end)))
	(kill-ring-save (region-beginning) (region-end)))
  (ak-paste-after-prepending-nl))

(defun ak-duplicate-after ()
  (interactive)
  (if (not (region-active-p))
	  (set-mark (line-beginning-position)))
  (ak-duplicate-region-after))

(global-set-key "\M-Y" 'ak-yank-pop-forwards)

;;; }}}
;;; compile functions {{{

(ak-log "compile")

(defun ak-compile ()
  (interactive)
  (ak-save-all)
  (compile compile-command))

(defun ak-kill-compilation ()
  (interactive)
  (kill-compilation))

(defun ak-push-mark ()
  (interactive)
  (push-mark))

(defun ak-newline-below ()
  (interactive)
  (end-of-line)
  (newline))

;;; }}}
;;; look {{{

(ak-log "before theme")

(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode t)
(show-paren-mode t)

(use-package spacemacs-theme :defer t)

(defun ak-switch-theme ()
  (interactive)
  (unless (boundp 'ak-dark-theme)
	(setq ak-dark-theme nil))
  (if ak-dark-theme
	  (progn (load-theme 'spacemacs-light t)
			 (setq ak-dark-theme nil))
	(progn (load-theme 'spacemacs-dark t)
		   (setq ak-dark-theme t)
		   (set-face-attribute 'default nil :height 150)
		   (set-face-attribute 'default nil :background "#000")
		   (set-face-attribute 'fringe nil :background "#000")
		   (set-face-attribute 'region nil :background "#05a")
		   (set-face-attribute 'font-lock-comment-face nil :foreground "#1a808e" :background "#000" :slant 'italic)
		   ;; (set-face-attribute hl-line-face nil :background "#000")
		   (set-face-attribute 'cursor nil :foreground "#fff")))
  (set-face-attribute hl-line-face nil :box '(:color "#bc6ec5" :line-width 1))
  (set-face-attribute 'show-paren-match nil :background "#bc6ec5" :foreground "#000" :box '(:color "#bc6ec5" :line-width 1)))

(ak-switch-theme)
(ak-log "after theme")

;; (menu-bar-mode -1)
;; (toggle-scroll-bar nil)
;; (setq frame-resize-pixelwise t)

;; (eval-after-load "ido"
  ;; '(set-face-attribute 'ido-first-match nil
					   ;; :foreground "#0f0"))

;; (set-face-attribute 'mode-line nil
;;     :background "#00f")

;; (set-face-attribute 'minibuffer-prompt nil
					;; :foreground "#0f0")

;; (set-face-attribute hl-line-face nil
					;; :background "#333")


;; (use-package all-the-icons
;;   :config
;;   ;; all-the-icons doesn't work without font-lock+
;;   ;; And font-lock+ doesn't have autoloads
;;   (use-package font-lock+
;; 	:straight (:host github :repo "emacsmirror/font-lock-plus")
;; 	:config (require 'font-lock+)))

;; disables scroll bars on new frames
(defun ak-disable-scroll-bars (frame)
  (modify-frame-parameters frame
						   '((vertical-scroll-bars . nil)
							 (horizontal-scroll-bars . nil))))
;; (add-hook 'after-make-frame-functions 'ak-disable-scroll-bars)

;;; }}}
;;; frame title {{{

(defvar ak-project-name "project A")

(defun ak-set-title ()
  (concat
   ;; (abbreviate-file-name (buffer-file-name))
   (if (and (boundp 'ak-projectile-current-project)
			ak-projectile-current-project)
	   (file-name-nondirectory (directory-file-name ak-projectile-current-project)))
   "  ::  "
   (buffer-name)
   "  "
   (format-time-string "%T")))

(setq frame-title-format
	  '(:eval (ak-set-title)))

(setq interval 1)

(defun run-every-ten-seconds ()
  (interactive)
  (set-frame-parameter nil 'title "42")
  (set-frame-parameter nil 'title nil))

(defun start-timer ()
  (interactive)
  (setq timer
		(run-at-time (current-time)  interval
					 'run-every-ten-seconds)))

(start-timer)

;; (run-at-time
;;  (time-add (current-time) (seconds-to-time interval))
;;  interval 'run-every-ten-seconds)

;; (setq display-line-numbers-type 'relative)
;; (global-display-line-numbers-mode)

;; }}}
;;; codestyle {{{

(setq-default
 tab-always-indent t
 tab-width 4)

(use-package editorconfig
  :demand t
  :config
  ;; (advice-add 'editorconfig-format-buffer :before 'outline-show-all)
  ;; (advice-add 'editorconfig-format-buffer :after 'outline-hide-other)
  )

(c-add-style "work"
               '((indent-tabs-mode . nil)                   
                 (c-basic-offset . 4)                       
                 (c-offsets-alist
                  (substatement-open . 0)                  
                  (case-label . +)                         
                  (inline-open . 0)                        
                  (block-open . 0)                         
                  (statement-cont . +)                     
                  (inextern-lang . 0)                      
                  (innamespace . 0))))

(setq c-default-style "work"
	  c-basic-offset 4)

;; (setq c-offsets-alist  '((arglist-intro vista-c-lineup-expression-plus-4)
;;                     (func-decl-cont . ++)
;;                     (member-init-intro . +)
;;                     (inher-intro . ++)
;;                     (comment-intro . 0)
;;                     (arglist-close . c-lineup-arglist)
;;                     (topmost-intro . 0)
;;                     (block-open . 0)
;;                     (inline-open . 0)
;;                     (substatement-open . 0)
;;                     (statement-cont
;;                      .
;;                      (,(when (fboundp 'c-no-indent-after-java-annotations)
;;                          'c-no-indent-after-java-annotations)
;;                       ,(when (fboundp 'c-lineup-assignments)
;;                          'c-lineup-assignments)
;;                       ++))
;;                     (label . /)
;;                     (case-label . +)
;;                     (statement-case-open . +)
;;                     (statement-case-intro . +) ; case w/o {
;;                     (access-label . /)
;;                     (innamespace . -)))


;;; }}}
;;; general config {{{

;; (add-to-list 'load-path "~/.emacs.d/general")

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer gdk-ov :keymaps 'ak-keymap-mode-map)
  (general-create-definer gdk)

  (defalias 'gkd 'general-key-dispatch)
  (defalias 'gsk 'general-simulate-keys))

;;; }}}
;;; leader-map {{{

(gdk :keymaps 'override
  ;; :states '(motion normal visual insert emacs)
  ;; :keymaps 'doc-view-mode-map
  "C-M-y"
  (gkd 'projectile-switch-project :timeout 1
	   "i" (ak-find-file-home ".ideavimrc")
	   "b" (ak-find-file-home ".qutebrowser/config.py")
	   "k" (ak-find-file-home ".config/karabiner/karabiner.json")
	   "t" (gkd 'transmission :timeout 0.5
				"t" (ak-find-file-home "Library/Application Support/transmission-daemon/settings.json"))
	   "v" (ak-find-file-home "yd/cfg/vim/min.vim")
	   "e"
	   (gkd (ak-find-file-home "yd/cfg/emacs/emacsrc.el") :timeout 0.5
			"i" (ak-find-file-home "yd/cfg/emacs/init.el"))
	   "q" (ak-find-file-home "yd/cfg/qmk_firmware/keyboards/dz60/keymaps/ak-first/keymap.c")
	   "s" (ak-find-file-home "yd/cfg/sh/sh.sh")
	   "w" (ak-find-file-home "yd/cfg/windows/start.ps1")
	   "i" (ak-find-file-home "yd/cfg/macos/init.sh")
	   "h" (ak-find-file-home ".hammerspoon/init.lua")
	   "m" (gkd (ak-find-file-home "yd/cfg/tmux/main.conf") :timeout 0.5
				"m" (ak-find-file-home "yd/cfg/tmux/copy-mode.conf"))

	   "l" 'ak-out-forward-and-eval
	   "r" 'evil-goto-first-line
	   "c" 'evil-goto-line
	   ;; "e"
	   ;; (gkd 'ak-eval :timeout 0.5
	   ;;     "e" 'eval-buffer)
	   "u" 'undo-tree-redo
	   ;;"m" 'aking/latex-convert-to-big
	   "p"
	   (gkd 'ak-buffer-file-name :timeout 0.5
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
	   "n" 'avy-goto-line
	   "o"
	   (gkd 'aking/dired-home :timeout 0.5
			"m" 'aking/dired-math
			"f" 'aking/dired-file
			"c" 'aking/dired-cs
			"d" 'aking/dired-dot
			"p" 'projectile-dired)
	   ";"
	   (gkd 'aking/latex-template :timeout 0.5
			"d" 'aking/test
			"n" 'aking/latex-new
			"t" 'aking/latex-template
			"s"
			(gkd  'aking/latex-upsync-default :timeout 0.5
				  "s" 'aking/latex-upsync))
	   ":"
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
;;; keymaps {{{

;; free some keybindings from evil-map for default bindings in underlying modes
(gdk :states  '(motion normal visual operator insert emacs)
  "C-k" nil								; kill-line restore
  "C-a" nil
  "C-e" nil
  "C-z" nil
  "C-y" nil
  "C-w" nil
  "C-f" nil
  "C-b" nil
  "h" nil)

(defun lambda-interactive (body)
  (lambda () (interactive) (eval body)))

(defun lambda-interactive-funcall (fn &rest args)
  (lambda () (interactive) (apply fn args)))

(defalias 'li 'lambda-interactive)
(defalias 'lif 'lambda-interactive-funcall)

(gdk :keymaps 'override ;; :states  '(motion normal visual operator insert emacs)
  "M-x"   (lif 'counsel-M-x "")
  "s-P"   (lif 'counsel-M-x "")
  "s-f" 'swiper
  "s-p" 'eval-expression
  "C-M-s-S-k" (lif 'kill-buffer (current-buffer))
  "s-,"
  (gkd (ak-find-file-home "yd/cfg/emacs/emacsrc.el") :timeout 0.5
	   "i" (ak-find-file-home "yd/cfg/emacs/init.el"))
  "s-p" 'counsel-buffer-or-recentf
  "s-E" 'counsel-ibuffer
  "s-o" (lif 'counsel-find-file)
  "s-O" (lif 'counsel-find-file "~/yd")
  
  "C-<return>" 'ak-eval
  "S-<return>" 'ak-eval-forward
  
  ;; info
  "C-h k" 'describe-key
  "M-s-c" 'ak-buffer-file-name
  "M-s-C" 'ak-default-directory

  ;; layout
  "C-M-s-S-o" 'other-window
  "C-M-s-!" 'delete-other-windows
  "C-M-s-}" (lif 'enlarge-window-horizontally 12)
  "C-M-s-{" (lif 'shrink-window-horizontally 12)
  "C-x C-j 0" 'ak-toggle-window-split
  "C-x k <escape>" 'ak-switch-to-previous-buffer
  
  ;; editing
  "s-M-C-P" 'ak-push-mark
  "M-s-<return>" (gsk "s-<right> <return>")
  "s-M-<right>" '(lif 'forward-whitespace 1)
  "s-M-<left>" '(lif 'forward-whitespace -1)
  "s-/" 'comment-line
  "s-<prior>" 'outline-hide-body
  "s-<next>" 'outline-toggle-children
  "M-a" 'move-beginning-of-line
  "s-<right>" 'move-end-of-line
  "s-<left>" 'back-to-indentation
  "s-<up>" 'beginning-of-buffer
  "s-<down>" 'end-of-buffer
  "M-<right>" 'forward-word
  "M-<left>" 'backward-word
  "s-<backspace>" 'ak-kill-line-0
  "s-z" 'undo-tree-undo
  "M-s-z" 'undo-tree-redo
  "s-c" 'kill-ring-save
  "s-x" 'kill-region
  "s-D" 'ak-duplicate

  "s-C-<up>" 'move-text-up
  "s-C-<down>" 'move-text-down
  
  "C-M-e" 'er/expand-region
  "s-l" 'ak-select-line
  "s-d" 'mark-word

  "s-M-C-G" 'grep
  "C-c +" 'evil-numbers/inc-at-pt
  "C-c -" 'evil-numbers/dec-at-pt
  "C-M-%" 'ak-query-replace-regexp-or-joker

  ;; idea
  "S-<f10>" 'ak-compile
  "s-<f2>" 'ak-kill-compilation
  "M-S-<f10>" 'compile
  "<f2>" 'next-error
  "M-s-l" 'ak-format-buffer

  "C-x z" 'ak-stage-and-commit

  ;; sublime
  "s-g" (gsk "C-M-s")
  "s-G" (gsk "C-M-r")

  ;; refactoring
  "<f12>" 'rtags-find-symbol-at-point
  
  "M-<f12>" 'ak-eshell-other-window
  )

(gdk :keymaps 'org-mode-map
  :states '(motion normal visual)
  "TAB" 'org-cycle
  "." 'org-cycle
  "(" 'outline-up-heading)

(add-hook 'lispy-mode-hook
		  (lambda ()
			(gdk :states '('normal 'visual) :keymaps 'lispy-mode-map
			  ";" 'lispy-comment)))

(add-hook 'undo-tree-visualizer-mode-hook
		  (lambda ()
			(gdk
			  :states 'emacs
			  :keymaps 'undo-tree-visualizer-mode-map
			  "c" 'next-line
			  "r" 'previous-line
			  "f" 'undo-tree-visualize-switch-branch-right
			  "g" 'undo-tree-visualize-switch-branch-left)))

(add-hook 'dired-mode-hook
		  (lambda ()
			(dired-hide-details-mode)
			(gdk
			  :states 'emacs
			  :keymaps 'dired-mode-map
			  "r" 'dired-previous-line
			  ;; (evil-define-key '(motion normal visual) 'dired-mode-map
			  "c" 'dired-next-line
			  ;; "g" 'dired-next-dirline
			  "f" 'dired-prev-dirline
			  "m" 'dired-mark
			  "d" 'dired-flag-file-deletion
			  "C" 'dired-do-copy
			  "R" 'dired-do-rename
			  "z" 'dired-up-directory
			  "l" 'dired-up-directory
			  "RET" 'ak-dired-find-file
			  "<mouse-1>" 'ak-dired-find-file
			  "<mouse-2>" 'dired-find-file
			  "<mouse-3>" 'dired-find-file-other-window)))

;;; }}}
