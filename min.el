;;; -*- lexical-binding: t -*-
;;; -*- enable-recursive-minibuffers: t -*-
;;; -*- load-prefer-newer t -*-

(setq ak-home "/Users/AK/")
;; (setq enable-recursive-minibuffers t)
(setq-default explicit-shell-file-name "/usr/local/bin/bash")

(defun ak-log (mes)
  (interactive)
  (message (concat mes " : " (current-time-string))))

;;; help {{{

;; minor (bound-and-true-p yas-minor-mode)
;; major (derived-mode-p 'json-mode 'emacs-lisp-)

;; (string= evil-state "normal")
;; (local-set-key "\C-w" 'ak-time) ; map <buffer> vim analogue
;; S - shift, s - super

;;; }}}
;;; straight {{{
(ak-log "straight")

;; (require 'package)

                                        ; list the packages you want
(setq package-list '(use-package smex counsel projectile))

                                        ; list the repositories containing them
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                          ("elpa" . "http://tromey.com/elpa/")))

;; (setq package-load-list '((bind-key t) (use-package t)))
                                        ; activate all the packages (in particular autoloads)

;; (setq package-enable-at-startup nil)
;; (package-initialize t)
;; (package-activate 'use-package)
                                        ; fetch the list of packages available
;; (unless package-archive-contents

;;   (package-refresh-contents))

                                        ; install the missing packages
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))

;; (require 'org)
                                        ;(org-babel-load-file  "/users/aleksey/dropbox/settings/emacs/basic-org.org")
;; (require 'use-package)

(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default 101)

;;; }}}
;;; packages {{{
(ak-log "packages")

(use-package general)
(use-package ace-window
  :config
  (setq aw-keys '(?d ?c ?r ?t ?n ?s))
  :general
  (:states '(normal visual motion insert emacs)
    "C-x o" 'ace-window)
  ("C-x o" 'ace-window))

(use-package lispy)

;; (use-package smex)

;;; }}}
;;; folding {{{
(ak-log "folding")

(defun ak-origami-triple-braces ()
  (interactive)
  (setq-local origami-fold-style 'triple-braces)
  (origami-mode 1)
  (run-at-time "0.1 sec" nil `(lambda ()
  (origami-show-only-node (current-buffer) (point)))))

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
  (ak-set-foldmarker (concat "{{" "{"))
  (run-at-time "1 sec" nil
    `(lambda ()
       (outline-hide-body)
       (outline-toggle-children))))

(add-hook 'emacs-lisp-mode-hook 'ak-set-vim-foldmarker-and-hide-except)
(add-hook 'emacs-lisp-mode-hook (lambda () (ak-test)))
(add-hook 'sh-mode-hook 'ak-set-vim-foldmarker-and-hide-except)
;; (add-hook 'find-file-hook 'ak-set-vim-foldmarker-and-hide-except)

(setq ak-test-variable '((lambda () (ak-test)) (lambda () ak-home)))

;; }}}
;;; projectile {{{
(ak-log "projectile")

(defcustom ak-projectile-project-name
  :type 'string)
(setq ak-projectile-current-project nil)

(use-package projectile
  :commands (projectile-switch-project projectile-find-file ak-set-main-project)
  :config

  (unless (and (boundp 'ak-already-loaded) ak-already-loaded)
    (fset 'ak-ump (symbol-function 'projectile-project-root)))
  (setq ak-already-loaded t)

  (defun ak-set-main-project (&optional dir)
    "Set the projectile main project based on the current buffer.
   When called with argument DIR, make that main project instead."
    (interactive)
    (unless dir
      (setq dir (ak-ump)))
    (message (concat "dir = " dir))
    (setq ak-projectile-current-project dir)
    (projectile-add-known-project dir)
    (projectile-switch-project-by-name dir))

  (defun ak-set-project-to-switch-as-main (project-to-switch &optional arg)
    (ak-set-main-project project-to-switch))

  (defun ak-use-main-project (oldfun &rest args)
    "Skip calling `projectile-project-root' when there is a main project defined."

    (if (and (boundp 'ak-projectile-current-project)
          ak-projectile-current-project)
      (progn
        (setq ak-projectile-project-name ak-projectile-current-project)
        ak-projectile-current-project)
      (funcall oldfun)))

  (defun ak-projectile-project-name (oldfun &rest args)
    (if (and nil (boundp 'ak-projectile-project-name)
          ak-projectile-project-name)
      ak-projectile-project-name
      (funcall oldfun)))

  ;; (defun set-main-pro (project-to-switch &optional arg))
  (advice-add #'projectile-project-root :around #'ak-use-main-project)
  (advice-add #'projectile-project-name :around #'ak-projectile-project-name)

  (advice-add #'projectile-switch-project-by-name
    :before #'ak-set-project-to-switch-as-main)

  (setq projectile-generic-command "find -L . -type f -print0"))

;;; }}}
;;; feel {{{
(ak-log "feel")

;; (electric-pair-mode 1)
(global-auto-revert-mode t)
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

(message "before evil")

(use-package evil
  :config
  (setq evil-cross-lines nil)
  (evil-mode 1)
  (eval-after-load "ibuf-ext" '(evil-make-overriding-map ibuffer-mode-map))
  (eval-after-load "debug" '(evil-make-overriding-map debugger-mode-map))
  (eval-after-load "help" '(evil-make-overriding-map help-mode-map))
  (eval-after-load "magit" '(evil-make-overriding-map magit-mode-map))
  (eval-after-load "magit-popup" '(evil-make-overriding-map magit-popup-mode-map))
  (eval-after-load "magit-log" '(evil-make-overriding-map magit-log-mode-map))
  (eval-after-load "package" '(evil-make-overriding-map package-menu-mode-map))
  (eval-after-load "diff-mode" '(evil-make-overriding-map diff-mode-map))
  (eval-after-load "compilation-mode" '(evil-make-overriding-map compilation-mode-map))
  (eval-after-load "eshell-mode" '(evil-make-overriding-map eshell-mode-map))
  (dolist (mode-map '((comint-mode . insert)
                       (with-editor-mode . insert)
                       (term-mode . insert)
                       (eshell-mode . insert)
                       (help-mode . emacs)
                       (fundamental-mode . normal)
                       (transmission-mode . emacs)
                       (transmission-files-mode . emacs)
                       (message-buffer-mode . emacs)
                       (dired-mode . emacs)
                       (undo-tree-visualizer-mode . emacs)
                       (compilation-mode . emacs)
                       (magit-mode . emacs)
                       (magit-log-mode . emacs)
                       (magit-popup-mode . emacs)))
    (evil-set-initial-state `,(car mode-map) `,(cdr mode-map))))

(message "after evil")

;;; }}}
;;; hooks {{{

(add-hook 'ibuffer-mode-hook
  (lambda ()
  (setq ibuffer-expert t)
  (ibuffer-auto-mode 1)
  (setq ibuffer-show-empty-filter-groups nil)
  (ibuffer-switch-to-saved-filter-groups "default")))

(use-package ibuffer-git)

(setq ibuffer-formats
  '((
      mark modified read-only locked " "
      (name 22 18 :left :elide)
      " " (size 9 -1 :right)
      " " (mode 20 16 :left :elide)
      " " (git-status 8 8 :left)
      " " filename-and-process)))


(setq ibuffer-hook nil)
(add-hook 'ibuffer-hook
  (lambda ()
    (setq-default ibuffer-saved-filter-groups
      (list (cons "default"
              (append
                '(("Help" (or (name . "\*Help\*")
                            (name . "\*Apropos\*")
                            (name . "\*info\*"))))
                '(("Dired" (mode . dired-mode)))
                '(("Temporary" (name . "\*.*\*")))
                (ibuffer-vc-generate-filter-groups-by-vc-root)
                '(("Magit" (or (mode . magit-status-mode)
                             (mode . magit-commit))))))))
    ;; (ibuffer-vc-set-filter-groups-by-vc-root)
    (ibuffer-switch-to-saved-filter-groups "default")
    (ibuffer-jump-to-buffer (buffer-name (cadr (buffer-list))))))

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

;;; }}}

;;; dired {{{

(defun ak-dired-find-file ()
  (interactive)
  (let ((file-path (dired-get-file-for-visit)))
    (message file-path)
    (if (file-directory-p file-path)
      (dired-find-file)
      (shell-command (concat "open \"" file-path "\"")))))

(defun ak-dired-opener (path &optional interactive)
  (interactive)
  (lambda () (interactive)
    (when (= (count-windows) 1)
      (split-window-right))
    (other-window 1)
    (if interactive
      (let ((default-directory path))
        (call-interactively 'dired))
      (dired path))))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(defun ak-dired ()
  (interactive)
  (let ((default-directory "~/yd/"))
    (call-interactively 'dired)))

;;; }}}
;;; funcs {{{

(defun ak-time ()
  (interactive)
  (message (current-time-string)))

(defun ak-test ()
  (interactive)
  (funcall (intern "ak-time")))

(advice-add #'other-window
  :after (lambda (a &optional b) (interactive "p") (ignore-errors (abort-recursive-edit))))

(defun ak-find-file-home (file)
  (interactive)
  (lambda () (interactive) (find-file (expand-file-name (concat ak-home file)))))

(defun ak-half-page-down ()
  (interactive)
  (next-line 20))

(defun ak-half-page-up ()
  (interactive)
  (previous-line 20))

(global-set-key (kbd "<next>") 'ak-half-page-down)
(global-set-key (kbd "<prior>") 'ak-half-page-up)

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
    (indent-region (region-beginning) (region-end)))
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

(evil-define-operator ak-evil-erase (beg end type register yank-handler)
  (interactive "<R><x><y>")
  (evil-delete beg end type ?_ yank-handler))

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

(evil-define-operator ak-evil-delete-char (beg end type)
  :motion evil-forward-char
  (interactive "<R>")
  (evil-delete beg end type ?_))

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

(evil-define-motion ak-end-of-line-or-block ()
  (let ((ak-line-end-position (1- (line-end-position))))
    (if (or (eq evil-state 'visual)
          (eq evil-state 'normal)
          (eq evil-state 'operator)
          (eq evil-state 'motion))
      (if (or (= (point) (line-end-position))
            (= (point) (1- (line-end-position))))
        (forward-paragraph)
        (evil-end-of-line)))))

(evil-define-motion ak-beginning-of-line-or-block ()
  (let ((first-nonblank (save-excursion (back-to-indentation) (point))))
    (if (= (point) first-nonblank)
      (backward-paragraph)
      (back-to-indentation))))

(defun ak-term ()
  (interactive)
  (term explicit-shell-file-name))

(defun ak-buffer-file-name ()
  (interactive)
  (kill-ring-save buffer-file-name))

(defun ak-eval-buffer ()
  (interactive)
  (evil-normal-state)
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
  (interactive)
  (kill-new default-directory))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
    ((looking-at "\\s)") (forward-char 1) (backward-list 1))
    (t (self-insert-command (or arg 1)))))

(defvar xah-brackets nil "string of left/right brackets pairs.")
(setq xah-brackets "()[]{}<>（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")

(defvar xah-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")
(progn
  ;; make xah-left-brackets based on xah-brackets
  (setq xah-left-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 0)
      (push (char-to-string (elt xah-brackets $x))
        xah-left-brackets)))
  (setq xah-left-brackets (reverse xah-left-brackets)))

(defvar xah-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "list of right bracket chars.")
(progn
  (setq xah-right-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 1)
      (push (char-to-string (elt xah-brackets $x))
        xah-right-brackets)))
  (setq xah-right-brackets (reverse xah-right-brackets)))

(defun xah-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-11-22"
  (interactive)
  (if (nth 3 (syntax-ppss))
    (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
      ((eq (char-after) ?\") (forward-sexp))
      ((eq (char-before) ?\") (backward-sexp ))
      ((looking-at (regexp-opt xah-left-brackets))
        (forward-sexp))
      ((looking-back (regexp-opt xah-right-brackets) (max (- (point) 1) 1))
        (backward-sexp))
      (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

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

;;; }}}
;;; compile {{{

(ak-log "compile")

(defun ak-compile ()
  (interactive)
  (ak-save-all)
  (compile compile-command))

;;; }}}
;;; look {{{

;; (use-package atom-dark-theme)
;; (load-theme 'atom-dark t)

(message (concat "before theme: " (current-time-string)))
(use-package spacemacs-theme
  :defer t
  :custom
  (spacemacs-theme-comment-bg nil)
  (spacemacs-theme-comment-italic t))

(load-theme 'spacemacs-dark t)

(message (concat "after theme: " (current-time-string)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq frame-resize-pixelwise t)

(set-face-attribute 'default nil :height 150)
(set-face-attribute 'default nil :background "#000")
(set-face-attribute 'fringe nil :background "#000")
;; (set-face-attribute 'font-lock-comment-face nil :foreground "#0cc")

;; (add-hook 'minibuffer-setup-hook
;;           (lambda ()
;;             (make-local-variable 'face-remapping-alist)
;;     (add-to-list 'face-remapping-alist '(default (:background "#f00")))))

(eval-after-load "ido"
  '(set-face-attribute 'ido-first-match nil
     :foreground "#0f0"))

;; (set-face-attribute 'mode-line nil
;;     :background "#00f")

(set-face-attribute 'minibuffer-prompt nil
  :foreground "#0f0")

(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil
  :background "#333")

(show-paren-mode 1)
(set-face-attribute 'show-paren-match nil
  :background "#a0f"
  ;; :box '(:color "deep pink" :line-width 2)
  )

(add-to-list 'load-path "~/.emacs.d/hl-line+/")
(require 'hl-line+)

(require 'ansi-color)
(setq ansi-color-names-vector
  (vector (frame-parameter nil 'background-color)
    "#f57900" "#8ae234" "#edd400" "#729fcf"
    "#ad7fa8" "cyan3" "#eeeeec")
  ansi-term-color-vector ansi-color-names-vector
  ansi-color-map (ansi-color-make-color-map))

(setq evil-mode-line-format nil
  evil-normal-state-cursor '(box "#00FFFF")
  evil-emacs-state-cursor '(box "#FF0000")
  evil-insert-state-cursor '(bar "#FF0000")
  evil-visual-state-cursor '(box "#F86155"))

(use-package all-the-icons
  :config
  ;; all-the-icons doesn't work without font-lock+
  ;; And font-lock+ doesn't have autoloads
  (use-package font-lock+
    :straight (:host github :repo "emacsmirror/font-lock-plus")
    :config (require 'font-lock+)))

(setq vj/font-name "Hack")
(defcustom vj/font-size 15 "My default font size")

(defun set-frame-font-size (&optional font-size)
  "Change fram font size to FONT-SIZE.
If no FONT-SIZE provided, reset the font size to its default variable."
  (let ((font-size
          (or font-size
            (eval (car (get 'vj/font-size 'standard-value))))))
    (customize-set-variable 'vj/font-size font-size)
    (set-frame-font
      (format "%s %d" vj/font-name font-size) nil t)))

(defun increase-frame-font ()
  "Increase frame font by one."
  (interactive)
  (set-frame-font-size (+ vj/font-size 1)))

(defun decrease-frame-font ()
  "Decrease frame font by one."
  (interactive)
  (set-frame-font-size (- vj/font-size 1)))

(defun reset-frame-font ()
  "Reset frame font to its default value."
  (interactive)
  (set-frame-font-size))

(defun vj/company-box-frontend-even-if-single (command)
  (cond ((or (eq command 'hide)
           (equal company-candidates-length 0))
          (company-box-hide))
    ((eq command 'update)
      (company-box-show))
    ((eq command 'post-command)
      (company-box--post-command))))

(use-package company-box
  :after (company all-the-icons)
  :hook (company-mode . company-box-mode)
  :config
  (setq
    company-box-icons-elisp
    (list (all-the-icons-material "functions"                  :height 0.8 :face 'all-the-icons-red)
      (all-the-icons-material "check_circle"               :height 0.8 :face 'all-the-icons-blue)
      (all-the-icons-material "stars"                      :height 0.8 :face 'all-the-icons-orange)
      (all-the-icons-material "format_paint" :height 0.8 :face 'all-the-icons-pink))
    company-box-icons-lsp
    `((1  . ,(all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green)) ; text
       (2  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; method
       (3  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; function
       (4  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; constructor
       (5  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; field
       (6  . ,(all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))  ; variable
       (7  . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))   ; class
       (8  . ,(all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-red))   ; interface
       (9  . ,(all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-red))   ; module
       (10 . ,(all-the-icons-material "settings"                 :height 0.8 :face 'all-the-icons-red))   ; property
       (11 . ,(all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-red))   ; unit
       (12 . ,(all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-red))   ; value
       (13 . ,(all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-red))   ; enum
       (14 . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-red))   ; keyword
       (15 . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-red))   ; snippet
       (16 . ,(all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-red))   ; color
       (17 . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-red))   ; file
       (18 . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-red))   ; reference
       (19 . ,(all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-red))   ; folder
       (20 . ,(all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-red))   ; enumMember
       (21 . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-red))   ; constant
       (22 . ,(all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-red))   ; struct
       (23 . ,(all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-red))   ; event
       (24 . ,(all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-red))   ; operator
       (25 . ,(all-the-icons-material "class" :height 0.8 :face 'all-the-icons-red))))
  (advice-add
    #'company-box-frontend :override #'vj/company-box-frontend-even-if-single))

;;; }}}
;;; frame title {{{

;; (defvar ak-project-name "project A")

;; (defun ak-set-title ()
;;   (concat
;;   ;; (abbreviate-file-name (buffer-file-name))
;;   (if (boundp 'ak-projectile-current-project)
;;   (file-name-nondirectory (directory-file-name ak-projectile-current-project)))
;;   "  ::  "
;;   (buffer-name)
;;   "  "
;;   (format-time-string "%T")))

;; ;; (let ((name
;; ;;        (if buffer-file-name
;; ;;            (buffer-file-name)
;; ;;          (buffer-name))))
;; ;;   (concat ak-project-name " : " name)))

;; (setq frame-title-format
;;   '(:eval (ak-set-title)))

;; (setq interval 1)

;; (defun run-every-ten-seconds ()
;;   (interactive)
;;   ;; (ak-set-frame-title)
;;   (set-frame-parameter nil 'title "dummy")
;;   (set-frame-parameter nil 'title nil)
;;   ;; (message (current-time-string))
;;   )

;; (defun start-timer ()
;;   (interactive)
;;   (setq timer
;;   (run-at-time (current-time)  interval
;;   'run-every-ten-seconds)))

;; (start-timer)

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
  (advice-add 'editorconfig-format-buffer :before 'outline-show-all)
  (advice-add 'editorconfig-format-buffer :after 'outline-hide-other))

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
(gdk :states '(normal visual)
  "q"
  (gkd 'evil-record-macro :timeout 1
    "<tab>" 'ak-outline-next-heading
    "-" 'ak-outline-next-heading
    "a" 'origami-recursively-toggle-node
    "r" 'origami-open-all-nodes
    "o" 'origami-recursively-toggle-node
    "m" 'origami-close-all-nodes))

(gdk :states '(motion normal visual insert emacs)
  ;; :keymaps 'doc-view-mode-map
  "C-x M-x"
  (gkd 'projectile-switch-project :timeout 1
    "i" (ak-find-file-home ".ideavimrc")
    "b" (ak-find-file-home ".qutebrowser/config.py")
    "k" (ak-find-file-home ".config/karabiner/karabiner.json")
    "t" (gkd 'transmission :timeout 0.5
          "t" (ak-find-file-home "Library/Application Support/transmission-daemon/settings.json"))
    "v" (ak-find-file-home "yd/cfg/vim/min.vim")
    "e" (ak-find-file-home "yd/cfg/emacs/min.el")
    "q" (ak-find-file-home "yd/cfg/qmk_firmware/ak-first-keymap.c")
    "s" (ak-find-file-home "yd/cfg/sh/sh.sh")

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
    ;; "g" 'preview-buffer
    ;; "w" 'aking/test
    "w" 'ak/view-pdf
    ;;"v" 'aking/view-pdf
    "h" 'avy-goto-word-1
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

(gdk :states '(motion normal visual emacs)
  "(" 'ak-half-page-up)

(gdk :states '(motion normal visual operator insert)
  "s-<right>" 'move-end-of-line
  "s-<left>" 'back-to-indentation)

;; (define-key key-translation-map (kbd "C-x D") nil)

(gdk :states '(motion normal visual operator insert emacs)
  "C-k" nil                             ; kill-line restore
  "C-a" nil
  "C-e" nil
  "C-z" nil
  "C-y" nil
  "C-w" nil
  "C-f" nil
  "C-b" nil
  "h" nil

  "M-x" '(lambda () (interactive) (counsel-M-x ""))
  "C-x C-c" 'save-buffers-kill-emacs
  "C-x C-j C-c" 'save-buffers-kill-terminal
  "C-x b" 'ibuffer
  "C-x g" 'magit-status
  "C-x e" 'ak-eval-buffer
  "C-x C-p" 'projectile-switch-project
  "C-x ;" 'comment-line
  "C-x f" 'projectile-find-file
  "C-x C-j b" 'ak-compile
  "C-x C-j C-b" 'ak-compile
  "C-x C-j i" 'kill-compilation
  "C-x C-j k <escape>" 'ak-switch-to-previous-buffer
  "C-x C-j k C-[" 'ak-switch-to-previous-buffer

  "C-x C-j d" (ak-dired-opener default-directory)
  "C-x C-j D" 'ak-test
  "C-x C-j C-d" 'projectile-dired
  "C-x C-j M-d" (ak-dired-opener "~/yd")
  "C-x C-j s-d" (ak-dired-opener "~/yd")

  "C-x C-j C-t" 'ak-eshell-other-window
  "C-x C-j 0" 'ak-toggle-window-split
  "C-M-%" 'ak-query-replace-regexp-or-joker
  "C-x C-j p" 'ak-buffer-file-name
  "s-C" 'ak-buffer-file-name
  "C-x C-j C-p" 'ak-default-directory

  ;; terminal
  "M-:" 'eval-expression
  "C-x o" 'other-window
  "C-x 0" 'delete-window
  "C-x 3" 'split-window-right
  "C-h k" 'describe-key

  "C-c C-s" 'eval-buffer
  "M-e" 'move-end-of-line
  "M-a" 'move-beginning-of-line
  "C-c C-y" 'ak-paste-after-prepending-nl
  "C-M-y" 'ak-paste-after-prepending-2nl
  "S-<f10>" 'recompile
  ;; "C-f" 'projectile-find-file
  "C-d" 'ak-duplicate-after
  "C-M-S-t" 'mode-line-other-buffer
  "s-<return>" 'ak-make
  "M-s-g" 'ak-generate-makefile
  "C-M-i" 'evil-jump-item
  ;;    :predicate '(not (derived-mode-p 'term-mode))
  "M-<right>" 'forward-word
  "M-<left>" 'evil-backward-word-begin
  "M-s-g" 'ak-generate-makefile
  "C-M-i" 'evil-jump-item
  "M-s-g" 'ak-generate-makefile
  "C-M-i" 'evil-jump-item
  "C-M-e" 'er/expand-region)

;; ----------------------------------------------------------------------------
;; motion keymap
;; ----------------------------------------------------------------------------

(gdk :states '(motion normal visual operator)
  "RET" 'ak-newline
  ;; basic movement:
  "t" 'evil-forward-char
  "m" 'evil-backward-char
  "n" 'evil-forward-word-begin
  "N" 'evil-forward-WORD-begin
  "p" 'evil-backward-word-begin
  "P" 'evil-backward-WORD-begin
  "\\" 'evil-forward-word-end
  "b" 'evil-forward-WORD-end
  "d" 'ak-beginning-of-line-or-block
  "s" 'ak-end-of-line-or-block
  "c" 'evil-next-line
  "r" 'evil-previous-line
  "f" 'ak-half-page-up
  "g" 'ak-half-page-down
  ;; advanced movement:
  "j" 'evil-forward-WORD-begin
  ;; "b" (gkd 'evil-find-char-to :timeout 0.5
  ;;    "r" 'ak-test)
  "|" 'evil-find-char-to
  "h" 'evil-search-forward
  "z" 'evil-jump-item
  "}" (gsk "C-o")
  "{" '(lambda () (interactive) (evil-first-non-blank) (evil-previous-open-brace))
  "%" 'ak-jump-item
  ")" 'ak-outline-next-heading
  "(" 'outline-previous-heading
  "-" 'outline-toggle-children
  ;; "(" 'forward-paragraph
  ;; ")" 'backward-paragraph
  ;; "(" 'ak-half-page-up
  ;; ")" 'ak-half-page-down
  ;; "-" (gsk "_ p l")
  ;; "-" (gkd 'evil-change :timeout 0.5
  ;;    "r" (gsk "^ i \"")
  ;;    "f" (gsk "^ i {")
  ;;    "[" (gsk "^ i [")
  ;;    "(" (gsk "^ i b")
  ;;    "g" (gsk "^ i <")
  ;;    "-" (gsk "^ i w")
  ;;    ")" (gsk "^ h )")
  ;;    "," (gsk "^ h ,")
  ;;    "." (gsk "^ h ."))
  "e" (gkd 'evil-change :timeout 1
        "r" (gsk "^ i \"")
        "[" (gsk "^ i [")
        "(" (gsk "^ i (")
        "g" (gsk "^ i <")
        "-" (gsk "^ i w")
        ":" (gsk "^ | :")
        "SPC" (gsk "^ | SPC")
        "\"" (gsk "^ | \"")
        "f" (gsk "^ | \"")
        ")" (gsk "^ | )")
        "}" (gsk "^ | }")
        "]" (gsk "^ | ]")
        ">" (gsk "^ | >")
        "c" (gsk "^ | ,")
        "." (gsk "^ | ."))
  ;; visual:
  "_" 'evil-visual-char
  "," (gsk "0 _ $")
  "H" 'evil-visual-line
  ;; yank/paste:
  "y" 'ak-yank
  ;; "b" 'evil-paste-after
  ;; "B" 'evil-paste-before
  "w" 'ak-paste-after-prepending-nl
  "W" 'ak-paste-before-appending-nl
  "v" 'ak-paste-after-prepending-2nl
  "V" 'ak-paste-before-appending-2nl
  ;; delete/change:
  "l" 'evil-delete
  "^" 'evil-change
  "k" 'ak-evil-delete-char
  "K" 'evil-delete-backward-char
  "j" 'ak-evil-erase
  ;; misc:
  "'" 'evil-join
  "J" (gsk "a <return>")
  ;; "DEL" 'projectile-find-file
  ;; foldig:
  "x" nil
  "x r" 'outline-show-all
  "x a" 'outline-show-subtree
  "TAB" 'outline-toggle-children
  "x m" 'outline-hide-body
  ;; "x r" 'vimish-fold-unfold-all
  ;; "x a" 'vimish-fold-unfold
  ;; "TAB" 'vimish-fold-toggle
  ;; "x m" 'vimish-fold-refold-all
  ;; "x a" 'origami-toggle-node
  ;; "TAB" 'origami-recursively-toggle-node
  ;; "x r" 'origami-open-all-nodes
  ;; "x o" 'origami-open-node
  ;; "x m" 'origami-close-all-nodes
  )

(gdk :states '(motion normal)
  "Z" (gsk "D %")
  "G" (gsk "0 D c s")
  "C" (gsk "D r s o s m")
  "F" (gsk "D s o r"))

(gdk :states '(visual)
  "TAB" 'ak-duplicate
  "c" (gsk "<down> $ m")
  "r" (gsk "<up> $ <left>")
  "Z" (gsk "D %")
  "C" (gsk "0 D c s")
  "R" (gsk "s D r")
  "G" (gsk "c s m")
  "F" (gsk "D s o r"))

(gdk :keymaps 'org-mode-map
  :states '(motion normal visual)
  "TAB" 'org-cycle
  "." 'org-cycle
  "(" 'outline-up-heading)

(evil-set-initial-state 'messages-buffer-mode 'motion)

(add-hook 'lispy-mode-hook
  (lambda ()
    (gdk :states '('normal 'visual) :keymaps 'lispy-mode-map
      ";" 'lispy-comment)))


;; (add-hook
;;  'transmission-mode-hook
;;  (lambda ()
;;    (evil-set-initial-state 'transmission-mode 'emacs)
;;    (gdk
;;      :states 'emacs
;;      :keymaps 'transmission-mode-map
;;      "c" 'next-line
;;      "r" 'previous-line)))

;; (evil-set-initial-state 'transmission-mode 'emacs)
(use-package transmission
  ;; :init
  ;; (evil-set-initial-state 'transmission-mode 'emacs)
  :general
  (:states 'emacs
    "c" 'next-line
    "r" 'previous-line))

(add-hook 'undo-tree-visualizer-mode-hook
  (lambda ()
    (gdk
      :states 'emacs
      :keymaps 'undo-tree-visualizer-mode-map
      "c" 'next-line
      "r" 'previous-line
      "f" 'undo-tree-visualize-switch-branch-right
      "g" 'undo-tree-visualize-switch-branch-left)))

(add-hook 'help-mode-hook
  (lambda ()
    (gdk
      :states 'emacs
      :keymaps 'help-mode-map
      ;; ")" 'ak-half-page-down
      ;; "(" 'ak-half-page-up
      )))

(add-hook 'transmission-files-mode-hook
  (lambda ()
    ;; (evil-set-initial-state 'transmission-files-mode 'emacs)
    (gdk
      :states 'emacs
      :keymaps 'transmission-files-mode-map
      "RET" 'ak-transmission-find-file
      "s-RET" 'ak-transmission-find-file-dir
      "c" 'next-line
      "r" 'previous-line)))

;; (evil-make-overriding-map transmission-files-mode-map)

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
      "C" 'dired-copy-file
      "R" 'dired-do-rename
      "z" 'dired-up-directory
      "RET" 'ak-dired-find-file
      "<mouse-1>" 'ak-dired-find-file
      "<mouse-2>" 'dired-find-file
      "<mouse-3>" 'dired-find-file-other-window)))




(add-hook 'dired-mode-hook
  (lambda ()
    (evil-define-key
      'normal dired-mode-map "r" 'dired-previous-line)))

(add-hook 'magit-status-mode-hook
  (lambda ()
    (gdk
      :keymaps 'magit-status-mode-map
      "C-x g" 'magit-commit-popup
      ")" 'magit-commit-popup
      "c" 'magit-section-forward
      "r" 'magit-section-backward)))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (gdk
      :keymaps 'ibuffer-mode-map
      :state 'emacs
      "c" 'ibuffer-forward-line
      "r" 'ibuffer-backward-line
      "g" 'ibuffer-forward-filter-group
      "f" 'ibuffer-backward-filter-group)))

(gdk :keymaps 'package-menu-mode-map
  :states 'emacs
  "r" 'previous-line)

;; (evil-global-set-key 'normal "r" 'evil-previous-line)

;; (evil-define-minor-mode-key 'normal 'dired-mode "r" 'dired-previous-line)

;; (gdk-ov :states '(motion normal visual)
;;   :predicate '(derived-mode-p 'magit-status-mode)
;;   "k" 'magit-commit-popup
;;   "j" 'magit-rebase-popup
;;   "c" 'evil-next-line
;;   "r" 'evil-previous-line)

;;   (gdk :keymaps 'latex-mode-map
;;     "SPC" 'aking/yas-expand-or-self-insert
;;     "q" 'aking/project-sq)

;;; }}}
;;; test stuff {{{

(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

;;; }}}

;;;(message "config successfully loaded")

;; mergetest
