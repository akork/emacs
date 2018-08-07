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
  
  (gmap :states '(normal visual motion)
    :keymaps 'magit-status-mode-map
    "k" 'magit-commit-popup
    "j" 'magit-rebase-popup
    "c" 'evil-next-line
    "r" 'evil-previous-line)

  (defun ak-org-edit-src ()
    (interactive)
    (if (derived-mode-p 'org-mode)
	(org-edit-special)
      (org-edit-src-exit)))

  (gmap :states '(normal visual motion operator)
    :predicate '(not (derived-mode-p 'magit-status-mode))
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
    "C-a" 'evil-first-non-blank)

  (gmap :states '(normal visual motion)
    :predicate '(derived-mode-p 'org-mode)
    "TAB" 'org-cycle
    "(" 'outline-up-heading
    "h" 'org-edit-special)


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
