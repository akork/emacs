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
