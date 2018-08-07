(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/use-package")
(require 'use-package)

(require 'org)
(org-babel-load-file  "/users/aleksey/dropbox/settings/emacs/basic-org.org")
 ;; (expand-file-name "/users/aleksey/dropbox/settings/emacs/basic.org"
 ;;                   user-emacs-directory))

;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;;   (add-to-list 'default-frame-alist '(ns-appearance . dark))

;;   (load-theme 'manoj-dark)
;;   (setq visible-bell 1)
;;   (setq scroll-margin 8)
;;   (setq scroll-conservatively 10000)
;;   (save-place-mode 1)

;; (require 'column-marker)

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
