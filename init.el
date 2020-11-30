(cond ((string= system-type "windows-nt") (setq ak-home "c:/users/ak"))
      ((string= system-type "darwin") (setq ak-home "/users/ak")))
(setq ak-cfg (concat ak-home "/yd/cfg"))

(load (concat ak-cfg "/emacs/emacsrc.el"))
