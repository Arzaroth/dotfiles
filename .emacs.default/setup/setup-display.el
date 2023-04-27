(column-number-mode 1)
(tool-bar-mode -1)
(global-hl-line-mode)
(load-theme 'sanityinc-tomorrow-bright t)
(setq term-default-bg-color nil)
(setq iswitchb-use-frame-buffer-list t
      iswitchb-max-to-show 20
      iswitchb-default-method 'samewindow)
(iswitchb-mode 1)
(setq ring-bell-function 'ignore)
(display-time-mode t)
(setq inhibit-startup-message t
      initial-scratch-message nil)
(blink-cursor-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)
(transient-mark-mode 1)
(delete-selection-mode 1)
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))
(setq show-trailing-whitespace t)

(provide 'setup-display)
