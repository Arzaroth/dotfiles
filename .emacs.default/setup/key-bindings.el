(global-set-key (kbd "S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-<down>") 'shrink-window)
(global-set-key (kbd "<select>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "C-c j") 'replace-string)
(global-set-key (kbd "C-c h") 'replace-regexp)
(global-set-key (kbd "C-c g") 'gdb-many-windows)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key [(control ?x) (control ?	)] 'auto-complete)
(global-set-key (kbd "M-/") 'undo-tree-redo)
(global-set-key (kbd "C-u") '(lambda () (interactive) (kill-line 0)))

(setq-default gdb-many-windows t)

(global-set-key (kbd "M-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "M->") 'mc/mark-next-like-this)

(global-set-key (kbd "M-0") 'bm-toggle)
(global-set-key (kbd "M-1") 'bm-next)
(global-set-key (kbd "M-3") 'bm-previous)

(global-set-key (kbd "<f2>") 'visit-ansi-term)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c c SPC") 'ace-jump-char-mode)

(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(provide 'key-bindings)
