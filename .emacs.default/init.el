(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))

(setq site-lisp-dir (expand-file-name "site-lisp" dotfiles-dir))
(setq setup-dir (expand-file-name "setup" dotfiles-dir))

(add-to-list 'load-path setup-dir)
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq custom-file (expand-file-name "custom.el" setup-dir))
(load custom-file)

(require 'yasnippet)
(require 'dropdown-list)
(yas-global-mode 1)
(setq yas-snippet-dirs (expand-file-name "snippets" dotfiles-dir))
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))

(require 'bm)
(require 'multiple-cursors)
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" dotfiles-dir))
(require 'ido)
(ido-mode t)
(ido-vertical-mode t)
(require 'ace-jump-mode)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'setup-modes)
(require 'setup-epitech)
(require 'setup-display)
(require 'setup-hooks)
(require 'setup-linum)
(require 'setup-parent)
(require 'setup-cedet)
(require 'setup-jedi)
(require 'setup-ac)
(require 'key-bindings)
(require 'misc)

(global-auto-revert-mode t)

(global-undo-tree-mode)

(setq-default indent-tabs-mode nil
	      standard-indent 4)

;; (require 'auto-complete-c-headers)
;; (add-to-list 'ac-sources 'ac-source-c-headers)
