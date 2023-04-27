;;; GESTION DES PARENTHESES
(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)
(setq blink-matching-paren t)
(setq blink-matching-paren-on-screen t)
(setq show-paren-style 'expression)
(setq blink-matching-paren-dont-ignore-comments t)
(defun goto-match-paren (arg)
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char
                                                1))
        ((looking-at "\\s\)") (forward-char 1)
         (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "M-%") 'goto-match-paren)

(require 'rainbow-delimiters)
(rainbow-delimiters-mode t)
(add-hook 'c-mode-hook (lambda () (rainbow-delimiters-mode t)))
(add-hook 'c++-mode-hook (lambda () (rainbow-delimiters-mode t)))
(add-hook 'python-mode-hook (lambda () (rainbow-delimiters-mode t)))
(add-hook 'slime-mode-hook (lambda () (rainbow-delimiters-mode t)))
(add-hook 'lisp-mode-hook (lambda () (rainbow-delimiters-mode t)))
(add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-delimiters-mode t)))

;;; AUTOPAIR
;; (require 'autopair)
;; (autopair-global-mode) ;; to enable in all buffers
;; (add-hook 'term-mode-hook
;; 	  #'(lambda ()
;; 	      (autopair-mode -1)))

(electric-pair-mode t)
(provide 'setup-parent)
