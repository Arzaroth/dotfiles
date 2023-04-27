;;; CLOSE ALL BUFFERS
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(require 'term)
(defun visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/zsh")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))

(defun kill-region-or-word ()
  "Call `kill-region' or `backward-kill-word' depending on
whether or not a region is selected."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (kill-region (point) (mark))
    (backward-kill-word 1)))
(global-set-key "\C-w" 'kill-region-or-word)

(defun buffer-same-mode (change-buffer-fun)
  (let ((current-mode major-mode)
        (next-mode nil))
    (while (not (eq next-mode current-mode))
      (funcall change-buffer-fun)
      (setq next-mode major-mode))))

(defun previous-buffer-same-mode ()
  (interactive)
  (buffer-same-mode #'previous-buffer))

(defun next-buffer-same-mode ()
  (interactive)
  (buffer-same-mode #'next-buffer))

(defun minimap-toggle ()
  "Toggle minimap for current buffer."
  (interactive)
  (if (null minimap-bufname)
      (minimap-create)
    (minimap-kill)))

(let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

(provide 'misc)
