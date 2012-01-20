(defvar this-file-argv nil)
(make-variable-buffer-local 'this-file-argv)

(defcustom simple-compile-modes-alist
  '((python-mode . ("python " kill-compilation))
    (ruby-mode . ("ruby "  kill-compilation))
    (perl-mode . ("perl " kill-compilation))
    (shell-script-mode . ("./" kill-compilation))
    )
  "Alist of modes mapping to the command to run")

(defun run-compile (&optional argv)
  (interactive)
  (if arg
      (setq this-file-argv argv))
  (when (equal this-file-argv nil)
    (simple-compile-change-argv))
  (let ((command-to-run (cadr (assoc major-mode simple-compile-modes-alist))))
    (async-shell-command (concat command-to-run (buffer-file-name) " " this-file-argv) "*Simple-Compile*")))

(defun simple-compile-change-argv ()
  (interactive)
  (setq this-file-argv  (read-from-minibuffer "What argv do you want: ")))

(provide 'simple-compile)
