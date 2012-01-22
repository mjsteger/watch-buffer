(defvar this-file-argv nil)
(make-variable-buffer-local 'this-file-argv)

(defcustom simple-compile-modes-alist
  '((python-mode . ("python " kill-compilation))
    (ruby-mode . ("ruby -w "  kill-compilation))
    (perl-mode . ("perl " kill-compilation))
    (shell-script-mode . ("./" kill-compilation))
    )
  "Alist of modes mapping to the command to run")

(defun run-compile (&optional argv)
  (interactive)
  (if argv
      (setq this-file-argv argv))
  (when (equal this-file-argv nil)
    (simple-compile-change-argv))
  (let ((command-to-run (cadr (assoc major-mode simple-compile-modes-alist))))
    (async-shell-command (concat command-to-run (buffer-file-name) " " this-file-argv) "*Simple-Compile*")))

(defun simple-compile-change-argv ()
  (interactive)
  (setq this-file-argv  (read-from-minibuffer "What argv do you want: ")))

(defun run-compile-additive (&optional argv)
  (interactive)
  (if argv
      (setq this-file-argv argv))
  (when (equal this-file-argv nil)
    (simple-compile-change-argv))
  (let ((command-to-run (cadr (assoc major-mode simple-compile-modes-alist)))
	(buffer-test-name (concat (file-name-sans-extension (buffer-file-name)) ".test"))
	(temp-file-name (concat (file-name-sans-extension (buffer-file-name)) ".tmp" )))
    (call-process-shell-command (concat "cat " (buffer-file-name) ">>" temp-file-name))
    (call-process-shell-command (concat "cat " buffer-test-name ">>" temp-file-name))
    (async-shell-command (concat command-to-run temp-file-name " " this-file-argv) "*Simple-Compile*")
    (call-process-shell-command (concat "rm " temp-file-name)))
  )

(provide 'simple-compile)
