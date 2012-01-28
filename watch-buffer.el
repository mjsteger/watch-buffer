;;; watch-buffer.el --- run a shell command when saving a buffer

;; Copyright (C) 2012  Michael Steger

;; Author: Michael Steger <mjsteger1@gmail.com>
;; Keywords: automation, convenience
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This extension provides a way to connect updating a buffer with running
;; a shell command.  So you can have a shell script which makes and runs a
;; c program, and then you would M-x watch-buffer, enter the shell script
;; to run, and every time you save the file it will run the shell script
;; asynchronously in a seperate buffer

;;; Code:

(require 'simple-compile)

(defcustom watch-buffer-types
  '(("watch-buffer" . (watch-buffer watch-buffer-async-shell-command))
    ("watch-buffer-silently" . (watch-buffer-silently call-process-shell-command))
    ("watch-buffer-elisp" . (watch-buffer-elisp watch-buffer-apply-elisp))
    ("watch-buffer-compile" . (watch-buffer-compile run-compile))
    ("watch-buffer-additive" . (watch-buffer-additive run-compile-additive))
    )"Assoc list of the tag, interactive command, and command to use to evaluate")

(defun general-watch (type &optional command)
  (if (equal command nil)
      (add-to-watcher (read-from-minibuffer "What command do you want: " ) type)
    (add-to-watcher command type)))

(defmacro watch-buffer-command (tag name)
  `(defun ,name (&optional command)
     (interactive)
     (general-watch ,tag command)))

(defun build-interactive-functions ()
  (mapcar (lambda (arg)
	    (eval `(watch-buffer-command ,(car arg) ,(cadr arg)))) watch-buffer-types))

(defun watch-buffer-async-shell-command (this-command)
  "Async-shell-command with the name of the buffer set to *Watch-Process"
  (async-shell-command this-command (concat "*Watch-Process-" this-command "*")))

(defun watch-buffer-apply-elisp (elisp-function)
  (let ((splitted-string (split-string elisp-function)))
    (apply (intern (car splitted-string)) (cdr splitted-string ))))

(defvar watch-buffer-commands-alist '()
  "Alist that holds the commands to run for this buffer. The format of
  the data will be an alist of lists, which can have as the car any of
  the variables currently defined in watch-buffer-types")
(make-variable-buffer-local 'watch-buffer-commands-alist)

(defun add-to-watcher (command tag)
  (if (not (assoc tag watch-buffer-commands-alist))
      (setq watch-buffer-commands-alist (cons (list tag) watch-buffer-commands-alist)))
  (let ((this-tag-list (assoc tag watch-buffer-commands-alist)))
    (setcdr this-tag-list (cons (list command) (cdr this-tag-list)))))

(defun watch-buffer-reload-commands ()
  (when watch-buffer-commands-alist
    (mapc 'apply-watch-buffer-command watch-buffer-commands-alist)))

(defun apply-watch-buffer-command (file-alist)
  (let ((command-to-apply (caddr (assoc (car file-alist) watch-buffer-types))))
    (print (cdr file-alist))
    (mapc (lambda (x) (eval `(,command-to-apply (car x)))) (cdr file-alist))))

(defun unwatch-buffer ()
  (interactive)
  (setq watch-buffer-commands-alist '())
  "Function to remove a buffer from the watch-buffer-commands-alist.")

(defun add-after-save-hook ()
  (add-hook 'after-save-hook 'watch-buffer-reload-commands)
  "Add the watch-buffers check to the after-save-hook")

(add-after-save-hook)
(build-interactive-functions)

(provide 'watch-buffer)
