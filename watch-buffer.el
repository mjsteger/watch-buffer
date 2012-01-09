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
;; This extension provides a way to connect updating a buffer with running a shell command
;; So you can have a shell script which makes and runs a c program, and then you would
;; M-x watch-buffer, enter the shell script to run, and every time you save the file it
;; will run the shell script asynchronously in a seperate buffer


(defgroup watch-buffer nil
  "Watching buffers, and running commands"
  :group 'watch)

(defvar *watched-buffers #s(hash-table size 100 test equal data())
  "Hash that holds the buffers to watch, and the commands to run")

(defun add-to-watcher (file command)
  (puthash file command *watched-buffers))

(defun remove-from-watcher (file)
  (remhash file *watched-buffers))

(defun should-reload ()
  (setq my-shell-command (gethash (buffer-file-name) *watched-buffers))
  (if (not (equal my-shell-command nil))
      (async-shell-command my-shell-command "*Watch-Process*")))

(defun watch-buffer ()
  (interactive)
  (add-to-watcher (buffer-file-name) (read-from-minibuffer "What command do you want: " ))
  "Function to add a buffer to the *watched-buffers hash, with the command to be run")

(defun unwatch-buffer ()
  (interactive)
  (remove-from-watcher (buffer-file-name))
  "Function to remove a buffer from the *watched-buffers hash.")

(defun add-after-save-hook ()
  (add-hook 'after-save-hook 'should-reload)
  "Add the watch-buffers check to the after-save-hook")

(add-after-save-hook)
