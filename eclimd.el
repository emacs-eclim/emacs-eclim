;;; eclimd.el --- Start and stop eclimd from within emacs  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2012 Vedat Hallac

;; Authors: Vedat Hallac
;; Version: 1.0
;; Created: 2012/05/11
;; Keywords: java, emacs-eclim

;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; Common code used by other files in this package.
;;
;;; Code:

(require 'eclim-common)
(require 'cl-lib)
(eval-when-compile (require 'eclim-macros))

(defgroup eclimd nil
  "eclimd customizations"
  :prefix "eclimd-"
  :group 'eclim)

(defcustom eclimd-executable
  nil
  "The eclimd executable to use.
Set to nil to auto-discover from `eclim-executable' value (the default value).
Set to \"eclimd\" if eclim and eclimd are in `exec-path'. Otherwise, set to
the full path to eclimd executable."
  :type '(choice (const :tag "Same directory as eclim-executable variable" nil)
                 (string :tag "Custom value" "eclimd"))
  :group 'eclimd)

(defcustom eclimd-default-workspace
  "~/workspace"
  "The default value to use when `start-eclimd' asks for a workspace."
  :type 'directory
  :group 'eclimd)

(defcustom eclimd-wait-for-process
  nil
  "Make `start-eclimd' block until the eclimd process is ready.
When this variable is nil, `start-eclimd' returns immediately after
eclimd process is started. Since the eclimd process startup takes a few seconds,
running eclim commands immediately after the function returns may cause failures.
You can freeze emacs until eclimd is ready to accept commands with this variable."
  :tag "Wait until eclimd is ready"
  :type 'boolean
  :group 'eclimd)

(defvar eclimd-process-buffer nil
  "Buffer used for communication with eclimd process")

(defvar eclimd-process nil
  "The active eclimd process")

(defvar eclimd-port nil)

(defconst eclimd-process-buffer-name "eclimd")

(defun eclimd--executable-path ()
  (if eclimd-executable
      (executable-find eclimd-executable)
    (let ((eclim-prog (executable-find eclim-executable)))
      (expand-file-name "eclimd" (file-name-directory eclim-prog)))))

(defconst eclimd--started-regexp
  "Eclim Server Started on\\(?: port\\|:\\) \\(?:\\(?:[0-9]+\\.\\)\\{3\\}[0-9]+:\\)?\\([0-9]+\\)")

(defun eclimd--read-workspace-dir ()
  (read-directory-name "Eclimd workspace directory: "
                       eclimd-default-workspace nil t))

(defcustom eclimd-autostart-with-default-workspace
  nil
  "Whether to skip asking the user about a workspace when eclimd gets autostarted.
When `eclimd-autostart' is set to t, this option controls whether
eclimd is started silently with `eclimd-default-workspace' as
workspace or whether the user is asked for a workspace as with
regular calls to `start-eclimd'."
  :tag "Autostart eclimd with default workspace"
  :type 'boolean
  :group 'eclimd)

(defun eclimd--autostart-workspace ()
  (if eclimd-autostart-with-default-workspace
      eclimd-default-workspace
    (eclimd--read-workspace-dir)))

(defcustom eclimd-autostart
  nil
  "Automatically start eclimd from within Emacs when needed.
You may want to set this to nil if you prefer starting eclimd
manually and don't want it to run as a child process of
Emacs. eclimd gets started either when `eclim-mode' is enabled or
the first time `global-eclim-mode' needs it to determine if
`eclim-mode' should be enabled in a buffer. See also
`eclimd-autostart-with-default-workspace'."
  :tag "Autostart eclimd"
  :type 'boolean
  :group 'eclimd)

(defvar eclimd--process-event-functions nil
  "List of functions to run when eclimd outputs text or changes state.
Functions receive the process, the output string and the process
state as argument. Any of the last two may be nil (but never
both). When a function returns nil it is removed from the list,
functions returning non-nil are kept.")

(defun eclimd--process-sentinel (proc state)
  (setq eclimd--process-event-functions
        (cl-remove-if-not (lambda (fun) (funcall fun proc nil state))
                          eclimd--process-event-functions)))

(defvar eclimd--comint-process-filter nil)

(defun eclimd--process-filter (proc string)
  (setq eclimd--process-event-functions
        (cl-remove-if-not (lambda (fun) (funcall fun proc string nil))
                          eclimd--process-event-functions))
  ;; Appends output to eclimd buffer.
  (when eclimd--comint-process-filter
    (funcall eclimd--comint-process-filter proc string)))

(defun eclimd--match-process-output (regexp &optional async callback)
  "Wait for the given process to output a string that matches the specified regexp.
When ASYNC is omitted or nil block and return the string used for
`match-string' if a match is found, and nil if the process is
killed. Execute CALLBACK when the process outputs the desired
string or terminates and pass the corresponding return value as
argument. CALLBACK can access the match data produced by calls to
`match-string' with REGEXP and the process output as argument,
after the function returns this data is no longer available."
  (let* ((output "")
         (terminated-p)
         (finished-p)
         (closure (lambda (proc string _state)
                    (setf output (concat output string))
                    (setf terminated-p (not (eq 'run (process-status proc))))
                    (setf finished-p (or terminated-p
                                         ;; Emacs automatically saves and
                                         ;; restores the match data when running
                                         ;; process filter functions and
                                         ;; sentinels (from where this closure
                                         ;; is called).
                                         (string-match regexp output)))
                    (when (and finished-p callback)
                      (funcall callback (unless terminated-p output)))
                    ;; Remove the closure from the hook when it has finished.
                    (not finished-p))))

    (if async
        (add-hook 'eclimd--process-event-functions closure)
      (unwind-protect
          (progn
            (add-hook 'eclimd--process-event-functions closure)
            (while (not finished-p)
              (accept-process-output eclimd-process))
            (unless terminated-p output))
        (remove-hook 'eclimd--process-event-functions closure)))))

(defun eclimd--await-connection (&optional async callback)
  "Execute callback when eclimd server becomes active.
CALLBACK receives no arguments. It is not called when eclimd
fails to start."
  (eclimd--match-process-output
   eclimd--started-regexp async
   (lambda (output)
     (when output
       (setq eclimd-port (match-string 1 output))
       (when callback (funcall callback))))))

(defun start-eclimd (workspace-dir &optional callback)
  "Start the eclimd process and optionally wait for it to be ready.
This will ask for a workspace directory, and it will attempt to
start eclimd program with the entered workspace directory. The
configurable variable `eclimd-default-workspace' controls the
default value of this directory. After having started the deamon,
it will block until eclimd is ready to receive commands since
otherwise those would fail. You can modify
`eclimd-wait-for-process' to prevent this command from
blocking. To stop the started process and you should use
`stop-eclimd'."
  (interactive (list (eclimd--read-workspace-dir)))
  (let ((eclimd-prog (eclimd--executable-path)))
    (if (not eclimd-prog)
        (message "Cannot start eclimd: check eclimd-executable variable.")
      (if eclimd-process
          (message "Cannot start eclimd: eclimd was already started.")
        (message (concat "Starting eclimd for workspace: " workspace-dir "..."))
        (setq eclimd-process-buffer
              (make-comint eclimd-process-buffer-name
                           eclimd-prog
                           nil
                           (concat "-Dosgi.instance.area.default="
                                   (replace-regexp-in-string "~" "@user.home" workspace-dir))))
        (setq eclimd-process (get-buffer-process eclimd-process-buffer))
        (setq eclimd--comint-process-filter (process-filter eclimd-process))
        (set-process-filter eclimd-process 'eclimd--process-filter)
        (set-process-sentinel eclimd-process 'eclimd--process-sentinel)
        (add-hook 'kill-emacs-hook #'stop-eclimd)
        ;; The flag is required because on exit Emacs asks the user about
        ;; running processes before running the `kill-emacs-hook'.
        (set-process-query-on-exit-flag eclimd-process nil)
        (eclimd--await-connection
         (not eclimd-wait-for-process)
         (lambda ()
           (message "eclimd serving at port %s" eclimd-port)
           (when callback (funcall callback))))))))

(defun eclimd--ensure-started (&optional async callback)
  "Ensure eclimd is running, autostarting it when possible.
An error is raised when `eclimd-autostart' is nil but there is no
eclimd process."
  (if (eclim--connected-p)
      (when callback (funcall callback))
    (if eclimd-autostart
        (if eclimd-process
            ;; `eclimd-process' is set but we can not connect to eclimd, thus
            ;; eclimd is currently being started.
            (eclimd--await-connection async callback)
          (let (eclimd-wait-for-process (not async))
            (start-eclimd (eclimd--autostart-workspace) callback)))
      (error "Autostarting of eclimd is disabled, please start eclimd manually."))))

(defun stop-eclimd ()
  "Gracefully terminate the started eclimd process.
This command asks the running eclimd process to terminate, kills
the *eclimd*-buffer and removes any hooks added by
`start-eclimd'."
  (interactive)
  (when eclimd-process
    (when (eclim--connected-p)
      (eclim/execute-command "shutdown")
      (eclimd--match-process-output "Process eclimd finished"))
    (delete-process eclimd-process)
    (setq eclimd-process nil))
  (when eclimd-process-buffer
    (kill-buffer eclimd-process-buffer)
    (setq eclimd-process-buffer nil))
  (remove-hook 'kill-emacs-hook #'stop-eclimd))

(provide 'eclimd)
;;; eclimd ends here
