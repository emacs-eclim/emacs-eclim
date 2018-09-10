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
(eval-when-compile
  (require 'eclim-macros)
  (require 'cl-lib)
  (require 'dash))

;;;###autoload(defalias 'start-eclimd 'eclimd-start)
(defalias 'stop-eclimd 'eclimd-stop)

(defgroup eclimd nil
  "eclimd customizations"
  :prefix "eclimd-"
  :group 'eclim)

(defcustom eclimd-executable
  nil
  "The eclimd executable to use.
Set to nil to auto-discover from `eclim-executable' value
\(the default).  Set to \"eclimd\" if eclim and eclimd are
in function `exec-path'.  Otherwise, set to the full path of the
eclimd executable."
  :type '(choice (const :tag "Same directory as eclim-executable variable" nil)
                 (string :tag "Custom value" "eclimd"))
  :group 'eclimd)

(defcustom eclimd-default-workspace
  "~/workspace"
  "The default value to use when `eclimd-start' asks for a workspace."
  :type 'directory
  :group 'eclimd)

(defcustom eclimd-wait-for-process
  nil
  "Non-nil means `eclimd-start' blocks until eclimd is ready.
When this variable is nil, `eclimd-start' returns
immediately after the eclimd process is started.  Since the
eclimd process startup takes a few seconds, running eclim
commands immediately after the function returns may cause
failures.  You can freeze Emacs until eclimd is ready to
accept commands with this variable."
  :tag "Wait until eclimd is ready"
  :type 'boolean
  :group 'eclimd)

(defvar eclimd-process-buffer nil
  "Buffer used for communication with the eclimd process.")

(defvar eclimd-process nil
  "The active eclimd process.")

(defvar eclimd-port nil
  "The port on which eclimd is serving.
This is nil unless the eclimd server is running and ready.")

(defconst eclimd-process-buffer-name "eclimd"
  "The name to use for the eclimd process buffer.")

(defun eclimd--executable-path ()
  "Return path to the eclimd executable.
This can be set explicitly with `eclimd-executable'.  If
that variable is not set, this function will attempt to
discover the actual path."
  (if eclimd-executable
      (executable-find eclimd-executable)
    (let ((eclim-prog (executable-find eclim-executable)))
      (expand-file-name "eclimd" (file-name-directory eclim-prog)))))

(defconst eclimd--started-regexp
  "Eclim Server Started \
on\\(?: port\\|:\\) \\(?:\\(?:[0-9]+\\.\\)\\{3\\}[0-9]+:\\)?\\([0-9]+\\)"
  "Regular expression to detect when eclimd has finished starting.
The one and only capturing subgroup matches the port number
on which eclimd is serving.")

(defun eclimd--read-workspace-dir ()
  "Prompt the user for the workspace directory and return it."
  (read-directory-name "Eclimd workspace directory: "
                       eclimd-default-workspace nil t))

(defcustom eclimd-autostart-with-default-workspace
  nil
  "Non-nil means do not ask for a workspace when autostarting eclimd.
When `eclimd-autostart' is non-nil, this option controls
whether eclimd is started silently with the workspace set to
`eclimd-default-workspace', or whether the user is asked for
a workspace as with regular calls to `eclimd-start'."
  :tag "Autostart eclimd with default workspace"
  :type 'boolean
  :group 'eclimd)

(defun eclimd--autostart-workspace ()
  "Return the workspace to use when autostarting eclimd.
If `eclimd-autostart-with-default-workspace' is nil, the
user is asked to provide the workspace.  Otherwise,
`eclimd-default-workspace' is assumed."
  (if eclimd-autostart-with-default-workspace
      eclimd-default-workspace
    (eclimd--read-workspace-dir)))

(defcustom eclimd-autostart
  nil
  "Non-nil means automatically start eclimd within Emacs when needed.
You may want to set this to nil if you prefer starting
eclimd manually and don't want it to run as a child process of Emacs.
When set, eclimd gets started either when command `eclim-mode' is enabled
for the first time function `global-eclim-mode' needs it to determine if
command `eclim-mode' should be enabled in a buffer.  See also
`eclimd-autostart-with-default-workspace'."
  :tag "Autostart eclimd"
  :type 'boolean
  :group 'eclimd)

(defvar eclimd--process-event-functions nil
  "List of functions to run when eclimd outputs text or changes state.
Functions receive the process, the output string and the
process state as argument.  Either of the last two may be
nil, but never both.  When a function returns nil it is
removed from the list, but functions returning non-nil are
kept.")

(defun eclimd--process-sentinel (proc state)
  "The sentinel used to process events from the eclimd buffer.
PROC is the eclimd process and STATE describes the change of
state.

Each of `eclimd--process-event-functions' will be called
with PROC and STATE.  The output string will be nil.  Any
of these functions which return non-nil will be removed from
the list."
  (setq eclimd--process-event-functions
        (cl-remove-if-not (lambda (fun) (funcall fun proc nil state))
                          eclimd--process-event-functions)))

(defvar eclimd--comint-process-filter nil
  "The default process filter for the eclimd process buffer.")

(defun eclimd--process-filter (proc string)
  "Fitler eclimd process output.
PROC is the eclimd process and STRING is a line of output
from eclimd.  STRING is output to the eclimd process buffer
if one exists.

Each of `eclimd--process-event-functions' will be called
with PROC and STRING.  The process state will be nil.  Any
of these functions which return non-nil will be removed from
the list."
  (setq eclimd--process-event-functions
        (cl-remove-if-not (lambda (fun) (funcall fun proc string nil))
                          eclimd--process-event-functions))
  ;; Appends output to eclimd buffer.
  (when eclimd--comint-process-filter
    (funcall eclimd--comint-process-filter proc string)))

(defun eclimd--match-process-output (regexp &optional async callback)
  "Wait for eclimd to output a string matching REGEXP.
When ASYNC is omitted or nil, block and return the string
used for `string-match' if a match is found, or nil if the
process is killed.  Execute CALLBACK when the process
outputs the desired string or terminates and pass the
corresponding return value as argument."
  (let* ((output "")
         (terminated-p)
         (finished-p)
         (match-data)
         (closure (lambda (proc string _state)
                    (setf output (concat output string))
                    (setf terminated-p (not (eq 'run (process-status proc))))
                    (setf finished-p (or terminated-p
                                         ;; Although Emacs already saves the
                                         ;; match data when calling process
                                         ;; filters/sentinels, one such call may
                                         ;; execute multiple closures.
                                         (save-match-data
                                           (-when-let
                                               (match-pos
                                                (string-match regexp output))
                                             ;; Remember the match data so the
                                             ;; closure can access it.
                                             (setf match-data (match-data))
                                             match-pos))))
                    (when (and finished-p callback)
                      (save-match-data
                        (set-match-data match-data)
                        (funcall callback (unless terminated-p output))))
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
  "Wait for the eclimd server to become active.
If ASYNC is omitted or nil, block until the eclimd server
becomes active.  Call CALLBACK with no arguments when the
connection is established, but not when eclimd fails to
start."
  (eclimd--match-process-output
   eclimd--started-regexp async
   (lambda (output)
     (when output
       (setq eclimd-port (match-string 1 output))
       (when callback (funcall callback))))))

;;;###autoload
(defun eclimd-start (workspace-dir &optional callback)
  "Start the eclimd server and optionally wait for it to be ready.

WORKSPACE-DIR is the desired workspace directory for which
eclimd will be started.  `eclimd-default-workspace' is used
as the default value of this directory.

If CALLBACK is non-nil, it is called with no arguments once
the server is ready.

After having started the server process, this function may
block until eclimd is ready to receive commands, depending
on the value of `eclimd-wait-for-process'.  Commands will
fail if they are executed before the server is ready.

To stop the server, you should use `eclimd-start'."
  (interactive (list (eclimd--read-workspace-dir)))
  (let ((eclimd-prog (eclimd--executable-path)))
    (if (not eclimd-prog)
        (message "Cannot start eclimd: check eclimd-executable variable.")
      (when (and eclimd-process (not (process-live-p eclimd-process)))
        (setq eclimd-process nil))
      (if eclimd-process
          (message "Cannot start eclimd: eclimd was already started.")
        (message (concat "Starting eclimd for workspace: " workspace-dir "..."))
        (setq eclimd-process-buffer
              (make-comint eclimd-process-buffer-name
                           eclimd-prog
                           nil
                           (concat "-Dosgi.instance.area.default="
                                   (replace-regexp-in-string "~" "@user.home"
                                                             workspace-dir))))
        (setq eclimd-process (get-buffer-process eclimd-process-buffer))
        (setq eclimd--comint-process-filter (process-filter eclimd-process))
        (set-process-filter eclimd-process 'eclimd--process-filter)
        (set-process-sentinel eclimd-process 'eclimd--process-sentinel)
        (add-hook 'kill-emacs-hook #'eclimd-stop)
        ;; The flag is required because on exit Emacs asks the user about
        ;; running processes before running the `kill-emacs-hook'.
        (set-process-query-on-exit-flag eclimd-process nil)
        (eclimd--await-connection
         (not eclimd-wait-for-process)
         (lambda ()
           (message "eclimd serving at port %s" eclimd-port)
           (eclim--problems-update-maybe)
           (when callback (funcall callback))))))))

(defun eclimd--ensure-started (&optional async callback)
  "Ensure eclimd is running, autostarting it when possible.

If ASYNC is non-nil, the eclimd process will be connected to
asynchronously.  After being connected, CALLBACK will be
invoked with no arguments.

An error is raised if both `eclimd-autostart' and ASYNC are
nil while there is no eclimd process.  If ASYNC is non-nil
and eclimd cannot be started or is already running, CALLBACK is
not executed.

An error is raised when both `eclimd-autostart' and ASYNC are nil
while there is no eclimd process.  If ASYNC is t and eclimd can
not be started / is already running, CALLBACK is not executed."
  (if (eclim--connected-p)
      (when callback (funcall callback))
    (if eclimd-process
        ;; `eclimd-process' is set but we can not connect to eclimd, thus
        ;; eclimd is currently being started.
        (eclimd--await-connection async callback)
      (if eclimd-autostart
          (let ((eclimd-wait-for-process (not async)))
            (eclimd-start (eclimd--autostart-workspace) callback))
        (let ((msg "Autostarting of eclimd is disabled, please start eclimd \
manually."))
          (if async (message msg) (error "%s" msg)))))))

(defun eclimd-stop ()
  "Gracefully terminate the eclimd process.
Also kill the *eclimd*-buffer and remove any hooks added by
`eclimd-start'."
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
  (remove-hook 'kill-emacs-hook #'eclimd-stop))

(provide 'eclimd)
;;; eclimd ends here
