;;; eclim.el --- an interface to the Eclipse IDE  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2009, 2012  Tassilo Horn <tassilo@member.fsf.org>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Contributors
;;
;;  - Nikolaj Schumacher <bugs * nschum de>
;;  - Yves Senn <yves senn * gmx ch>
;;  - Fredrik Appelberg <fredrik * bitbakery se>
;;  - Alessandro Arzilli <alessandro.arzilli * gmail com>
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "eclim--<descriptive-name>", and name eclim command invocations
;; "eclim/command-name", like eclim/project-list.
;;
;;; Commentary:
;;
;; Emacs-eclim uses the Eclim Server to integrate eclipse with Emacs.  This project wants to bring
;; some of the invaluable features from Eclipse to Emacs.  Please note, the eclim package is limited
;; to mostly Java support at this time.
;;
;;; Code:

(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 's)
(require 'json)
(require 'eclimd)
(require 'eclim-common)
(eval-when-compile (require 'eclim-macros))

;;** Basics

(defgroup eclim nil
  "Interface to the Eclipse IDE."
  :group 'tools)

(defun eclim-toggle-print-debug-messages ()
  "Enable or disable printing debug messages.
The value of the variable `eclim-print-debug-messages' is
inverted by this function."
  (interactive)
  (message "Debug messages %s."
           (if (setq eclim-print-debug-messages (not eclim-print-debug-messages))
               "on" "off")))

(defun eclim-quit-window (&optional kill-buffer)
  "Bury the buffer and delete its window.
With prefix argument KILL-BUFFER non-nil, kill the buffer
instead of burying it."
  (interactive "P")
  (quit-window kill-buffer (selected-window)))

(defvar eclim--currently-running-async-calls nil
  "The list of currently running asynchronous commands.
This allows preventing multiple executions of the same
command from running concurrently.")

(defun eclim--call-process-async (callback &rest args)
  "Call eclim asynchronously with the supplied arguments.
CALLBACK is a function that accepts a list of strings and
will be called on completion.  ARGS are the strings to be
passed as command line arguments to eclim.

This function is just like `eclim--call-process' except that
it is asynchronous."
  (let ((handler callback)
        (cmd (eclim--make-command args)))
    (when (not (cl-find cmd eclim--currently-running-async-calls :test #'string=))
      (let ((buf (get-buffer-create (generate-new-buffer-name "*eclim-async*"))))
        (when eclim-print-debug-messages
          (message "Executing: %s" cmd)
          (message "Using async buffer %s" buf))
        (push cmd eclim--currently-running-async-calls)
        (let ((proc (start-process-shell-command "eclim" buf (eclim--make-command args))))
          (let ((sentinel (lambda (process _signal)
                            (unwind-protect
                                (save-excursion
                                  (setq eclim--currently-running-async-calls (cl-remove-if (lambda (x) (string= cmd x)) eclim--currently-running-async-calls))
                                  (set-buffer (process-buffer process))
                                  (funcall handler (eclim--parse-result (buffer-substring 1 (point-max)))))
                              (kill-buffer buf)))))
            (set-process-sentinel proc sentinel)))))))

;; Commands

(defun eclim-file-locate (pattern &optional case-insensitive)
  "Find all files in the current workspace which match PATTERN.
PATTERN is a regular expression, not a glob pattern.  Also,
unlike Emacs, eclim does not require special characters such
as \"(\" or \".\" to be escaped, unless the intent is to
match those characters literally.

If CASE-INSENSITIVE is provided and non-nil, the case of the
file name is not important to the match.

If multiple files are found, a list is displayed from which
the user may select which file to visit.  However, if only
one file matching PATTERN, that file will be opened
immediately."
  (interactive (list (read-string "Pattern: ") "P"))
  (message "'%s' '%s'" pattern case-insensitive)
  (eclim/with-results hits ("locate_file" ("-p" (concat "^.*" pattern ".*$")) ("-s" "workspace") (if case-insensitive '("-i" "")))
    (eclim--find-display-results pattern
                                 (apply #'vector
                                        (mapcar (lambda (hit) (list (cons 'filename (assoc-default 'path hit))
                                                                    (cons 'line 1)
                                                                    (cons 'column 1)
                                                                    (cons 'message "")))
                                                hits))
                                 t)))

(defun eclim-find-file-path-strict (filename &optional project directory)
  "Locate a file with the basename FILENAME in Eclipse.
If PROJECT is a string, searches only that project; if nil,
the project of the current file; if t, searches all Eclipse
projects.  If DIRECTORY is specified, returns only files
that are under that directory.  Returns a list of matching
absolute paths; possibly empty.  This can be used to help
resolve exception stack traces, for example."
  (let* ((results (apply #'eclim--call-process "locate_file"
                        "-p" (regexp-quote filename)
                         (if (eq project t)
                             (list "-s" "workspace")
                           (list "-s" "project" "-n"
                                 (or project (eclim-project-name))))))
         (paths (mapcar #'(lambda(hit) (assoc-default 'path hit)) results)))
    (if directory
        (cl-remove-if-not #'(lambda (f) (file-in-directory-p f directory)) paths)
      paths)))


;;;###autoload
(defun eclim/workspace-dir ()
  "Return the path of the current eclim workspace directory."
  (eclim--call-process "workspace_dir"))

(defun eclim/jobs (&optional family)
  "Return all active eclim jobs.
If the optional argument FAMILY is provided, only return
jobs belonging to FAMILY.  FAMILY may be one of
\"auto_build\", \"auto_refresh\", \"manual_build\" or
\"manual_refresh\".  It is possible that the eclimd server
may define more families in the future."
  (eclim/execute-command "jobs" ("-f" family)))

;;** The minor mode and its keymap

(defvar eclim-mode-hook nil
  "Hook run after entering eclim mode.")

;;;###autoload
(define-minor-mode eclim-mode
  "An interface to the Eclipse IDE."
  nil
  (:eval (eclim-modeline-string))
  eclim-mode-map
  (if eclim-mode
      (progn
        (kill-local-variable 'eclim--project-dir)
        (kill-local-variable 'eclim-project-name)
        (kill-local-variable 'eclim--project-current-file)
        (add-hook 'find-file-hook 'eclim-problems-find-file-hook)
        (add-hook 'after-save-hook 'eclim--problems-update-maybe nil 't)
        (add-hook 'after-save-hook 'eclim--after-save-hook nil 't)
        (advice-add 'find-file :after #'eclim-problems-advice-find-file)
        (advice-add 'find-file-other-window :after #'eclim-problems-advice-find-file-other-window)
        (advice-add 'other-window :after #'eclim-problems-advice-other-window)
        (advice-add 'switch-to-buffer :after #'eclim-problems-advice-switch-to-buffer)
        (advice-add 'delete-file :around #'eclim-java-delete-file)
        (eclimd--ensure-started t (eclim--lambda-with-live-current-buffer
                                    (eclim--maybe-create-project))))
    (remove-hook 'find-file-hook 'eclim-problems-find-file-hook 't)
    (remove-hook 'after-save-hook 'eclim--problems-update-maybe 't)
    (remove-hook 'after-save-hook 'eclim--after-save-hook 't)
    (advice-remove 'find-file #'eclim-problems-highlight)
    (advice-remove 'find-file #'eclim-problems-advice-find-file)
    (advice-remove 'find-file-other-window #'eclim-problems-advice-find-file-other-window)
    (advice-remove 'other-window #'eclim-problems-advice-other-window)
    (advice-remove 'switch-to-buffer #'eclim-problems-advice-switch-to-buffer)
    (advice-remove 'delete-file #'eclim-java-delete-file)))

;; Request an eclipse source update when files are saved
(defun eclim--after-save-hook ()
  "Update an eclim buffer after saving."
  (when (eclim--accepted-p (buffer-file-name))
    (ignore-errors
      (apply 'eclim--call-process
             (cl-case major-mode
               (java-mode "java_src_update")
               (groovy-mode "groovy_src_update")
               (ruby-mode "ruby_src_update")
               (php-mode "php_src_update")
               (scala-mode "scala_src_update")
               ((c-mode c++-mode) "c_src_update")
               ((javascript-mode js-mode) "javascript_src_update")
               (python-mode "python_src_update"))
             (eclim--expand-args (list "-p" "-f")))))
  t)

(defun eclim--maybe-create-project ()
  "Create a project if the current file does not belong to one.
If the file already belongs to a project, no action is
taken.  Otherwise, the user will be prompted as to whether
or not a new project should be created to contain the
current file."
  (when (and (not (eclim-project-name))
             (y-or-n-p "Eclim mode was enabled in a buffer \
that is not organized in a Eclipse project. Create a new project? "))
    (call-interactively 'eclim-project-create)))

;;;###autoload
(define-globalized-minor-mode global-eclim-mode eclim-mode
  eclim--enable-for-accepted-files-in-project)

(defvar eclim--enable-for-accepted-files-in-project-running nil
  "Used to prevent recursive calls to function `global-eclim-mode'.
Such recursive calls are possible because
`eclimd--ensure-started' may create a comint buffer for
which Emacs checks whether `eclim-mode' should be enabled.")

(defun eclim--enable-for-accepted-files-in-project ()
  "Enable `eclim-mode' in accepted files that belong to a project.
A file is accepted if it's name is matched by any of
`eclim-accepted-file-regexps' elements.  Note that in order
to determine if a file is managed by a project, eclimd is
required to be running and will thus be autostarted."
  ;; Errors here can REALLY MESS UP AN EMACS SESSION. Can't emphasize enough.
  (ignore-errors
    (unless eclim--enable-for-accepted-files-in-project-running
      (let ((eclim--enable-for-accepted-files-in-project-running t))
        (when (and buffer-file-name
                   (eclim--accepted-filename-p buffer-file-name))
          (eclimd--ensure-started t (eclim--lambda-with-live-current-buffer
                                      (when (and (eclim--file-managed-p buffer-file-name)
                                                 (eclim--project-dir))
                                        (eclim-mode 1)))))))))

(require 'eclim-ant)
(require 'eclim-debug)
(require 'eclim-java)
(require 'eclim-java-run)
(require 'eclim-maven)
(require 'eclim-project)
(require 'eclim-problems)
(require 'eclim-scala)

(defun eclim-modeline-string ()
  "Return the string to display as eclim's modeline."
  (when eclim-mode
    (concat " Eclim" (eclim-problems-modeline-string))))

(provide 'eclim)
;;; eclim ends here
