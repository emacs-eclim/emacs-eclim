;;; eclim-common.el --- an interface to the Eclipse IDE.  -*- lexical-binding: t -*-
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
;;; Commentary:
;;
;; Common code used by other files in this package.
;;
;;; Code:


(eval-when-compile (require 'cl))
(require 'etags)
(require 'cl-lib)
(require 'json)
(require 'arc-mode)
(require 'popup)
(require 'dash)
;;(eval-when-compile (require 'eclim-macros))
(require 'eclim-macros)

;;;###autoload
(defvar eclim-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-TAB") 'eclim-complete)
    map)
  "The keymap used in `eclim-mode'.")

(defvar eclimd-process nil
  "The active eclimd process.")

(defvar eclim--file-coding-system-mapping
  '(("undecided-dos" . "iso-8859-1")
    ("dos" . "iso-8859-1")
    ("undecided-unix" . "iso-8859-1")
    ("utf-8-dos" . "utf-8")
    ("utf-8-unix" . "utf-8")
    ("utf-8-emacs-unix" . "utf-8"))
  "Associates Emacs coding system names with equivalent Java names.")

(defvar eclim--compressed-urls-regexp
  "^\\(\\(?:jar\\|file\\|zip\\):\\(?:file:\\)?//\\)"
  "Matches a prefix of a URL which identifies a file in an archive.
This allows removing the prefix so that only the archived
file path remains.")

(defvar eclim--compressed-file-path-replacement-regexp "\\\\"
  "Matches Windows-style path separators.
This allows replacing any Windows-style path separators with
Unix-style path separators in archived file name.")

(defvar eclim--compressed-file-path-removal-regexp "^/"
  "Matches the leading path separator in archived file paths.
This allows removing the leading path separator so that
archived file paths can be treated as relative paths and not
absolute paths.")


(defvar eclim-projects-for-archive-file (make-hash-table :test 'equal)
  "Hash table mapping archived file paths to enclosing project names.")

(defvar eclim--default-args
  '(("-n" . (eclim-project-name))
    ("-p" . (or (eclim-project-name) (error "Could not find eclipse project for %s" (buffer-name (current-buffer)))))
    ("-e" . (eclim--current-encoding))
    ("-f" . (eclim--project-current-file))
    ("-o" . (eclim--byte-offset))
    ("-s" . "project"))
  "Maps eclim command line arguments to default values.
The values are actually expressions which evaluate to the
default value of the corresponding argument.")

(defvar eclim--projects-cache nil
  "A cache of the names of the projects in the workspace.
Nil means the cache is not set.  If non-nil, the value will
be a list of strings which are the names of all the projects
in the current workspace.")

(defvar eclim--is-completing nil
  "Non-nil means that a completion operation is in progress.")

(defvar eclim-autoupdate-problems t
  "Non-nil means the problems buffer will be automatically updated.")

(defvar eclim--problems-project nil
  "The project to which the current problems apply.
The value is set to the current project each time the
problems buffer is updated.  This way, even if the current
project changes during and update, the problems buffer can
still be interpreted correctly.")

(defvar eclim--problems-file nil
  "The file to which the current problems apply.
This is only meaningful if `eclim--problems-filefilter' is
non-nil.")

(defvar eclim--problems-refreshing nil
  "Non-nil means the problems buffer is being refreshed.
This isn't important from a control-flow perspective, but it
allows us to give useful indication to the user.")

(defvar eclim--problems-list nil
  "The list of current problems.
Each problem is an association list with the following
symbolic keys:
- filename: The path of the file where the problem exists.
- line: The line number of the problem.
- column: The column number of the problem.
- message: The description of the problem.
- warning: :json-false if the problem is an error.")

(defvar eclim--problems-filter nil
  "Defines a problem filter by problem type.
A value of nil means all problems will be shown.  A value of
\"e\" means show only errors.  A value of \"w\" means show only
warnings."
  )

(defvar eclim--problems-filefilter nil
  "Non-nil means only keep problems which apply to the current file.")

(defvar eclim--problems-filter-description ""
  "Describes the kind of problems filter current in use.
Simultaneously indicates, in a human-readable form, the
values of `eclim--problems-filter' and
`eclim--problems-filefilter'.")

(defvar eclim--project-natures-cache nil
  "A cache of the available project nature aliases.
For example, the \"java\" might be an alias for the nature
ID \"org.eclipse.jdt.core.javanature\".")

(defcustom eclim-eclipse-dirs '("/Applications/eclipse" "/usr/lib/eclipse"
                                "/usr/local/lib/eclipse" "/usr/share/eclipse"
                                "/Applications/Eclipse.app/Contents/Eclipse/")
  "A list of possible paths to the eclipse directory."
  :type '(sexp)
  :group 'eclim)

(defcustom eclim-eclimrc nil
  "The file containing the run commands that Eclim will use when
invoked. See http://eclim.org/eclimd.html#eclimrc."
  :group 'eclim
  :type 'string)

(defcustom eclim-nailgun-port nil
  "The port that is used to start Eclimd when using Nailgun."
  :group 'eclim
  :type 'integer)

(defcustom eclim-auto-save t
  "Non-nil means the buffer is saved before retrieving completions.
Eclim can only complete correctly when the buffer has been
saved."
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-print-debug-messages nil
  "Non-nil means debug messages will be printed."
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-interactive-completion-function (if ido-mode 'ido-completing-read 'completing-read)
  "The function eclim should use to complete interactive choices."
  :group 'eclim
  :type 'function)

(defcustom eclim-use-yasnippet t
  "Non-nil enables eclim snippets."
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-accepted-file-regexps
  '("\\.java$" "\\.js$" "\\.xml$" "\\.rb$" "\\.groovy$" "\\.php$" "\\.c$" "\\.cc$" "\\.h$" "\\.scala$")
  "List of filename patterns which cause eclim to be enabled.
Each element is a regular expression which is matched
against filenames to decide if eclim should be automatically
started on a particular file.  By default all files part of
a project managed by eclim can be accepted (see
`eclim--accepted-filename-p' for more information).  It is
nevertheless possible to restrict eclim to some files by
changing this variable.  For example, a value of
\(\"\\\\.java\\\\'\" \"build\\\\.xml\\\\'\") can be used to
restrict the use of eclim to java and ant files."
  :group 'eclim
  :type '(repeat regexp))

(defcustom eclim-problems-refresh-delay 0.5
  "The time to wait before refreshing the problem list after saving.
The value is measured in seconds."
  :group 'eclim-problems
  :type 'number)

(defcustom eclim-problems-resize-file-column t
  "Non-nil means resize the file column in the problems buffer."
  :group 'eclim-problems
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-problems-show-pos nil
  "Non-nil means show each problem's position in the problems buffer."
  :group 'eclim-problems
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-problems-hl-errors t
  "Non-nil means highlight errors in the problem list buffer."
  :group 'eclim-problems
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-problems-suppress-highlights nil
  "Controls whether problems are highlighted in source files.
When nil, problems are highlighted in source files.  When
non-nil and not a function, problems are not highlighted.

If the value is a function, the function will be called with
no arguments to determine whether to suppress highlighting
for the current buffer.  Highlighting will be suppressed if
the function returns non-nil.

Even if highlighting is suppressed, error and warning counts
are still printed and they remain navigable.  This is
designed to be made buffer-local (by the user, not eclim)
most of the time, but it also works globally."
  :group 'eclim-problems
  :type '(choice (const :tag "Allow" nil)
                 (const :tag "Suppress" t)
                 (sexp :tag "Suppress when"
                       :value (lambda() 'for-example buffer-read-only))))

(defcustom eclim-problems-show-file-extension nil
  "Non-nil means that file extensions are shown in the problems buffer."
  :group 'eclim-problems
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defvar-local eclim--project-name nil
  "The cached name of the project to which the current file belongs.")

(defvar-local eclim--project-current-file nil
  "The cached path of the current file relative to the project.")

(defun eclim--current-region ()
  "Return the contents of the current region as a string.
The result includes the text properties of the region."
  (buffer-substring (region-beginning) (region-end)))

(defun eclim--current-region-no-properties ()
  "Return the contents of the current region as a string.
The result does not include the text properties of the region."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun eclim--command-should-sync-p (cmd args)
  "Return non-nil if buffers must be saved before executing CMD.
This decision depends on the whether CMD might need an
up-to-date version of a file.  In turn, this could depend on
the ARGS which are passed to CMD.  If CMD could possibly
require an up-to-date version of a file, all applicable
buffers must be saved before executing CMD."
  (and (eclim--args-contains args '("-f" "-o"))
       (not (or (string= cmd "project_by_resource")
                (string= cmd "project_link_resource")))))

(defun eclim/project-info (project)
  "Return information about PROJECT.
The return value is an association list.  The returned value
will always contain the following keys and values:
- name: The name of the project.
- open: Non-nil if the project is currently open.
- path: The full path to the project root directory.
- workspace: The workspace which contains the project.

The following optional keys and values may also be returned:
- depends: List of project names on which the project
           depends.
- natures: List of project natures assigned to the project.

It is an error if PROJECT is not a recognized project name."
  (eclim--check-project project)
  (eclim--call-process "project_info" "-p" project))

(define-error 'eclim--connection-refused-error
  "Eclim was unable to connect to eclimd. You can start eclimd using M-x start-eclimd")

(define-error 'eclim--eclimd-starting-error
  "Eclimd is currently being started. Please wait for it to be ready and retry."
  'eclim--connection-refused-error)

(defun eclim--parse-result (result)
  "Parse the result of an eclim operation.
Raises An error if RESULT is not valid JSON."
  (if (string-match (rx string-start (zero-or-more (any " " "\n" "\t")) string-end) result)
      nil
    (condition-case nil
        (json-read-from-string result)
      ('json-readtable-error
       (cond ((string-match "java.io.UnsupportedEncodingException: \\(.*\\)" result)
              (let ((e (match-string 1 result)))
                (error "Eclim doesn't know how to handle the encoding %s. You can avoid this by
evaluating (add-to-list 'eclim--file-coding-system-mapping '(\"%s\" . \"<encoding>\"))
where <encoding> is the corresponding java name for this encoding." e e)))
             ((string-match "No command '\\(.*\\)' found" result)
              (let ((c (assoc-default (match-string 1 result)
                                      '(("xml_complete" "XML" "Eclipse Web Developer Tools")
                                        ("groovy_complete" "Groovy" "Eclipse Groovy Development Tools")
                                        ("ruby_complete" "Ruby" "Eclipse Ruby Development Tools")
                                        ("c_complete" "C/C++" "Eclipse C/C++ Development Tools")
                                        ("php_complete" "PHP" "Eclipse PHP Development Tools")
                                        ("scala_complete" "Scala" "Eclipse Scala Development Tools")))))
                (if c (error "Eclim was not installed with %s support. Please make sure that %s are installed, then reinstall eclim." (first c) (second c))
                  (error "%s" result))))
             ((string-match ".*Exception: \\(.*\\)" result)
              (error "%s" (match-string 1 result)))
             ((string-match "connect: Connection refused" result)
              (if eclimd-process
                  (signal 'eclim--eclimd-starting-error nil)
                (signal 'eclim--connection-refused-error nil)))
             (t (error "%s" result)))))))

(defun eclim--completing-read (prompt choices)
  "Show an interactive PROMPT with completion for a list of CHOICES."
  (funcall eclim-interactive-completion-function prompt choices))

(defun eclim--call-process (&rest args)
  "Call eclim with ARGS.
Consider using `eclim/execute-command' instead, as it has
argument expansion, error checking, and some other niceties."
  (eclim--parse-result (apply 'eclim--call-process-no-parse args)))

(defun eclim--connected-p ()
  "Return non-nil if connected to the eclim server."
  (condition-case nil
      (progn (eclim--call-process "ping") t)
    ('eclim--connection-refused-error nil)))

(defun eclim-project-name (&optional filename)
  "Return a file's project name.
If the optional argument FILENAME is given, return that
file's project name.  Otherwise return the current file's
project name."
  (cl-labels ((get-project-name (file)
                                (if (not (string= file buffer-auto-save-file-name))
                                    (eclim/execute-command "project_by_resource" ("-f" file))
                                  nil)))
    (if filename
        (get-project-name filename)
      (or eclim--project-name
          (and buffer-file-name (setq eclim--project-name (get-project-name buffer-file-name)))
          (and buffer-file-name (gethash buffer-file-name eclim-projects-for-archive-file))))))

(defun eclim--expand-args (args)
  "Supply missing default values for eclim arguments.
ARGS is a list of command line arguments with which to call
the eclim server.  Each element should be either a string or
a list.  If it is a string, its default value is looked up
in `eclim--default-args' and used to construct a list.  The
argument lists are then appended together."
  (mapcar (lambda (arg) (if (numberp arg) (number-to-string arg) arg))
          (cl-loop for arg in args
                   append (if (stringp arg)
                              (list arg (eval (cdr (or (assoc arg eclim--default-args)
                                                       (error "No default value for %s found" arg)))))
                            (assert (listp arg))
                            (when arg
                              (assert (stringp (car arg)))
                              (assert (or (stringp (cadr arg))
                                          (numberp (cadr arg)))))
                            arg))))

(defun eclim--src-update (&optional save-others)
  "Automatically save the current buffer before calling eclim.
Automatic saving is only performed if `eclim-auto-save' is
non-nil.  Additionally, if automatic saving is enabled and
SAVE-OTHERS is non-nil, any other unsaved Java buffers are
saved as well."
  (when eclim-auto-save
    (when (buffer-modified-p) (save-buffer)) ;; auto-save current buffer, prompt on saving others
    (when save-others (save-some-buffers nil (lambda () (string-match "\\.java$" (buffer-file-name)))))))

(defun eclim--check-project (project)
  "Throw error if PROJECT is not a recognized project name."
  (let ((projects (or eclim--projects-cache
                      (setq eclim--projects-cache (mapcar (lambda (p) (assoc-default 'name p)) (eclim/project-list))))))
    (when (not (assoc-string project projects))
      (error "Invalid project: %s" project))))

(defun eclim--execute-command-internal (executor cmd args)
  "Invoke an eclim server command, returning the parsed output.

EXECUTOR is a function which takes two arguments.  The first
is a list of strings which are the command line tokens to
pass to the eclim server.  The second is a callback function
which must be invoked with no arguments upon completion of
the command invocation.  The return value must be the parsed
output of the command.

CMD is the eclim server command to invoke.

ARGS is the command line arguments to pass to the invocation
of CMD.  Each argument will be expanded using
`eclim--expand-args' to provide default values as necessary."
  (let* ((expargs (eclim--expand-args args))
         (sync (eclim--command-should-sync-p cmd args))
         (check (eclim--args-contains args '("-p"))))
    (when sync (eclim--src-update))
    (when check
      (ignore-errors
        (eclim--check-project (if (listp check) (eval (second check)) (eclim-project-name)))))
    (let ((attrs-before (if sync (file-attributes (buffer-file-name)) nil)))
      (funcall executor (cons cmd expargs)
               (lambda ()
                 (when sync
                   (let ((attrs-curr (file-attributes (buffer-file-name))))
                     (when (and (file-exists-p (buffer-file-name))
                                attrs-before
                                (or
                                 (not (= (second (sixth attrs-before)) (second (sixth attrs-curr)))) ;; mod time
                                 (not (= (eighth attrs-before) (eighth attrs-curr))))) ;; size time
                       (revert-buffer t t t)))))))))


(defun eclim/project-list ()
  "Return the names of the projects in the current workspace."
  (eclim/execute-command "project_list"))

(defun eclim--project-dir (&optional projectname)
  "Return this project's root directory.
If the optional argument PROJECTNAME is given, return that
project's root directory."
  (assoc-default 'path (eclim/project-info (or projectname (eclim-project-name)))))

(defun eclim--byte-offset (&optional _text)
  "Return the current position in the buffer in bytes.

For DOS buffers, account for newlines being two characters
rather than a single line feed character.

TODO: Remove the unused argument _TEXT.
TODO: Remove ugly newline counting altogether."
  (let ((current-offset (1-(position-bytes (point)))))
    (when (not current-offset) (setq current-offset 0))
    (if (string-match "dos" (symbol-name buffer-file-coding-system))
        (+ current-offset (how-many "\n" (point-min) (point)))
      current-offset)))

(defun eclim-homedir-executable-find ()
  "Attempt to find the eclim executable in the user home directory."
  (let ((file "~/.eclipse"))
    (and (file-exists-p
          (setq file (expand-file-name file)))
         (setq file (car (last (directory-files file t "^org.eclipse.platform_"))))
         (file-exists-p
          (setq file (expand-file-name "plugins" file)))
         (setq file (car (last (directory-files file t "^org.eclim_"))))
         (file-exists-p (setq file (expand-file-name "bin/eclim" file)))
         file)))

(defun eclim-executable-find ()
  "Attempt to find the eclim executable in one of `eclim-eclipse-dirs'."
  (let (file)
    (dolist (eclipse-root eclim-eclipse-dirs)
      (and (file-exists-p
            (setq file (expand-file-name "plugins" eclipse-root)))
           (setq file (car (last (directory-files file t "^org.eclim_"))))
           (file-exists-p (setq file (expand-file-name "bin/eclim" file)))
           (return file)))))

(defcustom eclim-executable
  (or (executable-find "eclim") (eclim-homedir-executable-find) (eclim-executable-find))
  "Location of eclim executable."
  :group 'eclim
  :type 'file)

(defun eclim-executable-get-command ()
  "Returns the Eclim executable command. If \"eclim-eclimrc\" and /
or \"eclim-nailgun-port\" are defined, then these are appended to
the command."
  (when (not eclim-executable)
    (error "Eclim installation not found. Please set eclim-executable."))
  (let ((command eclim-executable))
    (when eclim-eclimrc (setq command (concat command " -f " eclim-eclimrc)))
    (when eclim-nailgun-port (setq command (concat command " --nailgun-port " (number-to-string eclim-nailgun-port))))
    command))

(defun eclim--make-command (args)
  "Create a command string that can be executed from the shell.
The first element in ARGS is the name of the eclim
operation.  The rest are flags/values to be passed on to
eclimd."
  (when (not eclim-executable)
    (error "Eclim installation not found. Please set eclim-executable."))
  (cl-reduce (lambda (a b) (format "%s %s" a b))
          (append (list (eclim-executable-get-command) "-command" (first args))
                  (cl-loop for a = (cdr args) then (cdr (cdr a))
                           for arg = (first a)
                           for val = (second a)
                           while arg append (if val (list arg (shell-quote-argument val)) (list arg))))))

(defun eclim--call-process-no-parse (&rest args)
  "Call eclim using ARGS as command line arguments.
This function does not attempt to parse the result.  Instead
the output from eclim is returned as a string."
  (let ((cmd (eclim--make-command args)))
    (when eclim-print-debug-messages (message "Executing: %s" cmd))
    (shell-command-to-string cmd)))

(defun eclim--project-current-file ()
  "Return the path of the current file relative to the project."
  (or eclim--project-current-file
      (setq eclim--project-current-file
            (eclim/execute-command "project_link_resource" ("-f" buffer-file-name)))
      ;; command archive_read will extract archive file to /tmp directory, which is out of current project directory.
      (and buffer-file-name (gethash buffer-file-name eclim-projects-for-archive-file) buffer-file-name)))

(defun eclim--current-encoding ()
  "Return the encoding of the current file."
  (let* ((coding-system (symbol-name buffer-file-coding-system))
         (mapped-coding-system (cdr (assoc
                                     coding-system
                                     eclim--file-coding-system-mapping))))
    (if mapped-coding-system mapped-coding-system coding-system)))

(defun eclim--find-file (path-to-file)
  "Visit a file identified by PATH-TO-FILE even if it is in an archive."
  (if (not (string-match-p "!" path-to-file))
      (unless (and (buffer-file-name) (file-equal-p path-to-file (buffer-file-name)))
        (find-file path-to-file))
    (let* ((parts (split-string path-to-file "!"))
           (archive-name (replace-regexp-in-string eclim--compressed-urls-regexp "" (first parts)))
           (file-name (second parts)))
      (find-file-other-window archive-name)
      (goto-char (point-min))
      (re-search-forward (replace-regexp-in-string
                          eclim--compressed-file-path-removal-regexp ""
                          (regexp-quote (replace-regexp-in-string
                                         eclim--compressed-file-path-replacement-regexp
                                         "/" file-name))))
      (let ((old-buffer (current-buffer)))
        (archive-extract)
        (goto-char (point-min))
        (kill-buffer old-buffer)))))

(defun eclim--visit-declaration (line)
  "Visit a position determined by LINE, possibly in another file.
LINE is an association list with the following keys and
values:
- filename: The path to the file which should be visited.
- line: The line within the file to visit.
- column: The column within the line to visit."
  (if (boundp 'find-tag-marker-ring)
      (ring-insert find-tag-marker-ring (point-marker))
    (xref-push-marker-stack))
  (eclim--find-file (assoc-default 'filename line))
  (goto-char (point-min))
  (forward-line (1- (assoc-default 'line line)))
  (move-to-column (1- (assoc-default 'column line))))

(defun eclim-java-archive-file (file)
  "Read the contents of an archive FILE into the current project."
  (let ((eclim-auto-save nil))
    (eclim/with-results tmp-file ("archive_read" (list "-f" file))
      ;; archive file's project should be same as current context.
      (setf (gethash tmp-file eclim-projects-for-archive-file) (eclim-project-name))
      tmp-file)))

(defun eclim--format-find-result (line &optional directory)
  "Render a search result a string.

LINE is an association list with the following keys and
values:
- filename: The path of the file in which the result occurs.
- line: The line number of the result.
- column: The column number of the result.
- message: The message associated with the result.

DIRECTORY is the optional base directory which should be
removed from the beginning of file paths if possible."
  (let* ((converted-directory (replace-regexp-in-string "\\\\" "/" (assoc-default 'filename line)))
         (parts (split-string converted-directory "!"))
         (filename (replace-regexp-in-string
                    eclim--compressed-urls-regexp "" (first parts)))
         (filename-in-dir (if directory
                (replace-regexp-in-string (concat (regexp-quote directory) "/?")
                                          "" filename)
              filename)))
    (if (cdr parts)
        ;; Just put the jar path, since there's no easy way to instruct
        ;; compile-mode to go into an archive. Better than nothing.
        ;; TODO: revisit when an archive file-handler shows up somewhere.
        (format "%s:1: %s\n" filename-in-dir (assoc-default 'message line))
      (format "%s:%d:%d:%s\n"
              filename-in-dir
              (assoc-default 'line line)
              (assoc-default 'column line)
              (assoc-default 'message line)))))

(defun eclim--find-display-results (pattern results &optional open-single-file)
  "Display the results of a search operation.

PATTERN is the original pattern used for the search.

RESULTS is a list of \"lines\", where each line is an
association list with the following symbolic keys:
- filename: The path of the file where the match was found.
- line: The line where the match was found.
- column: The column where the match was found.
- message: The message associated with the result.

If OPEN-SINGLE-FILE is non-nil and only a single result
exists, the corresponding file will be opened and the cursor
will be moved to the location of the result."
  (let ((results
         (cl-loop for result across results
                  for file = (cdr (assoc 'filename result))
                  if (string-match (rx bol (or "jar" "zip") ":") file)
                  do (setf (cdr (assoc 'filename result)) (eclim-java-archive-file file))
                  finally (return results))))
    (if (and (= 1 (length results)) open-single-file)
        (eclim--visit-declaration (elt results 0))
      (pop-to-buffer (get-buffer-create "*eclim: find"))
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert (concat "-*- mode: eclim-find; default-directory: " default-directory " -*-"))
        (newline 2)
        (insert (concat "eclim java_search -p " pattern))
        (newline)
        (cl-loop for result across results
                 do (insert (eclim--format-find-result result default-directory)))
        (goto-char 0)
        (grep-mode)))))

(defun eclim--accepted-filename-p (filename)
  "Return non-nil if eclim should be enabled for a file.
The result is non-nil if and only if one of the regular
expressions in `eclim-accepted-file-regexps' matches
FILENAME."
  (if (cl-member-if
       (lambda (regexp) (string-match regexp filename))
       eclim-accepted-file-regexps)
      t))

(defun eclim--file-managed-p (&optional filename)
  "Return non-nil if and only if the file is managed by eclim.
If the optional argument FILENAME is given, the result is
computed for that file.  Otherwise, the result is computed
for the file visited by the current buffer."
  (ignore-errors
    (let ((file (or filename buffer-file-name)))
      (and file
           (eclim-project-name file)))))

(defun eclim--accepted-p (filename)
  "Return non-nil if eclim should automatically start for FILENAME."
  (and
   filename
   (eclim--accepted-filename-p filename)
   (eclim--file-managed-p filename)))

(defun eclim--java-identifier-at-point (&optional full position)
  "Determine the identifier and position of the token at `point'.
Returns a cons cell (BEG . IDENTIFIER) where BEG is the
buffer byte offset of the start of the token/identifier at
point, and IDENTIFIER is the string from BEG to (point).  If
argument FULL is non-nil, IDENTIFIER will contain the whole
identifier, not just the start.  If argument POSITION is
non-nil, BEG will contain the position of the identifier
instead of the byte offset (which only matters for buffers
containing non-ASCII characters)."
  (let ((boundary "\\([<>()\\[\\.\s\t\n!=,;]\\|]\\)"))
    ;; TODO: make this work for dos buffers
    (save-excursion
      (if (and full (re-search-forward boundary nil t))
          (backward-char))
      (let ((end (point))
            (start (progn
                     (if (re-search-backward boundary nil t) (forward-char))
                     (point))))
        (cons (if position (point) (eclim--byte-offset))
              (buffer-substring-no-properties start end))))))

(defun eclim--problems-update-maybe ()
  "Trigger a delayed refresh of the problems buffer.
The refresh is only triggered if auto-updated is enabled.
The delay is specified by `eclim-problems-refresh-delay'."
  (when (and (not eclim--is-completing)
             (eclim--project-dir)
             eclim-autoupdate-problems)
    (setq eclim--problems-project (eclim-project-name))
    (setq eclim--problems-file buffer-file-name)
    (run-with-idle-timer eclim-problems-refresh-delay nil (lambda () (eclim-problems-buffer-refresh)))))

(defun eclim-problems-buffer-refresh ()
  "Refresh the problem list and draw it on screen."
  (interactive)
  (eclim--with-problems-list problems
    (eclim--problems-buffer-redisplay)
    (if (not (minibuffer-window-active-p (minibuffer-window)))
        (if (string= "e" eclim--problems-filter)
            (message "Eclim reports %d errors." (length problems))
          (let ((warning-count (length (cl-remove-if-not (lambda (problem) (eq t (assoc-default 'warning problem)))
                                                         problems))))
            (message "Eclim reports %d errors, %d warnings."
                     (- (length problems) warning-count)
                     warning-count))))))

(defun eclim-java-correct (line offset)
  "Show a menu with possible correction at a given point.
LINE is the line on which the correction will apply.  OFFSET
is the byte offset into the buffer at which the correction
will apply.

When a correction is selected, it will be automatically
applied to the buffer."
  (eclim/with-results correction-info ("java_correct" "-p" "-f" ("-l" line) ("-o" offset))
    (if (stringp correction-info)
        (message correction-info)
      (-if-let* ((corrections (cdr (assoc 'corrections correction-info)))
                 (cmenu (mapcar 'eclim--java-make-popup-item corrections))
                 (choice (popup-menu* cmenu)))
          (eclim/with-results correction-info
            ("java_correct"
             ("-p" (eclim-project-name))
             "-f"
             ("-l" line)
             ("-o" offset)
             ("-a" choice))
            ;; Problem updates can be distracting, but here the user was
            ;; actively trying to fix one.
            (eclim--problems-update-maybe))
        (message "No automatic corrections found. Sorry")))))

(defun eclim--problems-update-filter-description ()
  "Update the mode-line description of the current problem filters.
See `eclim--problems-filter-description' for more information."
  (if eclim--problems-filefilter
      (if eclim--problems-filter
          (setq eclim--problems-filter-description (concat "(file-" eclim--problems-filter ")"))
        (setq eclim--problems-filter-description "(file)"))
    (if eclim--problems-filter
        (setq eclim--problems-filter-description (concat eclim--problems-project "(" eclim--problems-filter ")"))
      (setq eclim--problems-filter-description eclim--problems-project))))

(defun eclim--problems-buffer-redisplay ()
  "Draw the problem list on screen."
  (let ((buf (eclim--get-problems-buffer-create)))
    (with-current-buffer
        (set-buffer buf)
      (eclim--problems-update-filter-description)
      (save-excursion
        (dolist (b (mapcar #'window-buffer (window-list)))
          (set-buffer b)
          (eclim-problems-highlight)))
      (let ((inhibit-read-only t)
            (line-number (line-number-at-pos))
            (filecol-size (eclim--problems-filecol-size)))
        (erase-buffer)
        (cl-loop for problem across (eclim--problems-filtered)
                 do (eclim--insert-problem problem filecol-size))
        (goto-char (point-min))
        (forward-line (1- line-number))))))

(defun eclim--problems-filecol-size ()
  "Compute the width of the problems buffer column for file names.
If `eclim-problems-resize-file-column' is non-nil, the
column will be dynamically sized based on the actual file
names.  Otherwise, a static column width is used."
  (if eclim-problems-resize-file-column
      (min 40
           (apply #'max 0
                  (mapcar (lambda (problem)
                            (length (eclim--problems-cleanup-filename (assoc-default 'filename problem))))
                          (eclim--problems-filtered))))
    40))

(defun eclim--problems-filtered ()
  "Return a filtered list of problems reported by eclim.

Filtering is controlled by two variables:
`eclim--problems-filter' and `eclim--problems-filefilter'.
See the documentation for those variables for an explanation
of their effects."
  (eclim--filter-problems eclim--problems-filter eclim--problems-filefilter eclim--problems-file eclim--problems-list))

(defun eclim-problems-highlight ()
  "Highlight the currently active problems in the current buffer.
Highlighting only occurs if it is allowed by
`eclim-problems-suppress-highlights'"
  (interactive)
  (when (eclim--accepted-p (buffer-file-name))
    (save-restriction
      (widen)
      (eclim-problems-clear-highlights)
      (unless (if (functionp eclim-problems-suppress-highlights)
                  (funcall eclim-problems-suppress-highlights)
                eclim-problems-suppress-highlights)
        (cl-loop for problem across (cl-remove-if-not (lambda (p) (string= (assoc-default 'filename p) (file-truename (buffer-file-name)))) eclim--problems-list)
                 do (eclim--problems-insert-highlight problem))))))

(defun eclim--insert-problem (problem filecol-size)
  "Add a problem line to the problems buffer.

PROBLEM is an association list with the following symbolic
keys:
- filename: The path of the file where the problem exists.
- line: The line number of the problem.
- column: The column number of the problem.
- message: The description of the problem.
- warning: :json-false if the problem is an error.

FILECOL-SIZE is the width of the column reserved for displaying file names."
  (let* ((filecol-format-string (concat "%-" (number-to-string filecol-size) "s"))
         (problem-new-line-pos (cl-position ?\n (assoc-default 'message problem)))
         (problem-message
          (if problem-new-line-pos
              (concat (substring (assoc-default 'message problem)
                                 0 problem-new-line-pos)
                      "...")
            (assoc-default 'message problem)))
         (filename (truncate-string-to-width
                    (eclim--problems-cleanup-filename (assoc-default 'filename problem))
                    40 0 nil t))
         (text (if eclim-problems-show-pos
                   (format (concat filecol-format-string
                                   " | line %-12s"
                                   " | %s")
                           filename
                           (assoc-default 'line problem)
                           problem-message)
                 ;; else
                 (format (concat filecol-format-string
                                 " | %s")
                         filename
                         problem-message))))
    (when (and eclim-problems-hl-errors (eq :json-false (assoc-default 'warning problem)))
      (put-text-property 0 (length text) 'face 'bold text))
    (insert text)
    (insert "\n")))

(defun eclim-problems-clear-highlights ()
  "Clear problem highlighting in the current buffer.
This is temporary until the next refresh."
  (interactive)
  (remove-overlays nil nil 'category 'eclim-problem))

(defun eclim--problems-insert-highlight (problem)
  "Add highlighting to the current buffer to indicate PROBLEM.

PROBLEM is an association list with the following symbolic
keys:
- line: The line number of the problem.
- column: The column number of the problem.
- message: The description of the problem."
  (save-excursion
    (eclim--problem-goto-pos problem)
    (let* ((id (eclim--java-identifier-at-point t t))
           (start (car id))
           (end (+ (car id) (length (cdr id)))))
      (let ((highlight (make-overlay start end (current-buffer) t t)))
        (overlay-put highlight 'face
                     (if (eq t (assoc-default 'warning problem))
                         'eclim-problems-highlight-warning-face
                       'eclim-problems-highlight-error-face))
        (overlay-put highlight 'category 'eclim-problem)
        (overlay-put highlight 'kbd-help (assoc-default 'message problem))))))

(defun eclim--problems-cleanup-filename (filename)
  "Trim the directory (and optionally the extension) from FILENAME.

Whether or not the file extension is removed is controlled
by `eclim-problems-show-file-extension'."
  (let ((x (file-name-nondirectory filename)))
    (if eclim-problems-show-file-extension x (file-name-sans-extension x))))

(defun eclim--filter-problems (type-filter file-filter file problems)
  "Filter the problem set as configured by the user.

TYPE-FILTER determines how to filter problems by type.  See
`eclim--problems-filter' for more details.

FILE-FILTER determines how to filter problems by file name.
See `eclim--problems-filefilter' for more details.

FILE is the current file.  That is, if file filtering is
enabled, this is the path which all problem file names must
match.

PROBLEMS is the list of problems."
  (let ((type-filterp (eclim--choose-type-filter type-filter))
        (file-filterp (eclim--choose-file-filter file-filter file)))
    (cl-remove-if-not (lambda (x) (and (funcall type-filterp x) (funcall file-filterp x))) problems)))

(defun eclim--problem-goto-pos (p)
  "Move the cursor in the current buffer to position P.

P is an association list with the following symbolic keys:
- line: The line number of the desired position.
- column: The column number of the desired position."
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- (assoc-default 'line p)))
    (dotimes (_i (1- (assoc-default 'column p)))
      (forward-char))))

(defun eclim--choose-type-filter (type-filter)
  "Return a type filtering predicate.

If TYPE-FILTER is nil, the predicate always returns non-nil.
If TYPE-FILTER is \"e\", the predicate only returns non-nil
for errors.
If TYPE-FILTER is \"w\", the predicate only returns non-nil
for warnings."
  (cond
   ((not type-filter) '(lambda (_) t))
   ((string= "e" type-filter) 'eclim--error-filterp)
   (t 'eclim--warning-filterp)))

(defun eclim--choose-file-filter (file-filter file)
  "Return a file name filtering predicate.

The predicate accepts a single parameter which is an
association list containing the following symbolic keys and
corresponding values:
- filename: The filename to filter against.

If FILE-FILTER is nil, the predicate always returns non-nil.
Otherwise the predicate returns non-nil only if the
argument's file name is the same as FILE."
  (if (not file-filter)
      '(lambda (_) t)
    `(lambda (x) (string= (assoc-default 'filename x) ,file))))

(defun eclim--string-strip (content)
  "Remove trailing whitespace from CONTENT."
  (replace-regexp-in-string "\s*$" "" content))

(defun eclim-debug/jdb (command)
  "Execute the jdb with COMMAND as the command line.

Jdb is run in another window."
  (let ((buffer (current-buffer)))
    ;;(toggle-maximize-buffer) - Fixme: I don't know where this function is defined - NL 2016-08-31
    (switch-to-buffer-other-window buffer t)
    (jdb command)
    (switch-to-buffer-other-window buffer t)))

(provide 'eclim-common)
;;; eclim-common ends here
