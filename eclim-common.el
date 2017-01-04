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

(defvar eclimd-process)

(defvar eclim--file-coding-system-mapping
  '(("undecided-dos" . "iso-8859-1")
    ("dos" . "iso-8859-1")
    ("undecided-unix" . "iso-8859-1")
    ("utf-8-dos" . "utf-8")
    ("utf-8-unix" . "utf-8")
    ("utf-8-emacs-unix" . "utf-8")))

(defvar eclim--compressed-urls-regexp
  "^\\(\\(?:jar\\|file\\|zip\\):\\(?:file:\\)?//\\)")

(defvar eclim--compressed-file-path-replacement-regexp "\\\\")

(defvar eclim--compressed-file-path-removal-regexp "^/")


(defvar eclim-projects-for-archive-file (make-hash-table :test 'equal))

(defvar eclim--default-args
  '(("-n" . (eclim-project-name))
    ("-p" . (or (eclim-project-name) (error "Could not find eclipse project for %s" (buffer-name (current-buffer)))))
    ("-e" . (eclim--current-encoding))
    ("-f" . (eclim--project-current-file))
    ("-o" . (eclim--byte-offset))
    ("-s" . "project")))

(defvar eclim--projects-cache nil)

(defvar eclim--is-completing nil)

(defvar eclim-autoupdate-problems t)

(defvar eclim--problems-project nil) ;; problems are relative to this project

(defvar eclim--problems-file nil) ;; problems are relative to this file (when eclim--problems-filefilter is non-nil)

(defvar eclim--problems-refreshing nil) ;; Set to true while refreshing probs.

(defvar eclim--problems-list nil)

(defvar eclim--problems-filter nil) ;; nil -> all problems, w -> warnings, e -> errors

(defvar eclim--problems-filefilter nil) ;; should filter by file name

(defvar eclim--problems-filter-description "")

(defvar eclim--project-natures-cache nil)

(defcustom eclim-eclipse-dirs '("/Applications/eclipse" "/usr/lib/eclipse"
                                "/usr/local/lib/eclipse" "/usr/share/eclipse")
  "Path to the eclipse directory"
  :type '(sexp)
  :group 'eclim)

(defcustom eclim-auto-save t
  "Determines whether to save the buffer when retrieving completions.
eclim can only complete correctly when the buffer has been
saved."
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-print-debug-messages nil
  "Determines whether debug messages should be printed."
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-interactive-completion-function (if ido-mode 'ido-completing-read 'completing-read)
  "Defines a function which is used by eclim to complete a list of
choices interactively."
  :group 'eclim
  :type 'function)

(defcustom eclim-use-yasnippet t
  "Determines whether the eclim snippets get turned on or off"
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-accepted-file-regexps
  '("\\.java$" "\\.js$" "\\.xml$" "\\.rb$" "\\.groovy$" "\\.php$" "\\.c$" "\\.cc$" "\\.h$" "\\.scala$")
  "List of regular expressions that are matched against filenames
to decide if eclim should be automatically started on a
particular file. By default all files part of a project managed
by eclim can be accepted (see `eclim--accepted-filename-p' for more
information). It is nevertheless possible to restrict eclim to
some files by changing this variable. For example, a value
of (\"\\\\.java\\\\'\" \"build\\\\.xml\\\\'\") can be used to restrict
the use of eclim to java and ant files."
  :group 'eclim
  :type '(repeat regexp))

(defcustom eclim-problems-refresh-delay 0.5
  "The delay (in seconds) to wait before we refresh the problem list buffer after a file is saved."
  :group 'eclim-problems
  :type 'number)

(defcustom eclim-problems-resize-file-column t
  "Resizes file column in emacs-eclim problems mode"
  :group 'eclim-problems
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-problems-show-pos nil
  "Shows problem line/column in emacs-eclim problems mode"
  :group 'eclim-problems
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-problems-hl-errors t
  "Highlights errors in the problem list buffer"
  :group 'eclim-problems
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-problems-suppress-highlights nil
  "When set, error and warning highlights are disabled in source files,
although counts are printed and they remain navigable. This is
designed to be made buffer-local (by user, not eclim) most of the
time, but it also works globally."
  :group 'eclim-problems
  :type '(choice (const :tag "Allow" nil)
                  (const :tag "Suppress" t)
                  (sexp :tag "Suppress when"
                        :value (lambda() 'for-example buffer-read-only))))

(defcustom eclim-problems-show-file-extension nil
  "Shows file extensions in emacs-eclim problems mode"
  :group 'eclim-problems
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defvar-local eclim--project-name nil)

(defvar-local eclim--project-current-file nil)

(defun eclim--command-should-sync-p (cmd args)
  (and (eclim--args-contains args '("-f" "-o"))
       (not (or (string= cmd "project_by_resource")
                (string= cmd "project_link_resource")))))

(defun eclim/project-info (project)
  (eclim--check-project project)
  (eclim--call-process "project_info" "-p" project))

(define-error 'eclim--connection-refused-error
  "Eclim was unable to connect to eclimd. You can start eclimd using M-x start-eclimd")

(define-error 'eclim--eclimd-starting-error
  "Eclimd is currently being started. Please wait for it to be ready and retry."
  'eclim--connection-refused-error)

(defun eclim--parse-result (result)
  "Parses the result of an eclim operation, raising an error if
the result is not valid JSON."
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
                  (error result))))
             ((string-match ".*Exception: \\(.*\\)" result)
              (error (match-string 1 result)))
             ((string-match "connect: Connection refused" result)
              (if eclimd-process
                  (signal 'eclim--eclimd-starting-error nil)
                (signal 'eclim--connection-refused-error nil)))
             (t (error result)))))))

(defun eclim--completing-read (prompt choices)
  (funcall eclim-interactive-completion-function prompt choices))

(defun eclim--call-process (&rest args)
  "Calls eclim with the supplied arguments. Consider using
`eclim/execute-command' instead, as it has argument expansion,
error checking, and some other niceties.."
  (eclim--parse-result (apply 'eclim--call-process-no-parse args)))

(defun eclim--connected-p ()
  (condition-case nil
      (progn (eclim--call-process "ping") t)
    ('eclim--connection-refused-error nil)))

(defun eclim-project-name (&optional filename)
  "Returns this file's project name. If the optional argument
FILENAME is given, return that file's  project name instead."
  (cl-labels ((get-project-name (file)
                                (eclim/execute-command "project_by_resource" ("-f" file))))
    (if filename
        (get-project-name filename)
      (or eclim--project-name
          (and buffer-file-name (setq eclim--project-name (get-project-name buffer-file-name)))
          (and buffer-file-name (gethash buffer-file-name eclim-projects-for-archive-file))))))

(defun eclim--expand-args (args)
  "Supply missing default values for eclim arguments.
Takes a list of command-line arguments with which to call the
eclim server. Each element should be either a string or a
list. If it is a string, its default value is looked up in
`eclim--default-args' and used to construct a list. The argument
lists are then appended together."
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
  "If `eclim-auto-save' is non-nil, save the current java
buffer. In addition, if `save-others' is non-nil, also save any
other unsaved buffer. Finally, tell eclim to update its java
sources."
  (when eclim-auto-save
    (when (buffer-modified-p) (save-buffer)) ;; auto-save current buffer, prompt on saving others
    (when save-others (save-some-buffers nil (lambda () (string-match "\\.java$" (buffer-file-name)))))))

(defun eclim--check-project (project)
  (let ((projects (or eclim--projects-cache
                      (setq eclim--projects-cache (mapcar (lambda (p) (assoc-default 'name p)) (eclim/project-list))))))
    (when (not (assoc-string project projects))
      (error (concat "invalid project: " project))))) ;

(defun eclim--execute-command-internal (executor cmd args)
  (lexical-let* ((expargs (eclim--expand-args args))
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
  (eclim/execute-command "project_list"))

(defun eclim--project-dir (&optional projectname)
  "Return this project's root directory. If the optional
argument PROJECTNAME is given, return that project's root directory."
  (assoc-default 'path (eclim/project-info (or projectname (eclim-project-name)))))

(defun eclim--byte-offset (&optional text)
  ;; TODO: restricted the ugly newline counting to dos buffers => remove it all the way later
  (let ((current-offset (1-(position-bytes (point)))))
    (when (not current-offset) (setq current-offset 0))
    (if (string-match "dos" (symbol-name buffer-file-coding-system))
        (+ current-offset (how-many "\n" (point-min) (point)))
      current-offset)))

(defun eclim-homedir-executable-find ()
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

(defun eclim--make-command (args)
  "Creates a command string that can be executed from the
shell. The first element in ARGS is the name of the eclim
operation, and the rest are flags/values to be passed on to
eclimd."
  (when (not eclim-executable)
    (error "Eclim installation not found. Please set eclim-executable."))
  (cl-reduce (lambda (a b) (format "%s %s" a b))
          (append (list eclim-executable "-command" (first args))
                  (cl-loop for a = (cdr args) then (cdr (cdr a))
                           for arg = (first a)
                           for val = (second a)
                           while arg append (if val (list arg (shell-quote-argument val)) (list arg))))))

(defun eclim--call-process-no-parse (&rest args)
  "Calls eclim with the supplied arguments but does not attempt to parse the result. "
  (let ((cmd (eclim--make-command args)))
    (when eclim-print-debug-messages (message "Executing: %s" cmd))
    (shell-command-to-string cmd)))

(defun eclim--project-current-file ()
  (or eclim--project-current-file
      (setq eclim--project-current-file
            (eclim/execute-command "project_link_resource" ("-f" buffer-file-name)))
      ;; command archive_read will extract archive file to /tmp directory, which is out of current project directory.
      (and buffer-file-name (gethash buffer-file-name eclim-projects-for-archive-file) buffer-file-name)))

(defun eclim--current-encoding ()
  (let* ((coding-system (symbol-name buffer-file-coding-system))
         (mapped-coding-system (cdr (assoc
                                     coding-system
                                     eclim--file-coding-system-mapping))))
    (if mapped-coding-system mapped-coding-system coding-system)))

(defun eclim--find-file (path-to-file)
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
  (if (boundp 'find-tag-marker-ring)
      (ring-insert find-tag-marker-ring (point-marker))
    (xref-push-marker-stack))
  (eclim--find-file (assoc-default 'filename line))
  (goto-char (point-min))
  (forward-line (1- (assoc-default 'line line)))
  (move-to-column (1- (assoc-default 'column line))))

(defun eclim-java-archive-file (file)
  (let ((eclim-auto-save nil))
    (eclim/with-results tmp-file ("archive_read" ("-f" file))
      ;; archive file's project should be same as current context.
      (setf (gethash tmp-file eclim-projects-for-archive-file) (eclim-project-name))
      tmp-file)))

(defun eclim--format-find-result (line &optional directory)
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
  "Return t if and only one of the regular expressions in
`eclim-accepted-file-regexps' matches FILENAME."
  (if (cl-member-if
       (lambda (regexp) (string-match regexp filename))
       eclim-accepted-file-regexps)
      t))

(defun eclim--file-managed-p (&optional filename)
  "Return t if and only if this file is part of a project managed
by eclim. If the optional argument FILENAME is given, the return
value is computed for that file's instead."
  (ignore-errors
    (let ((file (or filename buffer-file-name)))
      (and file
           (eclim-project-name file)))))

(defun eclim--accepted-p (filename)
  "Return t if and only if eclim should be automatically started on filename."
  (and
   filename
   (eclim--accepted-filename-p filename)
   (eclim--file-managed-p filename)))

(defun eclim--java-identifier-at-point (&optional full position)
  "Returns a cons cell (BEG . IDENTIFIER) where BEG is the start
buffer byte offset of the token/identifier at point, and
IDENTIFIER is the string from BEG to (point). If argument FULL is
non-nill, IDENTIFIER will contain the whole identifier, not just
the start. If argument POSITION is non-nil, BEG will contain the
position of the identifier instead of the byte offset (which only
matters for buffers containing non-ASCII characters)."
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
  "If autoupdate is enabled, this function triggers a delayed
refresh of the problems buffer."
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
          (message "Eclim reports %d errors, %d warnings."
                   (length (cl-remove-if (lambda (p) (null '(assoc-default 'warning p))) problems))
                   (length (cl-remove-if-not (lambda (p) (null '(assoc-default 'warning p))) problems)))))))

(defun eclim-java-correct (line offset)
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
  (if eclim--problems-filefilter
      (if eclim--problems-filter
          (setq eclim--problems-filter-description (concat "(file-" eclim--problems-filter ")"))
        (setq eclim--problems-filter-description "(file)"))
    (if eclim--problems-filter
        (setq eclim--problems-filter-description (concat eclim--problems-project "(" eclim--problems-filter ")"))
      (setq eclim--problems-filter-description eclim--problems-project))))

(defun eclim--problems-buffer-redisplay ()
  "Draw the problem list on screen."
  (let ((buf (get-buffer "*eclim: problems*")))
    (when buf
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
          (forward-line (1- line-number)))))))

(defun eclim--problems-filecol-size ()
  (if eclim-problems-resize-file-column
      (min 40
           (apply #'max 0
                  (mapcar (lambda (problem)
                            (length (eclim--problems-cleanup-filename (assoc-default 'filename problem))))
                          (eclim--problems-filtered))))
    40))

(defun eclim--problems-filtered ()
  "Filter reported problems by eclim.

It filters out problems using the ECLIM--PROBLEMS-FILEFILTER
criteria. If IGNORE-TYPE-FILTER is nil (default), then problems
are also filtered according to ECLIM--PROBLEMS-FILTER, i.e.,
error type. Otherwise, error type is ignored. This is useful when
other mechanisms, like compilation's mode
COMPILATION-SKIP-THRESHOLD, implement this feature."
  (eclim--filter-problems eclim--problems-filter eclim--problems-filefilter eclim--problems-file eclim--problems-list))

(defun eclim-problems-highlight ()
  "Inserts the currently active problem highlights in the current buffer,
if `eclim-problems-suppress-highlights' allows it."
  (interactive)
  (when (eclim--accepted-p (buffer-file-name))
    (save-restriction
      (widen)
      (eclim-problems-clear-highlights)
      (unless (if (functionp eclim-problems-suppress-highlights)
                  (funcall eclim-problems-suppress-highlights)
                eclim-problems-suppress-highlights)
        (cl-loop for problem across (cl-remove-if-not (lambda (p) (string= (assoc-default 'filename p) (buffer-file-name))) eclim--problems-list)
                 do (eclim--problems-insert-highlight problem))))))

(defun eclim--insert-problem (problem filecol-size)
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
  "Clears all eclim problem highlights in the current buffer. This is temporary
until the next refresh."
  (interactive)
  (remove-overlays nil nil 'category 'eclim-problem))

(defun eclim--problems-insert-highlight (problem)
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
  (let ((x (file-name-nondirectory filename)))
    (if eclim-problems-show-file-extension x (file-name-sans-extension x))))

(defun eclim--filter-problems (type-filter file-filter file problems)
  (let ((type-filterp (eclim--choose-type-filter type-filter))
        (file-filterp (eclim--choose-file-filter file-filter file)))
    (cl-remove-if-not (lambda (x) (and (funcall type-filterp x) (funcall file-filterp x))) problems)))

(defun eclim--problem-goto-pos (p)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- (assoc-default 'line p)))
    (dotimes (i (1- (assoc-default 'column p)))
      (forward-char))))

(defun eclim--choose-type-filter (type-filter)
  (cond
   ((not type-filter) '(lambda (_) t))
   ((string= "e" type-filter) 'eclim--error-filterp)
   (t 'eclim--warning-filterp)))

(defun eclim--choose-file-filter (file-filter file)
  (if (not file-filter)
      '(lambda (_) t)
    `(lambda (x) (string= (assoc-default 'filename x) ,file))))

(defun eclim--string-strip (content)
  (replace-regexp-in-string "\s*$" "" content))

(defun eclim-debug/jdb (command)
  (let ((buffer (current-buffer)))
    ;;(toggle-maximize-buffer) - Fixme: I don't know where this function is defined - NL 2016-08-31
    (switch-to-buffer-other-window buffer t)
    (jdb command)
    (switch-to-buffer-other-window buffer t)))

(provide 'eclim-common)
;;; eclim-common ends here
