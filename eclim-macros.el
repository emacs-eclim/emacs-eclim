;;; eclim-macros.el --- an interface to the Eclipse IDE.  -*- lexical-binding: t -*-
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
;; Macros used by this package.
;;
;;; Code:
(eval-when-compile (require 'cl-lib))

(defun eclim--args-contains (args flags)
  "Validate that ARGS (not expanded) has the specified FLAGS."
  (cl-loop for f in flags
     return (cl-find f args
                     :test #'string=
                     :key (lambda (a) (if (listp a) (car a) a)))))

(defun eclim--evaluating-args-form (args)
  "Returns a form which evaluates the elements of the list ARGS.
If a list element is of the form (STRING EXPRESSION), only
EXPRESSION will be evaluated by the form to some RESULT,
and \(STRING RESULT) will be the element contained in the
list returned by the form.  Other list elements will be
evaluated directly and their result will be in the final
list."
  `(list ,@(mapcar (lambda (arg)
                     (if (and (listp arg)
                              (stringp (car arg)))
                         (list 'list (car arg) (cadr arg))
                       arg))
                   args)))

(defmacro eclim/execute-command (cmd &rest args)
  "Execute CMD after expanding ARGS with `eclim--expand-args'.
Automatically saves the current buffer (and optionally other
java buffers as well), performs an eclim source update
operation, and refreshes the current buffer if necessary.
Raises an error if the connection is refused.  Automatically
calls `eclim--check-project' if necessary."
  `(eclim--execute-command-internal
    (lambda (command-line on-complete-fn)
      (let ((res (apply 'eclim--call-process command-line)))
        (funcall on-complete-fn)
        res))
    ,cmd ,(eclim--evaluating-args-form args)))

(defmacro eclim/with-results (result params &rest body)
  "Call eclim, binding RESULT to the parsed output.
PARAMS is a list (CMD . ARGS) where CMD is the eclim command
to execute and ARGS is the unexpanded argument list.

If RESULT is non-nil after the command is finished, BODY is
executed."
  (declare (indent defun))
  (let ((sync (eclim--args-contains (cdr params) (list "-f" "-o"))))
    `(let* ((,result (eclim/execute-command ,@params))
            (eclim-auto-save (and eclim-auto-save (not ,sync))))
       (when ,result
         ,@body))))

(defmacro eclim/with-results-async (result params &rest body)
  "Call eclim asynchronously, binding RESULT to the parsed output.
PARAMS is a list (CMD . ARGS) where CMD is the eclim command
to execute and ARGS is the unexpanded argument list.

If RESULT is non-nil after the command is finished, BODY is
executed.

This function is the asynchronous version of
`eclim/with-results'."
  (declare (indent defun))
  (let ((sync (eclim--args-contains (cdr params) (list "-f" "-o"))))
    `(eclim/execute-command-async
      (lambda (,result)
        (let ((eclim-auto-save (and eclim-auto-save (not ,sync))))
          (when ,result ,@body)))
      ,@params)))

(defmacro eclim/execute-command-async (callback cmd &rest args)
  "Execute CMD asynchronously after expanding ARGS.
Calls `eclim--expand-args' on ARGS to supply default values
for arguments as needed, then calls eclim with the resulting
argument list.  Automatically saves the current buffer (and
optionally other java buffers as well)m, performs an eclim
source update operation, and refreshes the current buffer if
necessary.  Raises an exception if the connection is
refused.  Automatically calls `eclim-check-project' if
necessary.

CALLBACK is a function which is called with the parsed
results of the operation.

This function is the asynchronous version of
`eclim/execute-command'."
  `(eclim--execute-command-internal
    (lambda (command-line on-complete-fn)
      (let ((on-complete-fn on-complete-fn))
        (apply 'eclim--call-process-async
               (lambda (res)
                 (funcall on-complete-fn)
                 (when ,callback
                   (funcall ,callback res)))
               command-line)))
    ,cmd ,(eclim--evaluating-args-form args)))

(defmacro eclim--with-problems-list (problems &rest body)
  "Asynchronously refresh the problems list and operate on it.
If no project is set as the current problems project in
`eclim--problems-project', no update is performed.

The problems list will be stored in `eclim--problems-list'
and also bound to PROBLEMS while evaluating BODY."
  (declare (indent defun))
  (let ((res (cl-gensym)))
    `(when eclim--problems-project
       (setq eclim--problems-refreshing t)
       (eclim/with-results-async ,res
         ("problems" ("-p" eclim--problems-project)
          (when (string= "e" eclim--problems-filter)
            '("-e" "true")))
         (cl-loop for problem across ,res
            do (let ((filecell (assq 'filename problem)))
                 (when filecell (setcdr filecell (file-truename (cdr filecell))))))
         (setq eclim--problems-list ,res)
         (let ((,problems ,res))
           (setq eclim--problems-refreshing nil)
           ,@body)))))

(defmacro eclim--lambda-with-live-current-buffer (&rest body)
  "Create a closure which will execute BODY in the current buffer.
The current buffer is at the creation of the closure, not
when the closure is called.  BODY is only executed if the
current buffer is still live when the closure is called."
  (declare (indent defun))
  (let ((caller-current-buffer-symbol (make-symbol "caller-current-buffer")))
    `(let ((,caller-current-buffer-symbol (current-buffer)))
       (lambda ()
         (when (buffer-live-p ,caller-current-buffer-symbol)
           (with-current-buffer ,caller-current-buffer-symbol
             ,@body))))))

(provide 'eclim-macros)
;;; eclim-macros.el ends here
