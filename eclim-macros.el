;;; eclim-macros.el --- an interface to the Eclipse IDE.  -*- lexical-binding: t; -*-
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

(defun eclim--args-contains (args flags)
  "Validates that ARGS (not expanded) has the specified FLAGS."
  (cl-loop for f in flags
           return (cl-find f args :test #'string= :key (lambda (a) (if (listp a) (car a) a)))))

(defmacro eclim/execute-command (cmd &rest args)
  "Calls `eclim--expand-args' on ARGS, then calls eclim with the
results. Automatically saves the current buffer (and optionally
other java buffers as well), performs an eclim source update
operation, and refreshes the current buffer if necessary. Raises
an error if the connection is refused. Automatically calls
`eclim--check-project' if neccessary."
  `(eclim--execute-command-internal
    (lambda (command-line on-complete-fn)
      (let ((res (apply 'eclim--call-process command-line)))
        (funcall on-complete-fn)
        res))
    ,cmd ',args))

(defmacro eclim/with-results (result params &rest body)
  "Convenience macro. PARAMS is a list where the first element is
CMD to execute and the rest an ARGS list. Calls eclim with CMD
and the expanded ARGS list and binds RESULT to the results. If
RESULT is non-nil, BODY is executed."
  (declare (indent defun))
  (let ((sync (eclim--args-contains (cdr params) (list "-f" "-o"))))
    `(let* ((,result (eclim/execute-command ,@params))
            (eclim-auto-save (and eclim-auto-save (not ,sync))))
       (when ,result
         ,@body))))

(defmacro eclim/with-results-async (result params &rest body)
  "A convenience macro.  PARAMS is a list where the first element
is CMD to execute and the rest an ARGS list.  Calls Eclim with CMD
and the expanded ARGS list and binds RESULT to the results.  If
RESULT is non-nil, BODY is executed."
  (declare (indent defun))
  (let ((sync (eclim--args-contains (cdr params) (list "-f" "-o"))))
    `(eclim/execute-command-async
      (lambda (,result)
        (let ((eclim-auto-save (and eclim-auto-save (not ,sync))))
          (when ,result ,@body)))
      ,@params)))

(defmacro eclim/execute-command-async (callback cmd &rest args)
  "Calls `eclim--expand-args' on ARGS, then calls eclim with the
results. Automatically saves the current buffer (and optionally
other java buffers as well), performs an eclim source update
operation, and refreshes the current buffer if necessary. Raises
an error if the connection is refused. Automatically calls
`eclim--check-project' if neccessary. CALLBACK is a lambda
expression which is called with the results of the operation."
  `(eclim--execute-command-internal
    (lambda (command-line on-complete-fn)
      (lexical-let ((on-complete-fn on-complete-fn))
        (apply 'eclim--call-process-async
               (lambda (res)
                 (funcall on-complete-fn)
                 (when ,callback
                   (funcall ,callback res)))
               command-line)))
    ,cmd ',args))

(defmacro eclim--with-problems-list (problems &rest body)
  (declare (indent defun))
  "Utility macro to refresh the problem list and do operations on it asynchronously."
  (let ((res (cl-gensym)))
    `(when eclim--problems-project
       (setq eclim--problems-refreshing t)
       (eclim/with-results-async ,res ("problems" ("-p" eclim--problems-project) (when (string= "e" eclim--problems-filter) '("-e" "true")))
         (cl-loop for problem across ,res
                  do (let ((filecell (assq 'filename problem)))
                       (when filecell (setcdr filecell (file-truename (cdr filecell))))))
         (setq eclim--problems-list ,res)
         (let ((,problems ,res))
           (setq eclim--problems-refreshing nil)
           ,@body)))))

(defmacro eclim--lambda-with-live-current-buffer (&rest body)
  (declare (indent defun))
  "Create a closure that executes body with the callers current buffer if it's still live."
  (let ((caller-current-buffer-symbol (make-symbol "caller-current-buffer")))
    `(let ((,caller-current-buffer-symbol (current-buffer)))
       (lambda ()
         (when (buffer-live-p ,caller-current-buffer-symbol)
           (with-current-buffer ,caller-current-buffer-symbol
             ,@body))))))

(provide 'eclim-macros)
;;;
