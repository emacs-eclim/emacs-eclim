;;; test-helper.el --- Tests for sequences.el  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test helper for eclim.el

;;; Code:
(when (require 'undercover nil t)
  (undercover "*.el" (:exclude "*test*.el")))

(require 'f)

(defvar eclim-test-path
  (f-dirname (f-this-file)))

(defvar eclim-code-path
  (f-parent eclim-test-path))

(add-to-list 'load-path eclim-test-path)
(add-to-list 'load-path eclim-code-path)

(defun eclim-emacs-init (action)
  "Initialize Emacs with Cask packages an invoke ACTION."
  (let* ((load-prefer-newer t)
         (source-directory (locate-dominating-file eclim-test-path "Cask"))
         (pkg-rel-dir (format ".cask/%s.%S/elpa" emacs-major-version emacs-minor-version)))

    (setq package-user-dir (expand-file-name pkg-rel-dir source-directory))
    (package-initialize)

    (message "Running tests on Emacs %s, built at %s"
             emacs-version (format-time-string "%F" emacs-build-time))
    (require 'eclim)
    (require 'elisp-lint)
    (funcall action)))

(defun eclim-lint-files ()
  "Main entry point for linter."
  (eclim-emacs-init
   (lambda ()
     (setq elisp-lint-ignored-validators '("package-format"
                                           "fill-column"
                                           "byte-compile"
                                           "indent"))
     (add-hook 'emacs-lisp-mode-hook
               (lambda ()
                 (setq indent-tabs-mode nil)
                 (setq fill-column 80)))
     (let ((debug-on-error t))
       (elisp-lint-files-batch)))))

(defun eclim-run-tests ()
  "Main entry point for linter."
  (eclim-emacs-init
   (lambda ()
     (let ((tests (directory-files "./test" t "test.el")))
       (while tests
         (load-file (car tests))
         (setq tests (cdr tests))))
     (let ((debug-on-error t))
       (ert-run-tests-batch-and-exit)))))


;;; test-helper.el ends here
