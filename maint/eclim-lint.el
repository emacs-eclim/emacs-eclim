;;; eclim-lint.el --- Script to check the package for syntax errors  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Luis Gerhorst

;; Author: Luis Gerhorst <privat@luisgerhorst.de>
;; Keywords: lisp, maint

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

;; Code to check for syntax errors. See Makefile for usage.

;;; Code:

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

(defun eclim-package-lint ()
  "Checks that the metadata in Emacs Lisp files which to ensure they are intended to be packages."
  (eclim-emacs-init
   (lambda ()
     (eval-after-load 'flycheck
       '(flycheck-package-setup)))))

(provide 'eclim-lint)
;;; eclim-lint.el ends here
