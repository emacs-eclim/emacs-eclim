;; eclim-eclim-linter-init.el --- an interface to the Eclipse IDE.  -*- lexical-binding: t; -*-
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
;;; Commentary:
;;
;; Common code used by other files in this package.
;;
;;; Code:


(add-to-list 'load-path ".")
(add-to-list 'load-path "./test")
(add-to-list 'load-path "./test/elisp-lint")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)
(defvar optional-dependencies '((company "0.8.12")))
(package-install (caar optional-dependencies))
(package-install 'noflet)
(package-install 'el-mock)

(require 'cl)

(defun pkg-installed-p (pkg)
  (package-installed-p (car pkg) (version-to-list (cadr pkg))))

(condition-case err
    (let* ((pkg-info
            (with-temp-buffer
              (insert-file-contents "eclim-pkg.el")
              (goto-char (point-min))
              (read (current-buffer))))
           (name (cadr pkg-info))
           (needed-packages (cadr (nth 4 pkg-info))))
      (assert (equal name "eclim"))
      (message "Loaded eclim-pkg.el")
      (message "Installing dependencies: %S" needed-packages)
      (if (every #'pkg-installed-p needed-packages)
          (message "All dependencies present.")
        (package-refresh-contents)
        (dolist (p needed-packages)
          (unless (pkg-installed-p p)
            (package-install (car p))
            (when (not (pkg-installed-p p))
              (error (message "Failed to install %s at %s." p)))
            ))))
  (error (message "Error loading dependencies: %s" err)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq fill-column 80)
            (setq elisp-lint-ignored-validators '("package-format"
                                                  "fill-column"
                                                  "byte-compile"
                                                  "indent"))))
(let ((tests (directory-files "./test" t "test.el")))
  (while tests
    (load-file (car tests))
    (setq tests (cdr tests))))

(require 'elisp-lint)
(require 'eclim)

(provide emacs-eclim-linter-init)
;;; emacs-eclim-linter-init ends here
