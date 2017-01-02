;;; eclim-compile.el --- Eclim byte compiler            -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

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

;; This file provides non-interactive byte compilation for Eclim.
;;
;; It's essentially a wrapper around `batch-byte-compile' which sets some
;; additional byte compiler options for Eclim.
;;
;; This code was borrowed from the flycheck project (https://github.com/flycheck/flycheck)

;;; Code:

(require 'bytecomp)

(unless noninteractive
  (error "This file must not be used interactively"))

(defun eclim/batch-byte-compile ()
  "Like `batch-byte-compile', but set additional flags."
  (while command-line-args-left
    (let ((filename (pop command-line-args-left)))
      (unless (string-match "pkg.el$" filename)
        (byte-recompile-file filename t 0 )))))

;;; eclim-compile.el ends here
