;;; test-helper.el --- Tests for sequences.el

;; This file is part of GNU Emacs.

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

;; Tests for eclim.el

;;; Code:
(when (require 'undercover nil t)
  (undercover "*.el" (:exclude "*-test.el")))

(defvar eclim-test-path
  (f-dirname (f-this-file)))

(defvar eclim-code-path
  (f-parent eclim-test-path))

(add-to-list 'load-path eclim-test-path)
(add-to-list 'load-path eclim-code-path)

(require 'eclim)

(provide 'test-helper)
;;; test-helper.el ends here
