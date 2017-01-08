;;; company-emacs-eclim-tests.el --- Tests for sequences.el  -*- lexical-binding: t; -*-

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

;; Tests for eclim.el

;;; Code:

(require 'company-emacs-eclim)
(require 'noflet)

(ert-deftest compute-full-prefix-when-complete-import ()
  (with-temp-buffer
    (insert "import java.uti")
    (goto-char (point-max))
    (let ((before-prefix (company-emacs-eclim--before-prefix-in-buffer "uti")))
      (should (string-equal before-prefix "java.")))))

(ert-deftest when-completing-import-prefix-should-be-trimmed ()
  (let ((candidates (emacs-eclim--candidates-for-temp-buffer
                     '((content . "import java.uti")
                       (mocked-response . ("java.util" "java.util.stream"))
                       (prefix . "uti")))))
    (should (equal candidates '("util" "util.stream")))))

(ert-deftest when-completing-fields-candidates-shouldnt-change ()
  (let ((candidates (emacs-eclim--candidates-for-temp-buffer
                     '((content . "this.liba.sec")
                       (mocked-response . ("secondChild : String - LibraryA"))
                       (prefix . "sec")))))
    (should (equal candidates '("secondChild : String - LibraryA")))))

(ert-deftest when-completing-method-real-candidate-is-passed-as-test-property ()
  (let  ((candidates (emacs-eclim--candidates-for-temp-buffer
                      '((content . "this.liba.get")
                        (mocked-response . ("getterWithParams(int x, int y, int z) : int - LibraryA"))
                        (prefix . "get")))))
    (should (equal candidates '("getterWithParams")))
    (should (equal (get-text-property 0 'eclim-meta (first candidates)) "getterWithParams(int x, int y, int z) : int - LibraryA"))))

(defun emacs-eclim--candidates-for-temp-buffer (arg)
  (with-temp-buffer
    (insert (cdr (assoc 'content arg)))
    (goto-char (point-max))
    (noflet ((eclim--completion-candidates () (cdr (assoc 'mocked-response arg))))
      (company-emacs-eclim--candidates (cdr (assoc 'prefix arg))))))

(provide 'company-emacs-eclim-tests)
;;; company-emacs-eclim-tests.el ends here
