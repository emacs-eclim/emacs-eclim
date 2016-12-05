;;; maven-tests.el --- Tests for sequences.el

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

(ert-deftest regexp-match-filename-line-column-properly ()
  (let* ((error-string "[ERROR] /home/lklich/Dokumenty/Projekty/LibertyGlobal/eclim-test-mvn-project/lib-a/src/main/java/com/acme/liba/LibraryA.java:[22,12] method codePointAt in class java.lang.String cannot be applied to given types;")
         (matched (s-match (caar compilation-error-regexp-alist) error-string))
         (file (cadr matched))
         (line (caddr matched))
         (column (cadddr matched)))
    (should (string-equal file "/home/lklich/Dokumenty/Projekty/LibertyGlobal/eclim-test-mvn-project/lib-a/src/main/java/com/acme/liba/LibraryA.java"))
    (should (string-equal line "22"))
    (should (string-equal column "12"))))

(provide 'maven-tests)
;;; maven-tests.el ends here
