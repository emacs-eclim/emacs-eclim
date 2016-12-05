;;; command-tests.el --- Tests for sequences.el

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


(ert-deftest test-command-should-sync-for-normal-commands ()
  (should (eclim--command-should-sync-p "java_complete" '("-f" "~/src/test/Panda.java")))
  (should (not (eclim--command-should-sync-p "other_cmd" '("-x" "~/src/test/Panda.java")))))

(ert-deftest test-command-should-sync-for-special-commands ()
  (should (not (eclim--command-should-sync-p "project_by_resource" '("-f" "~/src/test/Panda.java"))))
  (should (not (eclim--command-should-sync-p "project_link_resource" '("-f" "~/src/test/Panda.java")))))

(ert-deftest test-make-command-should-create-an-executable-string ()
  (let ((eclim-executable "/usr/local/eclipse/eclim"))
    (should (equal (eclim--make-command '("cmd" "-a" "arg"))
                   "/usr/local/eclipse/eclim -command cmd -a arg"))))

(ert-deftest test-make-command-should-escape-arguments ()
  (let ((eclim-executable "/usr/local/eclipse/eclim"))
    (should (equal (eclim--make-command '("cmd" "-a" ""))
                   "/usr/local/eclipse/eclim -command cmd -a ''"))
    (should (equal (eclim--make-command '("cmd" "-a" " a b c "))
                   "/usr/local/eclipse/eclim -command cmd -a \\ a\\ b\\ c\\ "))))

(ert-deftest test-make-command-should-allow-nils()
  (let ((eclim-executable "/usr/local/eclipse/eclim"))
    (should (equal (eclim--make-command '("cmd" "-a" "arg" "-b" nil))
                   "/usr/local/eclipse/eclim -command cmd -a arg -b"))
    (should (equal (eclim--make-command '("cmd" "-a" nil "-b" "arg"))
                   "/usr/local/eclipse/eclim -command cmd -a -b arg"))))

(provide 'command-tests)
;;; command-tests.el ends here
