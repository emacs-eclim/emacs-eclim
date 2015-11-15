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



(setq test-eclim-executable "/usr/local/eclipse/eclim")

(ert-deftest test-command-should-sync-for-normal-commands ()
  (should (eclim--command-should-sync-p "java_complete" '("-f" "~/src/test/Panda.java")))
  (should (not (eclim--command-should-sync-p "other_cmd" '("-x" "~/src/test/Panda.java")))))

(ert-deftest test-command-should-sync-for-special-commands ()
  (should (not (eclim--command-should-sync-p "project_by_resource" '("-f" "~/src/test/Panda.java"))))
  (should (not (eclim--command-should-sync-p "project_link_resource" '("-f" "~/src/test/Panda.java")))))

(ert-deftest test-make-command-fails-for-eclim-command-nil ()
  (let ((eclim-executable nil))
    (should-error (eclim--make-command '("cmd" "-a" "arg") :type 'error))))

(ert-deftest test-make-command-should-create-an-executable-string ()
  (let ((eclim-executable test-eclim-executable))
    (should (equal (eclim--make-command '("cmd" "-a" "arg"))
                   (concat test-eclim-executable " -command cmd -a arg")))))

(ert-deftest test-make-command-should-escape-arguments ()
  (let ((eclim-executable test-eclim-executable))
    (should (equal (eclim--make-command '("cmd" "-a" ""))
                   (concat test-eclim-executable " -command cmd -a ''")))
    (should (equal (eclim--make-command '("cmd" "-a" " a b c "))
                   (concat test-eclim-executable " -command cmd -a \\ a\\ b\\ c\\ ")))))

(ert-deftest test-make-command-should-allow-nils()
  (let ((eclim-executable test-eclim-executable))
    (should (equal (eclim--make-command '("cmd" "-a" "arg" "-b" nil))
                   (concat test-eclim-executable " -command cmd -a arg -b")))
    (should (equal (eclim--make-command '("cmd" "-a" nil "-b" "arg"))
                   (concat test-eclim-executable " -command cmd -a -b arg")))))

;; (ert-deftest test-parse-result-should-parse-json ()
;;   (should (equal (eclim--parse-result "{\"a\":1,\"b\":\"2\"}") '((a . 1) (b . "2"))))
;;   (should (equal (eclim--parse-result "{\"a\":1,\"b\":null}") '((a . 1) (b)))))

(ert-deftest test-parse-result-should-parse-bad-json ()
  (should (equal (eclim--parse-result " ") nil))
  (should (equal (eclim--parse-result "\n") nil))
  (should (equal (eclim--parse-result "\t") nil)))

(ert-deftest test-parse-result-should-error-for-bad-encoding ()
  (should-error (eclim--parse-result "java.io.UnsupportedEncodingException: bad-encoding") :type 'error))

(ert-deftest test-parse-result-should-error-for-bad-command ()
  (should-error (eclim--parse-result "No command 'bad_command'") :type 'error))

(ert-deftest test-parse-result-should-error-for-bad-support ()
  (let ((support-types
         '(xml_complete groovy_complete ruby_complete c_complete php_complete scala_complete)))
    (loop
     for support-type in support-types
     do (should-error (eclim--parse-result (format "No command '%s'" support-type) :type 'error)))))

(ert-deftest test-parse-result-should-error-for-exception ()
  (should-error (eclim--parse-result "java.lang.NullPointerException") :type 'error))

(ert-deftest test-parse-result-should-error-for-bad-json ()
  (should-error (eclim--parse-result "xyz") :type 'error))

(provide 'command-test)
;;; command-tests.el ends here
