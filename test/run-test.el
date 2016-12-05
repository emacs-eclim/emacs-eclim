;;; run-tests.el --- Tests for sequences.el

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

(ert-deftest command-for-java-configuration ()
  (let* ((conf '((name . "Test run")
                 (main-class . "com.acme.Project")
                 (program-args . "arg1 arg2")
                 (vm-args . "-Dvm-arg1=42")))
         (run-command (eclim-java-run--command conf
                                               (eclim-java-run--java-vm-args "/opt/lib.jar"))))
    (should (string-equal run-command
                          "java -classpath /opt/lib.jar -Dvm-arg1=42 com.acme.Project arg1 arg2"))))

(ert-deftest command-for-debug-configuration ()
  (let* ((conf '((name . "Debug test run")
                 (main-class . "com.acme.Project")
                 (program-args . "arg1 arg2")
                 (vm-args . "-Dvm-arg1=42")
                 (debug . t)))
         (run-command (eclim-java-run--command conf
                                               (eclim-java-run--java-vm-args "/opt/lib.jar"))))
    (should (string-equal run-command
                          "jdb -classpath/opt/lib.jar -Dvm-arg1=42 com.acme.Project arg1 arg2"))))

(ert-deftest choose-configuration ()
  (let* ((conf1 '((name . "Debug")))
         (conf2 '((name . "Run")))
         (choosen-conf (eclim-java-run--configuration "Run" (list conf1 conf2))))
    (should (equal choosen-conf conf2))))

(ert-deftest run-java-opens-buffer-in-correct-dir-with-correct-name ()
  (let* ((conf '((name . "Run")
                 (main-class . "com.acme.Project")
                 (program-args . "arg1")
                 (vm-args . "-Dvm-args1=42")))
         (buffer (eclim-java-run--run-java conf "/opt/lib.jar" "/tmp/")))
    (with-current-buffer buffer
      (should (string-equal default-directory "/tmp/"))
      (should (string-equal (buffer-name) "*Run*")))))

(provide 'run-tests)
;;; run-tests.el ends here
