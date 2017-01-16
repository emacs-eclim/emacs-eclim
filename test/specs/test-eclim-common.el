;;; eclim-common-tests.el --- Tests for eclim-common.el  -*- lexical-binding: t; -*-

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

;; Specification tests for eclim-common.el

;;; Code:

(require 'undercover-init.el)
(require 'eclim-common "eclim-common.el")

(defun test-eclim-common--fake-reply (reply)
  (spy-on 'eclim--parse-result :and-return-value reply)
  (spy-on 'eclim--call-process-no-parse :and-return-value '(1 2)))

(defun test-eclim-common--fake-error-reply ()
  (spy-on 'eclim--parse-result :and-throw-error 'error)
  (spy-on 'eclim--call-process-no-parse :and-return-value '(1 2)))

(describe "eclim-common"

          (describe "eclim--file-managed-p"
                    (it "returns the project's name for a valid file name in the project"
                        (let ((project-name "eclim_proj")
                              (file-name "file.java"))
                          (spy-on 'eclim-project-name :and-return-value project-name)
                          (expect (eclim--file-managed-p file-name) :to-be project-name)))

                    (it "returns nil when no file name is given"
                        (spy-on 'buffer-file-name :and-return-value nil)
                        (expect (eclim--file-managed-p) :to-be nil))
                    )

          (describe "eclim--accepted-p"
                    (it "returns the project's name for a valid file name in the project"
                        (let ((project-name "eclim_proj")
                              (file-name "file.java"))
                          (spy-on 'eclim-project-name :and-return-value project-name)
                          (expect (eclim--accepted-p file-name) :to-be project-name)))
                    )

          (describe "eclim/project-info"

                    (it "returns a project's information"
                        (let ((project-name "proj")
                              (reply '((path . tmp)))
                              (result))
                          (spy-on 'eclim--check-project :and-return-value nil)
                          (spy-on 'eclim--call-process :and-return-value reply)
                          (expect (eclim/project-info project-name) :to-equal '((path . tmp)))))

                    (it "errors for an invalid project"
                        (let ((project-name "proj")
                              (reply '((path . tmp))))
                          (spy-on 'eclim--check-project :and-throw-error 'error)
                          (expect (lambda () (eclim/project-info project-name)) :to-throw 'error)))

                    (it "errors for invalid response from server"
                        (let ((project-name "proj")
                              (reply '((path . tmp))))
                          (spy-on 'eclim--check-project :and-return-value nil)
                          (spy-on 'eclim--call-process :and-throw-error 'error)
                          (expect (lambda () (eclim/project-info project-name)) :to-throw 'error)))
                    )

          (describe "eclim--parse-result"
                    (it "should parse valid JSON"
                        (expect (eclim--parse-result "{\"a\":1,\"b\":\"2\"}")
                                :to-have-same-items-as '((a . 1) (b . "2")))
                        (expect (eclim--parse-result "{\"a\":1,\"b\":null}")
                                :to-have-same-items-as '((a . 1) (b))))

                    (it "should parse whitespace strings"
                        (expect (eclim--parse-result " ") :to-equal nil)
                        (expect (eclim--parse-result "\n") :to-equal nil)
                        (expect (eclim--parse-result "\t") :to-equal nil))

                    (it "should throw an error for bad encoding"
                        (expect (lambda () (eclim--parse-result "java.io.UnsupportedEncodingException: bad-encoding"))
                                :to-throw 'error))

                    (it "should throw an error for a bad command"
                        (expect (lambda () (eclim--parse-result "No command 'bad_command' found"))
                                :to-throw 'error))

                    (it "should throw an error for a bad support"
                        (let ((support-types
                               '(xml_complete groovy_complete ruby_complete c_complete php_complete scala_complete)))
                          (loop
                           for support-type in support-types
                           do (expect (lambda () (eclim--parse-result (format "No command '%s' found" support-type)))
                                      :to-throw 'error))))

                    (it "should throw an error for an exception"
                        (expect (lambda () (eclim--parse-result "java.lang.Exception: Parse error at line 32"))
                                :to-throw 'error))

                    (it "should throw an error for connection refused when eclim server is starting"
                        (setq eclimd-process nil)
                        (expect (lambda () (eclim--parse-result "connect: Connection refused"))
                                :to-throw 'error))

                    (it "should throw an error for connection refused after eclim server has started"
                        (setq eclimd-process t)
                        (expect (lambda () (eclim--parse-result "connect: Connection refused"))
                                :to-throw 'error))

                    (it "should throw an error for a bad JSON reply"
                        (expect (lambda () (eclim--parse-result "xyz"))
                                :to-throw 'error))
                    )

          (describe "eclim--call-process"
                    (it "should invoke eclim-interactive-completion-function"
                        (spy-on 'completing-read :and-return-value nil)
                        (eclim--completing-read "prompt" '("a" "b" "c"))
                        (expect 'completing-read :to-have-been-called-with "prompt" '("a" "b" "c")))
                    )

          (describe "eclim--call-process"
                    (it "should invoke eclim--parse-result and eclim--call-process-no-parse"
                        (let ((reply '("1" "2")))
                          (test-eclim-common--fake-reply reply)
                          (expect (eclim--call-process '("a" "b")) :to-have-same-items-as reply)))
                    )

          (describe "eclim--connected-p"
                    (it "returns t when Eclim server process is present"
                        (test-eclim-common--fake-reply 1)
                        (expect (eclim--connected-p) :to-be t))

                    (it "throws an error when Eclim server is not present"
                        (test-eclim-common--fake-error-reply)
                        (expect (lambda () (eclim--connected-p)) :to-throw 'error))
                    )
          (describe "eclim-project-name"
                    (it "returns the project name for a file in the project"
                        (let ((proj-name "my_proj"))
                          (test-eclim-common--fake-reply proj-name)
                          (expect (eclim-project-name "file.java") :to-be proj-name)))

                    (describe "returns the project name for buffer it was called from"

                              (it "before eclim--project-name has been assigned"
                                  (let ((proj-name "my_proj"))
                                    (setq eclim--project-name proj-name)
                                    (setq buffer-file-name nil)
                                    (test-eclim-common--fake-reply proj-name)
                                    (expect (eclim-project-name) :to-be proj-name)))

                              (it "after eclim--project-name has been assigned"
                                  (let ((proj-name "my_proj"))
                                    (setq eclim--project-name nil)
                                    (setq buffer-file-name "file.java")
                                    (test-eclim-common--fake-reply proj-name)
                                    (expect (eclim-project-name) :to-be proj-name)))

                              (it "when the buffer is in an archive file"
                                  (let ((proj-name "my_proj")
                                        (file-name "file.java"))
                                    (setq eclim--project-name nil)
                                    (setq buffer-file-name file-name)
                                    (setf (gethash file-name eclim-projects-for-archive-file) proj-name)
                                    (test-eclim-common--fake-reply nil)
                                    (expect (eclim-project-name) :to-be proj-name)))
                              )
                    )
          )

;;; eclim-common-spec ends here
