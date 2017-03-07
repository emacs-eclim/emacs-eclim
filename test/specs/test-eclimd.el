;;; test-eclimd.el --- Tests for controlling eclimd  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Luis Gerhorst

;; Author: Luis Gerhorst <privat@luisgerhorst.de>

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

;; Buttercup tests for eclimd.el.

;;; Code:

(require 'undercover-init.el)
(require 'eclimd)

(defun test-eclimd--fake-process-events (events)
  "Spies on accept-process-output, simulating calls to process filter and sentinel.
Receives a list of events. Each event is either a string that
represents output from the process or a cons cell taking one of
the following forms:
\(:state-change . \"state--string\"\) -- \"state-string\" is passed to the sentinel.
\(:set-status . process-status\) -- specifies the value returned by `process-status'."
  (let ((current-status 'run))
    (spy-on 'accept-process-output :and-call-fake
            (lambda (&rest _)
              (let ((event (pop events)))
                ;; Do not walk past the end of the list. This prevents endless
                ;; looping.
                (expect event :to-be-truthy)
                (if (stringp event)
                    (save-match-data (eclimd--process-filter nil event))
                  (pcase (car event)
                    (:set-status (setq current-status (cdr event)))
                    (:state-change
                     (save-match-data
                       (eclimd--process-sentinel nil (cdr event))))))
                (expect event :to-be-truthy))))
    (spy-on 'process-status :and-call-fake
            (lambda (&rest _) current-status))))

(describe "eclimd"
          (describe "eclimd--match-process-output"
                    (it "executes callback before returning when the supplied regexp matches the output"
                        (test-eclimd--fake-process-events
                         '("a string\n"
                           "a different string to be matched plus bonus\n"
                           "some more text"))
                        (let ((marker nil))
                          (eclimd--match-process-output "string to be matched"
                                                        nil
                                                        (lambda (&rest _) (setq marker t)))
                          (expect marker :to-be-truthy)))
                    (it "returns when the regexp matches inside the concatinated output"
                        (test-eclimd--fake-process-events
                         '("a string\n"
                           "a different string "
                           "to be "
                           "matched\n"
                           "some more text"))
                        (eclimd--match-process-output "string to be matched" nil nil))
                    (it "returns the concatinated output until the match and passes it to the callback"
                        ;; This one is a little tricky since it is not specified
                        ;; how much of the output is included in the return
                        ;; value. It may or may not end immediatly after the
                        ;; string to be matched.
                        (let* ((events '("a string\n"
                                         "a different string "
                                         "to be "
                                         "matched"))
                               (expected-output (apply 'concat events))
                               (marker nil))
                          (test-eclimd--fake-process-events events)
                          (expect (eclimd--match-process-output
                                   "string to be matched" nil
                                   (lambda (output)
                                     (expect output :to-equal expected-output)
                                     (setq marker t)))
                                  :to-equal expected-output)
                          ;; Ensure the callback was actually executed.
                          (expect marker :to-be-truthy))))
          (describe "eclimd--await-connection"
                    (it "executes callback before returning when async is nil"
                        (test-eclimd--fake-process-events
                         '("/usr/bin/java -version\njava version \"1.8.0_112\"\n"
                           "2017-02-27 [org.eclim.eclipse.EclimDaemon] Eclim Server "
                           "Started on: 127.0.0.1:9091\n"))
                        (let ((marker nil))
                          (eclimd--await-connection nil (lambda () (setq marker t)))
                          (expect marker :to-be-truthy)))
                    (it "does not execute callback when eclimd fails to start"
                        (test-eclimd--fake-process-events
                         '("/usr/bin/java -version\n"
                           (:set-status . exit)
                           (:state-change . "exited abnormally with code 1\n")))
                        (let ((marker nil))
                          (eclimd--await-connection nil (lambda () (setq marker t)))
                          (expect marker :not :to-be-truthy)))))

;;; test-eclimd.el ends here
