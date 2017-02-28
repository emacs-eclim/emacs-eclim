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

(defvar test-eclimd--startup-process-events
  '("/usr/bin/java -version\njava version \"1.8.0_112\"\n"
    "2017-02-27 17:52:18,754 INFO  [org.eclim.eclipse.EclimDaemon] Eclim Server "
    "Started on: 127.0.0.1:9091\n"))

(defvar test-eclimd--failed-startup-process-events
  '("/usr/bin/java -version\n"
    (:status . exit)
    (:state . "exited abnormally with code 1\n")))

(defun test-eclimd--fake-process-events (events)
  "Spies on accept-process-output, simulating calls to process filter and sentinel.
Receives a list of events. Each event is either a string that
represents output from the process or a cons cell taking one of
the following forms:
(:state . \"state--string\") -- \"state-string\" is passed to the sentinel.
(:status . process-status) -- specifies the value returned by `process-status'."
  (let ((current-status 'run))
    (spy-on 'accept-process-output :and-call-fake
            (lambda (&rest _)
              (let ((event (pop events)))
                (if (stringp event)
                    (save-match-data (eclimd--process-filter nil event))
                  (pcase (car event)
                    (:status (setq current-status (cdr event)))
                    (:state
                     (save-match-data (eclimd--process-sentinel nil (cdr event)))))))))
    (spy-on 'process-status :and-call-fake
            (lambda (&rest _) current-status))))

(describe "eclimd"
          (describe "eclimd--await-connection"
                    (it "executes callback before returning when async is nil"
                        (test-eclimd--fake-process-events
                         test-eclimd--startup-process-events)
                        (let ((marker nil))
                          (eclimd--await-connection nil (lambda () (setq marker t)))
                          (expect marker :to-be-truthy)))
                    (it "does not execute callback when eclimd fails to start"
                        (test-eclimd--fake-process-events
                         test-eclimd--failed-startup-process-events)
                        (let ((marker nil))
                          (eclimd--await-connection nil (lambda () (setq marker t)))
                          (expect marker :not :to-be-truthy)))))

;;; test-eclimd.el ends here
