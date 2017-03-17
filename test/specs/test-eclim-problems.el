;;; test-eclim-problems.el --- Tests for eclim-problems.el  -*- lexical-binding: t; -*-

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

;; Specification tests for eclim-problems.el

;;; Code:

(require 'undercover-init.el)
(require 'eclim-problems "eclim-problems.el")

(describe "eclim-problems"

          (describe "eclim--get-problems-buffer"
                    (it "returns nil if no problems buffer exists"
                        (spy-on 'get-buffer :and-return-value nil)
                        (expect (eclim--get-problems-buffer) :to-be nil))

                    (it "returns existing problems buffer"
                        (let ((buffer (generate-new-buffer "test-problems-buffer")))
                          (spy-on 'get-buffer :and-return-value buffer)
                          (expect (eclim--get-problems-buffer) :to-be buffer)))
                    )

          (describe "eclim-get-problems-buffer-create"
                    (it "return existing problems buffer"
                        (let ((buffer (generate-new-buffer "test-problems-buffer")))
                          (spy-on 'eclim--get-problems-buffer :and-return-value buffer)
                          (expect (eclim--get-problems-buffer-create) :to-be buffer)))

                    (it "creates and returns a new problems buffer if none exists"
                        (let ((buffer (get-buffer-create eclim--problems-buffer-name))
                              (problems-buffer nil))
                          (with-current-buffer buffer
                            (comint-mode))
                          ;; These stop nested calls to shell-command-to-string
                          ;; which for some reason kill our buffer.
                          (spy-on 'eclim-project-name :and-return-value "project_name")
                          (spy-on 'eclim--problems-mode :and-return-value nil)
                          (spy-on 'eclim-problems-buffer-refresh :and-return-value nil)

                          ;; Fake deferred creation of the problems buffer.
                          (spy-on 'eclim--get-problems-buffer :and-call-fake (lambda () problems-buffer))
                          (spy-on 'get-buffer-create :and-call-fake (lambda (buffer-name) (setq problems-buffer buffer)))
                          (expect (eclim--get-problems-buffer-create) :to-be buffer)))
                    )
          )

;;; test-eclim-problems.el ends here
