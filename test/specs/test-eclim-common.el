;;; eclim-common-tests.el --- Tests for eclim-common.el  -*- lexical-binding: t; -*-

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

;; Specification tests for eclim-common.el

;;; Code:

(require 'undercover-init.el)
(require 'eclim-common "eclim-common.el")

(describe "eclim-common"

          (describe "eclim/project-info"

                    (it "returns a project's information"
                        (let ((project-name "proj")
                              (reply '((path . tmp)))
                              (result))
                          (spy-on 'eclim--check-project :and-return-value nil)
                          (spy-on 'eclim--call-process :and-return-value reply)
                          (setq result (eclim/project-info project-name))
                          (expect result :to-equal '((path . tmp)))))

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
          )

;;; eclim-common-spec ends here
