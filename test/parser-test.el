;;; parser-tests.el --- Tests for sequences.el

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

(ert-deftest java-parser-read-complex-signarure()
  (should (equal (eclim--java-parser-read "public Message<?> preSend(org.springframework.integration.Message<?>,org.springframework.integration.MessageChannel)")
                 '(public Message ((\?)) preSend ((org\.springframework\.integration\.Message ((\?))) (org\.springframework\.integration\.MessageChannel))))))

(ert-deftest parse-complex-signature ()
  (should (equal (eclim--java-parse-method-signature "public Message<?> preSend(org.springframework.integration.Message<?>,org.springframework.integration.MessageChannel)")
                 '((:arglist ((:type org\.springframework\.integration\.Message ((\?)))) ((:type org\.springframework\.integration\.MessageChannel))) (:name . preSend) (:return public Message ((\?)))))))


(provide 'parser-tests)
;;; parser-test.el ends here
