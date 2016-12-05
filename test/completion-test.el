;;; completion-tests.el --- Tests for sequences.el

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

(require 'company-emacs-eclim)

(ert-deftest completion-yasnippet-convert ()
  ;; Nested params should *not* be nested templates.
  (should (equal (eclim--completion-yasnippet-convert
                  "addAll(Collection<? super Object> c, T... elements)")
                 "addAll(${Collection<? super Object> c}, ${T... elements})"))
  ;; Corner case: no argument.
  (should (equal (eclim--completion-yasnippet-convert "toString()")
                 "toString()"))

  ;; Basic cases.
  (should (equal (eclim--completion-yasnippet-convert
                  "printf(Locale l, String format, Object... args)")
                 "printf(${Locale l}, ${String format}, ${Object... args})"))
  (should (equal (eclim--completion-yasnippet-convert "HashMap<K,V>")
                 "HashMap<${K}, ${V}>"))

  )

(ert-deftest completion-insert-empty-usable ()
  (let ((eclim-insertion-functions '(eclim-completion-insert-empty)))
    (cl-letf (((symbol-function 'eclim-java-import) #'ignore))
      (with-temp-buffer
        (insert "method(String arg1, List<String> arg2) - some.Class")
        (eclim--completion-action-java (line-beginning-position) (point))
        (should (equal (thing-at-point 'line) "method()"))
        (should (looking-at ")"))
        (erase-buffer)
        (insert "method2()")
        (should (equal (thing-at-point 'line) "method2()"))
        (should (eolp))
        ))))

(provide 'completion-tests)
;;; completion-tests.el ends here
