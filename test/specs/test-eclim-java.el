;;; test-eclim-java.el --- Tests for eclim-java.el  -*- lexical-binding: t; -*-

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

;; Specification tests for eclim-java.el

;;; Code:

(require 'undercover-init.el)
(require 'eclim-java "eclim-java.el")

(describe "eclim-java"

          (describe "eclim--java-get-selected-fields"
                    (it "returns nil if the region is not active"
                        (spy-on 'use-region-p :and-return-value nil)
                          (spy-on 'eclim--current-region-no-properties :and-return-value "
private int test;
")
                          (expect (eclim--java-get-selected-fields) :to-equal nil))

                    (it "returns non-nil if the region is active with fields"
                        (spy-on 'use-region-p :and-return-value t)
                          (spy-on 'eclim--current-region-no-properties :and-return-value "
private int test;
")
                          (expect (eclim--java-get-selected-fields) :to-equal (list "test")))

                    (it "returns nil if the region is active without fields"
                        (spy-on 'use-region-p :and-return-value t)
                          (spy-on 'eclim--current-region-no-properties :and-return-value "
\"Not a field\"
")
                          (expect (eclim--java-get-selected-fields) :to-equal nil))

                    (it "returns the field list in simple situations"
                          (spy-on 'use-region-p :and-return-value t)
                          (spy-on 'eclim--current-region-no-properties :and-return-value "
private int test = 54;
private int test2;
")
                          (expect (eclim--java-get-selected-fields) :to-equal (list "test" "test2")))

                    (it "returns the field list in complicated situations"
                          (spy-on 'use-region-p :and-return-value t)
                          (spy-on 'eclim--current-region-no-properties :and-return-value "
// Test comment
private int test = 54;
/* Test comment
 */
private String test2 =
	\"Hello, world!\"; // Trailing comment
/*
 * Test comment
 */
private String test3 /* Strange comment location */ = \"diff;icult\";
// Test comment
private String test4 = \"diff\\\"icult\"; /* Trailing comment */
// Test comment
private char test5 = 'H', /* trailing comment */ test6 = '\\'';
private String test7 = \"He;lo\"; private int test8 = 85;
private int test9 ;
")
                          (expect (eclim--java-get-selected-fields) :to-equal (list "test" "test2" "test3" "test4" "test5" "test6" "test7" "test8" "test9")))

                    (it "returns the field list without non-field parts"
                          (spy-on 'use-region-p :and-return-value t)
                          (spy-on 'eclim--current-region-no-properties :and-return-value "
public class Test {
	// Test comment
	private int test = 54;
	/* Test comment
	 */
	private String test2 =
		\"Hello, world!\"; // Trailing comment
	/*
	 * Test comment
	 */
	private String test3 /* Strange comment location */ = \"diff;icult\";
	// Test comment
	private String test4 = \"diff\\\"icult\"; /* Trailing comment */
	// Test comment
	private char test5 = 'H', /* trailing comment */ test6 = '\\'';
	private String test7 = \"He;lo\"; private int test8 = 85;
	private int tes
")
                          (expect (eclim--java-get-selected-fields) :to-equal (list "test" "test2" "test3" "test4" "test5" "test6" "test7" "test8")))
                    )
          )

;;; test-eclim-java.el ends here
