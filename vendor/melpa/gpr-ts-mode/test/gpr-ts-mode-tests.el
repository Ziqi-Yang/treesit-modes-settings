;;; gpr-ts-mode-tests.el --- Tests for Tree-sitter-based GNAT Project mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Troy Brown

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'ert-x)

(ert-deftest gpr-ts-mode-test-indentation ()
  (ert-test-erts-file (ert-resource-file "indent.erts")))

(ert-deftest gpr-ts-mode-test-indentation-with_declaration ()
  (ert-test-erts-file (ert-resource-file "indent-with_declaration.erts")))

;; NOTE:
;;
;; These test cases are broken out into separate files as there appears
;; to be bugs in the built-in ERTS test harness that have not yet been
;; addressed.  This is causing test cases to use the wrong "code"
;; segment when run interactively.
;;
;; See the following for details:
;;
;; - https://debbugs.gnu.org/cgi/bugreport.cgi?bug=62471
;;

(ert-deftest gpr-ts-mode-test-indentation-case_construction ()
  (ert-test-erts-file (ert-resource-file "indent-case_construction.erts")))

(ert-deftest gpr-ts-mode-test-indentation-case_construction-nl ()
  (ert-test-erts-file (ert-resource-file "indent-case_construction-nl.erts")))

(ert-deftest gpr-ts-mode-test-indentation-typed_string_declaration ()
  (ert-test-erts-file (ert-resource-file "indent-typed_string_declaration.erts")))

(ert-deftest gpr-ts-mode-test-indentation-typed_string_declaration-nl ()
  (ert-test-erts-file (ert-resource-file "indent-typed_string_declaration-nl.erts")))

(ert-deftest gpr-ts-mode-test-indentation-expression ()
  (ert-test-erts-file (ert-resource-file "indent-expression.erts")))

(ert-deftest gpr-ts-mode-test-indentation-paren_alignment ()
  (ert-test-erts-file (ert-resource-file "indent-paren_alignment.erts")))

(ert-deftest gpr-ts-mode-test-indentation-comment ()
  (ert-test-erts-file (ert-resource-file "indent-comment.erts")))

(ert-deftest gpr-ts-mode-test-indentation-variable_declaration ()
  (ert-test-erts-file (ert-resource-file "indent-variable_declaration.erts")))

(ert-deftest gpr-ts-mode-test-filling ()
  (ert-test-erts-file (ert-resource-file "filling.erts")))

(ert-deftest gpr-ts-mode-test-navigation ()
  (ert-test-erts-file (ert-resource-file "navigation.erts")))

(provide 'gpr-ts-mode-tests)

;;; gpr-ts-mode-tests.el ends here
