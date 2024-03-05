;;; ursa-ts-mode.el --- Major mode for Ursa, using tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.
;; Copyright (C) 2023 Reuben Thomas <rrt@sc3d.org>

;; Author     : Reuben Thomas <rrt@sc3d.org>
;; Maintainer : Reuben Thomas <rrt@sc3d.org>
;; Created    : August 2023
;; Version    : 1.3.5
;; Keywords   : ursalang, languages, tree-sitter
;; Package-Requires : ((emacs "29.1"))
;; URL: https://github.com/ursalang/ursa-ts-mode

;; This file is not part of GNU Emacs.

;; This package is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this package.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; An editing mode for the Ursa programming language:
;; https://ursalang.github.io
;;
;; This package was adapted from json-ts-mode.el by Theodor Thornhill,
;; from Emacs 29.1.

;;; Code:

(require 'treesit)
(require 'rx)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")


(defcustom ursa-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `ursa-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'ursa)

(defvar ursa-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?\\ "\\"    table)
    (modify-syntax-entry ?+  "."     table)
    (modify-syntax-entry ?-  "."     table)
    (modify-syntax-entry ?=  "."     table)
    (modify-syntax-entry ?%  "."     table)
    (modify-syntax-entry ?<  "."     table)
    (modify-syntax-entry ?>  "."     table)
    (modify-syntax-entry ?&  "."     table)
    (modify-syntax-entry ?|  "."     table)
    (modify-syntax-entry ?\' "\""    table)
    (modify-syntax-entry ?\240 "."   table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"  table)
    (modify-syntax-entry ?\^m "> b" table)
    table)
  "Syntax table for `ursa-ts-mode'.")


(defvar ursa-ts--indent-rules
  `((ursa
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((parent-is "block") parent-bol ursa-ts-mode-indent-offset)
     ((parent-is "list") parent-bol ursa-ts-mode-indent-offset)
     ((parent-is "object") parent-bol ursa-ts-mode-indent-offset)
     ((parent-is "map") parent-bol ursa-ts-mode-indent-offset))))

(defvar ursa-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'ursa
   :feature 'comment
   '((line_comment) @font-lock-comment-face)
   :language 'ursa
   :feature 'comment
   '((block_comment) @font-lock-comment-face)
   :language 'ursa
   :feature 'bracket
   '((["[" "]" "{" "}"]) @font-lock-bracket-face)
   :language 'ursa
   :feature 'keyword
   '((["and" "break" (continue) "else" "for" "fn" "if" "let" "loop" "not" "of" "or" "return" "use"])
     @font-lock-keyword-face)
   :language 'ursa
   :feature 'function-name
   '((call function: (identifier) @font-lock-function-name-face))
   :language 'ursa
   :feature 'constant
   '([(null) (bool)] @font-lock-constant-face)
   :language 'ursa
   :feature 'variable
   '([(identifier)] @font-lock-variable-name-face)
   :language 'ursa
   :feature 'delimiter
   '((["," ":"]) @font-lock-delimiter-face)
   :language 'ursa
   :feature 'number
   '((number) @font-lock-number-face)
   :language 'ursa
   :feature 'string
   '((string) @font-lock-string-face)
   :language 'ursa
   :feature 'pair
   :override t ; Needed for overriding string face on keys.
   '((property_identifier) @font-lock-property-use-face)
   :language 'ursa
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Font-lock settings for Ursa.")

(defun ursa-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ((or "pair" "object")
     (string-trim (treesit-node-text
                   (treesit-node-child-by-field-name
                    node "key")
                   t)
                  "\"" "\""))))

;;;###autoload
(define-derived-mode ursa-ts-mode prog-mode "Ursa"
  "Major mode for editing Ursa, powered by tree-sitter."
  :group 'ursa
  :syntax-table ursa-ts-mode--syntax-table

  (unless (treesit-ready-p 'ursa)
    (error "Tree-sitter for Ursa isn't available"))

  (treesit-parser-create 'ursa)

  ;; Comments.
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))

  ;; Indent.
  (setq-local treesit-simple-indent-rules ursa-ts--indent-rules)

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (rx (or "pair" "object")))
  (setq-local treesit-defun-name-function #'ursa-ts-mode--defun-name)

  (setq-local treesit-sentence-type-regexp "pair")

  ;; Font-lock.
  (setq-local treesit-font-lock-settings ursa-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment constant variable number pair string function-name keyword)
                (error)
                (bracket delimiter)))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
              '((nil "\\`pair\\'" nil nil)))

  (treesit-major-mode-setup))

;;;###autoload
(if (treesit-ready-p 'ursa)
    (add-to-list 'auto-mode-alist
                 '("\\.ursa\\'" . ursa-ts-mode)))

(add-to-list 'treesit-language-source-alist
      '((ursa "https://github.com/ursalang/tree-sitter-ursa")))

(provide 'ursa-ts-mode)

;;; ursa-ts-mode.el ends here
