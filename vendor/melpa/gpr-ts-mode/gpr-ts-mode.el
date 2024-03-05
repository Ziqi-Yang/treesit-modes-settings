;;; gpr-ts-mode.el --- Major mode for GNAT project files using Tree-Sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Troy Brown

;; Author: Troy Brown <brownts@troybrown.dev>
;; Created: February 2023
;; Version: 0.5.4
;; Keywords: gpr gnat ada languages tree-sitter
;; URL: https://github.com/brownts/gpr-ts-mode
;; Package-Requires: ((emacs "29.1"))

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

;;; Commentary:

;; This package provides GNAT Project syntax highlighting, indentation
;; and navigation using Tree-Sitter.  To use the `gpr-ts-mode' major
;; mode you will need the appropriate grammar installed.  By default,
;; on mode startup if the grammar is not detected, you will be
;; prompted to automatically install it.

;;; Code:

(require 'info-look)
(require 'lisp-mnt)
(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-type "treesit.c")

(defgroup gpr-ts nil
  "Major mode for GNAT Project files, using Tree-Sitter."
  :group 'languages
  :link '(emacs-library-link :tag "Source" "gpr-ts-mode.el")
  :link `(url-link :tag "Website"
                   ,(lm-website (locate-library "gpr-ts-mode.el")))
  :link '(custom-manual "(gpr-ts-mode)Top")
  :prefix "gpr-ts-mode-")

(defcustom gpr-ts-mode-indent-offset 3
  "Indentation of statements."
  :type 'integer
  :safe #'integerp
  :group 'gpr-ts
  :link '(custom-manual :tag "Indentation" "(gpr-ts-mode)Indentation")
  :package-version "0.5.0")

(defcustom gpr-ts-mode-indent-when-offset gpr-ts-mode-indent-offset
  "Indentation of `when' relative to `case'."
  :type 'integer
  :safe #'integerp
  :group 'gpr-ts
  :link '(custom-manual :tag "Indentation" "(gpr-ts-mode)Indentation")
  :package-version "0.5.0")

(defcustom gpr-ts-mode-indent-broken-offset (- gpr-ts-mode-indent-offset 1)
  "Indentation for the continuation of a broken line."
  :type 'integer
  :safe #'integerp
  :group 'gpr-ts
  :link '(custom-manual :tag "Indentation" "(gpr-ts-mode)Indentation")
  :package-version "0.5.0")

(defcustom gpr-ts-mode-indent-exp-item-offset (- gpr-ts-mode-indent-offset 1)
  "Indentation for the continuation of an expression."
  :type 'integer
  :safe #'integerp
  :group 'gpr-ts
  :link '(custom-manual :tag "Indentation" "(gpr-ts-mode)Indentation")
  :package-version "0.5.0")

(defcustom gpr-ts-mode-grammar "https://github.com/brownts/tree-sitter-gpr"
  "Configuration for downloading and installing the tree-sitter language grammar.

Additional settings beyond the git repository can also be
specified.  See `treesit-language-source-alist' for full details."
  :type '(choice (string :tag "Git Repository")
                 (list :tag "All Options"
                       (string :tag "Git Repository")
                       (choice :tag "Revision" (const :tag "Default" nil) string)
                       (choice :tag "Source Directory" (const :tag "Default" nil) string)
                       (choice :tag "C Compiler" (const :tag "Default" nil) string)
                       (choice :tag "C++ Compiler" (const :tag "Default" nil) string)))
  :group 'gpr-ts
  :link '(custom-manual :tag "Grammar Installation" "(gpr-ts-mode)Grammar Installation")
  :package-version "0.5.0")

(defcustom gpr-ts-mode-grammar-install 'prompt
  "Configuration for installation of tree-sitter language grammar library."
  :type '(choice (const :tag "Automatically Install" auto)
                 (const :tag "Prompt to Install" prompt)
                 (const :tag "Do not install" nil))
  :group 'gpr-ts
  :link '(custom-manual :tag "Grammar Installation" "(gpr-ts-mode)Grammar Installation")
  :package-version "0.5.0")

(defvar gpr-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?-  ". 12" table)
    (modify-syntax-entry ?=  "."    table)
    (modify-syntax-entry ?&  "."    table)
    (modify-syntax-entry ?\| "."    table)
    (modify-syntax-entry ?>  "."    table)
    (modify-syntax-entry ?\' "."    table)
    (modify-syntax-entry ?\\ "."    table)
    (modify-syntax-entry ?\n ">"    table)
    table)
  "Syntax table for `gpr-ts-mode'.")

(defun gpr-ts-mode--indent-recompute (symbol newval operation where)
  "Recompute indentation variables when SYMBOL is changed.

SYMBOL is expected to be `gpr-ts-mode-indent-offset', and
OPERATION is queried to check that it is a `set' operation (as
defined by `add-variable-watcher'), otherwise nothing is updated.
Assuming the global value has not been updated by the user, the
indentation variables are updated using the NEWVAL of SYMBOL and
made buffer-local WHERE indicates a buffer-local modification of
SYMBOL, else the default value is updated instead."
  (when (and (eq symbol 'gpr-ts-mode-indent-offset)
             (eq operation 'set))
    (dolist (indent-symbol '(gpr-ts-mode-indent-when-offset
                             gpr-ts-mode-indent-broken-offset
                             gpr-ts-mode-indent-exp-item-offset))
      (let* ((valspec (or (custom-variable-theme-value indent-symbol)
                          (get indent-symbol 'standard-value)))
             (cur-custom-value (eval (car valspec)))
             ;; This routine is invoked before SYMBOL is updated to
             ;; NEWVAL so we need to bind it to the new value so the
             ;; other indentation variables are evaluated using the
             ;; updated value.
             (gpr-ts-mode-indent-offset newval)
             (new-custom-value (eval (car valspec))))
        ;; Only update if not globally modified by the user outside of
        ;; the customization system (e.g., via `set-default'), or the
        ;; symbol is already buffer local.
        (when (or (eql cur-custom-value (default-value indent-symbol))
                  (and where (buffer-local-boundp indent-symbol where)))
          (if where
              (with-current-buffer where
                (set (make-local-variable indent-symbol) new-custom-value))
            (set-default indent-symbol new-custom-value)))))))

(add-variable-watcher 'gpr-ts-mode-indent-offset #'gpr-ts-mode--indent-recompute)

(defun gpr-ts-mode--anchor-first-sibling-matching (type)
  "Position of first sibling of node whose type matches TYPE."
  (lambda (_n parent &rest _)
    (treesit-node-start
     (car
      (treesit-filter-child
       parent
       (lambda (n)
         (equal (treesit-node-type n) type)))))))

(defun gpr-ts-mode--after-first-sibling-p (sibling)
  "Determine if the location of node comes after SIBLING."
  (lambda (_node parent bol &rest _)
    (if-let ((sibling-node
              (gpr-ts-mode--first-child-matching parent sibling)))
        (< (treesit-node-start sibling-node) bol))))

(defun gpr-ts-mode--before-first-sibling-p (sibling)
  "Determine if the location of node comes before SIBLING."
  (lambda (_node parent bol &rest _)
    (if-let ((sibling-node
              (gpr-ts-mode--first-child-matching parent sibling)))
        (> (treesit-node-start sibling-node) bol))))

(defun gpr-ts-mode--between-siblings-p (first-sibling last-sibling)
  "Deterine if node is between FIRST-SIBLING and LAST-SIBLING."
  (lambda (node parent bol &rest _)
    (let ((after (gpr-ts-mode--after-first-sibling-p first-sibling))
          (before (gpr-ts-mode--before-first-sibling-p last-sibling)))
      (and (funcall after node parent bol)
           (funcall before node parent bol)))))

(defun gpr-ts-mode--sibling-exists-p (sibling)
  "Determine if SIBLING exists."
  (lambda (_node parent _bol &rest _)
    (gpr-ts-mode--first-child-matching parent sibling)))

(defun gpr-ts-mode--next-sibling (node parent bol &rest _)
  "Determine next sibling in PARENT after this NODE or BOL."
  (if node
      (treesit-node-next-sibling node)
    (car
     (treesit-filter-child
      parent
      (lambda (n)
        (> (treesit-node-start n) bol))))))

(defalias 'gpr-ts-mode--next-sibling-exists-p
  'gpr-ts-mode--next-sibling)

(defun gpr-ts-mode--next-sibling-not-matching (type)
  "Locate next sibling not matching TYPE."
  (lambda (node parent bol &rest _)
    (let ((sibling-node (gpr-ts-mode--next-sibling node parent bol)))
      (while (and sibling-node
                  (equal (treesit-node-type sibling-node) type))
        (setq sibling-node (treesit-node-next-sibling sibling-node)))
      sibling-node)))

(defalias 'gpr-ts-mode--next-sibling-not-matching-exists-p
  'gpr-ts-mode--next-sibling-not-matching)

(defun gpr-ts-mode--anchor-of-next-sibling-not-matching (type)
  "Determine indentation anchor of next sibling not matching TYPE."
  (lambda (node parent bol &rest _)
    (let ((sibling-node
           (funcall (gpr-ts-mode--next-sibling-not-matching type) node parent bol)))
      (car (treesit-simple-indent sibling-node parent (treesit-node-start sibling-node))))))

(defun gpr-ts-mode--offset-of-next-sibling-not-matching (type)
  "Determine indentation offset of next sibling not matching TYPE."
  (lambda (node parent bol &rest _)
    (let ((sibling-node
           (funcall (gpr-ts-mode--next-sibling-not-matching type) node parent bol)))
      (cdr (treesit-simple-indent sibling-node parent (treesit-node-start sibling-node))))))

(defun gpr-ts-mode--anchor-next-sibling (node parent bol &rest _)
  "Determine location of next sibling after NODE in PARENT or BOL."
  (treesit-node-start
   (gpr-ts-mode--next-sibling node parent bol)))

(defun gpr-ts-mode--anchor-prev-sibling-matching (sibling-type)
  "Locate previous sibling matching SIBLING-TYPE."
  (lambda (_node parent bol &rest _)
    (treesit-node-start
     (car
      (last
       (treesit-filter-child
        parent
        (lambda (n)
          (and
           (equal (treesit-node-type n) sibling-type)
           (< (treesit-node-start n) bol)))))))))

(defun gpr-ts-mode--first-child-matching (parent type)
  "Find first child of PARENT matching TYPE.
Return nil if no child of that type is found."
  (car
   (treesit-filter-child
    parent
    (lambda (n)
      (equal (treesit-node-type n) type)))))

(defvar gpr-ts-mode--keywords
  '("abstract" "all" "at"
    "case"
    "end" "extends" "external" "external_as_list"
    "for"
    "is"
    "limited"
    "null"
    "others"
    "package" ;"project"
    "renames"
    "type"
    "use"
    "when" "with")
  "GPR keywords for tree-sitter font-locking.")

(defvar gpr-ts-mode--indent-rules
  `((gpr
     ;; top-level
     ((parent-is ,(rx bos "project" eos)) column-0 0)
     ;; with_declaration
     ((and (parent-is "with_declaration")
           (or (node-is "string_literal")
               (node-is ","))
           (gpr-ts-mode--after-first-sibling-p "string_literal"))
      (gpr-ts-mode--anchor-first-sibling-matching "string_literal")
      0)
     ;; associative_array_index
     ((node-is "associative_array_index")
      (gpr-ts-mode--anchor-first-sibling-matching "(")
      1)
     ;; expression / expression_list
     ((and (parent-is "expression_list")
           (or (node-is ,(rx bos "expression" eos))
               (node-is ","))
           (gpr-ts-mode--after-first-sibling-p "expression"))
      (gpr-ts-mode--anchor-first-sibling-matching "expression")
      0)
     ((and (parent-is "expression_list")
           (node-is ,(rx bos "expression" eos)))
      (gpr-ts-mode--anchor-first-sibling-matching "(")
      1)
     ((parent-is ,(rx bos "expression" eos))
      parent
      gpr-ts-mode-indent-exp-item-offset)
     ((node-is "expression_list")
      parent
      gpr-ts-mode-indent-broken-offset)
     ((node-is ,(rx bos "expression" eos))
      parent
      gpr-ts-mode-indent-broken-offset)
     ;; typed_string_declaration
     ((and (parent-is "typed_string_declaration")
           (or (node-is "string_literal")
               (node-is ","))
           (gpr-ts-mode--after-first-sibling-p "string_literal"))
      (gpr-ts-mode--anchor-first-sibling-matching "string_literal")
      0)
     ((match "string_literal" "typed_string_declaration")
      (gpr-ts-mode--anchor-first-sibling-matching "(")
      1)
     ;; attribute_reference
     ((and (parent-is "attribute_reference")
           (or (node-is "string_literal")
               (node-is "others")))
      (gpr-ts-mode--anchor-first-sibling-matching "(")
      1)
     ;; attribute_declaration
     ((and (parent-is "attribute_declaration")
           (node-is "("))
      (gpr-ts-mode--anchor-first-sibling-matching "identifier")
      gpr-ts-mode-indent-broken-offset)
     ;; case_construction / case_item / discrete_choice_list
     ((parent-is "discrete_choice_list")
      parent
      0)
     ((node-is "discrete_choice_list")
      parent
      gpr-ts-mode-indent-broken-offset)
     ((node-is "case_item")
      parent-bol
      gpr-ts-mode-indent-when-offset)
     ((parent-is "case_item")
      parent-bol
      gpr-ts-mode-indent-offset)
     ((match "variable_reference" "case_construction")
      parent-bol
      gpr-ts-mode-indent-broken-offset)
     ((and (parent-is "case_construction")
           no-node
           (gpr-ts-mode--between-siblings-p "case_item" "end"))
      (gpr-ts-mode--anchor-prev-sibling-matching "case_item")
      gpr-ts-mode-indent-offset)
     ((and (parent-is "case_construction")
           (or no-node (node-is "comment"))
           (gpr-ts-mode--between-siblings-p "is" "end"))
      parent
      gpr-ts-mode-indent-when-offset)
     ;; general indentation for package and project declaration bodies.
     ((and (or (parent-is "project_declaration")
               (parent-is "package_declaration"))
           (gpr-ts-mode--between-siblings-p "is" "end"))
      parent
      gpr-ts-mode-indent-offset)
     ;; general indentation for newline and comments.
     ;;
     ;; NOTE: Indent to where next non-comment sibling would be
     ;; indented.  This may not be aligned to sibling if sibling isn't
     ;; properly indented, however it prevents a two-pass indentation
     ;; when region is indented, since comments won't have to be
     ;; reindented once sibling becomes properly aligned.
     ((and (or no-node (node-is "comment"))
           (gpr-ts-mode--next-sibling-not-matching-exists-p "comment"))
      (gpr-ts-mode--anchor-of-next-sibling-not-matching "comment")
      (gpr-ts-mode--offset-of-next-sibling-not-matching "comment"))
     ;; variable_declaration
     ((node-is ,(rx bos (or ":" ":=") eos))
      parent
      gpr-ts-mode-indent-broken-offset)
     ;; name (e.g., identifier . identifier)
     ((parent-is ,(rx bos "name" eos))
      parent
      0)
     ;; error recovery for string lists with syntax errors
     ((and (or (node-is "string_literal")
               (node-is ","))
           (gpr-ts-mode--after-first-sibling-p "string_literal"))
      (gpr-ts-mode--anchor-first-sibling-matching "string_literal")
      0)
     ;; name, identifier, string_literal and string_literal_at.
     ((or (node-is ,(rx bos "name" eos))
          (node-is "identifier")
          (node-is "string_literal")
          (parent-is "string_literal_at"))
      parent
      gpr-ts-mode-indent-broken-offset)
     ;; keywords / semicolon / project / project qualifier
     ((or (node-is ,(eval `(rx bos (or ,@gpr-ts-mode--keywords ";") eos)))
          (match ,(rx bos "project" eos) "project_declaration")
          (parent-is "project_qualifier"))
      parent
      0)
     ;; non-expression opening parenthesis
     ((node-is "(") parent gpr-ts-mode-indent-broken-offset)
     ;; closing parenthesis (including expression)
     ((and (node-is ")")
           (gpr-ts-mode--sibling-exists-p "("))
      (gpr-ts-mode--anchor-first-sibling-matching "(")
      0)
     ;; trival recovery for syntax error or unexpected broken line
     (catch-all prev-line 0)))
  "Tree-sitter indent rules for `gpr-ts-mode'.")

(defvar gpr-ts-mode--font-lock-settings
  (treesit-font-lock-rules

   ;; Attributes
   :language 'gpr
   :feature 'attribute
   '((attribute_reference (identifier) @font-lock-property-use-face))

   ;; Brackets
   :language 'gpr
   :feature 'bracket
   '((["(" ")"]) @font-lock-bracket-face)

   ;; Comments
   :language 'gpr
   :feature 'comment
   '((comment) @font-lock-comment-face)

   ;; Definition
   :language 'gpr
   :feature 'definition
   '((package_declaration
      [name: (identifier) endname: (identifier)] @font-lock-function-name-face)
     (typed_string_declaration name: (identifier) @font-lock-type-face)
     (variable_declaration name: (identifier) @font-lock-variable-name-face)
     (attribute_declaration name: (identifier) @font-lock-property-name-face))

   ;; Delimiters
   :language 'gpr
   :feature 'delimiter
   '(["," "." ":" ";"] @font-lock-delimiter-face)

   ;; Functions
   :language 'gpr
   :feature 'function
   :override 'prepend
   '((builtin_function_call name: _ @font-lock-function-call-face))

   ;; Keywords
   :language 'gpr
   :feature 'keyword
   `(([,@gpr-ts-mode--keywords] @font-lock-keyword-face)
     (project_declaration "project" @font-lock-keyword-face)
     ((project_qualifier) @font-lock-keyword-face))

   ;; Numeric literals
   :language 'gpr
   :feature 'number
   '((numeric_literal) @font-lock-number-face)

   ;; Package
   :language 'gpr
   :feature 'package
   '((package_declaration
      [ origname: (name (identifier) @font-lock-function-call-face :anchor)
        basename: (name (identifier) @font-lock-function-call-face :anchor)])
     (variable_reference (name (identifier) @font-lock-function-call-face :anchor) "'"))

   ;; String literals
   :language 'gpr
   :feature 'string
   '((string_literal) @font-lock-string-face)

   ;; Types
   :language 'gpr
   :feature 'type
   '((variable_declaration type: (name (identifier) @font-lock-type-face :anchor)))

   ;; Variables
   :language 'gpr
   :feature 'variable
   '((variable_reference (name (identifier) @font-lock-variable-use-face :anchor) :anchor))

   ;; Operators
   :language 'gpr
   :feature 'operator
   '([":=" "&" "|" "=>"] @font-lock-operator-face)

   ;; Syntax errors
   :language 'gpr
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))

  "Font-lock settings for `gpr-ts-mode'.")

;;; Imenu

(defun gpr-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (treesit-node-text
   (pcase (treesit-node-type node)
     ((or "project_declaration"
          "package_declaration")
      (treesit-node-child-by-field-name node "name")))
   t))

(defun gpr-ts-mode--type-name (node)
  "Return the type name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (treesit-node-text
   (pcase (treesit-node-type node)
     ("typed_string_declaration"
      (treesit-node-child-by-field-name node "name")))
   t))

(defun gpr-ts-mode--typed-variable-p (node)
  "Determine if NODE is a typed variable.
Return non-nil to indicate that it is."
  (treesit-node-child-by-field-name node "type"))

(defun gpr-ts-mode--typed-variable-name (node)
  "Return type variable name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (treesit-node-text
   (pcase (treesit-node-type node)
     ("variable_declaration"
      (treesit-node-child-by-field-name node "name")))
   t))

;;;###autoload
(define-derived-mode gpr-ts-mode prog-mode "GNAT Project"
  "Major mode for editing GNAT Project files, powered by tree-sitter."
  :group 'gpr-ts

  ;; Grammar.
  (setq-local treesit-language-source-alist
              `((gpr . ,(ensure-list gpr-ts-mode-grammar))))

  (when (and (treesit-available-p)
             (not (treesit-language-available-p 'gpr))
             (pcase gpr-ts-mode-grammar-install
               ('auto t)
               ('prompt
                ;; Use `read-key' instead of `read-from-minibuffer' as
                ;; this is less intrusive.  The later will start
                ;; `minibuffer-mode' which impacts buffer local
                ;; variables, especially font lock, preventing proper
                ;; mode initialization and results in improper
                ;; fontification of the buffer immediately after
                ;; installing the grammar.
                (let ((y-or-n-p-use-read-key t))
                  (y-or-n-p
                   (format
                    (concat "Tree-sitter grammar for GPR is missing.  "
                            "Install it from %s? ")
                    (car (alist-get 'gpr treesit-language-source-alist))))))
               (_ nil)))
    (message "Installing the tree-sitter grammar for GPR")
    (treesit-install-language-grammar 'gpr))

  (unless (treesit-ready-p 'gpr)
    (error "Tree-sitter for GPR isn't available"))

  (treesit-parser-create 'gpr)

  ;; Comments.
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx "--" (* "-") (* (syntax whitespace))))

  ;; Navigation.
  (setq-local treesit-defun-type-regexp (rx (or "project_declaration"
                                                "package_declaration")))
  (setq-local treesit-defun-name-function #'gpr-ts-mode--defun-name)

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
              `(("Type"
                 ,(rx "typed_string_declaration")
                 nil
                 gpr-ts-mode--type-name)
                ("Variable"
                 ,(rx "variable_declaration")
                 gpr-ts-mode--typed-variable-p
                 gpr-ts-mode--typed-variable-name)
                ("Package"
                 ,(rx "package_declaration")
                 nil
                 nil)
                ("Project"
                 ,(rx "project_declaration")
                 nil
                 nil)))

  ;; Indent.
  (setq-local treesit-simple-indent-rules gpr-ts-mode--indent-rules)
  (setq-local electric-indent-chars (append ";>," electric-indent-chars))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings gpr-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string type)
                (attribute function number operator package variable)
                (bracket delimiter error)))

  (treesit-major-mode-setup))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               `(,(rx (or ".gpr" ".cgpr") eos) . gpr-ts-mode))
  (add-to-list 'major-mode-remap-alist '(gpr-mode . gpr-ts-mode)))

(info-lookup-add-help
 :topic 'symbol
 :mode '(emacs-lisp-mode . "gpr")
 :regexp "\\bgpr-ts-[^][()`'‘’,\" \t\n]+"
 :doc-spec '(("(gpr-ts-mode)Variable Index" nil "^ -+ .*: " "\\( \\|$\\)")))

(provide 'gpr-ts-mode)

;;; gpr-ts-mode.el ends here
