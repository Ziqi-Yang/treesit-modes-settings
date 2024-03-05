# graphql-ts-mode

[![MELPA](https://melpa.org/packages/graphql-ts-mode-badge.svg)](https://melpa.org/#/graphql-ts-mode)

This is a major mode for editing [GraphQL][gql] documents based on the built in
support for [tree-sitter][ts] in Emacs.

[gql]: https://graphql.org/
[ts]: https://tree-sitter.github.io/tree-sitter/

## Installation

Requires Emacs 29.1 or later with tree-sitter support. This was developed for
the [grammar available here][grammar].

[grammar]: https://github.com/bkegley/tree-sitter-graphql

The installation snippets below add configuration to automatically install the
grammar using `treesit-install-language-grammar`. This requires a working C
compiler. Install the grammar before opening a GraphQL file to avoid errors.

### From MELPA

This package is available from [MELPA](https://melpa.org/).

```
(use-package graphql-ts-mode
  :ensure t
  :mode ("\\.graphql\\'" "\\.gql\\'")
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(graphql "https://github.com/bkegley/tree-sitter-graphql"))))
```

### From source

Clone this repository to `~/.emacs.d/lisp/graphql-ts-mode/` or another path,
then configure it like this:

```elisp
(use-package graphql-ts-mode
  :ensure nil
  :load-path "lisp/graphql-ts-mode/"
  :mode ("\\.graphql\\'" "\\.gql\\'")
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(graphql "https://github.com/bkegley/tree-sitter-graphql"))))
```

## Comparison to graphql-mode

There is an existing major mode for GraphQL that does not require tree-sitter,
called [graphql-mode][graphql-mode].

[graphql-mode]: https://github.com/davazp/graphql-mode

The major differences between graphql-ts-mode and graphql-mode are:

- graphql-mode has features to send actual queries, graphql-ts-mode does not
- graphql-ts-mode should perform better on large files
- graphql-ts-mode has more extensive syntax highlighting
