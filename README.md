# Treesit Modes Settings

A collection of treesit major mode settings. Those treesit major modes
are from either Emacs master branch or the Melpa.

``` lisp
'((lang0 (:feature nil
          :treesit-font-lock-settings nil
          :treesit-font-lock-feature-list nil
          :treesit-simple-indent-rules nil))
  (lang1 ...))
```

## Usage

see *Commentary* in
[./treesit-modes-settings.el](./treesit-modes-settings.el).

## Dev Notes

Install [just](https://github.com/casey/just)

``` bash
just --list
```
