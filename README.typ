= Treesit Modes Settings

A collection of treesit major mode settings. Those treesit major modes are from
either Emacs master branch or the Melpa.

```lisp
'((lang0 (:feature nil
          :treesit-font-lock-settings nil
          :treesit-font-lock-feature-list nil
          :treesit-simple-indent-rules nil))
  (lang1 ...))
```

== Usage

see _Commentary_ in #link("./treesit-modes-settings.el")[./treesit-modes-settings.el].


== Dev Notes

Install #link("https://github.com/casey/just")[just]

```bash
just --list
```

