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

## Supported Treesit Major Modes

``` bash
❯ just list-major-modes 
cmake-ts-mode.el
conf-mode.el
csharp-mode.el
css-mode.el
c-ts-mode.el
dockerfile-ts-mode.el
elixir-ts-mode.el
erts-mode.el
gdb-mi.el
go-ts-mode.el
heex-ts-mode.el
html-ts-mode.el
java-ts-mode.el
js.el
json-ts-mode.el
lua-ts-mode.el
python.el
ruby-ts-mode.el
rust-ts-mode.el
sh-script.el
toml-ts-mode.el
typescript-ts-mode.el
yaml-ts-mode.el
ada-ts-mode
astro-ts-mode
awk-ts-mode
clojure-ts-mode
deno-ts-mode
elixir-ts-mode
gpr-ts-mode
graphql-ts-mode
heex-ts-mode
jack-ts-mode
jq-ts-mode
julia-ts-mode
kotlin-ts-mode
llvm-ts-mode
mermaid-ts-mode
nix-ts-mode
noir-ts-mode
nushell-ts-mode
ocaml-ts-mode
prisma-ts-mode
protobuf-ts-mode
scala-ts-mode
templ-ts-mode
uiua-ts-mode
ursa-ts-mode
verilog-ts-mode
vhdl-ts-mode
vimscript-ts-mode
wat-ts-mode
```

## Usage

see *Commentary* in
[./treesit-modes-settings.el](./treesit-modes-settings.el).

## Dev Notes

Install [just](https://github.com/casey/just)

``` bash
just --list
```
