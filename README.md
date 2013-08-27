bin-doc
=======

Store OCamlDoc info into binary `cmd` and `cmdi` files.

This installs `ocamlbindoc` and `ocamlbindoc.opt`, which must be run with the
same command-line flags as `ocamlc` or `ocamlc.opt` (this is a temporary
measure until we patch the main compiler frontends to understand `cmd` files).

You can automate this with a simple script in `~/.opam/4.01.0beta1/bin`.  First
move `ocamlc` to `ocamlc.orig` and `ocamlc.opt` or `ocamlc.opt.orig`, and then
install these new scripts (and make them executable).

ocamlc:
```
#!/bin/sh
ocamlc.opt.orig "$@"
ocamlbindoc.opt "$@"
```

ocamlc.opt:
```
#!/bin/sh -ex
ocamlc.orig "$@"
ocamlbindoc "$@"
```
