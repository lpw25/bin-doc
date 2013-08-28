bin-doc
=======

Store OCamlDoc info into binary `cmd` and `cmdi` files.

This installs `ocamlbindoc` and `ocamlbindoc.opt`, which must be run with the
same command-line flags as `ocamlc` or `ocamlc.opt` (this is a temporary
measure until we patch the main compiler frontends to understand `cmd` files).

You can automate this by installing wrapper scripts in
`~/.opam/4.01.0beta1/bin`.  Just run `make install-divert` to rename the
default `ocamlc` and replace them with ones that invoke both `ocamlc` and
`ocamlbindoc`.  Make sure you only do this in a custom OPAM compiler switch, or
else you will end up modifying your system compiler.

You will also eventually need the `cmt` files generated by the `-bin-annot`
flag to the compiler.  Starting with 4.01 of the compiler, export this in your
environment to add the flag to the compiler automatically.

```
$ export OCAMLPARAM=bin-annot=1,_
```

