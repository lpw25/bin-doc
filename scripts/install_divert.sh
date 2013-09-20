#!/usr/bin/env bash
set -e

dir=$(dirname $(which ocamlc))
echo Installing divert in: ${dir}

if [ -e "${dir}/ocamlc.orig" ]; then
  echo Divert already installed, aborting.
  exit 1
fi

mv "${dir}/ocamlc" "${dir}/ocamlc.orig"
mv "${dir}/ocamlc.opt" "${dir}/ocamlc.opt.orig"
cp ocamlc ${dir}/ocamlc
cp ocamlc.opt ${dir}/ocamlc.opt
cp opamdoc-generate ${dir}/opamdoc-generate
cp opamdoc-rebuild ${dir}/opamdoc-rebuild

if [ -x ../bin/ocamlbindoc ]; then
  cp ../bin/ocamlbindoc* ${dir}/
fi

echo Succeeded.
