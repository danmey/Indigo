#!/bin/sh

set -e

TARGET=indigo
FLAGS="-use-ocamlfind"
OCAMLBUILD=ocamlbuild

ocb()
{
  $OCAMLBUILD $FLAGS $*
}

# Not sure why we need it
symlink()
{
    for f in `cat $TARGET.itarget`; do ln -s _build/$f `basename $f`; done
}

rule() {
  case $1 in
    clean)  ocb -clean;;
    native) ocb $TARGET.otarget; symlink;;
    # byte)   ocb $TARGET.byte;;
    all)    ocb $TARGET.otarget; symlink;;
    depend) echo "Not needed.";;
    *)      echo "Unknown action $1";;
  esac;
}

if [ $# -eq 0 ]; then
  rule all
else
  while [ $# -gt 0 ]; do
    rule $1;
    shift
  done
fi
