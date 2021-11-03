#!/bin/sh

actual_bin=$(readlink -f "$0")
proj_dir="$(dirname $actual_bin)"
olddir="$(pwd)"

cd "$proj_dir"
stack install

ec="$?"
if [ "$ec" -ne 0 ] ; then
  exit "$ec"
fi

cd "$olddir"

ln -sf "$HOME/.local/bin/jrahm-xmonad" "$1"
