#!/bin/bash

real_dir=$(dirname $(readlink -f "$0"))
cd "$real_dir"

mkdir -p "$HOME/.xmonad"
mkdir -p "$HOME/.config"

mkdir -p build/extras/HOME/.xmonad

cc -o \
  build/extras/HOME/.xmonad/xmobar-battery \
  xmobar/extras/battery/battery.c \
  -lm

GLOBIGNORE=".:.."
shopt -u dotglob

cd "$HOME"
cp -rsvf                           \
  "$real_dir"/extras/HOME/*        \
  "$real_dir"/extras/HOME/.*       \
  "$real_dir"/build/extras/HOME/*  \
  "$real_dir"/build/extras/HOME/.* \
  .
cd "$real_dir"
