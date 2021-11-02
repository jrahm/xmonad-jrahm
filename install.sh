#!/bin/bash

real_dir=$(dirname $(readlink -f "$0"))
cd "$real_dir"

mkdir -p "$HOME/.xmonad"

cc -o ~/.xmonad/xmobar-battery xmobar/extras/battery/battery.c -lm

ln -sfv "$real_dir/build-script.sh" "$HOME/.xmonad/build"
ln -sfv "$real_dir/compton.conf" "$HOME/.config/compton.conf"
ln -sfv "$real_dir/startup" "$HOME/.xmonad/startup"
ln -sfv "$real_dir/xmobarrc" "$HOME/.xmobarrc"
ln -sfv "$real_dir/assets/wallpaper.jpg" "$HOME/.xmonad/wallpaper.jpg"
