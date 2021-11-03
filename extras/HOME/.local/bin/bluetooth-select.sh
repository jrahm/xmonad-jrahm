#!/bin/bash

devices="$(bluetoothctl -- devices | sed 's#^Device ##')"
selection="$(
  echo -e "$devices\nDisconnect" |
    dmenu -i -nf "#8888ff" -sb "#8888ff" -p "Connect Bluetooth" -l 12)"

macaddr="${selection%% *}"

if [[ "$macaddr" == "Disconnect" ]] ; then
  exec bluetoothctl -- disconnect
fi

exec bluetoothctl -- connect "$macaddr"
