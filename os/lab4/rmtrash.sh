#!/bin/bash
if [[ ! -d /home/.trash ]]; then
mkdir /home/.trash
fi
if [[ ! -f /home/.trash.log ]]; then
touch /home/.trash.log
fi
if [[ ! -f "$1" ]]; then
echo "$1 doesnt exist"
exit 1
fi
link=$(date +%Y.%m.%d.%T)
placehecalledhome=$(pwd)"/""$1"
ln "$1" /home/.trash/$link
rm "$1"
echo "$placehecalledhome : $link" >> /home/.trash.log
echo "$1 dead"