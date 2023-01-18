#!/bin/bash
while read l; do
f=$(echo $l | awk -F" : " '{print $1}')
link=$(echo $l | awk -F" : " '{print $2}')
if grep -q "$1" <<< "$l"; then
read -p "untrash $f? (y/n): " reply < /dev/tty
if [[ $reply == "y" ]]; then
prevhome=$(dirname "$f")
if [[ ! -d $prevhome ]]; then
echo "previous directory $prevhome dead, $f is going to untrash to /home"
f=/home/"$1"
fi
while [[ -f "$f" ]]; do
read -p "$f already exists, come up with another name: " newname < /dev/tty
f=$(dirname "$f")"/"$newname
done
ln /home/.trash/$link "$f"
rm /home/.trash/$link
echo "untrashed. enjoy $f"
exit 0
fi
fi
done < /home/.trash.log
echo "no $1 in .trash.log"