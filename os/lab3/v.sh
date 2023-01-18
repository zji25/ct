#!/bin/bash
while true; do
read line;
if [[ $line == "QUIT" ]]; then
echo $line > pi
exit 0
elif [[ $line =~ ^[0-9]+$ ]] || [[ "$line" == "+" ]] || [[ "$line" == "*" ]]; then
echo "$line" > pi
else
echo "wrong input" > pi
exit 1
fi
done