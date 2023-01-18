#!/bin/bash
mode="+"
x=1
(tail -f pi) |
while true; do
read l;
if [[ "$l" == "+" ]]; then
mode="+"
elif [[ "$l" == "*" ]]; then
mode="/*"
elif [[ $l =~ ^[0-9]+$ ]]; then
if [[ "$mode" == "+" ]]; then
x=$(($x + $l))
else
x=$(($x * $l))
fi
echo $x
elif [[ $l == "QUIT" ]]; then
echo "quit v_pr 0"
killall tail
exit 0
else
echo "quit v_pr 1"
killall tail
exit 1
fi
done