#!/bin/bash
while true; do
read line;
if [[ "$line" == "+" ]]; then
kill -USR1 $(cat .pid)
elif [[ "$line" == "*" ]]; then
kill -USR2 $(cat .pid)
elif [[ "$line" == "TERM" ]]; then
kill -SIGTERM $(cat .pid)
exit 0
fi
done