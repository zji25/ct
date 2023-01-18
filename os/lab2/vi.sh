#!/bin/bash
max=0 pid=-1
for pr in /proc/[0-9]*; do
x=$(cat $pr/status | grep VmSize: | awk '{print $2}')
if [[ $x -ge $max ]]; then
max=$x pid=$pr
fi
done
pr=${pid#"/proc/"}
echo "pid="$pr "VmSize="$max
