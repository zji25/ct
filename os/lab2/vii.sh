#!/bin/bash
> seven
> seven2
> seven3
for pr in /proc/[0-9]*; do
x=$(cat $pr/io | grep rchar: | awk '{print $2}')
echo $pr $x >> seven
done
echo "first loop done"
#sleep 10
echo "woke up"
for pr in /proc/[0-9]*; do
xx=$(cat $pr/io | grep rchar: | awk '{print $2}')
yy=$(grep -E "$pr " seven | awk '{print $2}')
if [[ ! -z "$yy" ]]; then
zz=$(echo "$xx - $yy" | bc)
echo $pr ":" $zz >> seven2
fi
done
echo "second loop done"
sort -k3 -n -r seven2 | head -3 >> seven3
while read line; do
l=${line#"/proc/"}
ll=$(echo $l | awk '{print $1}')
lll=$(ps -A u | grep $ll)
llll=$(echo $lll | awk '{print $11}')
echo "pid =" $l ": cmd =" $llll
done < seven3
