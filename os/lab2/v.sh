#!/bin/bash
> five
zxc="Average_Running_Children_of_ParentID="
ppid="-1" counter=0 sum=0
while read line; do
x=$((echo $line) | awk '{print $7}')
xx=$((echo $line) | awk '{print $11}')
if [[ $ppid -eq $x ]] ; then
let counter+=1
sum=$(echo "$sum + $xx" | bc)
else
if [[ $ppid -ne "-1" ]]; then
awk '{print $4 $1 " is " $2/$3}' <<< "${ppid} ${sum} ${counter} ${zxc}" >> five
echo >> five
fi
ppid=$x sum=$xx counter=1
fi
echo $line >> five
done < four
awk '{print $4 $1 " is " $2/$3}' <<< "${ppid} ${sum} ${counter} ${zxc}" >> five
