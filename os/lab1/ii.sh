#!/bin/bash
res=""
while read -r line;
do
if [ $line == "q" ];
then break
else
res+="$line"
fi
done
echo "$res"
