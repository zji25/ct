#!/bin/bash
outp="full.log"
if [ -e "$outp" ];
then echo "full.log already exists"
exit 1
fi
inp="/var/log/anaconda/X.log"
touch "$outp"
while IFS= read -r line
do
if [[ "$line" == *"(WW)"* ]];
then echo "${line/(WW)/Warning:}" >> "$outp"
fi
done < "$inp"
while IFS= read -r line
do
if [[ "$line" == *"(II)"* ]];
then echo "${line/(II)/Information:}" >> "$outp"
fi
done < "$inp"
cat "$outp"
