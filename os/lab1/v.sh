#!/bin/bash
if [ -e "info.log" ];
then echo "info.log already exists"
exit 1
else
touch "info.log"
input="/var/log/anaconda/syslog"
while IFS= read -r line
do
sec=$(echo $line | awk '{print $2}')
if [ "$sec" == "INFO" ]; 
then echo "$line" >> "info.log"
fi
done < "$input"
fi
