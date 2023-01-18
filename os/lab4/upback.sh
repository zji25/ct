#!/bin/bash
cd /home
if [[ ! -d /home/user/restore ]]; then
mkdir /home/user/restore
fi
lastbp=$(ls /home | grep "^Backup-" | sort -n | tail -1)
if [[ -z $lastbp ]]; then
echo "theres no backup"
exit 1
fi
echo "latest backup" $lastbp
for f in $lastbp/*; do
date=$(echo $f | awk -F"/" '{print $2}' | awk -F"-" '{print $2 $3 $4}')
if [[ -n "$date" ]] && [[ ${#date} == 8 ]]; then
continue
fi
cp $f "/home/user/restore"
done
ls /home/user/restore