#!/bin/bash
date=$(date +%Y.%m.%d.%T)
mkdir /home/test &&
{
echo "catalog test created successfully" > ~/report ;
touch /home/test/$date
} ;
ping -q -c 1 -W 1 www.net_nikogo.ru &>/dev/null
status=$(echo $?)
echo "status" $status
if [[ $status != 0 ]]; then
echo "host unavailable at " $date >> ~/report
fi