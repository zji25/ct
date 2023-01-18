#!/bin/bash
chmod +x iv_bg.sh
{
./iv_bg.sh &
./iv_bg.sh &
./iv_bg.sh &
} ;
pid1=$(pgrep "iv_bg.sh" -o)
pid3=$(pgrep "iv_bg.sh" -n)
cpu=$(ps -p $pid1 -o %cpu)
echo $cpu
renice -n 19 -p $pid1
sleep 5
cpu=$(ps -p $pid1 -o %cpu)
echo $cpu
kill $pid3
cpu=$(ps -p $pid1 -o %cpu)
echo $cpu
kill $pid1
kill $(pgrep "iv_bg.sh" -o)