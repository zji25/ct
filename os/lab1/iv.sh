#!/bin/bash
current=$(pwd)
if [ $current == "/home" ];
then echo "current directory: " $current
exit 0
else
echo "error: pls run this script from /home"
exit 1
fi
