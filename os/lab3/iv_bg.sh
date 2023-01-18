#!/bin/bash
x=0
while true; do
((++x))
if [ $x -eq 100 ]; then
x=0
fi
done