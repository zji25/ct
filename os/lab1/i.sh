#!/bin/bash
if [[ $1 -le $3 && $2 -le $3 ]];
then echo $3
elif [[ $1 -le $2 && $3 -le $2 ]];
then echo $2
else
echo $1
fi