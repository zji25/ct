#!/bin/bash
echo $$ > .pid
x=1
mode="+"
add()
{
mode="+"
}
trap 'add' USR1
mul()
{
mode="*"
}
trap 'mul' USR2
term()
{
exit 0
}
trap 'term' SIGTERM
while true; do
if [[ "$mode" == "+" ]]; then
x=$(($x+2))
elif [[ "$mode" == "*" ]]; then
x=$(($x*2))
fi
echo $x
sleep 1
done