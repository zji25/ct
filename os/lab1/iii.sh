#!/bin/bash
select action in nano vi links exit;
do 
case $REPLY in
1)
nano;;
2)
vi;;
3)
links;;
4)
break;;
esac
done
