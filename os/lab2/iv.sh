#!/bin/bash
> four
for prr in /proc/[0-9]*; do
ppid=$(cat $prr/status | grep PPid: | awk '{print $2}')
sum_exec_rt=$(cat $prr/sched | grep se.sum_exec_runtime | awk '{print $3}')
nr_switches=$(cat $prr/sched | grep nr_switches | awk '{print $3}')
art=$(awk '{print $1/$2}' <<< "${sum_exec_rt} ${nr_switches}")
pr=${prr#"/proc/"}
echo ProcessID = $pr : Parent_ProcessID = $ppid : Average_Running_Time = $art >> four
done
sort -n -k7 -o four four
