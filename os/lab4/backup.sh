#!/bin/bash
report="/home/user/backup-report"
atm=$(date +"%Y-%m-%d")
cd /home
lastbp=$(ls /home | grep "^Backup-" | sort -n | tail -1 | awk -F"-" '{print $2"-"$3"-"$4}')
cutoff=$(date +"%Y0%m0%d" -d '7 days ago')
zeros=0
if [[ ! -z $lastbp ]]; then
zeros=$(echo $lastbp | tr - 0)
fi
if [[ $zeros -le $cutoff ]] ; then
mkdir "/home/Backup-$atm"
cp -R /home/user/source/* "/home/Backup-$atm"
echo "Backup-$atm created at $(date +%F.%T)" >> $report
ls -R /home/Backup-$atm >> $report
echo "Backup-$atm created"
exit 0
fi
newfiles=""
modfiles=""
for ff in /home/user/source/*; do
f=$(echo $ff | awk -F"/" '{print $5}')
bf="/home/Backup-$lastbp/$f"
if [[ -f $bf ]]; then
ffsize=$(stat --printf="%s" $ff)
bfsize=$(stat --printf="%s" $bf)
if [[ $ffsize -ne $bfsize ]]; then
mv $bf $bf-$atm
cp $ff $bf
modfiles=$modfiles"\n"$bf
newfiles=$newfiles"\n"$bf"-"$atm
else
cp $ff "/home/Backup-$lastbp"
newfiles=$newfiles"\n"$ff
fi
fi
done
echo "Backup-$lastbp updated"
printf "Backup-$lastbp updated at $atm" >> $report
printf "\nnew files added:"$newfiles"\nfiles modified:"$modfiles"\n" >> $report
