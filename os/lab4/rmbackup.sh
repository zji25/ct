#!/bin/bash
for f in /home/Backup-*; do
rm -rf $f
done
> /home/user/backup-report