#!/bin/bash
if [ -e "emails.lst" ];
then echo "emails.lst already exists"
exit 1
fi
grep -Eroha "[[:alnum:]]+@[[:alnum:]]+?\.[[:alpha:]]+" "/etc" > "emails.lst"
# -E for patterns as extended regex
# -r recursive search on directory
# -o prints only matching substrings on sep lines
# -h no prefixing filenames
# -a remove 'binary file matches' -- process binary files as text files
cat "emails.lst"
