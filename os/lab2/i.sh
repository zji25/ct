#!/bin/bash
> one
ps -A u | grep -E ^root.*$ > one
echo "$(cat one | wc -l)"$'\n'"$(cat one)" > one
