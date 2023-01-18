#!/bin/bash
man bash | tr -d '[[:punct:]]' | tr A-Z a-z | tr ' ' '\n' | awk 'length>3' | sort | uniq -c | sort -nr | head -3 | awk '{print $2}'
