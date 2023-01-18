#!/bin/bash
ps -axo pid,etimes | sort -k2 -n | head -1 | awk '{print $1}'
