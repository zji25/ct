#!/bin/bash
at -m now+2minutes -f i.sh
tail -n 0 -f ~/report &