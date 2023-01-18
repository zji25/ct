#!/bin/bash
> two
ps aux | grep "/sbin/" | awk '{print $2}' >> two
