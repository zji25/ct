#!/usr/bin/perl
while (<>) {
    print if /\([^\(\)]*\w+[^(\)]*\)/;
}