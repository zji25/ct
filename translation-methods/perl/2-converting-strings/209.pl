#!/usr/bin/perl
while (<>) {
    s/\([^)]*\)/\(\)/g;
    print;
}