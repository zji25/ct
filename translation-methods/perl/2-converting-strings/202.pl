#!/usr/bin/perl
while (<>) {
    s/\bhuman\b/computer/g;
    print;
}