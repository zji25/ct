#!/usr/bin/perl
while (<>) {
    print if /\b(\w+)\g1\b/;
}