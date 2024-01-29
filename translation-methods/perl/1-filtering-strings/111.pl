#!/usr/bin/perl
while (<>) {
    print if /\b0+\b|\b0*(1(01*0)*10*)+\b/;
}