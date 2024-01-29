#!/usr/bin/perl
while (<>) {
    print if /(^|\W)\d+(\W|$)/;
}