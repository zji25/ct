#!/usr/bin/perl
while (<>) {
    s/\b[Aa]+\b/argh/;
    print;
}