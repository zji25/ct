#!/usr/bin/perl
while (<>) {
    print if /(.*)(cat)(.*)(cat)(.*)/;
}