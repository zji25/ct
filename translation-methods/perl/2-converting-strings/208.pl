#!/usr/bin/perl
while (<>) {
    s/(\d+)0\b/$1/g;
    print;
}