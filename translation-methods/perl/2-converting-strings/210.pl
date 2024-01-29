#!/usr/bin/perl
while (<>) {
    s/(a(.*?)a){3}/bad/g;
    print;
}
