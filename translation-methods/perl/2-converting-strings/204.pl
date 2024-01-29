#!/usr/bin/perl
while (<>) {
    s/(\W*)(\w+)(\W+)(\w+)(.*)/$1$4$3$2$5/;
    print;
}