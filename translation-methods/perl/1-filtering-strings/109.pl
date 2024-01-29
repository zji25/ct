#!/usr/bin/perl
while (<>) {
    print if /^\S(.*)\S$|^\S$|^$/;
}