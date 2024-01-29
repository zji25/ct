#!/usr/bin/perl
my @lines = ();
my $line;
while(my $line = <>) {
    chomp $line;
    $line =~ s/<[^>]*>//g;
    push(@lines, $line);    
}
$f = 1;
$s = 0;
foreach my $line (@lines) {
    if ($line =~ /^\s*$/) {
        if (not $f) {
            $s = $s + 1;
        }
    }
    else {
        $f = 0;
        if ($s >= 1) {
            print("\n");
        }
    $s = 0;
    $line =~ s/^\s+|\s+$//g;
    $line =~ s/(\s+)/ /g;
    print("$line\n");
    }
}