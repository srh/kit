#!/usr/bin/perl

# Usage:
#   $ valgrind --tool=callgrind ./a.out ...
#   $ objdump -t a.out > foo
#   $ cleanup.pl foo callgrind.out.1234 > outfile

use strict;

my $tabfile = $ARGV[0];
my $cafile = $ARGV[1];

my %h;

open(my $TB, '<', $tabfile) or die;
while (<$TB>) {
    if (/^([0-9a-fA-F]{8}) \w.* {8}(\w.*)\n/) {
        my $s = $1;
        my $fn = $2;
        $fn =~ s/[^a-zA-Z_0-9\[\],\$]//g;
        $h{$s} = $fn;
    }
}
close($TB) or die;

open(my $CB, '<', $cafile) or die;
while (<$CB>) {
    if (m/0x([0-9a-fA-F]{8})/) {
        if (exists $h{$1}) {
            my $repl = $h{$1};
            s/0x$1/$repl/;
        }
    }
    print;
}
close($CB) or die;
