#!/bin/perl -w

# Quick-n-dirty conversion from utf8 to latin-1 encoding. Doesn't
# handle chars above \u07ff and complains about chars above \u00ff as
# they can't be represented in latin-1.

use strict;

$/ = undef;

while (<>) {
  my @chars = /./msg;
  for (my $i = 0; $i <= $#chars; $i++) {
    my $c = $chars[$i];
    if ((ord($c) & 0xe0) == 0xc0) {
      my $nc = $chars[++$i];
      if ((ord($nc) & 0xc0) != 0x80) { die "*** Bad second char: $nc\n"; }
      my $unicode = ((ord($c) & 0x1f) << 6) + (ord($nc) & 0x3f);
      if ($unicode > 0xff) {
        die sprintf("*** $ARGV:$. Big char (%x)\n", $unicode);
      } else {
        $c = chr($unicode);
      }
    } elsif (ord($c) > 0xc0) {
      die "*** $ARGV:$. Really big char.\n";
    }
    print $c;
  }
} continue {
  close ARGV if eof;
}

__END__

