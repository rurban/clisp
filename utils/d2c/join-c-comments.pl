#!/usr/bin/perl -w

use strict;

my @line_buffer = ();
my $header_comment = undef;

while (<>) {
  m!^#line 1 ".*?\.d"$! and next;
  if (m!^\s*/\*.*\*/\s*$!) {
    push @line_buffer, $_;
  } else {
    dump_line_buffer();
    print;
  }
}
dump_line_buffer();

sub dump_line_buffer {
  if (@line_buffer) {
    if ($header_comment) {
      push @line_buffer, $header_comment;
      undef $header_comment;
    }
    if ($#line_buffer == 0) {
      print $line_buffer[0];
    } else {
      print "/*";
      for (my $i = 0; $i <= $#line_buffer; $i++) {
        my $line = $line_buffer[$i];
        my ($guts) = $line =~ m!/\*(.*?)\s*\*/!;
        print "$guts";
        if ($i < $#line_buffer) { print "\n"; }
      }
      print " */\n";
    }
    @line_buffer = ();
  }
}

__END__
