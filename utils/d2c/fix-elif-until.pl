#!/usr/bin/perl -w

use strict;

$/ = undef;

my $text = <>;
$text =~ s/^\s*\#\s*define\s+(until|elif|loop)\b.*$//gm;
$text =~ s/(\s+)elif\b/$1else if/g;
$text =~ s/\bloop\b/while (1)/g;

OUTER:
while ($text =~ m/until\s*/g) {
  my $pos    = pos $text;
  my $end    = length $text;
  my $parens = 0;

  for (my $i = $pos; $i < $end; $i++) {
    my $c = substr($text, $i, 1);
    if ($c eq '(') { $parens++; }
    if ($c eq ')') { $parens--; }
    if ($parens == 0) {
      substr($text, $i, 0, ")");
      substr($text, $pos, 0, "(!");
      $text =~ s/until(\s*)/while$1/;
      pos $text = $i + 3;
      next OUTER;
    }
  }
}

print $text;

__END__

while (<>) {
  #/^\s*\#\s*define\s+(local|global|var|until|elif)\b/ and next;
  /^\s*\#\s*define\s+(until|elif)\b/ and next;
  s/(\s+)elif\b/$1else if/g;
  #s/\blocal\b/static/g;
  #s/\bglobal\b//g;
  #s/\bvar\b//g;
  if (/until\s*\(/) {
    fix_until_line($_);
  } else {
    print;
  }
}

sub fix_untils {
  my ($line) = @_;
 OUTER:
  while ($line =~ m/until\s*/g) {
    my $pos = pos $line;
    my $end = length $line;
    my $parens = 0;
    for (my $i = $pos; $i < $end; $i++) {
      my $c = substr($line, $i, 1);
      if ($c eq '(') { $parens++; }
      if ($c eq ')') { $parens--; }
      if ($parens == 0) {
        substr($line, $i, 0, ")");
        substr($line, $pos, 0, "(!");
        $line =~ s/until(\s*)/while$1/;
        pos $line = $i + 3;
        next OUTER;
      }
    }
  }
  print $line;
}


__END__
