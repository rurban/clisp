#!/bin/perl -w

# Transliterate 8-bit latin-1 chars used in the .d files to 7-bit
# ascii equivalents.

use strict;


# ü (FC) (1078 instances) -> ue
# ä (E4) (766 instances)  -> ae
# ö (F6) (348 instances)  -> oe
# Ü (DC) (224 instances)  -> Ue
# ß (DF) (92 instances)   -> ss
# ¿ (BF) (19 instances)   -> ?
# é (E9) (5 instances)    -> e
# µ (B5) (2 instances)    -> micro
# Ö (D6) (2 instances)    -> Oe
# è (E8) (2 instances)    -> e
# « (AB) (1 instance)     -> "
# » (BB) (1 instance)     -> "
# û (FB) (1 instance)     -> u

while (<>) {
  s/ü/ue/g;
  s/ä/ae/g;
  s/ö/oe/g;
  s/Ü/Ue/g;
  s/ß/ss/g;
  s/¿/?/g;
  s/é/e/g;
  s/µ/micro/g;
  s/Ö/Oe/g;
  s/è/e/g;
  s/\«/\"/g;
  s/\»/\"/g;
  s/û/u/g;
  print;
}

__END__
