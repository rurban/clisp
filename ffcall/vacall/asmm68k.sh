#!/bin/sh
# Translate the assembler syntax of m68k assembler programs
# Usage: asmm68k < sunos-asm-file > portable-asm-file
# The portable-asm-file has to be
#   1. preprocessed,
#   2. grep -v '^ *#line' | grep -v '^#'
#   3. sed -e 's,% ,%,g' -e 's,//.*$,,'
sed -e '# ----------- Remove #APP/#NO_APP lines' \
    -e '/^#APP$/d' \
    -e '/^#NO_APP$/d' \
    -e '# ----------- Remove gcc self-identification' \
    -e '/gcc2_compiled/d' \
    -e '/gnu_compiled_c/d' \
| \
sed -e '# ----------- Prefix register names with $, to be turned into % later' \
    -e 's/,/, /g' \
    -e 's/\([^A-Za-z0-9_]\)\([ad][0-7]\|sp\|fp[0-7]\)\([^A-Za-z0-9_]\)/\1$\2\3/g' \
    -e 's/\([^A-Za-z0-9_]\)\([ad][0-7]\|sp\|fp[0-7]\)$/\1$\2/g' \
    -e 's/, /,/g' \
    -e '# ----------- Declare global symbols as functions (we have no variables)' \
    -e 's/\.globl _\([A-Za-z0-9_]*\)$/.globl _\1\' \
    -e '	DECLARE_FUNCTION(\1)/' \
    -e '# ----------- Global symbols depends on ASM_UNDERSCORE' \
    -e 's/_\([A-Za-z0-9_:]*\)/C(\1)/'
