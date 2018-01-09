#!/bin/sh
# Translate the assembler syntax of m68k assembler programs
# Usage: asm-m68k.sh < sunos-asm-file > portable-asm-file
# The portable-asm-file has to be
#   1. preprocessed,
#   2. grep -v '^ *#line' | grep -v '^#'
#   3. sed -e 's,% ,%,g' -e 's,//.*$,,'

# Copyright (C) 1999-2017 Bruno Haible <bruno@clisp.org>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

syntax="$1"

case "$syntax" in
  "") echo "missing syntax" 1>&2; exit 1;;
  mit | motorola) ;;
  *) echo "invalid syntax: $syntax" 1>&2; exit 1;;
esac

tmpscript1=sed$$tmp1
tmpscript2=sed$$tmp2
tmpremove='rm -f $tmpscript1 $tmpscript2'
trap "$tmpremove" 1 2 15

cat > $tmpscript1 << \EOF
# ----------- Remove #APP/#NO_APP lines
/^#APP$/d
/^#NO_APP$/d
# ----------- Remove gcc self-identification
/gcc2_compiled/d
/gnu_compiled_c/d
/\.ident/d
EOF

if test $syntax = mit; then
cat > $tmpscript2 << \EOF
# ----------- Global symbols depends on ASM_UNDERSCORE
s/^L\([A-Za-z0-9_]\+\)/L(\1)/
s/ L\([A-Za-z0-9_]\+\)/ L(\1)/
# ----------- Prefix register names with $, to be turned into % later
s/,/, /g
s/\([^A-Za-z0-9_]\)\([ad][0-7]\|sp\|fp[0-7]\|pc\)\([^A-Za-z0-9_]\)/\1$\2\3/g
s/\([^A-Za-z0-9_]\)\([ad][0-7]\|sp\|fp[0-7]\|pc\)$/\1$\2/g
s/, /,/g
# ----------- Declare global symbols as functions (we have no variables)
s/\.globl[ 	]_\([A-Za-z0-9_]*\)$/.globl _\1\
	DECLARE_FUNCTION(\1)/
# ----------- Global symbols depends on ASM_UNDERSCORE
s/_\([A-Za-z0-9_]*\)/C(\1)/
s/^C(\([A-Za-z0-9_]*\)):/FUNBEGIN(\1)/
/FUNBEGIN(/{
h
}
${
p
s/.*//
x
s/FUNBEGIN/FUNEND/
}
EOF
fi

if test $syntax = motorola; then
cat > $tmpscript2 << \EOF
# ----------- Global symbols depends on ASM_UNDERSCORE
s/^\([A-Za-z0-9_]\+\)/C(\1)/
s/\.L\([A-Za-z0-9_]\+\)/L(\1)/
s/\.globl[ 	]\([A-Za-z0-9_]*\)/.globl C(\1)/
# ----------- Introduce macro syntax for assembler pseudo-ops
/\.file\([ 	]\+\)/d
s/^C(\([A-Za-z0-9_]*\)):/FUNBEGIN(\1)/
# ----------- Massage the beginning of functions
/\.type/{
s/\.type[ 	]\([A-Za-z0-9_]*\), *@function/DECLARE_FUNCTION(\1)/
}
# ----------- Massage the end of functions
s/\.size[ 	]\([A-Za-z0-9_]*\),\(.*\)/FUNEND(\1)/
EOF
fi

sed -f $tmpscript1 | \
sed -f $tmpscript2

eval "$tmpremove"
