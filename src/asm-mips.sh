#!/bin/sh
# Translate the assembler syntax of mips assembler programs
# Usage: asm-mips.sh < irix-asm-file > portable-asm-file
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

tmpscript1=sed$$tmp1
tmpscript2=sed$$tmp2
tmpremove='rm -f $tmpscript1 $tmpscript2'
trap "$tmpremove" 1 2 15

cat > $tmpscript1 << \EOF
# ----------- Remove gcc self-identification
/gcc2_compiled/d
/gnu_compiled_c/d
/\.ident/d
EOF

cat > $tmpscript2 << \EOF
# ----------- Remove comments, they would cause trouble in preprocessing
s,#.*$,,
# ----------- Remove assembler pseudo-ops that the IRIX assembler does not understand
/\.section/d
/\.previous/d
/\.abicalls/d
# ----------- Massage the beginning of functions
/\.type/{
s/\.type[ 	]\+\([A-Za-z0-9_]*\), *@function/DECLARE_FUNCTION(\1)/
}
EOF

sed -f $tmpscript1 | \
sed -f $tmpscript2

eval "$tmpremove"
