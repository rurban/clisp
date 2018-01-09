#!/bin/sh
# Translate the assembler syntax of i386 assembler programs
# Usage: asm-i386.sh [-no-C] < gas-asm-file > portable-asm-file
# The portable-asm-file has to be
#   1. preprocessed,
#   2. grep -v '^ *#line' | grep -v '^#'
#   3. sed -e 's,% ,%,g' -e 's,\. ,.,g' -e 's,@ ,@,g' -e 's,//.*$,,' -e 's/##//g'
# Warning! All comments are stripped.

# Copyright (C) 1997-2018 Bruno Haible <bruno@clisp.org>
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

tmpscript01=sed$$tmp01
tmpscript02=sed$$tmp02
tmpscript03=sed$$tmp03
tmpscript04=sed$$tmp04
tmpscript05=sed$$tmp05
tmpscript06=sed$$tmp06
tmpscript07=sed$$tmp07
tmpscript08=sed$$tmp08
tmpscript09=sed$$tmp09
tmpscript10=sed$$tmp10
tmpremove='rm -f $tmpscript01 $tmpscript02 $tmpscript03 $tmpscript04 $tmpscript05 $tmpscript06 $tmpscript07 $tmpscript08 $tmpscript09 $tmpscript10'
trap "$tmpremove" 1 2 15

cat > $tmpscript01 << \EOF
# ----------- Strip comments
s,# .*,,
s,[ 	][ 	]*$,,
EOF

cat > $tmpscript02 << \EOF
# ----------- Remove #APP/#NO_APP lines and gcc self-identification, add a blank line at the end
/^#APP$/d
/^#NO_APP$/d
/gcc2_compiled/d
/gnu_compiled_c/d
/\.ident/d
EOF

cat > $tmpscript03 << \EOF
# ----------- Global symbols depends on ASM_UNDERSCORE
s/^\([A-Za-z0-9_]\+\)/C(\1)/
s/\.L\([A-Za-z0-9_]\+\)/L(\1)/
# ----------- Massage the beginning of functions
/\.type/{
s/\.type[ 	]\([A-Za-z0-9_]*\), *@function/DECLARE_FUNCTION(\1)/
}
# ----------- Massage the end of functions
s/\.size[ 	]\([A-Za-z0-9_]*\),\(.*\)/FUNEND(\1,\2)/
# ----------- Introduce conditionals for function references in PIC code
# Note: This is hairy. It assumes that the register clobbered with
# the 'addl $_GLOBAL_OFFSET_TABLE_ ...' instruction is the same as
# the register that the @GOTOFF instructions reference.
/^	addl	\$_GLOBAL_OFFSET_TABLE_,/{
x
s/.*/.LgotGOT/
x
s/^	addl	\$_GLOBAL_OFFSET_TABLE_, *\(%.*\)/#ifdef __ELF__\
	addl	$_GLOBAL_OFFSET_TABLE_,\1\
#else\
.LgotGOT:\
#endif/
}
/^	addl	\$_GLOBAL_OFFSET_TABLE_+\[\.-\(.*\)\],/{
h
s/^	addl	\$_GLOBAL_OFFSET_TABLE_+\[\.-\(.*\)\],.*/\1/
x
s/^	addl	\$_GLOBAL_OFFSET_TABLE_+\[\.-\(.*\)\], *\(%.*\)/#ifdef __ELF__\
	addl	\$_GLOBAL_OFFSET_TABLE_+[.-\1],\2\
#endif/
}
/@GOTOFF(/{
G
s/^	leal	\([A-Za-z0-9_]\+\)@GOTOFF\(.*\)\
\(.*\)/#ifdef __ELF__\
	leal	\1@GOTOFF\2\
#else\
	leal	C(\1)-\3\2\
#endif/
}
EOF

cat > $tmpscript04 << \EOF
# ----------- Introduce macro syntax for operands
s/\([-+0-9A-Z_]\+\)[(]%\(e..\)[)]/MEM_DISP(\2,\1)/g
s/[(]%\(e..\)[)]/MEM(\1)/g
s/\([-+0-9A-Z_]\+\)[(],%\(e..\),\([0-9]*\)[)]/MEM_DISP_SHINDEX0(\1,\2,\3)/g
s/\([-+0-9A-Z_]\+\)[(]%\(e..\),%\(e..\),\([0-9]*\)[)]/MEM_DISP_SHINDEX(\2,\1,\3,\4)/g
s/[(]%\(e..\),%\(e..\),\([0-9]*\)[)]/MEM_SHINDEX(\1,\2,\3)/g
s/[(]%\(e..\),%\(e..\)[)]/MEM_INDEX(\1,\2)/g
EOF

cat > $tmpscript05 << \EOF
# ----------- Introduce macro syntax for instructions
s/\(push\|pop\|mul\|div\|not\|neg\|inc\|dec\|fld\|fstp\)\(.\)\([ 	]\+\)\(.*\)$/INSN1(\1,\2	,\4)/
s/\(call\|jmp\|jc\|jnc\|je\|jne\|jz\|jnz\|ja\|jae\|jb\|jbe\|jl\|jge\|js\|jns\)\([ 	]\+\)\(.*\)$/INSN1(\1,_	,\3)/
s/\(movs\|movz\)\(.\)l\([ 	]\+\)\(.*\)$/INSN2MOVXL(\1,\2,\4)/
s/\(mov\|add\|sub\|adc\|sbb\|xor\|test\|cmp\|rcl\|rcr\|and\|or\|sar\|shr\|shl\|lea\)\(.\)\([ 	]\+\)\(.*\)$/INSN2(\1,\2	,\4)/
s/\(shld\|shrd\)\(.\)\([ 	]\+\)%cl,[ 	]*\(.*\)$/INSN2SHCL(\1,\2	,\4)/
s/rep[ 	];/REP/
s/repz[ 	];/REPZ/
EOF

cat > $tmpscript06 << \EOF
# ----------- Add size prefixes to memory references
s/\([(]f[^(,]*,s.*\), *MEM/\1,X4 MEM/g
s/\([(]f[^(,]*,l.*\), *MEM/\1,X8 MEM/g
s/\([(][^(,]*,b.*\), *MEM/\1,X1 MEM/g
s/\([(][^(,]*,w.*\), *MEM/\1,X2 MEM/g
s/\([(][^(,]*,l.*\), *MEM/\1,X4 MEM/g
EOF

cat > $tmpscript07 << \EOF
# ----------- Introduce macro syntax for register names
s/%\(e..\)/R(\1)/g
s/%\(..\)/R(\1)/g
s/\$\([-0-9]*\)/NUM(\1)/g
EOF

cat > $tmpscript08 << \EOF
# ----------- Treat table jumps (hairy)
# (Needed because the MSVC inline assembler does not have pseudo-ops.
# Note that we transform a table of absolute addresses with 4 bytes
# per entry into a table of absolute addresses with 8 bytes per entry.)
s/^	\.long \(.*\)$/#ifdef _MSC_VER\
	nop\
	nop\
	push \1\
#else\
	.long \1\
#endif/
s/^	\(INSN1[(]jmp,_[^,]*,\)\*MEM_DISP_SHINDEX0[(]\([^,)]*\),\([^,)]*\),4[)][)]$/#ifdef _MSC_VER\
	INSN2(lea,l	,MEM_DISP_SHINDEX0(\2+8,\3,8),R(\3))\
	INSN2(mov,l	,X4 MEM_DISP(\3,-4),R(\3))\
	INSN1(jmp,_	,INDIR(R(\3)))\
#else\
	\1INDIR(MEM_DISP_SHINDEX0(\2,\3,4)))\
#endif/
EOF

cat > $tmpscript09 << \EOF
# ----------- Treat indirect calls
s/\(INSN1[(]\(call\|jmp\),_[^,]*,\)\*\(\(R\)[(][^)]*[)]\)[)]$/\1INDIR(\3))/
s/\(INSN1[(]\(call\|jmp\),_[^,]*,\)\*\(\(MEM\|MEM_DISP\|C\)[(][^)]*[)]\)[)]$/\1INDIR(X4 \3))/
EOF

cat > $tmpscript10 << \EOF
# ----------- Introduce macro syntax for assembler pseudo-ops
/\.file\([ 	]\+\)/d
s/\.text/TEXT()/
s/^\([^#]*\)\.align \(.*\)/\1ALIGN(\2)/
s/\.p2align \([^,]*\),,\(.*\)/P2ALIGN(\1,\2)/
s/\.globl\( \+\)\(.*\)$/GLOBL(C(\2))/
# ----------- Declare global symbols as functions (we have no variables)
s/^C(\([A-Za-z0-9_]*\)):/FUNBEGIN(\1)/
EOF

sed -f $tmpscript01 | \
sed -f $tmpscript02 | \
(cat - ; echo) | \
(if [ $# = 1 -a "x$1" = "x-no-C" ] ; then cat - ; else sed -f $tmpscript03 ; fi) | \
sed -f $tmpscript04 | \
sed -f $tmpscript05 | \
sed -f $tmpscript06 | \
sed -f $tmpscript07 | \
sed -f $tmpscript08 | \
sed -f $tmpscript09 | \
sed -f $tmpscript10

eval "$tmpremove"

