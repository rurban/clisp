#!/bin/sh
# Translate the assembler syntax of i386 assembler programs
# Usage: asmsyntax [-no-C] < gas-asm-file > all-asm-file
# Warning! All comments are stripped.

sed -e '# ----------- Strip comments' \
    -e 's,# .*,,' \
    -e 's,[ 	][ 	]*$,,' \
| \
sed -e '# ----------- Remove #APP/#NO_APP lines, add a blank line at the end' \
    -e '/^#APP$/d' \
    -e '/^#NO_APP$/d' \
    -e '/gcc2_compiled/d' \
    -e '/gnu_compiled_c/d' \
| \
(cat - ; echo) \
| \
(if [ $# = 1 -a "x$1" = "x-no-C" ] ; then \
  cat - \
; else \
sed -e '# ----------- Global symbols depends on ASM_UNDERSCORE' \
    -e 's/_\([A-Za-z0-9_:]*\)/C(\1)/' \
; fi) \
| \
sed -e '# ----------- Introduce macro syntax for operands' \
    -e 's/\([-+0-9A-Z_]\+\)[(]%\(e..\)[)]/MEM_DISP(\2,\1)/g' \
    -e 's/[(]%\(e..\)[)]/MEM(\1)/g' \
    -e 's/\([-+0-9A-Z_]\+\)[(],%\(e..\),\([0-9]*\)[)]/MEM_DISP_SHINDEX0(\1,\2,\3)/g' \
    -e 's/\([-+0-9A-Z_]\+\)[(]%\(e..\),%\(e..\),\([0-9]*\)[)]/MEM_DISP_SHINDEX(\2,\1,\3,\4)/g' \
    -e 's/[(]%\(e..\),%\(e..\),\([0-9]*\)[)]/MEM_SHINDEX(\1,\2,\3)/g' \
    -e 's/[(]%\(e..\),%\(e..\)[)]/MEM_INDEX(\1,\2)/g' \
| \
sed -e '# ----------- Introduce macro syntax for instructions' \
    -e 's/\(push\|pop\|mul\|div\|not\|neg\|inc\|dec\|fld\|fstp\)\(.\)\( \+\)\(.*\)$/INSN1(\1,\2	,\4)/' \
    -e 's/\(call\|jmp\|jc\|jnc\|je\|jne\|jz\|jnz\|ja\|jae\|jb\|jbe\|jl\|jge\|js\|jns\)\( \+\)\(.*\)$/INSN1(\1,_	,\3)/' \
    -e 's/\(movs\|movz\)\(.\)l\( \+\)\(.*\)$/INSN2MOVX(\1,\2	,\4)/' \
    -e 's/\(mov\|add\|sub\|adc\|sbb\|xor\|test\|cmp\|rcl\|rcr\|and\|or\|sar\|shr\|shl\|lea\)\(.\)\( \+\)\(.*\)$/INSN2(\1,\2	,\4)/' \
    -e 's/\(shld\|shrd\)\(.\)\( \+\)shcl\( \+\)\(.*\)$/INSN2SHCL(\1,\2	,\5)/' \
    -e 's/rep ;/REP/' \
    -e 's/repz ;/REPZ/' \
| \
sed -e '# ----------- Add size prefixes to memory references' \
    -e 's/\([(]f[^(,]*,s.*\),MEM/\1,X4 MEM/g' \
    -e 's/\([(]f[^(,]*,l.*\),MEM/\1,X8 MEM/g' \
    -e 's/\([(][^(,]*,b.*\),MEM/\1,X1 MEM/g' \
    -e 's/\([(][^(,]*,w.*\),MEM/\1,X2 MEM/g' \
    -e 's/\([(][^(,]*,l.*\),MEM/\1,X4 MEM/g' \
| \
sed -e '# ----------- Introduce macro syntax for register names' \
    -e 's/%\(e..\)/R(\1)/g' \
    -e 's/%\(..\)/R(\1)/g' \
    -e 's/\$\([-0-9]*\)/NUM(\1)/g' \
| \
sed -e '# ----------- Treat table jumps (hairy)' \
    -e '# (Needed because the MSVC inline assembler does not have pseudo-ops.' \
    -e '# Note that we transform a table of absolute addresses with 4 bytes' \
    -e '# per entry into a table of absolute addresses with 8 bytes per entry.)' \
    -e 's/^	\.long \(.*\)$/#ifdef _MSC_VER\' \
    -e '	nop\' \
    -e '	nop\' \
    -e '	push \1\' \
    -e '#else\' \
    -e '	.long \1\' \
    -e '#endif/' \
    -e 's/^	\(INSN1[(]jmp,_[^,]*,\)\*MEM_DISP_SHINDEX0[(]\([^,)]*\),\([^,)]*\),4[)][)]$/#ifdef _MSC_VER\' \
    -e '	INSN2(lea,l	,MEM_DISP_SHINDEX0(\2+8,\3,8),R(\3))\' \
    -e '	INSN2(mov,l	,X4 MEM_DISP(\3,-4),R(\3))\' \
    -e '	INSN1(jmp,_	,INDIR(R(\3)))\' \
    -e '#else\' \
    -e '	\1INDIR(MEM_DISP_SHINDEX0(\2,\3,4)))\' \
    -e '#endif/' \
| \
sed -e '# ----------- Treat indirect calls' \
    -e 's/\(INSN1[(]\(call\|jmp\),_[^,]*,\)\*\(R[(][^)]*[)]\)[)]$/\1INDIR(\3))/' \
| \
sed -e '# ----------- Introduce macro syntax for assembler pseudo-ops' \
    -e '/\.file\([ 	]\+\)/d' \
    -e 's/\.text/TEXT()/' \
    -e 's/^\([^#]*\)\.align \(.*\)/\1ALIGN(\2)/' \
    -e 's/\.globl\( \+\)\(.*\)$/GLOBL(\2)/' \
    -e 's/^C(\([A-Za-z0-9_]*\):)/FUNBEGIN(\1)/' \
    -e '# The next 5 lines add FUNEND() after each ret followed by an empty line' \
    -e '/[ 	]ret *$/{' \
    -e 'n' \
    -e '/^$/s/^$/FUNEND()\' \
    -e '/' \
    -e '}' \
| \
(if [ $# = 1 -a "x$1" = "x-no-C" ] ; then \
  cat - \
; else \
sed -e '# ----------- Declare global symbols as functions (we have no variables)' \
    -e 's/GLOBL(C(\([A-Za-z0-9_]*\)))$/GLOBL(C(\1))\' \
    -e '	DECLARE_FUNCTION(\1)/' \
; fi)

