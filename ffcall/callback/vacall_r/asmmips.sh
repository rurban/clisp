#!/bin/sh
# Translate the assembler syntax of mips assembler programs
# Usage: asmmips < irix-asm-file > portable-asm-file
# The portable-asm-file has to be
#   1. preprocessed,
#   2. grep -v '^ *#line' | grep -v '^#'
#   3. sed -e 's,% ,%,g' -e 's,//.*$,,'
sed -e '# ----------- Remove gcc self-identification' \
    -e '/gcc2_compiled/d' \
    -e '/gnu_compiled_c/d' \
| \
sed -e '# ----------- Remove comments, they would cause trouble in preprocessing' \
    -e 's,#.*$,,' \
    -e '# ----------- Declare global symbols as functions (we have no variables)' \
    -e 's/\.globl	\([A-Za-z0-9_]*\)$/.globl	\1\' \
    -e '	DECLARE_FUNCTION(\1)/'
