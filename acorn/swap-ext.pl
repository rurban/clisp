#!/usr/local/bin/perl
# swap extensions in the automatically generated makefile so that it can
# be used by AMU (Acorn Make Utility).
# 1.  .1 .c .d .h .i .o .s single letter extensions are swapped
# 2.  .dvi .fas .lsp .man .mem .txt .html .in .sed multi letter extensions are
#     swapped
# 3.  comment5 recognizes filenames ending in .d but not filenames beginning
#     with d.
#     change "$(COMMENT5) d.eval > TMPPIPE1" into "$(COMMENT5) eval.d TMPPIPE1"
# 4.  Undo 1. for comment5 commands not piping output, but generating file
#     directly. change "(COMMENT5) d.ansidecl"
#     into "(COMMENT5) ansidecl.d"
# 5.  Undo 1. for files generated from UnixLib linked utils, such as ansidecl
#     change "(.d|>) c.eval" into "(.d|>) eval.c"
# 6.  Undo 2. for arguments to clisp
# 7.  Undo 2. for arguments to clisp
# 8.  change "../" into "^."
# 9.  changed "README.*" into "README_*"

while (<>) {
   unless (/cdir/) {
      s/(\b\w+)\.([1cdhios])(\s)/$2\.$1$3/g;
      s/(\b[\w-]+)\.(dvi|fas|lsp|man|mem|txt|html|in|sed)(\s)/$2\.$1$3/g;
   }
   s/(\(COMMENT5\)) d\.(\w+) > /$1 $2\.d /g;
   s/(\(COMMENT5\)) d\.(\w+)/$1 $2\.d/g;
   s/(\.d|>) c\.(\w+)/$1 $2\.c/g;
   s/-c lsp\.(\w+)(\s)/-c $1\.lsp$2/g;
   s/-c stage\.lsp\.(\w+)(\s)/-c stage\.$1\.lsp$2/g;
   s/\.\.\//\^\./g;
   s/(README)\.(\w+)/$1_$2/g;
   print;
}
