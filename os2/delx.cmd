REM deletes extra (unused) files of CLISP
del configure
cd src
del configure.*
del makemake.*
del unixconf.*
del machine.*
del ari68000.*
del ari68020.*
del arimips.*
del arisparc.*
del arihppa.*
del arivax.*
del unix.*
del cc_sparc.*
del sp68000.*
del spmips.*
del spsparc.*
del alglobal.*
del dbxtypes.*
del socket.*
del readline\configure.*
del readline\examples\configure.*
del newreadline\configure.*
del newreadline\examples\configure.*
cd ..
rm -r cygwin32
rm -r modules
rm -r nextapp
rm -r win32gcc
rm -r win32msvc
