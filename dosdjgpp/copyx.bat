REM copies some DOS or DJGPP specific files into SRC
copy dos\forall.bat src
copy dos\*.in src
copy dosdjgpp\makefile src
copy src\readline\makefile.go32 src\readline\makefile
copy dos\rlobjects src\readline\objects
copy dosdjgpp\makefile.rle src\readline\examples\makefile
copy src\newreadline\makefile.go32 src\newreadline\makefile
copy dos\rlobjects src\newreadline\objects
copy dosdjgpp\rlconfig.h src\newreadline\config.h
