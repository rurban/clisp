REM copies some OS/2 specific files into SRC
copy os2\forall.cmd src
copy dos\*.in src
copy os2\makefile src
copy src\readline\makefile.os2 src\readline\makefile
copy src\readline\examples\makefile.dos src\readline\examples\makefile
copy src\newreadline\makefile.os2 src\newreadline\makefile
copy dos\rlconfig.h src\newreadline\config.h
copy src\gettext\intl\makefile.dos src\gettext\intl\makefile
copy dos\gtobjects src\gettext\intl\objects
copy dos\gtconfig.h src\gettext\config.h
