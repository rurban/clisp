REM copies some DOS specific files into SRC
copy dos\forall.bat src
copy dos\*.in src
copy dos\makefile src
copy src\readline\makefile.dos src\readline\makefile
copy dos\rlobjects src\readline\objects
copy src\readline\examples\makefile.dos src\readline\examples\makefile
copy src\newreadline\makefile.dos src\newreadline\makefile
copy dos\rlobjects src\newreadline\objects
copy dos\rlconfig.h src\newreadline\config.h
copy src\gettext\intl\makefile.dos src\gettext\intl\makefile
copy dos\gtobjects src\gettext\intl\objects
copy dos\gtconfig.h src\gettext\config.h
