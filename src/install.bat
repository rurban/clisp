@echo off

echo +==========================================================+
echo | this will install CLISP on your system,                  |
echo | associating file types FAS, MEM and LISP with CLISP.     |
echo | it will also create a shortcut to CLISP on your desktop. |
echo |               press C-c to abort                         |
echo +==========================================================+
pause

if exist src\install.lisp goto installsrc
if exist install.lisp goto install
goto notfound
:installsrc
full/lisp.exe -B . -M full/lispinit.mem -norc -C src\install.lisp
goto exit
:install
full/lisp.exe -B . -M full/lispinit.mem -norc -C install.lisp
goto exit
:notfound
echo Sorry, install.lisp not found, cannot install
:exit
pause
