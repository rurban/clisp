Esto es CLISP, una implementación de Common Lisp.


¿ Qué es LISP ?
---------------

LISP es un lenguaje de programación inventado por J. McCarthy en
1959. Aunque ha habido muchos dialectos de él, actualmente se ha
estandarizado y difundido ampliamente gracias al estandar industrial
COMMON LISP. Hay aplicaciones en los dominios del procesamiento del
conocimiento simbólico (IA), cálculo numérico (MACLISP generaba código
tan bueno como el de FORTRAN), y en programas ampliamente utilizados
como editores (EMACS) y CAD (AUTOCAD). Si lo desea, puede consultar la
introducción al lenguaje LISP:

  Sheila Hughes: Lisp. Pitman Publishing Limited, London 1986.
  107 pages.

Después de un rato, necesitará el texto estandar que contiene la
definición del lenguaje:

Guy L. Steele Jr.: Common Lisp - The Language. Digital Press.
  1. edition 1984, 465 pages.
  2. edition 1990, 1032 pages.

Este libro está disponible en formato HTML via FTP en:
  ftp.cs.cmu.edu:/user/ai/lang/lisp/doc/cltl/cltl_ht.tgz

y puede consultarse a través de WWW en:

  http://www.cs.cmu.edu:8001/Web/Groups/AI/html/cltl/cltl2.html o	
  http://www.cs.cmu.edu:8001/afs/cs/project/ai-repository/ai/html/cltl/cltl2.html .

Nota para los expertos: Este texto estandar se ha convertido en un
estándar ANSI, que puede obtenerse <<<exceptionally>>> sin cargo alguno en:

  http://www.harlequin.com/books/HyperSpec/

LISP se ejecuta en un entorno interactivo. Usted introduce formas, que
serán evaluadas de inmediato. Por lo tanto, puede inspeccionar
variables, invocar funciones con unos argumentos concretos o definir
sus propias funciones.


Contenidos:
-----------

Consta de los siguientes ficheros:

#ifdef UNIX
#ifndef UNIX_BINARY_DISTRIB
   base/lisp.a            programa principal, listo para ser enlazado
#endif
#ifdef UNIX_BINARY_DISTRIB
   base/lisp.run          programa principal
#endif
   base/lispinit.mem      imagen de memoria necesaria para la inicialización
   doc/clisp.1            manual en formato man de Unix
   doc/clisp.man          manual
   doc/clisp.html         manual en format HTML
   doc/impnotes.html      notas de la implementación
#ifdef GNU_READLINE
   doc/clreadline.3       manual de edición de línea en formato man de Unix
   doc/clreadline.man     manual de edición de línea
#endif
   doc/LISP-tutorial.txt  tutorial de LISP para aprendices
   doc/CLOS-guide.txt     breve guía de CLOS
   README                 este texto
   SUMMARY                pequeña descripción de CLISP
   ANNOUNCE               declaración
   NEWS                   lista de modificaciones desde la última versión
   COPYRIGHT              derechos de autor <<<copyright>>>
   GNU-GPL                licencia de software libre
#ifdef GNU_READLINE
   doc/readline.dvi       documentación de la librería GNU readline
                          en formato DVI
#endif
   doc/editors.txt        Lista de editores que soportan Lisp
   emacs/*.el             personalización de Emacs, véase doc/editors.txt
#ifndef UNIX_BINARY_DISTRIB
   src/clisp.c            fuentes del programa
#endif
   src/config.lsp         configuración dependiente del lugar

y - cuando le apetezca, si le gusta leer código fuente -

   src/*.lsp              el código fuente de lispinit.mem
   src/*.fas              los mismos ficheros, una vez compilados
#if !defined(UNIX_BINARY_DISTRIB) && defined(GNU_READLINE)

Para crear el ejecutable, también necesitará:

   base/libreadline.a     librería GNU readline

o

   base/libnoreadline.a   sustituto ficticio de la librería GNU readline
#endif
#else /* !defined(UNIX) */
#ifdef AMIGAOS
      lisp.run           programa principal
#endif
#ifdef MSDOS
      lisp.exe           programa principal
#ifndef OS2
      lisp_1mb.exe       programa principal, utilice éste si sólo 
                         dispone de 1 o 2 MB de RAM
#endif
#endif
#ifdef RISCOS
      lisp               programa principal
#endif
      lispinit.mem       imagen de memoria necesaria para la inicialización
#ifdef GNU_GETTEXT
      locale/*/LC_MESSAGES/clisp.mo  <<<localized messages databases>>>
#endif
      clisp.1            manual en formato `man' de Unix
#ifdef AMIGAOS
      clisp.doc          manual
#else
      clisp.man          manual
#endif
      clisp.html         manual en format HTML
#ifdef MSDOS
      clisp.dvi          manual en formato DVI
#endif
      impnotes.html      notas de la implementación
#ifdef GNU_READLINE
      clreadline.3       manual de edición de línea en formato `man' de Unix
      clreadline.man     manual de edición de línea
      clreadline.html    manual de edición de línea en format HTML
#ifdef MSDOS
      clreadline.dvi     manual de edición de línea en formato DVI
#endif
#endif
      LISP-tutorial.txt  tutorial de LISP para aprendices
      CLOS-guide.txt     breve guía de CLOS
      editors.txt        <<<some words about text editors for Lisp>>>
#ifdef EMUNIX
      emx.exe            extensor DOS rsx para ejecutar clisp bajo DOS o OS/2
      emx-user.doc       guía del usuario de aplicaciones emx
      emx-faq.doc        preguntas frecuentes sobre las aplicaciones emx
#ifdef EMUNIX_PORTABEL
      emx.dll            librería de enlazamiento dinámico de OS/2 que contiene emx
      emxlibc.dll        librería de enlazamiento dinámico de OS/2 que contiene emx libc
#endif
      termcap.dat        base de datos del terminal 
#endif
#if defined(MSDOS) && !defined(OS2)
      rsx.exe            extensor DOS rsx para ejecutar clisp bajo Windows
      rsx-read.txt       descripción general de rsx
      rsx.hlp            <<<general documentation for rsx>>>
      delay.exe          programa auxiliar para ejecutar clisp bajo Windows
#endif
#ifdef RISCOS
      !Run               fichero de ejecución para CLISP
      !Sprites           icono de CLISP
#endif
      README             este texto
      SUMMARY            pequeña descripción de CLISP
      ANNOUNCE           declaración
      NEWS               lista de modificaciones desde la última versión
      COPYRIGHT          derechos de autor
      GNU-GPL            licencia de software libre
#ifdef GNU_READLINE
      readline.dvi	 documentación de la librería GNU readline en formato DVI
#endif
      config.lsp         configuración dependiente del lugar
#if !(defined(UNIX) || defined(WIN32))
      timezone.lsp       zona horaria dependiente del lugar
#endif

y - cuando le apetezca, si le gusta leer código fuente -

      *.lsp              el código fuente de lispinit.mem
#if !defined(MSDOS)
      *.fas              los mismos ficheros, una vez compilados
#endif
#endif

#ifdef MSDOS

Requisitos Hardware:
--------------------

#ifndef OS2
La versión para DOS de CLISP necesita una CPU 80386 (SX o DX) o un 80486
con, al menos, 1 MB de RAM.
#else
La versión para OS/2 de CLISP necesita una CPU 80386 (SX o DX) o un 80486,
ejecutando OS/2 2.0.
#endif
También se ejecuta en un Pentium; los resultados que produce CLISP no
están afectados por el error de división del Pentium de Intel.

#endif
#ifdef AMIGAOS

Requisitos Hardware:
--------------------

Esta versión para Amiga de CLISP requiere, al menos, 1.5 MB de RAM. La
versión denominada CLISP-LOW se ejecuta en máquinas sin más memoria
que la que puede direccionarse en un rango de 24 bits: en el 68000,
A2620 y A2630. La versión denominada CLISP-HIGH se ejecuta en memorias
que se direccionan con 27 bits (en el rango de direcciones de
#x00000000 to #x07FFFFFF), pero sólo en las CPUs 68020/030/040(/060?):
en A3000 y A4000 sin placas de memoria Zorro-III. La versión
denominada CLISP-00 se ejecuta únicamente en una CPU 68000/010, pero
es más rápida que CLISP-LOW. La versión denominada CLISP-WIDE utiliza
enteros de 64 bits y se ejecuta sobre cualquier memoria en un
procesador 68020 o superior: sobre A4000 con VMM. El esfuerzo
adicional para el tratamiento de números enteros de 64 bits hace que
CLISP-WIDE sea más lento que CLISP-HIGH.

#endif
#ifdef RISCOS

Requisitos Hardware:
--------------------

Esta versión de CLISP requiere un PC Acorn Archimedes o Acorn RISC
con, al menos, 4 MB de Ram y RISC OS 3.0 o superior. Más adelante se
explica como crear una versión de CLISP que se ejecute con solo 2 MB.

#endif
#if defined(SINGLEMAP_MEMORY) && (defined(UNIX_LINUX) || !defined(HAVE_MMAP_ANON))

Requisitos Software:
--------------------

#ifdef UNIX_LINUX
#ifdef GENERATIONAL_GC
#ifdef IMMUTABLE
Esta versión de CLISP necesita Linux 1.2.2 o más reciente.
#else
Esta versión de CLISP necesita Linux 1.1.52 o más reciente.
#endif
#else
Esta versión de CLISP necesita Linux 0.99.7 o más reciente.
#endif
#endif
#if !defined(HAVE_MACH_VM) && !defined(HAVE_MMAP_ANON) /* impliziert HAVE_MMAP_DEVZERO */
/dev/zero debe ser legible por cualquiera. Para ello, debe ejecutar el
comando "chmod a+r /dev/zero".
#endif

#endif
#ifdef AMIGAOS

Requisitos Software:
--------------------

Esta versión de CLISP necesita OS 2.04 (V37) o más reciente.

#endif

Instalación:
------------

#ifdef OS2
Antes que nada, instale emx.dll y emxlibc.dll en un directorio aparte,
por ejemplo c:\emx\dll. Añada c:\emx\dll (asegúrese de colocar la
unidad de disco correcta) a la sentencia LIBPATH de su fichero
config.sys. Reinicie su ordenador, de modo que se active la nueva
instrucción LIBPATH y las nuevas variables de entorno.

#endif
#ifdef EMUNIX

Para que las líneas de entrada demasiado largas puedan mostrarse de
una manera elegante, es necesario que tenga una linea del tipo:

    DEVICE=ANSI.SYS

en su fichero CONFIG.SYS. Más aún, la variable de entorno TERM debe
estar definida, y la variable de entorno TERMCAP debe contener el
nombre del fichero (con la ruta completa) de la base de datos
TERMCAP.DAT, con la definición de las capacidades del terminal. Es una
buena idea, añadir estas instrucciones en el fichero CLISP.BAT que se
construye más adelante. Si lo desea, puede instalar el fichero
TERMCAP.DAT en un directorio aparte, por ejemplo c:\emx\etc.

#endif
#if defined(UNIX) || defined(WIN32)
#if defined(UNIX) && !defined(UNIX_BINARY_DISTRIB)
Teclee

         make

#if 0 /* def GNU_READLINE - man muß Makefile verändern */
Si desea renunciar a las capacidades de edición de lectura de la
librería GNU readline, debería haber reemplazado "libreadline.a" en la
línea LIBS del fichero BASE/MAKEVARS por "libnoreadline.a".

#endif
#endif
Cambie las cadenas en SRC/CONFIG.LSP, empleando para ello un editor de
textos.
#else
Edite el fichero CONFIG.LSP y modifíquelo adecuadamente para su
estación, con especial atención a las definiciones de short-site-name
y long-site-name. Si lo desea, también puede modificar la definición
de la zona horaria al final del fichero TIMEZONE.LSP.
#endif
Luego ejecute

#if defined(MSDOS) || defined(WIN32_NATIVE)
         lisp.exe -M lispinit.mem
#endif
#ifdef AMIGAOS
         lisp.run -M lispinit.mem
#endif
#ifdef UNIX
         base/lisp.run -M base/lispinit.mem
#endif
#ifdef RISCOS
         lisp -M mem.lispinit

o haga doble click sobre el directorio !Clisp.
#endif

Cuando aparezca el inductor de comandos

      > _

teclee

#ifdef RISCOS
        (cd "<clisp$path>.")

para asegurarse de que el directorio !Clisp es el que está actualmente
seleccionado. Luego

#endif
#if defined(UNIX) || defined(WIN32)
        (compile-file "src/config.lsp")
        (load "src/config.fas")
#else
        (compile-file "config.lsp")
        (load "config.fas")

y - si modificó el fichero TIMEZONE.LSP -

        (compile-file "timezone.lsp")
        (load "timezone.fas")
#endif

y luego

#ifdef UNIX
        (cd "base/")
#endif
        (saveinitmem)

para sobreescribir el fichero LISPINIT.MEM con su configuración. A
continuación

        (exit)

#ifdef UNIX
El resto se hace simplemente con

        make install

En vez de esto, puede hacerlo usted mismo, paso por paso:

#endif
#ifndef RISCOS
Luego cree un directorio, y ponga en él el ejecutable con la imagen de
memoria.
#endif
#ifdef UNIX
Le recomiendo /usr/local/lib/lisp :

   mkdir /usr/local/lib/lisp
   mv base/lisp.run /usr/local/lib/lisp
   mv base/lispinit.mem /usr/local/lib/lisp
#endif
#if defined(MSDOS) || defined(WIN32_NATIVE)
Suponiendo D:\LIB\LISP :

   mkdir d:\lib\lisp
   copy lisp.exe d:\lib\lisp
   copy lispinit.mem d:\lib\lisp
#endif

#if defined(MSDOS) || defined(WIN32_NATIVE)
Y cree un fichero de ejecución por lotes que ejecute lisp:

#ifndef OS2
   copy con c:\bat\clisp.bat
#else
   copy con c:\cmd\clisp.cmd
#endif
#ifdef EMUNIX
   set TERM=ansi
   set TERMCAP=c:/emx/etc/termcap.dat
#endif
   d:\lib\lisp\lisp.exe -M d:\lib\lisp\lispinit.mem -B d:\lib\lisp\ %1 %2 %3 %4 %5 %6 %7 %8 %9
   [Ctrl-Z]
#endif
#ifdef UNIX
Y cree el programa que ejeute lisp:

#ifdef UNIX_BINARY_DISTRIB
   cc -O -DLISPLIBDIR='"/usr/local/lib/lisp"' \
         -DLOCALEDIR='"/usr/local/share/locale"' \
      src/clisp.c -o /usr/local/bin/clisp
#else
   ./hardcode -DLISPLIBDIR='/usr/local/lib/lisp' \
              -DLOCALEDIR='/usr/local/share/locale' \
              clisp /usr/local/bin/clisp
#endif

#ifdef GNU_READLINE
Ahora, instale las páginas de man.
#else
Ahora, instale la página man.
#endif

   mv doc/clisp.1 /usr/local/man/man1/clisp.1
#ifdef GNU_READLINE
   mv doc/clreadline.3 /usr/local/man/man3/clreadline.3
#endif

and try

   man clisp
#endif

#if defined(MSDOS) && !defined(OS2)

Instalación en Microsoft Windows:
---------------------------------

CLISP también se ejecuta en una ventana de DOS bajo Microsoft Windows
3.1. Para ello, es necesario llevar a cabo los siguientes pasos:

1. Consiga e instale
     ftp://clisp.cons.org/pub/lisp/clisp/binaries/dos/clisp.zip
   tal y como se describe más arriba.

2. Si RSX.EXE no está ya en el fichero CLISP.ZIP, consiga e instálelo
   en, por ejemplo, C:\RSX. 
     ftp://ftp.uni-bielefeld.de/pub/systems/msdos/misc/rsx510b.zip

3. Ejecute el editor de PIF e introduzca lo siguiente:

   Programa:                c:\rsx\bin\rsx.exe
   Nombre del programa:     COMMON LISP
   Parámetros del programa: -Ra c:\lib\lisp\lisp.exe -M c:\lib\lisp\lispinit.mem -B c:\lib\lisp\
                            (tal vez después de -Ra deba añadir también -Rs1024)
   Directorio de inicio:    e:\lisp  (o donde corresponda)
   Pantalla:                Texto
   Requisitos de memoria:   requiere:  500      máximo:  640
   Memoria EMS:             requiere:    0      máximo:    0
   Memoria XMS:             requiere: 1024      máximo:   -1
   Display:                 [como quiera]
   Quit_closes_window:      [como quiera]
   Ejecución:               [como quiera]
   other_options:           [como quiera]

   (Probablemente eligirá como directorio de inicio, aquél que
   contiene sus programas lisp, en vez de e:\lisp.)

   Guardelo con el nombre WINCLISP.PIF.

4. En el administrador de programas, en un grupo adecuado:

   Menú "File" -> "New" -> "Program", ventana "Propiedades del programa".
   Introduzca ahí :

   Descripción:            COMMON LISP
   Línea de comandos:      winclisp.pif
   Directorio de inicio:   e:\lisp
   Combinación de teclas:  Ctrl+Alt+Shift+L      [como más le guste]

Presionando con el ratón en el grupo recién creado o pulsando la
combinación de teclas indicada anteriormente, ejecutará CLISP.

Notas:

* Copiar y Pegar en las ventanas de DOS (via el menú "Edit" -> "Mark"
  resp. menú "Edit" -> "Insert") inserta un <Enter> al final. Por eso,
  no es posible volver a editar una línea copiada.

* Pero las facilidades de edición mencionadas en CLISP.MAN y
  READLINE.DVI sí que funcionan.

#endif
#ifdef AMIGAOS

Nota:
-----

Puede ejecutar CLISP desde Workbench(tm). Los siguientes Tooltypes son
reconocidos en el icono Tool:

   WINDOW=<ventana o especificación de `pipe'>
   ARGS=<argumentos del tipo CLI>

Por ejemplo,

   WINDOW=CON:0/11/640/200/CLISP-Listener/CLOSE
   ARGS=-M lispinit.mem

#endif
#ifdef RISCOS

¿Corto de memoria?
------------------

Si sólo dispone de 2 MB de RAM, puede crear un CLISP "desmontado" que
requiere menos memoria, pero que no dispondrá de algunas partes
definidas en CLtL2, dpANS-LOOP, CLOS, Condiciones y flujos genéricos:
Reemplace DEFS2.FAS, LOOP.FAS, CLOS.FAS, CONDITIO.FAS, DEFS3.FAS,
GSTREAM.FAS por ficheros vacíos y ejecute:

   lisp
   > (load "init.fas")
   > (saveinitmem)
   > (exit)

Esto sobreescribirá el fichero LISPINIT.MEM por otro más pequeño.

#endif

Cuando encuentre problemas:
---------------------------

#ifdef EMUNIX
Si clisp no se ejecuta de ninguna manera, consulte
EMX-USER.DOC. LISP.EXE es una aplicación EMX, de modo que todo lo que
se menciona ahí, se aplica a LISP.EXE.

#endif
Después de un error, se encontrará en el depurador:

     1. Break> _

En él, usted puede evaluar formas como siempre. Más aún:

     Help
               invoca la ayuda
     Abort     o
     Unwind
               retrocede hasta el bucle de entrada más reciente
     Backtrace
               muestra los contenidos de la pila, útil para la depuración

Y puede consultar el valor de las variables de las funciones donde se
produjo el error.

#ifdef UNIX
Cuando los problemas sean mayores, por ejemplo `core dumps', por favor
#endif
#ifdef AMIGAOS
Cuando los problemas sean mayor, por ejemplo "guru"s, por favor
#endif
#ifdef MSDOS
Cuando los problemas sean mayor, por ejemplo "register dumps", por favor
#endif
#ifdef RISCOS
Cuando los problemas sean mayores, por ejemplo, "stack dumps", por favor
#endif
envíe una descripción del error y una descripción de cómo reproducir
el error a los autores o al "mantenedor". Por favor, acompañe su mensaje
de la versión de CLISP que puede obtener invocando la función
(lisp-implementation-version).


Código fuente:
--------------

El código fuente de CLISP está disponible en
     ftp://clisp.cons.org/pub/lisp/clisp/source/clispsrc*
#ifdef UNIX_LINUX
La última distribución binaria de CLISP para Linux tiene su propio
código fuente en
     ftp://sunsite.unc.edu/pub/Linux/devel/lang/lisp/clisp-source.tar.gz
#endif


#if 0
<<<Mailing lists:>>>
#endif
Lista de correo:
----------------

#if 0
Hay una lista de correo para los usuarios de CLISP. Ése es el foro
adecuado para cualquier cuestión relacionada con CLISP, problemas de
instalación, errores, paquetes de aplicaciones, etc.
#endif
<<<There are three mailing lists for users of CLISP. You find subscription
information and archives on the homepage http://clisp.cons.org/.>>>


Agradecimientos:
----------------

#ifdef MSDOS
Si le parece que CLISP es rápido y sin errores y le gusta utilizarlo,
le agradeceremos una donación de $25 (o cualquier cantidad que usted
considere oportuna). La mayor parte del software para DOS cuesta algo
de modo que, probablemente, ya estará acostumbrado a pagar.

En otro caso, envíenos tantas sugerencias como considere para
mejorarlo. O échele un ojo a CLISP, mejórelo usted mismo y envíenos
los parches.

#endif
Estamos muy agradecidos a 
  * Guy L. Steele y otros muchos por la especificación de Common Lisp.
#ifdef UNIX
  * El proyecto GNU de Richard Stallman para el GCC, Autoconf y la librería
    readline.
#else
#ifdef GNU_READLINE
  * El proyecto GNU de Richard Stallman para el GCC y la librería readline.
#else
#ifdef GNU
  * El proyecto GNU de Richard Stallman para el GCC.
#endif
#endif
#endif
#ifdef EMUNIX
  * Eberhard Mattes por EMX.
#endif


Autores:
--------

        Bruno Haible
        Michael Stoll

Email: clisp-list@lists.sourceforge.net
#ifdef AMIGAOS

Migración a Amiga por:
----------------------

        Jörg Höhle

Email: Joerg.Hoehle@gmd.de
#endif
#ifdef RISCOS

Migración a Acorn RISC OS por:
------------------------------

        Peter Burwood

Email: clisp@arcangel.dircon.co.uk
#endif

"Mantenedor":
-------------

        Marcus Daniels

Email: marcus@sysc.pdx.edu
