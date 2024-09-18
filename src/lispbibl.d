/*
 * Main include-file for CLISP
 * Bruno Haible 1990-2011, 2016-2018, 2020-2024
 * Marcus Daniels 11.11.1994
 * Sam Steingold 1998-2012, 2016-2018
 * Vladimir Tzankov 2008-2012, 2017
 * German comments translated into English: Stefan Kain 2001-09-24

 Flags that may be set through CFLAGS:

   Flags that enable/disable features:

     Flags that enable features:
       Unicode characters (prerequisite for internationalization):
         ENABLE_UNICODE
       Foreign function interface (based on the GNU libffcall library):
         DYNAMIC_FFI
       Dynamic loading of modules:
         DYNAMIC_MODULES
       Just-in-time compiler (experimental):
         USE_JITC
       Multithreading (experimental):
         MULTITHREAD
       Create binary package on Unix (use this if you want to install
       binaries created on one machine on other machines):
         UNIX_BINARY_DISTRIB

     Flags that disable features (enabled by default):
       Advanced line editing in terminal emulators
       (based on the GNU readline library):
         NO_READLINE
       Advanced user interface in terminal emulators
       (based on the termcap or GNU ncurses library):
         NO_TERMCAP_NCURSES
       Internationalization
       (based on the libintl library from GNU gettext):
         NO_GETTEXT

   Flags that determine how clisp features are implemented:
     Multithreading:
       Flavor of thread implementation:
         POSIX_THREADS
       Thread local storage storage when no compiler support is available
       (32 bit platforms only). The SP is mapped to clisp_thread_t pointer:
         USE_CUSTOM_TLS={1,2,3}  - see comments for the options

   Flags that determine the object representation:
     Store only minimal type information in a pointer:
       HEAPCODES
     Store a good amount of type information in a pointer:
       TYPECODES
     More detailed object representation schemes:
       Object representation schemes on 32-bit platforms:
         ONE_FREE_BIT_HEAPCODES
         KERNELVOID32_HEAPCODES
       Specific variants of KERNELVOID32_HEAPCODES on 32-bit platforms:
         KERNELVOID32A_HEAPCODES
         KERNELVOID32B_HEAPCODES
       Object representation schemes on 64-bit platforms:
         ONE_FREE_BIT_HEAPCODES
         GENERIC64_HEAPCODES
       Specific variants of GENERIC64_HEAPCODES on 64-bit platforms:
         GENERIC64A_HEAPCODES
         GENERIC64B_HEAPCODES
         GENERIC64C_HEAPCODES
     Use 64-bit pointers on 32-bit platforms
     (try to avoid it: wastes a lot of memory):
       WIDE_SOFT, WIDE_SOFT_LARGEFIXNUM

   Flags that determine how the memory management is implemented:

     Flags that determine how to get memory from the OS:
       Whether to use a fixed heap size and allocate all the heap at the start
       (try to avoid it):
         NO_VIRTUAL_MEMORY
       Don't make assumptions about the address space layout. In particular,
       ignore the values of CODE_ADDRESS_RANGE, MALLOC_ADDRESS_RANGE,
       SHLIB_ADDRESS_RANGE, STACK_ADDRESS_RANGE determined by an autoconf test.
         NO_ADDRESS_SPACE_ASSUMPTIONS
       Assuming an object representation with TYPECODES, put objects
       at their address by using mmap with MAP_FIXED; every such memory
       range is mapped exactly once: SINGLEMAP_MEMORY
         NO_SINGLEMAP
       Put objects at their address by using mmap with MAP_FIXED:
       TRIVIALMAP_MEMORY
         NO_TRIVIALMAP
       In case of SPVW_PAGES, to prefer SPVW_PURE over SPVW_MIXED:
         PREFER_PURE_PAGES
       Depending on these, there is an automatic determination of
       - SPVW_BLOCKS vs. SPVW_PAGES,
       - SPVW_MIXED vs. SPVW_PURE,
       - MAP_MEMORY_TABLES,
       - SINGLEMAP_MEMORY_STACK,
       - TRIVIALMAP_MEMORY_STACK.

     Flags that determine the GC algorithm:
       Whether to use generational GC (quite advanced fiddling with memory page
       permissions and page faults): GENERATIONAL_GC
         NO_GENERATIONAL_GC
       Whether to use the GC code in its state before MULTITHREAD support
       was added (that is, in clisp-2.47 state):
         OLD_GC
       Whether conses are garbage-collected through an algorithm that preserves
       locality (but makes debugging of GC crashes very hard): MORRIS_GC
         NO_MORRIS_GC
       Whether conses are allocated in their block or page in ascending or
       descending order:
         CONS_HEAP_GROWS_DOWN
         CONS_HEAP_GROWS_UP

   Flags that enable/disable optimizations (no features, just speed):

     Flags that enable optimizations:
       Exploit GCC global register variables (risky):
         USE_GCC_REGISTER_VARIABLES

     Flags that disable optimizations:
       Safety level:
         SAFETY={0,1,2,3}
       Low-cost memory allocation on the C stack:
         NO_ALLOCA
       Hand-written assembler code for arithmetic, especially multi-precision
       arithmetic:
         NO_ARI_ASM
       Hand-written assembler code for stack pointer access:
         NO_SP_ASM
       Use of inline assembler code (__asm__):
         NO_ASM
       Memory saving representation of strings with only 8-bit characters
       or only 16-bit characters:
         NO_SMALL_SSTRING
       Memory saving representation of variable binding frames, that stores
       the binding state bits in the symbol word:
         NO_SYMBOLFLAGS

    Flags for debugging of clisp internals (for the clisp developers only):
    Some of these are turned on by the configure option '--with-debug'.
      DEBUG_GCSAFETY (requires the configure option or env variable CC="g++")
      DEBUG_OS_ERROR
      DEBUG_SPVW
      DEBUG_BYTECODE
      DEBUG_BACKTRACE (slows down the interpreter a lot)
      DEBUG_COMPILER

 Flags that you cannot set from CFLAGS (they are automatically determined)
 and that enable/disable optimizations:
   Access the machine stack pointer directly:
     SP_register
   Use a register instead of a global variable for the Lisp stack pointer:
     STACK_register
   Clumsy saving/restoring of STACK when invoking system code:
     HAVE_SAVED_STACK
   Represent single-floats as immediate objects:
     IMMEDIATE_FFLOAT

 */

/* this machine: WIN32 or GENERIC_UNIX */
#if (defined(__unix) || defined(__unix__) || defined(_AIX) || defined(sinix) || defined(__MACH__) || defined(__POSIX__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__BEOS__) || defined(__HAIKU__) || defined(__minix)) && !defined(unix)
  #define unix
#endif
#if defined(_WIN32) && (defined(_MSC_VER) || defined(__MINGW32__))
  #undef WIN32                  /* because of __MINGW32__ */
  #define WIN32
#endif
#if !defined(WIN32)
  #if defined(unix)
    #define GENERIC_UNIX
  #else
    #error Unknown machine type!
  #endif
#endif
/* additional specification of the machine: */
#if defined(WIN32)
  /* declare availability of typical PC facilities,
   like a console with a graphics mode that differs from the text mode,
   or a keyboard with function keys F1..F12. */
  #define PC386 /* IBMPC-compatible with 80386/80486-processor */
#endif
#ifdef GENERIC_UNIX
  #if (defined(unix) && (defined(linux) || defined(__CYGWIN__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__DragonFly__)) && (defined(i386) || defined(__i386__) || defined(__x86_64__) || defined(__amd64__)))
    #define PC386
  #endif
  #if (defined(sun) && defined(unix) && defined(sparc))
    #define SUN4
  #endif
  #if defined(hp9000s800) || defined(__hp9000s800)
    #define HP8XX
  #endif
#endif

/* Determine the processor:
 M68K == all processors of the Motorola 68000 series, starting at MC68020
 SPARC == the Sun SPARC processor
 HPPA == all processors of the HP Precision Architecture
 MIPS == all processors of the MIPS series
 POWERPC == the IBM RS/6000 and PowerPC processor family
 I80386 == all processors of the Intel 8086 series, starting at 80386,
           nowadays called IA32
 ARM == the ARM processor
 DECALPHA == the DEC Alpha superchip
 IA64 == the Intel IA-64 latecomer chip
 AMD64 == the AMD hammer chip
 S390 == the IBM S/390 processor
 RISCV64 == the 64-bit RISC-V processor family */
  /* 32-bit processors: */
  #if defined(m68k) || defined(__m68k__)
    #define M68K
  #endif
  /* Processors with 32-bit and 64-bit instruction sets: */
  #if defined(i386) || defined(__i386) || defined(__i386__) || defined(_I386) \
      || defined(_M_IX86) || defined(_X86_) /* native Windows */
    #define I80386
  #endif
  #if defined(__x86_64__) || defined(__amd64__)
    #define AMD64
  #endif
  #if defined(arm) || defined(__arm) || defined(__arm__)
    #define ARM
  #endif
  #ifdef __aarch64__
    #define ARM64
  #endif
  #if defined(sparc) || defined(__sparc__)
    #define SPARC
    #if defined(__sparcv9) || defined(__arch64__)
      #define SPARC64
    #endif
  #endif
  #if defined(mips) || defined(__mips) || defined(__mips__)
    #define MIPS
    #ifdef __mips64
      #define MIPS64
    #elif defined(_MIPS_SZLONG)
      #if (_MIPS_SZLONG == 64)
        /* We should also check for (_MIPS_SZPTR == 64), but gcc keeps this at 32. */
        #define MIPS64
      #endif
    #endif
  #endif
  #if defined(HP8XX) || defined(hppa) || defined(__hppa) || defined(__hppa__)
    #define HPPA
    #if defined(__LP64__)
      #define HPPA64
    #endif
  #endif
  #if defined(_IBMR2) || defined(__powerpc) || defined(__powerpc__) || defined(__ppc) || defined(__ppc__)
    #define POWERPC
    #if defined(__powerpc64__) || defined(__ppc64__) || defined(_ARCH_PPC64)
      #define POWERPC64
    #endif
  #endif
  #if defined(__s390__)
    #define S390
    #if defined(__s390x__)
      #define S390_64
    #endif
  #endif
  #if defined(__riscv) && (__riscv_xlen == 64)
    #define RISCV64
  #endif
  /* 64-bit processors: */
  #ifdef __alpha
    #define DECALPHA
  #endif
  #ifdef __ia64__
    #define IA64
  #endif

/* Selection of the operating system */
#ifdef WIN32
  /* Windows NT, Windows 95 */
  #define WIN32_NATIVE  /* native NT API, no DOS calls */
#endif
#ifdef GENERIC_UNIX
  #define UNIX
  #ifdef __linux__
    #define UNIX_LINUX  /* Linux (Linus Torvalds Unix) */
  #endif
  #ifdef __GNU__
    #define UNIX_HURD  /* the GNU system (Hurd + glibc) */
  #endif
  #ifdef __NetBSD__
    #define UNIX_NETBSD
  #endif
  #if defined(__FreeBSD__) || defined(__DragonFly__)
    /* FreeBSD or its fork called DragonFly BSD. */
    #define UNIX_FREEBSD
  #elif defined(__FreeBSD_kernel__)
    #define UNIX_GNU_FREEBSD /* GNU/kFreeBSD */
  #endif
  #ifdef __OpenBSD__
    #define UNIX_OPENBSD
  #endif
  #if defined(SVR3) || defined(__SVR3) || defined(SVR4) || defined(__SVR4) || defined(SYSTYPE_SVR4) || defined(__SYSTYPE_SVR4) || defined(__svr4__) || defined(USG) /* ?? */
    #define UNIX_SYSV  /* UNIX System V */
  #endif
  #ifdef _AIX
    #define UNIX_AIX  /* IBM AIX */
  #endif
  #if defined(__APPLE__) && defined(__MACH__)
    #define UNIX_MACOSX  /* MacOS X a.k.a. Darwin */
    /* MacOSX pathnames are UTF-8 strings, not byte sequences
       https://sourceforge.net/p/clisp/mailman/message/27345286/
       http://developer.apple.com/library/mac/#qa/qa2001/qa1173.html */
    #define CONSTANT_PATHNAME_ENCODING  Symbol_value(S(utf_8))
  #endif
  #ifdef __CYGWIN__
    #define UNIX_CYGWIN  /* Cygwin (UNIXlike on Windows) */
  #endif
  #ifdef __BEOS__
    #define UNIX_BEOS  /* BeOS (UNIXlike) */
  #endif
  #ifdef __HAIKU__
    #define UNIX_HAIKU  /* Haiku (a BeOS derivative) */
  #endif
  #ifdef __minix
    #define UNIX_MINIX  /* Minix 3 */
  #endif
#endif
%% #ifdef WIN32_NATIVE
%%   puts("#define WIN32_NATIVE");
%% #endif
%% #ifdef UNIX
%%   puts("#define UNIX");
%% #endif


/* Determine properties of compiler and environment: */
#if defined(UNIX) || defined(__MINGW32__)
  #include "config.h"  /* configuration generated by configure */
  #include "intparam.h"  /* integer-type characteristics created by the machine */
  #include "floatparam.h" /* floating-point type characteristics */
#elif defined(WIN32) && !defined(__MINGW32__)
  #include "version.h"          /* defines PACKAGE_* */
  #define char_bitsize 8
  #define short_bitsize 16
  #define int_bitsize 32
  #if defined(I80386)
    #define long_bitsize 32
  #elif defined(DECALPHA)
    #define long_bitsize 64
  #endif
  #if defined(I80386)
    #define pointer_bitsize 32
  #elif defined(DECALPHA)
    #define pointer_bitsize 64
  #endif
  #define alignment_long 4
  #if defined(I80386) || defined(ARM) || defined(DECALPHA)
    #define short_little_endian
    #define long_little_endian
  #endif
  #define stack_grows_down
  #define CODE_ADDRESS_RANGE 0
  #define MALLOC_ADDRESS_RANGE 0
  #define SHLIB_ADDRESS_RANGE 0
  #define STACK_ADDRESS_RANGE ~0UL
  #define ICONV_CONST
#else
  #error where is the configuration for your platform?
#endif

/* These properties of compiler and environment match the CPU.
   For example, on DECALPHA we have long_bitsize = pointer_bitsize = 64.
   But there are special ABIs:
     * On AMD64, you can have long_bitsize = pointer_bitsize = 32.
       This is the so-called x32 ABI. It is advertised through the _ILP32
       predefined macro.
     * On MIPS64, you can have long_bitsize = pointer_bitsize = 32.
       This is the so-called n32 ABI. It is advertised through the _ABIN32
       predefined macro.
     * On ARM64, you can have long_bitsize = pointer_bitsize = 32.
       This is the so-called ilp32 ABI. It is advertised through the _ILP32
       predefined macro.
 */


/* A more precise classification of the operating system: */
#if defined(UNIX) && defined(SIGNALBLOCK_BSD) && !defined(SIGNALBLOCK_SYSV)
  #define UNIX_BSD  /* BSD Unix */
#endif
#if defined(__sun)
  #define UNIX_SUNOS5  /* Sun OS Version 5.x (Solaris 2) */
#endif

/* On Linux/arm64, MALLOC_ADDRESS_RANGE comes out as a value < 1*2^32, but
   for larger malloc()s, the address can be around 0x20*2^32 or 0x7F*2^32. */
#if defined(UNIX_LINUX) && defined(ARM64)
  #undef MALLOC_ADDRESS_RANGE
  #define MALLOC_ADDRESS_RANGE STACK_ADDRESS_RANGE
#endif


/* Choose the character set: */
#if defined(UNIX) || defined(WIN32)
  #define ISOLATIN_CHS  /* ISO 8859-1, see isolatin.chs */
  /* Most Unix systems today support the ISO Latin-1 character set, in
   particular because they have X11 and the X11 fonts are in ISO Latin-1.
   Exceptions below.
   On Win32, the standard character set is ISO-8859-1. Only the DOS box
   displays CP437, but we convert from ISO-8859-1 to CP437 in the
   low-level output routine full_write(). */
#endif
#if defined(UNIX_BEOS) || defined(UNIX_HAIKU)
  /* The default encoding on BeOS is UTF-8, not ISO 8859-1.
   If compiling with Unicode support, we use it. Else fall back to ASCII. */
  #undef ISOLATIN_CHS
  #ifdef ENABLE_UNICODE
    #define UTF8_CHS  /* UTF-8 */
  #endif
#endif
#ifdef HP8XX
  #undef ISOLATIN_CHS
  #define HPROMAN8_CHS  /* HP-Roman8, see hproman8.chs */
  /* under X-Term however: #define ISOLATIN_CHS ?? */
#endif
#if !(defined(ISOLATIN_CHS) || defined(HPROMAN8_CHS))
  #define ASCII_CHS  /* Default: plain ASCII charset without special chars */
#endif


/* Choose the compiler: */
#if defined(__GNUC__)
  #define GNU
  #if (__GNUC__ >= 3)
    #undef _Noreturn
    #define _Noreturn __attribute__((noreturn))
  #endif
  /* known bugs */
  #if (__GNUC__ < 2) || ((__GNUC__ == 2) && (__GNUC_MINOR__ < 95))
    #error "The minimum supported GCC version is GCC 2.95.3"
  #endif
  #if defined(__cplusplus) && (__GNUC__ == 4) && ((__GNUC_MINOR__ == 2) || ((__GNUC_MINOR__ == 3) && (__GNUC_PATCHLEVEL__ < 1))) && !defined(__clang__)
    #error g++ 4.2.* and 4.3.0 are not supported due to g++ bug 35708
  #endif
#endif
#if defined(__STDC__) || defined(__cplusplus)
  /* ANSI C compilers define __STDC__ (but some define __STDC__=0 !).
   HP aCC is an example of a C++ compiler which defines __cplusplus but
   not __STDC__. */
  #define ANSI
#endif
#if defined(_MSC_VER)
  #define MICROSOFT
#endif
#if defined(__INTEL_COMPILER)
  #define INTEL
#endif


/* Selection of floating-point capabilities:
 FAST_DOUBLE should be defined if there is a floating-point coprocessor
 with a 'double'-type IEEE-Floating-Points with 64 Bits.
 FAST_FLOAT should be defined if there is a floating-point co-processor
 with a 'float'-type IEEE-Floating-Points with 32 Bits,
 and a C-Compiler that generates 'float'-operations
 instead of 'double'-operations */
#if (float_mant_bits == 24) && (float_rounds == rounds_to_nearest) && float_rounds_correctly && !defined(FLOAT_OVERFLOW_EXCEPTION) && !defined(FLOAT_UNDERFLOW_EXCEPTION) && !defined(FLOAT_INEXACT_EXCEPTION)
  #define FAST_FLOAT
#endif
#if (double_mant_bits == 53) && (double_rounds == rounds_to_nearest) && double_rounds_correctly && !defined(DOUBLE_OVERFLOW_EXCEPTION) && !defined(DOUBLE_UNDERFLOW_EXCEPTION) && !defined(DOUBLE_INEXACT_EXCEPTION)
  #define FAST_DOUBLE
#endif
#ifdef ARM
  /* The processor is little-endian w.r.t. integer types but stores 'double'
   floats in big-endian word order! */
  #undef FAST_DOUBLE
#endif
#ifdef NO_FAST_DOUBLE
  #undef FAST_DOUBLE
#endif
#ifdef NO_FAST_FLOAT
  #undef FAST_FLOAT
#endif

/* Selection of the safety-level:
 SAFETY=0 : all optimizations are turned on
 SAFETY=1 : all optimizations on, but keep STACKCHECKs
 SAFETY=2 : only simple assembler-support
 SAFETY=3 : no optimizations */
#ifndef SAFETY
  #define SAFETY 0
#endif
#if SAFETY >= 3
  #ifndef NO_ASM
    #define NO_ASM
  #endif
  #ifndef NO_ARI_ASM
    #define NO_ARI_ASM
  #endif
  #ifndef NO_FAST_DISPATCH
    #define NO_FAST_DISPATCH
  #endif
#endif

/* We don't support pre-ANSI-C compilers any more. */
#if !defined(ANSI)
  #error An ANSI C or C++ compiler is required to compile CLISP!
#endif

/* Choose the appropriate designated field initializer syntax.
 The standard syntax nowadays is   .field = value
 but it not supported in GNU g++ < 4.7.
 The older GNU syntax              field : value
 is supported at least throughout GNU gcc and g++ 2.95 to 4.8
 but is marked obsolete and elicits a warning in clang. */
#if defined(GNU) && !defined(__clang__) && defined(__cplusplus) && (__GNUC__ + (__GNUC_MINOR__ >= 7) <= 4)
  #define designated_init(field,value) field:value
#else
  #define designated_init(field,value) .field=value
#endif
%% export_def(designated_init(field,value));

/* A property of the compiler:
   Whether it supports aligning variables on 8-byte boundaries. */
#if defined(GNU) || defined(__SUNPRO_C)
  #define HAVE_GLOBAL_VAR_ALIGN
#endif

/* A property of the processor:
 The sequence in which words/long-words are being put into bytes */
#if defined(short_little_endian) || defined(int_little_endian) || defined(long_little_endian)
  /* Z80, VAX, I80386, DECALPHA, MIPSEL, IA64, AMD64, RISCV64, ...:
   Low Byte is the lowest, High Byte in a higher address */
  #if defined(BIG_ENDIAN_P)
    #error Bogus BIG_ENDIAN_P!
  #endif
  #define BIG_ENDIAN_P  0
#endif
#if defined(short_big_endian) || defined(int_big_endian) || defined(long_big_endian)
  /* M68K, SPARC, HPPA, MIPSEB, POWERPC, S390, ...:
   High Byte is the lowest, Low Byte is a higher adress (easier to read) */
  #if defined(BIG_ENDIAN_P)
    #error Bogus BIG_ENDIAN_P!
  #endif
  #define BIG_ENDIAN_P  1
#endif
#if !defined(BIG_ENDIAN_P)
  #error Bogus BIG_ENDIAN_P!
#endif
%% export_def(BIG_ENDIAN_P);

/* A property of the processor (and C compiler): The alignment of C functions.
 (See gcc's machine descriptions, macro FUNCTION_BOUNDARY, for information.) */
#if defined(__frv__)
  #define C_CODE_ALIGNMENT  16
  #define log2_C_CODE_ALIGNMENT  4
#endif
#if defined(IA64)
  /* A function pointer on ia64 is a pointer to a two 8-bytes-word structure
     (first word: a code pointer, second word: a value which will be put in
     register %r1).
     The HP-UX linker does not guarantee an alignment of 16, only 8. */
  #define C_CODE_ALIGNMENT  8
  #define log2_C_CODE_ALIGNMENT  3
#endif
#if defined(HPPA64)
  /* A function pointer on hppa64 is a pointer to a four-word structure
     (third word: a code pointer, fourth word: a value which will be put in
     register %r27). */
  #define C_CODE_ALIGNMENT  8
  #define log2_C_CODE_ALIGNMENT  3
#endif
#if defined(S390) || defined(__SPU__) || defined(__tilegx__) || defined(__tilepro__)
  #define C_CODE_ALIGNMENT  8
  #define log2_C_CODE_ALIGNMENT  3
#endif
#if (defined(I80386) && defined(GNU)) || defined(DECALPHA) || defined(SPARC) || defined(MIPS) || defined(POWERPC) || defined(ARM64) || defined(AMD64) || defined(__arc__) || defined(__bfin__) || defined(__TMS320C6X__) || defined(__epiphany__) || defined(__fr30__) || defined(__FT32__) || defined(__iq2000__) || defined(__lm32__) || defined(__M32R__) || defined(__m88k__) || defined(__MICROBLAZE__) || defined(__mmix__) || defined(__nds32__) || defined(__NIOS2__) || defined(__nvptx__) || defined(__VISIUM__) || defined(__xtensa__)
  /* When using gcc on i386, this assumes that -malign-functions has not been
   used to specify an alignment smaller than 4 bytes. */
  #define C_CODE_ALIGNMENT  4
  #define log2_C_CODE_ALIGNMENT  2
#endif
#if defined(HPPA) && !defined(HPPA64)
  /* A function pointer on hppa is either
     - a code pointer == 0 mod 4, or
     - a pointer to a two-word structure (first word: a code pointer,
       second word: a value which will be put in register %r19),
       incremented by 2, hence == 2 mod 4.
   The current compilers only emit the second kind of function pointers,
   hence we can assume that all function pointers are == 2 mod 4. */
  #define C_CODE_ALIGNMENT  2
  #define log2_C_CODE_ALIGNMENT  1
  #define C_FUNCTION_POINTER_BIAS 2
#endif
#if defined(M68K) || defined(RISCV64) || defined(__CR16__) || defined(__cris__) || defined(__H8300__) || defined(__mcore__) || defined(__mep__) || defined(__moxie__) || defined(__MSP430__) || defined(__pdp11__) || defined(__sh__) || defined(__xstormy16__) || defined(__v850__) || defined(__vax__)
  #define C_CODE_ALIGNMENT  2
  #define log2_C_CODE_ALIGNMENT  1
#endif
#if defined(ARM)
  /* A function pointer on ARM is either
     - a code pointer (of code that uses the ARM instruction set), == 0 mod 4,
     or
     - a code pointer (of code that uses the Thumb instruction set), == 0 mod 2,
       plus 1.
     See https://stackoverflow.com/questions/22205183/least-significant-bits-in-function-pointer
     For GCC, which mode / instruction set is used can be specified through the
     options -marm and -mthumb. */
  #if defined(__thumb__)
    #define C_CODE_ALIGNMENT  1
    #define log2_C_CODE_ALIGNMENT  0
  #else
    #define C_CODE_ALIGNMENT  4
    #define log2_C_CODE_ALIGNMENT  2
  #endif
#endif
#if defined(__AVR__) || defined(__m32c__) || defined(__mn10300__) || defined(__RL78__)
  #define C_CODE_ALIGNMENT  1
  #define log2_C_CODE_ALIGNMENT  0
#endif
#if !defined(C_CODE_ALIGNMENT) /* e.g. (defined(I80386) && defined(MICROSOFT)) */
  #define C_CODE_ALIGNMENT  1
  #define log2_C_CODE_ALIGNMENT  0
#endif
#if !defined(C_FUNCTION_POINTER_BIAS)
  #define C_FUNCTION_POINTER_BIAS 0
#endif

/* Flags for the system's include files. */

/* Width of object representation:
   WIDE means than an object (pointer) occupies 64 bits (instead of 32 bits).
 WIDE_HARD means on a 64-bit platform.
 WIDE_SOFT means on a 32-bit platform, each object pointer occupies 2 words. */

#if (long_bitsize==64) && (pointer_bitsize==64)
  #define WIDE_HARD
#endif

/* On 32-bit platforms, the only object representation choice that does not
   make assumptions about the address space layout is WIDE_SOFT. */
#if defined(NO_ADDRESS_SPACE_ASSUMPTIONS) && !defined(WIDE_HARD) && !defined(WIDE_SOFT)
  #define WIDE_SOFT
#endif

/* WIDE_SOFT_LARGEFIXNUM is a special case of WIDE_SOFT. */
#if defined(WIDE_SOFT_LARGEFIXNUM) && !defined(WIDE_SOFT)
  #define WIDE_SOFT
#endif

#if defined(WIDE) && !(defined(WIDE_HARD) || defined(WIDE_SOFT))
  #define WIDE_SOFT
#endif
#if defined(WIDE_HARD) || defined(WIDE_SOFT)
  #ifndef WIDE
    #define WIDE
  #endif
#endif
/* Now: defined(WIDE) == defined(WIDE_HARD) || defined(WIDE_SOFT) */

/* this is necessary to avoid messages like
   Info: resolving _mv_space by linking to __imp__mv_space (auto-import)
   on woe32. */
#if defined(DYNAMIC_MODULES)
  #if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN)
    #define DYNAMIC_TABLES 1
    #define modexp  __declspec(dllexport)
    #define modimp  __declspec(dllimport)
    #define EXECUTABLE_NAME "lisp.exe"
  #else
    #define DYNAMIC_TABLES 0
    #define modexp
    #define modimp  extern
  #endif
#else
  #define DYNAMIC_TABLES 0
  #define modexp
  #define modimp  extern
#endif
%% export_def(DYNAMIC_TABLES);
%% export_def(modexp);
%% export_def(modimp);

/* Global register declarations.
   Speed benefit: Just putting the STACK into a register, brought 5% of speed
   around 1992. Now, with an AMD Athlon CPU from 2000, with good caches, it
   still brings 4%.
   The declarations must occur before any system include files define any
   inline function, which is the case on UNIX_GNU.
   Only GCC supports global register variables. Not Apple's variant of GCC.
   Not clang, which disguises as GCC.
   And only the C frontend, not the C++ frontend, understands the syntax.
   And gcc-3.0 to 3.3.3 has severe bugs with global register variables, see
   CLISP bugs 710737 and 723097 and
   http://gcc.gnu.org/bugzilla/show_bug.cgi?id=7871
   http://gcc.gnu.org/bugzilla/show_bug.cgi?id=10684
   http://gcc.gnu.org/bugzilla/show_bug.cgi?id=14937
   http://gcc.gnu.org/bugzilla/show_bug.cgi?id=14938
   Likewise, gcc-4.2 has severe bugs with global register variables, see
   CLISP bug 1836142 and http://gcc.gnu.org/bugzilla/show_bug.cgi?id=34300
   Likewise for gcc-4.3-20080215 and probably future versions of GCC as well.
   Therefore for these versions of gcc enable the global register variables
   only when USE_GCC_REGISTER_VARIABLES is explicitly defined.  */
#if defined(GNU) && !(__APPLE_CC__ > 1) && !defined(__clang__) && !defined(__cplusplus) && !(__GNUC__ == 3 && (__GNUC_MINOR__ < 3 || (__GNUC_MINOR__ == 3 && __GNUC_PATCHLEVEL__ < 4))) && !(((__GNUC__ == 4 && __GNUC_MINOR__ >= 2) || __GNUC__ > 4) && !defined(USE_GCC_REGISTER_VARIABLES)) && !defined(MULTITHREAD) && (SAFETY < 2) && !defined(USE_JITC)
/* Overview of use of registers in gcc terminology:
 fixed: mentioned in FIXED_REGISTERS
 used:  mentioned in CALL_USED_REGISTERS but not FIXED_REGISTERS
                     (i.e. caller-saved)
 save:  otherwise (i.e. call-preserved, callee-saved)

               STACK    mv_count  value1   back_trace
 M68K          used
 I80386        save
 SPARC (gcc2)  fixed    fixed     fixed    used
 MIPS
 HPPA          save     save      save     save
 ARM           save
 DECALPHA      save     save      save
 IA64
 AMD64
 S390          save

 Special notes:
 - gcc3/Sparc (Linux & Solaris) handles registers differently from gcc2. FIXME
 - If STACK is in a "used"/"save" register, it needs to be saved into
   saved_STACK upon begin_call(), so that asynchronous interrupts will
   be able to restore it.
 - All of the "used" registers need to be backed up upon begin_call()
   and restored during end_call().
 - All of the "save" registers need to be backed up upon begin_callback()
   and restored during end_callback().
 - When the interpreter does a longjmp(), the registers STACK, mv_count,
   value1 may need to be temporarily saved. This is highly machine
   dependent and is indicated by the NEED_temp_xxxx macros.

   * Register for STACK. */
  #if defined(M68K)
    #define STACK_register "a4" /* highest address register after sp=A7,fp=A6/A5 */
  #endif
  #if defined(I80386) && !(defined(UNIX_BEOS) || defined(UNIX_HAIKU)) && !defined(DYNAMIC_MODULES)
    /* On BeOS and Haiku, everything is compiled as PIC, hence %ebx is already booked.
     If DYNAMIC_MODULES is defined, external modules are compiled as PIC,
     which is why %ebx is already in use. */
    #if (__GNUC__ >= 2) /* The register names have changed */
      #define STACK_register  "%ebx"  /* one of the call-saved registers without special hardware commands */
    #else
      #define STACK_register  "bx"
    #endif
  #endif
  #if defined(SPARC)
    #define STACK_register  "%g5"  /* a global register */
  #endif
  #if defined(HPPA)
    #define STACK_register  "%r10"  /* one of the general registers %r5..%r18 */
  #endif
  #if defined(ARM)
    #define STACK_register  "%r8"   /* one of the general registers %r4..%r8 */
  #endif
  #if defined(DECALPHA)
    #define STACK_register  "$9"    /* one of the general registers $9..$14 */
  #endif
  #if defined(S390) && ((__GNUC__ > 3) || ((__GNUC__ == 3) && (__GNUC_MINOR__ >= 1)))
    /* global register assignment did not work on s390 until gcc 3.1 */
    #define STACK_register  "9"     /* one of the general registers %r8..%r9 */
  #endif
  /* What about NEED_temp_STACK ?? Needed if STACK is in a "used" register??
   Register for mv_count. */
  #if defined(SPARC)
    #define mv_count_register  "%g6"
    #if defined(UNIX_NETBSD)
      #define NEED_temp_mv_count
    #endif
  #endif
  #if defined(HPPA)
    #define mv_count_register  "%r11"  /* one of the general registers %r5..%r18 */
    #define NEED_temp_mv_count
  #endif
  #if defined(DECALPHA)
    #define mv_count_register  "$10"  /* one of the general registers $9..$14 */
    #define NEED_temp_mv_count
  #endif
  /* Register for value1. */
  #if !(defined(WIDE) && !defined(WIDE_HARD))
    #if defined(SPARC)
      #define value1_register  "%g7"
      #if defined(UNIX_NETBSD)
        #define NEED_temp_value1
      #endif
    #endif
    #if defined(HPPA)
      #define value1_register  "%r12"  /* one of the general registers %r5..%r18 */
      #define NEED_temp_value1
    #endif
    #if defined(DECALPHA)
      #define value1_register  "$11"  /* one of the general registers $9..$14 */
      #define NEED_temp_value1
    #endif
  #endif
  /* Register for back_trace. */
  #if !(defined(WIDE) && !defined(WIDE_HARD))
    #if defined(SPARC)
      #define back_trace_register  "%g4"  /* a global register */
      /* %g4 seems to be a scratch-register as of lately with gcc 2.3
       This causes problems with libc.so.1.6.1 (and higher) (in getwd())
       That's why HAVE_SAVED_back_trace has been defined above. */
    #endif
    #if defined(HPPA)
      #define back_trace_register  "%r13"  /* one of the general registers  %r5..%r18 */
    #endif
  #endif
  /* Declare the registers now (before any system include file which could
   contain some inline functions). */
  #ifdef STACK_register
    register long STACK_reg __asm__(STACK_register);
  #endif
  #ifdef mv_count_register
    register long mv_count_reg __asm__(mv_count_register);
  #endif
  #ifdef value1_register
    register long value1_reg __asm__(value1_register);
  #endif
  #ifdef back_trace_register
    register long back_trace_reg __asm__(back_trace_register);
  #endif
  /* Saving "save" registers. */
  #if (defined(I80386) || defined(HPPA) || defined(ARM) || defined(DECALPHA) || defined(S390)) && (defined(STACK_register) || defined(mv_count_register) || defined(value1_register) || defined(back_trace_register))
    #define HAVE_SAVED_REGISTERS
    struct registers {
      #ifdef STACK_register
        long STACK_register_contents;
      #endif
      #ifdef mv_count_register
        long mv_count_register_contents;
      #endif
      #ifdef value1_register
        long value1_register_contents;
      #endif
      #ifdef back_trace_register
        long back_trace_register_contents;
      #endif
    };
    extern  struct registers * callback_saved_registers;
    #ifdef STACK_register
      #define SAVE_STACK_register(registers)     \
              registers->STACK_register_contents = STACK_reg
      #define RESTORE_STACK_register(registers)  \
              STACK_reg = registers->STACK_register_contents
    #else
      #define SAVE_STACK_register(registers)
      #define RESTORE_STACK_register(registers)
    #endif
    #ifdef mv_count_register
      #define SAVE_mv_count_register(registers)     \
              registers->mv_count_register_contents = mv_count_reg
      #define RESTORE_mv_count_register(registers)  \
              mv_count_reg = registers->mv_count_register_contents
    #else
      #define SAVE_mv_count_register(registers)
      #define RESTORE_mv_count_register(registers)
    #endif
    #ifdef value1_register
      #define SAVE_value1_register(registers)     \
              registers->value1_register_contents = value1_reg
      #define RESTORE_value1_register(registers)  \
              value1_reg = registers->value1_register_contents
    #else
      #define SAVE_value1_register(registers)
      #define RESTORE_value1_register(registers)
    #endif
    #ifdef back_trace_register
      #define SAVE_back_trace_register(registers)     \
              registers->back_trace_register_contents = back_trace_reg
      #define RESTORE_back_trace_register(registers)  \
              back_trace_reg = registers->back_trace_register_contents
    #else
      #define SAVE_back_trace_register(registers)
      #define RESTORE_back_trace_register(registers)
    #endif
    #define SAVE_REGISTERS(inner_statement)                                  \
      do {                                                                   \
        var struct registers * registers = alloca(sizeof(struct registers)); \
        SAVE_STACK_register(registers);                                      \
        SAVE_mv_count_register(registers);                                   \
        SAVE_value1_register(registers);                                     \
        SAVE_back_trace_register(registers);                                 \
        inner_statement;                                                     \
        { var gcv_object_t* top_of_frame = STACK;                            \
          pushSTACK(fake_gcv_object((aint)callback_saved_registers));        \
          finish_frame(CALLBACK);                                            \
        }                                                                    \
        callback_saved_registers = registers;                                \
      } while(0)
    #define RESTORE_REGISTERS(inner_statement)                                \
      do {                                                                    \
        var struct registers * registers = callback_saved_registers;          \
        if (!(framecode(STACK_0) == CALLBACK_frame_info)) abort();            \
        callback_saved_registers = (struct registers *)(aint)as_oint(STACK_1);\
        skipSTACK(2);                                                         \
        inner_statement;                                                      \
        RESTORE_STACK_register(registers);                                    \
        RESTORE_mv_count_register(registers);                                 \
        RESTORE_value1_register(registers);                                   \
        RESTORE_back_trace_register(registers);                               \
      } while(0)
  #endif
  /* Saving the STACK (for asynchronous interrupts).
   If STACK is a global variable or lies in a register which is left
   untouched by operating system and library (this is the case on SUN4),
   we don't need to worry about it. */
  #if defined(STACK_register) && !defined(SUN4)
    #define HAVE_SAVED_STACK
  #endif
  /* Saving "used" registers. */
  #if defined(mv_count_register) && 0
    #define HAVE_SAVED_mv_count
  #endif
  #if defined(value1_register) && 0
    #define HAVE_SAVED_value1
  #endif
  #if defined(back_trace_register) && defined(SPARC)
    #define HAVE_SAVED_back_trace
  #endif
#endif
#ifndef HAVE_SAVED_REGISTERS
  #define SAVE_REGISTERS(inner_statement)
  #define RESTORE_REGISTERS(inner_statement)
#endif
%% #ifdef HAVE_SAVED_REGISTERS
%%   puts("#ifndef IN_MODULE_CC");
%%   #ifdef STACK_register
%%     printf("register long STACK_reg __asm__(\"%s\");\n",STACK_register);
%%   #endif
%%   #ifdef mv_count_register
%%     printf("register long mv_count_reg __asm__(\"%s\");\n",mv_count_register);
%%   #endif
%%   #ifdef value1_register
%%     printf("register long value1_reg __asm__(\"%s\");\n",value1_register);
%%   #endif
%%   #ifdef back_trace_register
%%     printf("register long back_trace_reg __asm__(\"%s\");\n",back_trace_register);
%%   #endif
%%   print("struct registers { ");
%%   #ifdef STACK_register
%%     print("long STACK_register_contents; ");
%%   #endif
%%   #ifdef mv_count_register
%%     print("long mv_count_register_contents; ");
%%   #endif
%%   #ifdef value1_register
%%     print("long value1_register_contents; ");
%%   #endif
%%   #ifdef back_trace_register
%%     print("long back_trace_register_contents; ");
%%   #endif
%%   puts("};");
%%   puts("extern struct registers * callback_saved_registers;");
%%   puts("#endif");
%% #endif

#define VALUES_IF(cond)                         \
  do { value1 = (cond) ? T : NIL; mv_count = 1; } while (0)
%% export_def(VALUES_IF(C));

#define VALUES0                                 \
  do { value1 = NIL; mv_count = 0; } while (0)
%% export_def(VALUES0);

#define VALUES1(A)                               \
  do { value1 = (A); mv_count = 1; } while (0)
%% export_def(VALUES1(A));

#define VALUES2(A,B)                            \
  do { value1 = (A); value2 = (B); mv_count = 2; } while (0)
%% export_def(VALUES2(A,B));

#define VALUES3(A,B,C)                          \
  do { value1 = (A); value2 = (B); value3 = (C); mv_count = 3; } while (0)
%% export_def(VALUES3(A,B,C));

/* ###################### Macros for C #################### */

#if !defined(return_void)
  /* To return a type of value void: return_void(...); */
  #ifdef GNU
    #define return_void  return /* 'return void;' is admissible */
  #else
    /* In general it is not legal to return `void' values. */
    #define return_void  /* Don't use 'return' for expressions of type 'void'. */
  #endif
#endif
#if defined(GNU) && defined(__GNUG__)
  /* Although legal, g++ warns about 'return void;'. Shut up the warning. */
  #undef return_void
  #define return_void
#endif

#if !defined(GNU) && !defined(inline)
  #define inline      /* inline foo() {...} --> foo() {...} */
#endif
%% puts("#if !defined(__GNUC__) && !defined(inline)");
%% puts("#define inline");
%% puts("#endif");

/* Definitions for C++-Compilers: */
#ifdef __cplusplus
  #define BEGIN_DECLS  extern "C" {
  #define END_DECLS    }
#else
  #define BEGIN_DECLS
  #define END_DECLS
#endif
%% export_def(BEGIN_DECLS);
%% export_def(END_DECLS);

/* Empty macro-arguments:
 Some compilers (ie. cc under HP-UX) seem to interpret a macro call
 foo(arg1,...,argn,) as equivalent to foo(arg1,...,argn), which will
 yield an error. _EMA_ stands for "empty macro argument".
 It will be inserted by CC_NEED_DEEMA,
 each time between comma and closing parentheses.
 It is also needed when potentially empty arguments
 are returned to other macros */

#define _EMA_

/* Concatenation of two macro-expanded tokens:
 Example:
   #undef x
   #define y 16
   CONCAT(x,y)        ==>  'x16' (not 'xy' !) */
#define CONCAT_(xxx,yyy)  xxx##yyy
#define CONCAT3_(aaa,bbb,ccc)  aaa##bbb##ccc
#define CONCAT4_(aaa,bbb,ccc,ddd)  aaa##bbb##ccc##ddd
#define CONCAT5_(aaa,bbb,ccc,ddd,eee)  aaa##bbb##ccc##ddd##eee
#define CONCAT6_(aaa,bbb,ccc,ddd,eee,fff)  aaa##bbb##ccc##ddd##eee##fff
#define CONCAT7_(aaa,bbb,ccc,ddd,eee,fff,ggg)  aaa##bbb##ccc##ddd##eee##fff##ggg
#define CONCAT(xxx,yyy)  CONCAT_(xxx,yyy)
#define CONCAT3(aaa,bbb,ccc)  CONCAT3_(aaa,bbb,ccc)
#define CONCAT4(aaa,bbb,ccc,ddd)  CONCAT4_(aaa,bbb,ccc,ddd)
#define CONCAT5(aaa,bbb,ccc,ddd,eee)  CONCAT5_(aaa,bbb,ccc,ddd,eee)
#define CONCAT6(aaa,bbb,ccc,ddd,eee,fff)  CONCAT6_(aaa,bbb,ccc,ddd,eee,fff)
#define CONCAT7(aaa,bbb,ccc,ddd,eee,fff,ggg)  CONCAT7_(aaa,bbb,ccc,ddd,eee,fff,ggg)
%% puts("#define CONCAT_(xxx,yyy)  xxx##yyy");
%% puts("#define CONCAT3_(aaa,bbb,ccc)  aaa##bbb##ccc");
%% #if notused
%% puts("#define CONCAT4_(aaa,bbb,ccc,ddd)  aaa##bbb##ccc##ddd");
%% puts("#define CONCAT5_(aaa,bbb,ccc,ddd,eee)  aaa##bbb##ccc##ddd##eee");
%% #endif
%% puts("#define CONCAT(xxx,yyy)  CONCAT_(xxx,yyy)");
%% puts("#define CONCAT3(aaa,bbb,ccc)  CONCAT3_(aaa,bbb,ccc)");
%% #if notused
%% puts("#define CONCAT4(aaa,bbb,ccc,ddd)  CONCAT4_(aaa,bbb,ccc,ddd)");
%% puts("#define CONCAT5(aaa,bbb,ccc,ddd,eee)  CONCAT5_(aaa,bbb,ccc,ddd,eee)");
%% #endif

/* Generation of goto-tag macros:
 GENTAG(end)  ==>  end116
 This allows a macro defining marks to be used more than once per function
 but still only once per source-line. */
#define GENTAG(xxx)  CONCAT(xxx,__LINE__)

/* Converting tokens to strings:
 STRING(token)  ==>  "token" */
#define STRING(token) #token
#define STRINGIFY(token) STRING(token)
%% puts("#define STRING(token) #token");
%% puts("#define STRINGIFY(token) STRING(token)");

/* Storage-Class-Specifier in top-level-declarations:
 for variables:
   global           globally visible variable
   local            variable that is only visible in the file (local)
   extern           pointer to a variable that's defined externally
 for functions:
   global           globally visible function
   local            function that is only visible in the file (local)
   extern           pointer to a function that's defined externally
   extern_C         pointer to a c-function that's defined externally
   _Noreturn        function that will never return (draft C1X)
   maygc            function that can trigger GC */
#define global
/* #define local  static */ /* done below */
/* #define extern extern */
#ifdef __cplusplus
  #define extern_C  extern "C"
#else
  #define extern_C  extern
#endif

/* A function that can trigger GC is declared either as
   - maygc, if (1) all callers must assume the worst case: that it triggers GC,
            and (2) the function uses only the 'object's passed as arguments and
            on the STACK, but no objects stored in other non-GCsafe locations.
   - / * maygc * / otherwise. If (1) is not fulfilled, the functions begins
                   with an appropriate GCTRIGGER_IF statement. If (2) is not
                   fulfilled, the GCTRIGGER call needs to mention all other
                   non-GCsafe locations whose values are used by the function,
                   such as 'value1' or 'mv_space'. */
#define maygc

/* Storage-Class-Specifier in declarations at the beginning of a block:
 var                       will lead a variable declaration
 used by utils/varbrace to allow declarations mixed with other statements */
#define var

/* Ignore C++ keyword. */
#define export export_sym

/* Swap the contents of two variables:  swap(register int, x1, x2); */
#define swap(swap_type,swap_var1,swap_var2)  \
  do { var swap_type swap_temp;                                          \
    swap_temp = swap_var1; swap_var1 = swap_var2; swap_var2 = swap_temp; \
  } while(0)

/* Marking a program line that may not be reached: NOTREACHED; */
#define NOTREACHED  error_notreached(__FILE__,__LINE__)
%% puts("#define NOTREACHED  error_notreached(__FILE__,__LINE__)");

/* Asserting an arithmetic expression: ASSERT(expr); */
#define ASSERT(expr)  do { if (!(expr)) NOTREACHED; } while(0)
%%  puts("#define ASSERT(expr)  do { if (!(expr)) NOTREACHED; } while(0)");

#include <alloca.h>
%% include_file("alloca.h");
#include <stdlib.h>
%% puts("#include <stdlib.h>");
#include <sys/types.h>
%% puts("#include <sys/types.h>");
#include <unistd.h>
#include <sys/socket.h>         /* declares select, used in stream.d */
#include <sys/select.h>
#include <locale.h>
#include <errno.h>
#include <string.h> /* declares strlen() et al */
#include <noreturn.h>      /* defines _GL_NORETURN_FUNC, _GL_NORETURN_FUNCPTR */

/* Storage-Class-Specifier for identifiers only visible in the file. */
/* Can't define this earlier, because <sys/types.h> on Haiku uses the
   identifier 'local'. */
#define local static

#define MALLOC(size,type)   (type*)malloc((size)*sizeof(type))

/* Literal constants of 64-bit integer types
 LL(nnnn)  = nnnn parsed as a sint64
 ULL(nnnn) = nnnn parsed as a uint64 */
#if defined(HAVE_LONG_LONG_INT)
  #define LL(nnnn) nnnn##LL
  #define ULL(nnnn) nnnn##ULL
#elif defined(MICROSOFT)
  #define LL(nnnn) nnnn##i64
  #define ULL(nnnn) nnnn##ui64
#endif
%% #if defined(HAVE_LONG_LONG_INT)
%%   puts("#define LL(nnnn) nnnn##LL");
%%   puts("#define ULL(nnnn) nnnn##ULL");
%% #elif defined(MICROSOFT)
%%   puts("#define LL(nnnn) nnnn##i64");
%%   puts("#define ULL(nnnn) nnnn##ui64");
%% #endif

/* Synonyms for Byte, Word, Longword:
 SBYTE   = signed 8 bit integer
 UBYTE   = unsigned 8 bit int
 SWORD   = signed 16 bit int
 UWORD   = unsigned 16 bit int
 SLONG   = signed 32 bit int
 ULONG   = unsigned 32 bit int
 On the other hand, "char" is only used as an element of a string
 You never really compute with a "char"; it might depend on
 __CHAR_UNSIGNED___! */
#if (char_bitsize==8)
  #ifdef __CHAR_UNSIGNED__
    typedef signed char  SBYTE;
  #else
    typedef char         SBYTE;
  #endif
  typedef unsigned char  UBYTE;
#else
  #error No 8 bit integer type? -- Which Integer-type has 8 Bit?
#endif
#if (short_bitsize==16)
  typedef short          SWORD;
  typedef unsigned short UWORD;
#else
  #error No 16 bit integer type? -- Which Integer-type has 16 Bit?
#endif
#if (long_bitsize==32)
  typedef long           SLONG;
  typedef unsigned long  ULONG;
#elif (int_bitsize==32)
  typedef int            SLONG;
  typedef unsigned int   ULONG;
#else
  #error No 32 bit integer type? -- Which Integer-type has 32 Bit?
#endif
#if (long_bitsize==64) && !defined(UNIX_CYGWIN)
  typedef long           SLONGLONG;
  typedef unsigned long  ULONGLONG;
  #ifndef HAVE_LONG_LONG_INT
  #define HAVE_LONG_LONG_INT
  #endif
#elif defined(MICROSOFT)
  typedef __int64           SLONGLONG;
  typedef unsigned __int64  ULONGLONG;
  #define HAVE_LONG_LONG_INT
#elif defined(HAVE_LONG_LONG_INT)
 #if defined(long_long_bitsize) && (long_long_bitsize==64)
  typedef long long           SLONGLONG;
  typedef unsigned long long  ULONGLONG;
 #else /* useless type */
  #undef HAVE_LONG_LONG_INT
 #endif
#endif
#if defined(WIDE) && !defined(HAVE_LONG_LONG_INT)
  #error No 64 bit integer type? -- Which Integer-type has 64 Bit?
#endif
%% #ifdef __CHAR_UNSIGNED__
%%   emit_typedef("signed char","SBYTE");
%% #else
%%   emit_typedef("char","SBYTE");
%% #endif
%% emit_typedef("unsigned char","UBYTE");
%% emit_typedef("short","SWORD");
%% emit_typedef("unsigned short","UWORD");
%% #if (long_bitsize==32)
%%   emit_typedef("long","SLONG");
%%   emit_typedef("unsigned long","ULONG");
%% #elif (int_bitsize==32)
%%   emit_typedef("int","SLONG");
%%   emit_typedef("unsigned int","ULONG");
%% #endif
%% #if (long_bitsize==64) && !defined(UNIX_CYGWIN)
%%   emit_typedef("long","SLONGLONG");
%%   emit_typedef("unsigned long","ULONGLONG");
%% #elif defined(MICROSOFT)
%%   emit_typedef("__int64","SLONGLONG");
%%   emit_typedef("unsigned __int64","ULONGLONG");
%% #elif defined(HAVE_LONG_LONG_INT)
%%   emit_typedef("long long","SLONGLONG");
%%   emit_typedef("unsigned long long","ULONGLONG");
%% #endif

#include <stdbool.h>  /* boolean values */
%% include_file("stdbool.h");

/* Type for signed values, results of comparisons, tertiary enums
 with values +1, 0, -1 */
typedef signed int  signean;
#define signean_plus    1 /* +1 */
#define signean_null    0 /*  0 */
#define signean_minus  -1 /* -1 */

/* Null pointers */
#ifdef __cplusplus
  #undef NULL
  #define NULL  0
#elif !(defined(INTEL) || defined(_AIX))
  #undef NULL
  #define NULL  ((void*) 0L)
#endif
%% puts("#undef NULL");
%% export_def(NULL);

/* libc I/O */
#include <stdio.h>
/* Use fprintf and printf only for format strings that take at least 1 argument.
   For literal strings, use print and fprint.
   Avoid using fputs, puts, fputc, putc, putchar directly, because these APIs
   are hard to memorize (fputs, fputc, putc don't take the stream first; puts
   outputs an extra newline) or redundant (fputc, putc, putchar are special
   cases of fputs that GCC recognizes anyway). */
#define fprint(fp,string) fputs(string,fp)
#define print(string) fputs(string,stdout)

/* A more precise classification of the operating system:
 (This test works only after at least one system header has been included.) */
#if (__GLIBC__ >= 2)
  #define UNIX_GNU /* glibc2 (may be UNIX_LINUX, UNIX_HURD or UNIX_GNU_FREEBSD) */
#endif

/* Determine the offset of a component 'ident' in a struct of the type 'type':
 See 0 as pointer to 'type', put a struct 'type' there and determine the
 address of its component 'ident' and return it as number: */
#include <stddef.h>
#ifndef offsetof
  #define offsetof(type,ident)  ((ULONG)&(((type*)0)->ident))
#endif
/* Determine the offset of an array 'ident' in a struct of the type 'type': */
#if defined(__cplusplus) || defined(MICROSOFT)
  #define offsetofa(type,ident)  offsetof(type,ident)
#else
  #define offsetofa(type,ident)  offsetof(type,ident[0])
#endif

/* alignof(type) is a constant expression, returning the alignment of type. */
#ifdef __cplusplus
  #ifdef GNU
    #define alignof(type)  __alignof__(type)
  #else
    template <class type> struct alignof_helper { char slot1; type slot2; };
    #define alignof(type)  offsetof(alignof_helper<type>, slot2)
  #endif
#else
  #define alignof(type)  offsetof(struct { char slot1; type slot2; }, slot2)
#endif

/* Unspecified length of arrays in structures:
 struct { ...; ...; type x[unspecified]; }
 Instead of sizeof(..) you'll always have to use offsetof(..,x). */
#if defined(GNU) || defined(MICROSOFT) /* GNU & MS C are able to work with arrays of length 0 */
  #define unspecified 0
#elif 0
  /* Usually one would omit the array's limit */
  #define unspecified
#else
  /* However, some old compilers will only work with this: */
  #define unspecified 1
#endif
%% export_def(unspecified);

/* Pointer arithmetics: add a given offset (measured in bytes)
 to a pointer. */
#if defined(GNU) || (pointer_bitsize > 32)
 /* Essential for GNU-C for initialization of static-variables
   (must be a bug in 'c-typeck.c' in 'initializer_constant_valid_p'):
   The only correct way, if sizeof(ULONG) < sizeof(void*): */
  #define pointerplus(pointer,offset)  ((UBYTE*)(pointer)+(offset))
#else
  /* Cheap way: */
  #define pointerplus(pointer,offset)  ((void*)((ULONG)(pointer)+(offset)))
#endif
%% export_def(pointerplus(pointer,offset));

/* Bit number n (0<=n<32)
 This is an unsigned expression, in order to avoid signed integer overflow
 in expressions like bit(31) or bit(31)-1. */
#define bit(n)  (1UL<<(n))
/* Bit number n (0<n<=32) mod 2^32 */
#define bitm(n)  (2UL<<((n)-1))
/* Bit-test of bit n in x, n constant, x an oint: */
#if !defined(SPARC)
  #define bit_test(x,n)  ((x) & bit(n))
#else
  /* On SPARC-processors, long constants are slower than shifts. */
  #if defined(SPARC64)
    #if !defined(GNU)
      #define bit_test(x,n)  \
        ((n)<12 ? ((x) & bit(n)) : ((sint64)((uint64)(x) << (63-(n))) < 0))
    #else /* the GNU-compiler will optimize boolean expressions better this way: */
      #define bit_test(x,n)  \
        (   ( ((n)<12) && ((x) & bit(n)) )                           \
         || ( ((n)>=12) && ((sint64)((uint64)(x) << (63-(n))) < 0) ) \
        )
    #endif
  #else
    #if !defined(GNU)
      #define bit_test(x,n)  \
        ((n)<12 ? ((x) & bit(n)) : ((sint32)((uint32)(x) << (31-(n))) < 0))
    #else /* the GNU-compiler will optimize boolean expressions better this way: */
      #define bit_test(x,n)  \
        (   ( ((n)<12) && ((x) & bit(n)) )                           \
         || ( ((n)>=12) && ((sint32)((uint32)(x) << (31-(n))) < 0) ) \
        )
    #endif
  #endif
#endif
/* Minus bit number n (0<=n<32) */
#define minus_bit(n)  (-1L<<(n))
/* Minus bit number n (0<n<=32) mod 2^32 */
#define minus_bitm(n)  (-2L<<((n)-1))
%% export_def(bit(n));
%% #if notused
%% export_def(bitm(n));
%% #endif
%% export_def(bit_test(x,n));
%% export_def(minus_bit(n));
%% #if notused
%% export_def(minus_bitm(n));
%% #endif

/* floor(a,b) yields for a>=0, b>0  floor(a/b).
 b should be a 'constant expression'. */
#define floor(a_from_floor,b_from_floor)  ((a_from_floor) / (b_from_floor))
%% /* FIXME: Difference between lispbibl.d and clisp.h */
%% puts("#define ifloor(a_from_floor,b_from_floor)  ((a_from_floor) / (b_from_floor))");

/* ceiling(a,b) yields for a>=0, b>0  ceiling(a/b) = floor((a+b-1)/b).
 b should be a 'constant expression'. */
#define ceiling(a_from_ceiling,b_from_ceiling)  \
  (((a_from_ceiling) + (b_from_ceiling) - 1) / (b_from_ceiling))
%% export_def(ceiling(a_from_ceiling,b_from_ceiling));

/* round_down(a,b) rounds a>=0 so that b>0 divides it.
 b should be a 'constant expression'. */
#define round_down(a_from_round,b_from_round)  \
  (floor(a_from_round,b_from_round)*(b_from_round))
%% /* FIXME: Difference between lispbibl.d and clisp.h */
%% puts("#define round_down(a_from_round,b_from_round)  (ifloor(a_from_round,b_from_round)*(b_from_round))");

/* round_up(a,b) rounds a>=0 so that b>0 divides it.
 b should be a 'constant expression'. */
#define round_up(a_from_round,b_from_round)  \
  (ceiling(a_from_round,b_from_round)*(b_from_round))
%% export_def(round_up(a_from_round,b_from_round));

/* non-local exits */
#include <setjmp.h>
%% #ifdef export_unwind_protect_macros
%%   puts("#include <setjmp.h>");
%% #endif
#if defined(UNIX) && defined(HAVE__JMP)
  /* The "_" routines are more efficient - do not save/restore signal masks,
   see https://sourceforge.net/p/clisp/mailman/message/19448465/ */
  #undef setjmp
  #undef longjmp
  #define setjmp  _setjmp
  #define longjmp  _longjmp
  #ifdef LONGJMP_RETURNS
    /* _longjmp(jmpbuf,value) can return if jmpbuf is invalid. */
    #undef longjmp
    #define longjmp(x,y)  (_longjmp(x,y), NOTREACHED)
  #endif
#endif
/* A longjmp() can only be called using an `int'.
 But if we want to use a `long' and if sizeof(int) < sizeof(long),
 we'll need a global variable: */
#if (int_bitsize == long_bitsize)
  #define setjmpl(x)  setjmp(x)
  #define longjmpl(x,y)  longjmp(x,y)
#else /* (int_bitsize < long_bitsize) */
  #ifndef MULTITHREAD
   /* MT: following is per thread in MT builds */
    extern long jmpl_value;
  #endif
  #define setjmpl(x)  (setjmp(x) ? jmpl_value : 0)
  #define longjmpl(x,y)  (jmpl_value = (y), longjmp(x,1))
#endif
%% #ifdef export_unwind_protect_macros
%%   #if (int_bitsize < long_bitsize) && !defined(MULTITHREAD)
%%     exportV(long,jmpl_value);
%%   #endif
%%   export_def(setjmpl(x))
%%   export_def(longjmpl(x))
%% #endif

/* An alloca() replacement, used for DYNAMIC_ARRAY and SAVE_NUM_STACK.
 See spvw_alloca.d. */
#if !(defined(GNU) || (defined(UNIX) && !defined(NO_ALLOCA) && !defined(SPARC)) || defined(MICROSOFT))
  #define NEED_MALLOCA
  extern void* malloca (size_t size);
  extern void freea (void* ptr);
#endif

/* Dynamically allocated array with dynamic extent:
 Example:
     var DYNAMIC_ARRAY(my_array,uintL,n);
     ...
     FREE_DYNAMIC_ARRAY(my_array);
 Attention: depending on your implementation my_array is either the array
 itself or a pointer to the array! Always use my_array only as expression! */
#if defined(GNU)
  /* can deal with dynamically allocated arrays in the machine stack
   { var uintL my_array[n]; ... } */
  #define DYNAMIC_ARRAY(arrayvar,arrayeltype,arraysize)  \
    arrayeltype arrayvar[arraysize]
  #define FREE_DYNAMIC_ARRAY(arrayvar)
#elif (defined(UNIX) && (defined(_AIX) || !defined(NO_ALLOCA))) || defined(MICROSOFT)
  /* Allocate space in machine stack.
   { var uintL* my_array = (uintL*)alloca(n*sizeof(uintL)); ... } */
  #define DYNAMIC_ARRAY(arrayvar,arrayeltype,arraysize)  \
    arrayeltype* arrayvar = (arrayeltype*)alloca((arraysize)*sizeof(arrayeltype))
  #define FREE_DYNAMIC_ARRAY(arrayvar)
  /* no error check?? */
#else
  /* Allocate space somewhere else and then free it.
   { var uintL* my_array = (uintL*)malloc(n*sizeof(uintL)); ... free(my_array); } */
  #define DYNAMIC_ARRAY(arrayvar,arrayeltype,arraysize)  \
    arrayeltype* arrayvar = (arrayeltype*)malloca((arraysize)*sizeof(arrayeltype))
  #define FREE_DYNAMIC_ARRAY(arrayvar)  freea(arrayvar)
#endif
%% export_def(DYNAMIC_ARRAY(arrayvar,arrayeltype,arraysize));
%% export_def(FREE_DYNAMIC_ARRAY(arrayvar));

/* Signed/Unsigned-Integer-types with given minumum size: */
typedef UBYTE   uint1;   /* unsigned 1 bit Integer */
typedef SBYTE   sint1;   /* signed 1 bit Integer */
typedef UBYTE   uint2;   /* unsigned 2 bit Integer */
typedef SBYTE   sint2;   /* signed 2 bit Integer */
typedef UBYTE   uint3;   /* unsigned 3 bit Integer */
typedef SBYTE   sint3;   /* signed 3 bit Integer */
typedef UBYTE   uint4;   /* unsigned 4 bit Integer */
typedef SBYTE   sint4;   /* signed 4 bit Integer */
typedef UBYTE   uint5;   /* unsigned 5 bit Integer */
typedef SBYTE   sint5;   /* signed 5 bit Integer */
typedef UBYTE   uint6;   /* unsigned 6 bit Integer */
typedef SBYTE   sint6;   /* signed 6 bit Integer */
typedef UBYTE   uint7;   /* unsigned 7 bit Integer */
typedef SBYTE   sint7;   /* signed 7 bit Integer */
typedef UBYTE   uint8;   /* unsigned 8 bit Integer */
typedef SBYTE   sint8;   /* signed 8 bit Integer */
typedef UWORD   uint9;   /* unsigned 9 bit Integer */
typedef SWORD   sint9;   /* signed 9 bit Integer */
typedef UWORD   uint10;  /* unsigned 10 bit Integer */
typedef SWORD   sint10;  /* signed 10 bit Integer */
typedef UWORD   uint11;  /* unsigned 11 bit Integer */
typedef SWORD   sint11;  /* signed 11 bit Integer */
typedef UWORD   uint12;  /* unsigned 12 bit Integer */
typedef SWORD   sint12;  /* signed 12 bit Integer */
typedef UWORD   uint13;  /* unsigned 13 bit Integer */
typedef SWORD   sint13;  /* signed 13 bit Integer */
typedef UWORD   uint14;  /* unsigned 14 bit Integer */
typedef SWORD   sint14;  /* signed 14 bit Integer */
typedef UWORD   uint15;  /* unsigned 15 bit Integer */
typedef SWORD   sint15;  /* signed 15 bit Integer */
typedef UWORD   uint16;  /* unsigned 16 bit Integer */
typedef SWORD   sint16;  /* signed 16 bit Integer */
typedef ULONG   uint17;  /* unsigned 17 bit Integer */
typedef SLONG   sint17;  /* signed 17 bit Integer */
typedef ULONG   uint18;  /* unsigned 18 bit Integer */
typedef SLONG   sint18;  /* signed 18 bit Integer */
typedef ULONG   uint19;  /* unsigned 19 bit Integer */
typedef SLONG   sint19;  /* signed 19 bit Integer */
typedef ULONG   uint20;  /* unsigned 20 bit Integer */
typedef SLONG   sint20;  /* signed 20 bit Integer */
typedef ULONG   uint21;  /* unsigned 21 bit Integer */
typedef SLONG   sint21;  /* signed 21 bit Integer */
typedef ULONG   uint22;  /* unsigned 22 bit Integer */
typedef SLONG   sint22;  /* signed 22 bit Integer */
typedef ULONG   uint23;  /* unsigned 23 bit Integer */
typedef SLONG   sint23;  /* signed 23 bit Integer */
typedef ULONG   uint24;  /* unsigned 24 bit Integer */
typedef SLONG   sint24;  /* signed 24 bit Integer */
typedef ULONG   uint25;  /* unsigned 25 bit Integer */
typedef SLONG   sint25;  /* signed 25 bit Integer */
typedef ULONG   uint26;  /* unsigned 26 bit Integer */
typedef SLONG   sint26;  /* signed 26 bit Integer */
typedef ULONG   uint27;  /* unsigned 27 bit Integer */
typedef SLONG   sint27;  /* signed 27 bit Integer */
typedef ULONG   uint28;  /* unsigned 28 bit Integer */
typedef SLONG   sint28;  /* signed 28 bit Integer */
typedef ULONG   uint29;  /* unsigned 29 bit Integer */
typedef SLONG   sint29;  /* signed 29 bit Integer */
typedef ULONG   uint30;  /* unsigned 30 bit Integer */
typedef SLONG   sint30;  /* signed 30 bit Integer */
typedef ULONG   uint31;  /* unsigned 31 bit Integer */
typedef SLONG   sint31;  /* signed 31 bit Integer */
typedef ULONG   uint32;  /* unsigned 32 bit Integer */
typedef SLONG   sint32;  /* signed 32 bit Integer */
#ifdef HAVE_LONG_LONG_INT
  typedef ULONGLONG  uint33;  /* unsigned 33 bit Integer */
  typedef SLONGLONG  sint33;  /* signed 33 bit Integer */
  typedef ULONGLONG  uint48;  /* unsigned 48 bit Integer */
  typedef SLONGLONG  sint48;  /* signed 48 bit Integer */
  typedef ULONGLONG  uint64;  /* unsigned 64 bit Integer */
  typedef SLONGLONG  sint64;  /* signed 64 bit Integer */
#endif
#define exact_uint_size_p(n) (((n)==char_bitsize)||((n)==short_bitsize)||((n)==int_bitsize)||((n)==long_bitsize))
#define signed_int_with_n_bits(n) CONCAT(sint,n)
#define unsigned_int_with_n_bits(n) CONCAT(uint,n)
/* Use 'uintn' and 'sintn' for Integers with exactly specified width.
 exact_uint_size_p(n) specifies, whether the uint with n Bits has really
 only n Bits. */
%% { int i;
%%   for (i=1; i<=8; i++) {
%%     sprintf(buf,"uint%d",i); emit_typedef("UBYTE",buf);
%%     sprintf(buf,"sint%d",i); emit_typedef("SBYTE",buf);
%%   }
%%   for (i=9; i<=16; i++) {
%%     sprintf(buf,"uint%d",i); emit_typedef("UWORD",buf);
%%     sprintf(buf,"sint%d",i); emit_typedef("SWORD",buf);
%%   }
%%   for (i=17; i<=32; i++) {
%%     sprintf(buf,"uint%d",i); emit_typedef("ULONG",buf);
%%     sprintf(buf,"sint%d",i); emit_typedef("SLONG",buf);
%%   }
%%   #ifdef HAVE_LONG_LONG_INT
%%     for (i=33; i<=64; i++)
%%       if ((i==33) || (i==48) || (i==64)) {
%%         sprintf(buf,"uint%d",i); emit_typedef("ULONGLONG",buf);
%%         sprintf(buf,"sint%d",i); emit_typedef("SLONGLONG",buf);
%%       }
%%   #endif
%% }

/* 'uintX' and 'sintX' mean unsigned bzw. signed integer - types with
 wordsize X (X=B,W,L,Q) here as well. */
#define intBsize 8
  typedef signed_int_with_n_bits(intBsize)    sintB;
  typedef unsigned_int_with_n_bits(intBsize)  uintB;
#define intWsize 16
  typedef signed_int_with_n_bits(intWsize)    sintW;
  typedef unsigned_int_with_n_bits(intWsize)  uintW;
#define intLsize 32
  typedef signed_int_with_n_bits(intLsize)    sintL;
  typedef unsigned_int_with_n_bits(intLsize)  uintL;
#if (long_bitsize==64) || defined(MIPS64) || defined(IA64) || defined(AMD64) || defined(ARM64)
  /* Machine has real 64-bit integers in hardware. */
  #define intQsize 64
  typedef signed_int_with_n_bits(intQsize)    sintQ;
  typedef unsigned_int_with_n_bits(intQsize)  uintQ;
  /* Bit number n (0<=n<64)
   This is an uintQ expression, in order to avoid signed integer overflow
   in expressions like bitQ(63) or bitQ(63)-1. */
  #define bitQ(n)  ((uintQ)1<<(n))
  /* Bit number n (0<n<=64) mod 2^64 */
  #define bitQm(n)  ((uintQ)2<<((n)-1))
  /* Bit-test of bit n in x, n constant, x an uintQ or sintQ: */
  #define bitQ_test(x,n)  ((x) & bitQ(n))
  /* Minus bit number n (0<=n<64) */
  #define minus_bitQ(n)  (-(sintQ)1<<(n))
  /* Minus bit number n (0<n<=64) mod 2^64 */
  #define minus_bitQm(n)  (-(sintQ)2<<((n)-1))
  typedef sintQ  sintL2;
  typedef uintQ  uintL2;
#else
  /* Emulate 64-Bit-numbers using two 32-Bit-numbers. */
  typedef struct { sintL hi; uintL lo; } sintL2; /* signed 64 Bit integer */
  typedef struct { uintL hi; uintL lo; } uintL2; /* unsigned 64 Bit integer */
#endif
/* Use 'uintX' and 'sintX' for Integers with approximately given width
 and a minumum of storage space. */
%% sprintf(buf,"sint%d",intBsize); emit_typedef(buf,"sintB");
%% sprintf(buf,"uint%d",intBsize); emit_typedef(buf,"uintB");
%% sprintf(buf,"sint%d",intWsize); emit_typedef(buf,"sintW");
%% sprintf(buf,"uint%d",intWsize); emit_typedef(buf,"uintW");
%% sprintf(buf,"sint%d",intLsize); emit_typedef(buf,"sintL");
%% sprintf(buf,"uint%d",intLsize); emit_typedef(buf,"uintL");
%% #if notused
%% #ifdef intQsize
%%   sprintf(buf,"sint%d",intQsize); emit_typedef(buf,"sintQ");
%%   sprintf(buf,"uint%d",intQsize); emit_typedef(buf,"uintQ");
%% #else
%%   emit_typedef("struct { sintL hi; uintL lo; }","sintL2");
%%   emit_typedef("struct { uintL hi; uintL lo; }","uintL2");
%% #endif
%% #endif

/* From here on 'uintP' and 'sintP' are unsigned or signed integer types,
 which are as wide as void* - pointers */
typedef signed_int_with_n_bits(pointer_bitsize)    sintP;
typedef unsigned_int_with_n_bits(pointer_bitsize)  uintP;
%% sprintf(buf,"sint%d",pointer_bitsize); emit_typedef(buf,"sintP");
%% sprintf(buf,"uint%d",pointer_bitsize); emit_typedef(buf,"uintP");

/* From here on 'uintXY' and 'sintXY' mean unsigned or signed integer types,
 with word sizes X or Y (X,Y=B,W,L). */
#if defined(M68K)
  /* The 68000 offers good processing of uintB and uintW, especially
   DBRA-commands for uintW. */
  #define intBWsize intBsize
  #define intWLsize intWsize
  #define intBWLsize intBsize
#elif defined(SPARC) || defined(HPPA) || defined(MIPS) || defined(POWERPC) || defined(S390)
  /* The Sparc-processor computes rather badly with uintB and uintW.
   Other 32-Bit-processoren have similar weaknesses. */
  #define intBWsize intWsize
  #define intWLsize intLsize
  #define intBWLsize intLsize
#elif defined(I80386) || defined(AMD64)
  /* If you compute using uintB and uintW on a 80386, there will be many
   Zero-Extends, that will - because there aren't enough registers - load
   other variables into memory, which is rather unnecessary. */
  #define intBWsize intWsize
  #define intWLsize intLsize
  #define intBWLsize intLsize
#elif defined(ARM)
  /* The ARM computes very badly when it uses uintB and uintW. */
  #define intBWsize intBsize
  #define intWLsize intLsize
  #define intBWLsize intLsize
#elif defined(DECALPHA) || defined(IA64)
  /* 64-bit processors also compute badly with uintB and uintW. */
  #define intBWsize intWsize
  #define intWLsize intLsize
  #define intBWLsize intLsize
#elif 1
  /* For unknown CPUs, we prefer slightly suboptimal code to a compilation failure. */
  #define intBWsize intWsize
  #define intWLsize intLsize
  #define intBWLsize intLsize
#else
  #error Preferred integer sizes depend on CPU -- readjust intBWsize, intWLsize, intBWLsize!
#endif
typedef signed_int_with_n_bits(intBWsize)     sintBW;
typedef unsigned_int_with_n_bits(intBWsize)   uintBW;
typedef signed_int_with_n_bits(intWLsize)     sintWL;
typedef unsigned_int_with_n_bits(intWLsize)   uintWL;
typedef signed_int_with_n_bits(intBWLsize)    sintBWL;
typedef unsigned_int_with_n_bits(intBWLsize)  uintBWL;
/* Use 'uintXY' and 'sintXY' for integers with given minumum width,
 that allow easy computations. */
%% #if notused
%% sprintf(buf,"sint%d",intBWsize); emit_typedef(buf,"sintBW");
%% sprintf(buf,"uint%d",intBWsize); emit_typedef(buf,"uintBW");
%% sprintf(buf,"sint%d",intWLsize); emit_typedef(buf,"sintWL");
%% #endif
%% sprintf(buf,"uint%d",intWLsize); emit_typedef(buf,"uintWL");
%% #if notused
%% sprintf(buf,"sint%d",intBWLsize); emit_typedef(buf,"sintBWL");
%% #endif
%% sprintf(buf,"uint%d",intBWLsize); emit_typedef(buf,"uintBWL");

/* Loop that will excute as statement a certain number of times:
 dotimesW(countvar,count,statement);  if count fits into a uintW,
 dotimesL(countvar,count,statement);  if  count only fits into a uintL,
 dotimesV(countvar,count,statement);  if  count only fits into a uintV,
 dotimespW(countvar,count,statement);  if count fits into a uintW and is >0,
 dotimespL(countvar,count,statement);  if count fits only into a uintL and is >0.
 dotimespV(countvar,count,statement);  if count fits only into a uintV and is >0.
 The variable countvar has to be declared previously, be of type uintW or uintL,
 and will be changed by this expression.
 It must not be used in the statement itself!
 The expression count will only be evaluated once (at the beginning). */
#if defined(GNU) && defined(M68K)
  /* GNU-C on a 680X0 can be persuaded to use the DBRA-instruction: */
  #define fast_dotimesW
  /* To find out, what the best was to 'persuade' GNU-C is, check the
   code, that'll be generated for spvw.d:gc_markphase().
   Or a small test program (dbratest.c), that is compiled with
   "gcc -O6 -da -S dbratest.c", and take a look at dbratest.s
   and dbratest.c.flow as well as dbratest.c.combine. */
  #if (__GNUC__<2) /* GNU C Version 1 */
    #define dotimesW_(countvar_from_dotimesW,count_from_dotimesW,statement_from_dotimesW)  \
      { countvar_from_dotimesW = (count_from_dotimesW);     \
        if (!(countvar_from_dotimesW==0))                   \
          { countvar_from_dotimesW--;                       \
            do {statement_from_dotimesW}                    \
            while ((sintW)--countvar_from_dotimesW != -1);  \
      }   }
    #define dotimespW_(countvar_from_dotimespW,count_from_dotimespW,statement_from_dotimespW)  \
      { countvar_from_dotimespW = (count_from_dotimespW)-1;                         \
        do {statement_from_dotimespW} while ((sintW)--countvar_from_dotimespW != -1); \
      }
  #else
    #define dotimesW_(countvar_from_dotimesW,count_from_dotimesW,statement_from_dotimesW)  \
      { countvar_from_dotimesW = (count_from_dotimesW);        \
        if (!(countvar_from_dotimesW==0))                      \
          { countvar_from_dotimesW--;                          \
            do {statement_from_dotimesW}                       \
            while ((sintW)(--countvar_from_dotimesW)+1 != 0);  \
      }   }
    #define dotimespW_(countvar_from_dotimespW,count_from_dotimespW,statement_from_dotimespW)  \
      { countvar_from_dotimespW = (count_from_dotimespW)-1;                            \
        do {statement_from_dotimespW} while ((sintW)(--countvar_from_dotimespW)+1 != 0); \
      }
  #endif
#else
  #define dotimesW_(countvar_from_dotimesW,count_from_dotimesW,statement_from_dotimesW)  \
    { countvar_from_dotimesW = (count_from_dotimesW);         \
      while (countvar_from_dotimesW != 0)                       \
        {statement_from_dotimesW; countvar_from_dotimesW--; } \
    }
  #define dotimespW_(countvar_from_dotimespW,count_from_dotimespW,statement_from_dotimespW)  \
    { countvar_from_dotimespW = (count_from_dotimespW);                   \
      do {statement_from_dotimespW} while (--countvar_from_dotimespW != 0); \
    }
#endif
#if defined(GNU) && defined(M68K)
  /* GNU-C on a 680X0 can be 'persuaded' to use the DBRA-instruction
   in an intelligent manner: */
  #define fast_dotimesL
  #define dotimesL_(countvar_from_dotimesL,count_from_dotimesL,statement_from_dotimesL)  \
    { countvar_from_dotimesL = (count_from_dotimesL);           \
      if (!(countvar_from_dotimesL==0))                         \
        { countvar_from_dotimesL--;                             \
          do {statement_from_dotimesL}                          \
          while ((sintL)(--countvar_from_dotimesL)  !=  -1);    \
    }   }
  #define dotimespL_(countvar_from_dotimespL,count_from_dotimespL,statement_from_dotimespL)  \
    { countvar_from_dotimespL = (count_from_dotimespL)-1;                             \
      do {statement_from_dotimespL} while ((sintL)(--countvar_from_dotimespL)  !=  -1); \
    }
#endif
#ifndef dotimesL_
  #define dotimesL_(countvar_from_dotimesL,count_from_dotimesL,statement_from_dotimesL)  \
    { countvar_from_dotimesL = (count_from_dotimesL);         \
      while (countvar_from_dotimesL != 0)                       \
        {statement_from_dotimesL; countvar_from_dotimesL--; } \
    }
  #define dotimespL_(countvar_from_dotimespL,count_from_dotimespL,statement_from_dotimespL)  \
    { countvar_from_dotimespL = (count_from_dotimespL);                   \
      do {statement_from_dotimespL} while (--countvar_from_dotimespL != 0); \
    }
#endif
#if defined(GNU) && defined(__OPTIMIZE__)
  /* It happened twice to me that I used dotimesL on a
   variable of type uintC. I check for that now, so that
   Joerg and Marcus won't have to search for that anymore.
   The GCC will optimize the dummy-call away, if things go by plan.
   If not, you'll see a linker error. */
  #define dotimes_check_sizeof(countvar,type)  \
    if (!(sizeof(countvar)==sizeof(type))) { dotimes_called_with_count_of_wrong_size(); }
  extern void dotimes_called_with_count_of_wrong_size (void); /* non-existing function */
%% exportF(void,dotimes_called_with_count_of_wrong_size,(void));
#else
  #define dotimes_check_sizeof(countvar,type)
#endif
#define dotimesW(countvar_from_dotimesW,count_from_dotimesW,statement_from_dotimesW) \
  do { dotimes_check_sizeof(countvar_from_dotimesW,uintW); \
    dotimesW_(countvar_from_dotimesW,count_from_dotimesW,statement_from_dotimesW); \
  } while(0)
#define dotimespW(countvar_from_dotimespW,count_from_dotimespW,statement_from_dotimespW) \
  do { dotimes_check_sizeof(countvar_from_dotimespW,uintW); \
    dotimespW_(countvar_from_dotimespW,count_from_dotimespW,statement_from_dotimespW); \
  } while(0)
#define dotimesL(countvar_from_dotimesL,count_from_dotimesL,statement_from_dotimesL) \
  do { dotimes_check_sizeof(countvar_from_dotimesL,uintL); \
    dotimesL_(countvar_from_dotimesL,count_from_dotimesL,statement_from_dotimesL); \
  } while(0)
#define dotimespL(countvar_from_dotimespL,count_from_dotimespL,statement_from_dotimespL) \
  do { dotimes_check_sizeof(countvar_from_dotimespL,uintL); \
    dotimespL_(countvar_from_dotimespL,count_from_dotimespL,statement_from_dotimespL); \
  } while(0)
#define dotimesV(countvar_from_dotimesV,count_from_dotimesV,statement_from_dotimesV) \
  do { dotimes_check_sizeof(countvar_from_dotimesV,uintV); \
    dotimesL_(countvar_from_dotimesV,count_from_dotimesV,statement_from_dotimesV); \
  } while(0)
#define dotimespV(countvar_from_dotimespV,count_from_dotimespV,statement_from_dotimespV) \
  do { dotimes_check_sizeof(countvar_from_dotimespV,uintV); \
    dotimespL_(countvar_from_dotimespV,count_from_dotimespV,statement_from_dotimespV); \
  } while(0)
/* doconsttimes(count,statement);
 executes a statement count times (count times the code!),
 where count is a constant-expression >=0, <=8. */
#define doconsttimes(count_from_doconsttimes,statement_from_doconsttimes) \
 do { if (0 < (count_from_doconsttimes)) { statement_from_doconsttimes; } \
      if (1 < (count_from_doconsttimes)) { statement_from_doconsttimes; } \
      if (2 < (count_from_doconsttimes)) { statement_from_doconsttimes; } \
      if (3 < (count_from_doconsttimes)) { statement_from_doconsttimes; } \
      if (4 < (count_from_doconsttimes)) { statement_from_doconsttimes; } \
      if (5 < (count_from_doconsttimes)) { statement_from_doconsttimes; } \
      if (6 < (count_from_doconsttimes)) { statement_from_doconsttimes; } \
      if (7 < (count_from_doconsttimes)) { statement_from_doconsttimes; } \
 } while(0)
/* DOCONSTTIMES(count,macroname);
 calls the macro macroname count times (count times the code!),
 where count is a constant-expression >=0, <=8.
 And macroname will get the values 0,...,count-1 in sequence. */
#define DOCONSTTIMES(count_from_DOCONSTTIMES,macroname_from_DOCONSTTIMES)  \
 do { if (0 < (count_from_DOCONSTTIMES)) { macroname_from_DOCONSTTIMES((0 < (count_from_DOCONSTTIMES) ? 0 : 0)); } \
      if (1 < (count_from_DOCONSTTIMES)) { macroname_from_DOCONSTTIMES((1 < (count_from_DOCONSTTIMES) ? 1 : 0)); } \
      if (2 < (count_from_DOCONSTTIMES)) { macroname_from_DOCONSTTIMES((2 < (count_from_DOCONSTTIMES) ? 2 : 0)); } \
      if (3 < (count_from_DOCONSTTIMES)) { macroname_from_DOCONSTTIMES((3 < (count_from_DOCONSTTIMES) ? 3 : 0)); } \
      if (4 < (count_from_DOCONSTTIMES)) { macroname_from_DOCONSTTIMES((4 < (count_from_DOCONSTTIMES) ? 4 : 0)); } \
      if (5 < (count_from_DOCONSTTIMES)) { macroname_from_DOCONSTTIMES((5 < (count_from_DOCONSTTIMES) ? 5 : 0)); } \
      if (6 < (count_from_DOCONSTTIMES)) { macroname_from_DOCONSTTIMES((6 < (count_from_DOCONSTTIMES) ? 6 : 0)); } \
      if (7 < (count_from_DOCONSTTIMES)) { macroname_from_DOCONSTTIMES((7 < (count_from_DOCONSTTIMES) ? 7 : 0)); } \
 } while(0)

/* From here on  uintC means an unsigned integer type, that'll allow
 easy counting. Subset relation: uintW <= uintC <= uintL. */
#define intCsize intWLsize
#define uintC uintWL
#define sintC sintWL
#if (intCsize==intWsize)
  #define dotimesC dotimesW
  #define dotimespC dotimespW
#endif
#if (intCsize==intLsize)
  #define dotimesC dotimesL
  #define dotimespC dotimespL
#endif
/* Use 'uintC' for counters, which are small most of the time. */
%% export_def(uintC);
%% #if notused
%% export_def(sintC);
%% #endif

/* The arithmetics use "digit sequences" of "digits".
 They are unsigned ints with intDsize bits (should be =8 or =16 or =32).
 If  HAVE_DD: "double-digits" are unsigned ints with 2*intDsize<=32 bits. */
#if 1 /* defined(M68K) || defined(I80386) || defined(SPARC) || defined(HPPA) || defined(MIPS) || defined(POWERPC) || defined(ARM) || defined(DECALPHA) || defined(IA64) || defined(AMD64) || defined(S390) || defined(RISCV64) || ... */
  #define intDsize 32
  #define intDDsize 64  /* = 2*intDsize */
  #define log2_intDsize  5  /* = log2(intDsize) */
#else
  #error Preferred digit size depends on CPU -- readjust intDsize!
#endif
typedef unsigned_int_with_n_bits(intDsize)  uintD;
typedef signed_int_with_n_bits(intDsize)    sintD;
#if (intDDsize<=32) || ((intDDsize<=64) && ((long_bitsize==64) || defined(MIPS64) || defined(IA64) || defined(AMD64) || defined(ARM64)))
  #define HAVE_DD 1
  typedef unsigned_int_with_n_bits(intDDsize)  uintDD;
  typedef signed_int_with_n_bits(intDDsize)    sintDD;
#else
  #define HAVE_DD 0
#endif
%% #if notused
%% sprintf(buf,"sint%d",intDsize); emit_typedef(buf,"sintD");
%% #endif
%% sprintf(buf,"uint%d",intDsize); emit_typedef(buf,"uintD");


/* ###################### OS-related routines  #################### */

/* general standard constants for control chars: */
#define BS    8  /*  #\Backspace     Backspace */
#define TAB   9  /*  #\Tab           Tabulator */
#define LF   10  /*  #\Linefeed      linefeed */
#define CR   13  /*  #\Return        carriage return */
#define PG   12  /*  #\Page          form feed, new page */

/* Desired reaction when an I/O operation cannot be completed immediately. */
typedef enum {
  persev_full,      /* Continue the I/O operation until the whole buffer is
                       handled or EOF or an error occurred. May hang. */
  persev_partial,   /* Continue the I/O operation until some (non-empty) part
                       of the buffer is handled or EOF or an error occurred.
                       May hang. */
  persev_immediate, /* Act immediately. Perform I/O only if we know in advance
                       that it will not block. In case of doubt, perform it
                       anyway. May return with 0 bytes handled. Does usually
                       not hang. */
  persev_bonus      /* Act immediately. Perform I/O only if we know in advance
                       that it will not block. In case of doubt, don't perform
                       it. May return with 0 bytes handled. Does not hang. */
} perseverance_t;
%% printf("typedef enum { persev_full=%d, persev_partial=%d, persev_immediate=%d, persev_bonus=%d } perseverance_t;\n",persev_full,persev_partial,persev_immediate,persev_bonus);

#if defined(UNIX) || defined(WIN32)

#ifdef UNIX
  #include "unix.c"
#endif
#ifdef WIN32_NATIVE
  #include "win32.c"
#endif
%% #if defined(UNIX)
%%   emit_typedef("int","Handle");
%%   emit_typedef("int","SOCKET");
%%   #ifdef UNIX_CYGWIN
%%     puts("#include <windows.h>");
%%     puts("#undef WIN32");
%%     exportF(long,time_t_from_filetime,(const FILETIME * ptr));
%%     exportF(void,time_t_to_filetime,(time_t time_in, FILETIME * out));
%%   #endif
%% #elif defined(WIN32_NATIVE)
%%   puts("#include <winsock2.h>"); /* defines SOCKET */
%%   puts("#include <windows.h>");
%%   export_def(Handle);
%% #else
%%   puts("#error what is Handle on your platform?!");
%% #endif
%% #if defined(UNIX)
%%   exportF(ssize_t,fd_read,(int fd, void* buf, size_t nbyte, perseverance_t persev));
%%   exportF(ssize_t,fd_write,(int fd, const void* buf, size_t nbyte, perseverance_t persev));
%% #elif defined(WIN32_NATIVE)
%%   exportF(ssize_t,fd_read,(Handle fd, void* buf, size_t nbyte, perseverance_t persev));
%%   exportF(ssize_t,fd_write,(Handle fd, const void* buf, size_t nbyte, perseverance_t persev));
%% #endif

/* execute statement on interrupt:
 interruptp(statement); */
#if defined(UNIX) || defined(WIN32_NATIVE)
  /* A keyboard interrupt (signal SIGINT, generated by Ctrl-C)
   is pending for one second. It can be treated with 'interruptp' in
   a continuable manner in that time. After this time has passed, the
   program will be interrupted and cannot be continued. */
  #if !defined(MULTITHREAD)
   #define PENDING_INTERRUPTS
   /* Flag telling whether a Ctrl-C has been seen and is waiting to be
      handled. */
   extern uintB interrupt_pending;
   #define interruptp(statement)  if (interrupt_pending) { statement; }
  #else
   /* In MT interrupt_pending and interuptp are not used at all.
      actually even tast_break() is obsolete.*/
   #define interruptp(statement)
  #endif
#endif
/* used by EVAL, IO, SPVW, STREAM */

#endif /* UNIX || WIN32 */

#if (defined(UNIX) || defined(WIN32_NATIVE)) && defined(HAVE_LIBSIGSEGV)
  /* Support for fault handling. */
  #include <sigsegv.h>
  #if defined(UNIX_CYGWIN)
    /* <sigsegv.h> includes <windows.h> */
    #undef WIN32
  #endif
#endif

/* Ignoring of a value (instead of assigning it to a variable)
 unused ...
 <sigsegv.h> includes <windows.h> which uses unused! */
#ifndef unused                  /* win32.d defines unused */
 #ifdef GNU     /* to prevent a gcc-warning "statement with no effect" */
  #define unused  (void)
 #else
  #define unused
 #endif
#endif

/* Consensys and Solaris: "#define DS 3", "#define SP ESP", "#define EAX 11".
 Grr... */
#undef DS
#undef SP
#undef EAX
/* 386BSD does "#define CBLOCK 64". Grr... */
#undef CBLOCK
/* AIX 3.2.5 does "#define hz 100". Grr... */
#undef hz

#ifdef UNIX
  /* Handling of UNIX errors
   OS_error();
   > int errno: error code */
    extern _Noreturn void OS_error (void);
  /* used by SPVW, STREAM, PATHNAME, GRAPH */
#endif
#if defined(WIN32_NATIVE)
  /* Handling of Win32 errors
   OS_error();
   > GetLastError(): error code */
    extern _Noreturn void OS_error (void);
#endif
#if defined(DEBUG_OS_ERROR)
  /* Show the file and line number of the caller of OS_error(). For debugging. */
  #define OS_error()  \
    (fprintf(stderr,"\n[%s:%d] ",__FILE__,__LINE__), (OS_error)())
#endif
%% exportE(OS_error,(void));

/* Handling of ANSI C errors
 ANSIC_error();
 > int errno: error code */
#ifdef UNIX
  #define ANSIC_error OS_error
%% export_def(ANSIC_error);
#else
  _Noreturn extern void ANSIC_error (void);
%% exportE(ANSIC_error,(void));
#endif
/* used by SPVW, STREAM */

/* ##################### Further system-dependencies #################### */

/* At first dependencies that are visible to the LISP-level: */

/* setting of the table of character-names: */
#ifdef WIN32
  #define WIN32_CHARNAMES
#endif
#ifdef UNIX
  #define UNIX_CHARNAMES
#endif
/* When changed: extend CONSTOBJ, CHARSTRG, FORMAT.LISP. */

/* Whether to use the GNU gettext library for internationalization: */
#if defined(ENABLE_NLS) && !defined(NO_GETTEXT)
  #define GNU_GETTEXT
#endif

/* Whether to create a stream *KEYBOARD-INPUT*
 and whether it will be used for the stream *TERMINAL-IO*: */
#if (defined(UNIX) && !defined(NO_TERMCAP_NCURSES)) || defined(WIN32_NATIVE)
  #define KEYBOARD
  #if 0
    #define TERMINAL_USES_KEYBOARD
  #endif
#endif
/* When changed: extend stream.d, keyboard.lisp */

/* Whether to use the GNU readline library for *TERMINAL-IO*: */
#if defined(HAVE_READLINE) && !defined(NO_READLINE)
  #define GNU_READLINE
#endif

/* Whether there are Window-streams and a package SCREEN: */
#if (defined(UNIX) && !defined(NO_TERMCAP_NCURSES)) || defined(WIN32_NATIVE)
  #define SCREEN
#endif
/* When changed: extend stream.d (loads of work!). */

/* Whether there are Pipe-streams: */
#if defined(UNIX) || defined(WIN32_NATIVE)
  #define PIPES
  #if defined(UNIX) || defined(WIN32_NATIVE)
    #define PIPES2  /* bidirectional pipes */
  #endif
#endif
/* When changed: extend stream.d and runprog.lisp. */

/* If the system has sockets, we support socket streams:
 We assume that if we have gethostbyname(), we have a networking OS
 (Unix or Win32). Then we decide independently about UNIX domain connections
 and TCP/IP connections. */
#if defined(HAVE_GETHOSTBYNAME) /* ==> defined(UNIX) || defined(WIN32_NATIVE) */
  #ifdef HAVE_SYS_UN_H  /* have <sys/un.h> and Unix domain sockets? */
    #define UNIXCONN  /* use Unix domain sockets */
  #endif
  #define TCPCONN  /* use TCP/IP sockets */
  /* Now, which kinds of socket streams: */
  #define X11SOCKETS  /* works even without TCPCONN (very young Linux) */
  #define SOCKET_STREAMS
#endif
/* When changed: extend stream.d, socket.d */

/* Whether there are generic streams: */
#if 1
  #define GENERIC_STREAMS
#endif
/* When changed: do nothing */

/* Whether a foreign function interface is provided: */
#if (defined(UNIX) && !defined(UNIX_BINARY_DISTRIB)) || defined(DYNAMIC_FFI)
  #define HAVE_FFI
#endif
/* When changed: ?? */

/* Now the ones that are only relevant internally: */

/* Whether the GC closes files that aren't referenced any longer: */
#if defined(UNIX) || defined(WIN32)
  #define GC_CLOSES_FILES
#endif
/* When changed: do nothing */

/* How time is measured: */
#if defined(UNIX)
  #define TIME_UNIX
#elif defined(WIN32_NATIVE)
  #define TIME_WIN32
#else
  #error how do you measure time on this system
#endif
/* When changed: extend time.d */

/* Whether the operating system provides virtual memory. */
#if (defined(UNIX) || defined(WIN32)) && !defined(NO_VIRTUAL_MEMORY)
  #define VIRTUAL_MEMORY
#endif
/* When changed: do nothing */

/* Where the operating system allocates memory (via mmap or malloc).
   Some of these locations may be randomized to some extent; cf.
   "address space layout randomization" (ASLR)
   <https://en.wikipedia.org/wiki/Address_space_layout_randomization>.

   To determine the address space layout:
   1) Attempt to build clisp with
        $ make -f Makefile.devel build-porting32-gcc-portability
      (for 32-bit ABIs) or
        $ make -f Makefile.devel build-porting64-gcc-portability
      The build does not need to complete; you only need the lisp.run
      program.
        $ cd build-porting{32,64}-gcc-portability
   2) Gain an understanding of the address space layout:
        $ grep HIGHEST config.status
        $ grep RANGE config.status
        $ ./lisp.run -mm
      If some randomization is present, run
        $ for i in `seq 100`; do ./lisp.run -mm; done | sort
      You can use the tools listed in unix/PLATFORMS here.
   3) Define the values
        MAPPABLE_ADDRESS_RANGE_START
        MAPPABLE_ADDRESS_RANGE_END
      here. Or for two ranges:
        MAPPABLE_ADDRESS_RANGE1_START
        MAPPABLE_ADDRESS_RANGE1_END
        MAPPABLE_ADDRESS_RANGE2_START
        MAPPABLE_ADDRESS_RANGE2_END
      The _START values should be multiples of the page size.
      The _END values + 1 should be multiples of the page size.
      If no such range can be determined, define ADDRESS_RANGE_RANDOMIZED.
   4) Recompile lisp.run and run
        $ make marc.out
*/
/* Sort order: Keep this list sorted by
     1. word size (32-bit before 64-bit),
     2. operating system (Linux, *BSD, macOS, proprietary Unices, Windows)
     3. CPU and ABI (alphabetically) */
#if !defined(WIDE_HARD)
  /* 32-bit platforms */
  #if defined(UNIX_LINUX) && defined(AMD64)
    /* On Linux/x86_64 with 32-bit x32 ABI:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0x01000000UL ... 0x02000000UL
       SHLIB_ADDRESS_RANGE  = 0xF7000000UL
       STACK_ADDRESS_RANGE  = 0xFF000000UL
       There is room from 0x03000000UL to 0xF7000000UL, but let's keep some
       distance. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x10000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0xEFFFFFFFUL
  #endif
  #if defined(UNIX_LINUX) && defined(ARM)
    /* On Linux/arm:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0x00000000UL ... 0x02000000UL
       SHLIB_ADDRESS_RANGE  = 0xB6000000UL
       STACK_ADDRESS_RANGE  = 0xBE000000UL
       On Linux/arm64 with CC="arm-linux-gnueabihf-gcc-4.8":
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0x00000000UL
       SHLIB_ADDRESS_RANGE  = 0x40000000UL or 0xF7000000UL
       STACK_ADDRESS_RANGE  = 0xFF000000UL
       There is room from 0x43000000UL to 0xB6000000UL, but let's keep some
       distance. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x48000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0xAFFFFFFFUL
  #endif
  #if defined(UNIX_LINUX) && defined(HPPA)
    /* On Linux/hppa in qemu user-mode emulation:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0x00000000UL
       SHLIB_ADDRESS_RANGE  = 0xF6000000UL
       STACK_ADDRESS_RANGE  = 0xF6000000UL
       There is room from 0x00000000UL to 0xF6000000UL, but let's keep some
       distance. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x08000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0xEFFFFFFFUL
  #endif
  #if defined(UNIX_LINUX) && defined(I80386)
    /* On Linux/i386:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL (Debian 9.1) or 0x08000000UL (Ubuntu 17.04)
       MALLOC_ADDRESS_RANGE = 0x01000000UL (Debian 9.1) or 0x08000000UL (Ubuntu 17.04)
       SHLIB_ADDRESS_RANGE  = 0xB7000000UL
       STACK_ADDRESS_RANGE  = 0xBF000000UL
       On Linux/x86_64 with 32-bit i386 ABI:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x08000000UL
       MALLOC_ADDRESS_RANGE = 0x08000000UL ... 0x0A000000UL
       SHLIB_ADDRESS_RANGE  = 0xF7000000UL
       STACK_ADDRESS_RANGE  = 0xFF000000UL
       On some Linux/i386 Debian build machines:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x56000000UL
       MALLOC_ADDRESS_RANGE = 0x56000000UL ... 0x58000000UL
       SHLIB_ADDRESS_RANGE  = 0xF7000000UL
       STACK_ADDRESS_RANGE  = 0xFF000000UL
       On Linux/i386 (Alpine 3.7):
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x10000000UL ... 0x1B000000UL
       MALLOC_ADDRESS_RANGE = 0x10000000UL ... 0x1B000000UL
       SHLIB_ADDRESS_RANGE  = 0x48000000UL ... 0x57000000UL
       STACK_ADDRESS_RANGE  = 0x58000000UL ... 0x5F000000UL
       Additionally, the ranges 0x70000000UL ... 0x78FFFFFFUL, 0xA8000000UL ... 0xB7FFFFFFUL
       are used, and addresses >= 0x60000000UL cannot be allocated.
       There is room from 0x1C000000UL to 0x48000000UL,
       but let's keep some distance. */
    /* Force the same CODE_ADDRESS_RANGE across platforms. */
    #if (CODE_ADDRESS_RANGE == 0x00000000UL || CODE_ADDRESS_RANGE == 0x08000000UL || (CODE_ADDRESS_RANGE >= 0x10000000UL && CODE_ADDRESS_RANGE < 0x1C000000UL) || CODE_ADDRESS_RANGE == 0x56000000UL)
      #undef CODE_ADDRESS_RANGE
      #define CODE_ADDRESS_RANGE 0x5F000000UL
    #endif
    #define MAPPABLE_ADDRESS_RANGE_START 0x1E000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x45FFFFFFUL
  #endif
  #if defined(UNIX_LINUX) && defined(M68K)
    /* On Linux/m68k in qemu user-mode emulation:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x80000000UL
       MALLOC_ADDRESS_RANGE = 0x80000000UL
       SHLIB_ADDRESS_RANGE  = 0xF6000000UL
       STACK_ADDRESS_RANGE  = 0xF6000000UL
       There is room from 0x00000000UL to 0x80000000UL, but let's keep some
       distance. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x01000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x7EFFFFFFUL
  #endif
  #if defined(UNIX_LINUX) && (defined(MIPS) || defined(MIPS64))
   #if !(_MIPS_SIM == _ABIN32)
    /* On Linux/mipseb and Linux/mipsel with o32 ABI:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0x00000000UL
       SHLIB_ADDRESS_RANGE  = 0x77000000UL
       STACK_ADDRESS_RANGE  = 0x7F000000UL
       On Linux/mips64eb with o32 ABI:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0x00000000UL
       SHLIB_ADDRESS_RANGE  = 0x2B000000UL
       STACK_ADDRESS_RANGE  = 0x7F000000UL
       On some Linux/mips Debian build machines:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x55000000UL ... 0x56000000UL
       MALLOC_ADDRESS_RANGE = 0x55000000UL ... 0x56000000UL
       SHLIB_ADDRESS_RANGE  = 0x77000000UL
       STACK_ADDRESS_RANGE  = 0x7F000000UL
       There is room from 0x2C000000UL to 0x53000000UL
       and from 0x58000000UL to 0x76000000UL. */
    /* Force the same CODE_ADDRESS_RANGE across platforms. */
    #if (CODE_ADDRESS_RANGE == 0x00000000UL || (CODE_ADDRESS_RANGE >= 0x55000000UL && CODE_ADDRESS_RANGE < 0x57000000UL))
      #undef CODE_ADDRESS_RANGE
      #define CODE_ADDRESS_RANGE 0x57000000UL
    #endif
    #define MAPPABLE_ADDRESS_RANGE_START 0x2C000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x52FFFFFFUL
   #else
    /* On Linux/mips64eb and Linux/mips64el with n32 ABI:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x10000000UL
       MALLOC_ADDRESS_RANGE = 0x10000000UL
       SHLIB_ADDRESS_RANGE  = 0x77000000UL or (sometimes) 0x2B000000UL
       STACK_ADDRESS_RANGE  = 0x7F000000UL
       There is room from 0x11000000UL to 0x76000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x2C000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x75FFFFFFUL
   #endif
  #endif
  #if defined(UNIX_LINUX) && defined(POWERPC)
    /* On Linux/powerpc64 with 32-bit ABI:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x10000000UL
       MALLOC_ADDRESS_RANGE = 0x10000000UL
       SHLIB_ADDRESS_RANGE  = 0x0F000000UL
       STACK_ADDRESS_RANGE  = 0xFF000000UL
       On some Linux/powerpc machines (Linux 4.6):
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x10000000UL
       MALLOC_ADDRESS_RANGE = 0x10000000UL
       SHLIB_ADDRESS_RANGE  = 0x0F000000UL
       STACK_ADDRESS_RANGE  = 0xBF000000UL
       On some Linux/powerpc build machines:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0x00000000UL
       SHLIB_ADDRESS_RANGE  = 0x00000000UL
       STACK_ADDRESS_RANGE  = 0xFF000000UL
       On some Linux/powerpc build machines:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x20000000UL
       MALLOC_ADDRESS_RANGE = 0x20000000UL
       SHLIB_ADDRESS_RANGE  = 0x20000000UL
       STACK_ADDRESS_RANGE  = 0xFF000000UL
       There is room from 0x21000000UL to 0xB7000000UL, but let's keep some
       distance. */
    /* Force the same CODE_ADDRESS_RANGE across platforms. */
    #if (CODE_ADDRESS_RANGE == 0x00000000UL || CODE_ADDRESS_RANGE == 0x10000000UL || CODE_ADDRESS_RANGE == 0x20000000UL)
      #undef CODE_ADDRESS_RANGE
      #define CODE_ADDRESS_RANGE 0x30000000UL
    #endif
    #define MAPPABLE_ADDRESS_RANGE_START 0x30000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0xAFFFFFFFUL
  #endif
  #if defined(UNIX_LINUX) && defined(S390)
    /* On Linux/s390x with 32-bit ABI:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0x00000000UL
       SHLIB_ADDRESS_RANGE  = 0x7C000000UL or 0x7D000000UL
       STACK_ADDRESS_RANGE  = 0x7F000000UL
       There is room from 0x01000000UL to 0x7C000000UL, but let's keep some
       distance. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x08000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x6FFFFFFFUL
  #endif
  #if defined(UNIX_LINUX) && defined(SPARC)
    /* On Linux 3.2/sparc64 with 32-bit ABI:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0x00000000UL
       SHLIB_ADDRESS_RANGE  = 0x70000000UL
       STACK_ADDRESS_RANGE  = 0xFF000000UL
       On Linux 4.13/sparc64 with 32-bit ABI:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x70000000UL
       MALLOC_ADDRESS_RANGE = 0x70000000UL
       SHLIB_ADDRESS_RANGE  = 0xF7000000UL
       STACK_ADDRESS_RANGE  = 0xFF000000UL
       There is room from 0x01000000UL to 0x70000000UL. */
    /* Force the same CODE_ADDRESS_RANGE across platforms. */
    #if (CODE_ADDRESS_RANGE == 0x00000000UL || CODE_ADDRESS_RANGE == 0x70000000UL)
      #undef CODE_ADDRESS_RANGE
      #define CODE_ADDRESS_RANGE 0x70000000UL
    #endif
    #define MAPPABLE_ADDRESS_RANGE_START 0x01000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x6FFFFFFFUL
  #endif
  #if defined(UNIX_HURD) && defined(I80386)
    /* On Hurd/i386:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL or 0x08000000UL
       MALLOC_ADDRESS_RANGE = 0x08000000UL
       SHLIB_ADDRESS_RANGE  = 0x01000000UL
       STACK_ADDRESS_RANGE  = 0x01000000UL
       Addresses >= 0xC0000000UL are not mmapable.
       There is room from 0x11000000UL to 0xBFFFFFFFUL, but let's keep some
       distance. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x18000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0xBFFFFFFFUL
  #endif
  #if (defined(__FreeBSD__) || defined(UNIX_GNU_FREEBSD)) && defined(I80386)
    /* On FreeBSD/i386:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x08000000UL
       MALLOC_ADDRESS_RANGE = 0x28000000UL
       SHLIB_ADDRESS_RANGE  = 0x28000000UL
       STACK_ADDRESS_RANGE  = 0xBF000000UL
       On GNU/kFreeBSD/i386:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x08000000UL
       MALLOC_ADDRESS_RANGE = 0x08000000UL
       SHLIB_ADDRESS_RANGE  = 0x28000000UL
       STACK_ADDRESS_RANGE  = 0xBF000000UL
       There is room from 0x29000000UL to 0xBF000000UL, but let's keep some
       distance. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x30000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0xB7FFFFFFUL
  #endif
  #if defined(__DragonFly__) && defined(I80386)
    /* On DragonFly/i386:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x08000000UL
       MALLOC_ADDRESS_RANGE = 0x28000000UL
       SHLIB_ADDRESS_RANGE  = 0x28000000UL
       STACK_ADDRESS_RANGE  = 0x9F000000UL
       There is room from 0x29000000UL to 0x9F000000UL, but let's keep some
       distance. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x30000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x97FFFFFFUL
  #endif
  #if defined(UNIX_NETBSD) && defined(I80386)
    /* On NetBSD/i386:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x08000000UL
       MALLOC_ADDRESS_RANGE = 0xBB000000UL
       SHLIB_ADDRESS_RANGE  = 0xBB000000UL
       STACK_ADDRESS_RANGE  = 0xBF000000UL
       There is room from 0x09000000UL to 0xBB000000UL, but let's keep some
       distance. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x10000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0xAFFFFFFFUL
  #endif
  #if defined(UNIX_NETBSD) && defined(SPARC)
    /* On NetBSD/sparc:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0xED000000UL
       SHLIB_ADDRESS_RANGE  = 0xED000000UL
       STACK_ADDRESS_RANGE  = 0xEF000000UL
       On NetBSD/sparc64 with 32-bit ABI:
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0x20000000UL ... 0x30000000UL
       SHLIB_ADDRESS_RANGE  = 0x20000000UL ... 0x40000000UL
       STACK_ADDRESS_RANGE  = 0xFF000000UL
       There is room from 0x41000000UL to 0xED000000UL, but let's keep some
       distance. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x48000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0xDFFFFFFFUL
  #endif
  #if defined(UNIX_OPENBSD) && defined(I80386)
    /* On OpenBSD/i386:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   varies from 0x14000000UL to 0x1B000000UL
       MALLOC_ADDRESS_RANGE varies from 0x74000000UL..0x84000000UL to 0x7A000000UL..0x8A000000UL
       SHLIB_ADDRESS_RANGE  varies from 0x21000000UL to 0x2F000000UL
       STACK_ADDRESS_RANGE  = 0xCF000000UL
       The allocated ranges are randomized across the ranges
       from 0x00000000UL to 0x0FFFFFFFUL,
       from 0x14000000UL to 0x1BFFFFFFUL,
       from 0x20000000UL to 0x2FFFFFFFUL,
       from 0x34000000UL to 0x3BFFFFFFUL,
       from 0x74000000UL to 0x8BFFFFFFUL,
       from 0xCD000000UL to 0xCDFFFFFFUL,
       from 0xCF000000UL to 0xCFFFFFFFUL.
       There is room from 0x3C000000UL to 0x74000000UL
       and from 0x8C000000UL to 0xCD000000UL, but let's keep some
       distance. */
    /* Force the same CODE_ADDRESS_RANGE across platforms. */
    #if (CODE_ADDRESS_RANGE >= 0x14000000UL || CODE_ADDRESS_RANGE <= 0x1B000000UL)
      #undef CODE_ADDRESS_RANGE
      #define CODE_ADDRESS_RANGE 0x1B000000UL
    #endif
    #define MAPPABLE_ADDRESS_RANGE_START 0x40000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x6FFFFFFFUL
  #endif
  #if defined(UNIX_MACOSX) && defined(I80386)
    /* On Mac OS X 10.5.8/x86_64 with 32-bit ABI:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0x00000000UL
       SHLIB_ADDRESS_RANGE  = 0x8F000000UL ... 0xA0000000UL
       STACK_ADDRESS_RANGE  = 0xB0000000UL ... 0xBF000000UL
       There is room from 0x02000000UL to 0x8F000000UL, but let's keep some
       distance. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x10000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x87FFFFFFUL
  #endif
  #if defined(UNIX_MACOSX) && defined(POWERPC)
    /* On Mac OS X 10.5.8/PowerPC and
       on Mac OS X 10.5.8/x86_64 with CC="gcc -arch ppc":
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0x00000000UL and 0x01000000UL
       SHLIB_ADDRESS_RANGE  = 0x8F000000UL ... 0xA0000000UL
       STACK_ADDRESS_RANGE  = 0xBC000000UL ... 0xBF000000UL
       There is room from 0x02000000UL to 0x80000000UL, but let's keep some
       distance. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x10000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x7FFFFFFFUL
  #endif
  #if defined(UNIX_AIX) && defined(POWERPC)
    /* On AIX/POWER with 32-bit ABI:
       CODE_ADDRESS_RANGE   = 0x10000000UL (r-x) and 0x20000000UL (rw-)
       MALLOC_ADDRESS_RANGE = 0x20000000UL or 0x30000000UL
       SHLIB_ADDRESS_RANGE  = 0xD0000000UL (r-x) and 0xF0000000UL (rw-)
       STACK_ADDRESS_RANGE  = 0x2F000000UL
       See also "The 32-bit AIX Virtual Memory Model"
       <https://www.ibm.com/support/knowledgecenter/en/SSYKE2_8.0.0/com.ibm.java.aix.80.doc/diag/problem_determination/aix_mem_32.html>.
       There is room from 0x40000000UL to 0xD0000000UL. */
    /* Force the same CODE_ADDRESS_RANGE across platforms. */
    #if (CODE_ADDRESS_RANGE == 0x10000000UL || CODE_ADDRESS_RANGE == 0x20000000UL)
      #undef CODE_ADDRESS_RANGE
      #define CODE_ADDRESS_RANGE 0x30000000UL
    #endif
    #define MAPPABLE_ADDRESS_RANGE_START 0x40000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0xCFFFFFFFUL
  #endif
  #if defined(UNIX_SUNOS5) && defined(I80386)
    /* On Solaris 10/x86_64 with 32-bit ABI:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x08000000UL
       MALLOC_ADDRESS_RANGE = 0x08000000UL
       SHLIB_ADDRESS_RANGE  = 0xFE000000UL
       STACK_ADDRESS_RANGE  = 0x08000000UL
       On Solaris 11/x86_64 with 32-bit ABI:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x08000000UL
       MALLOC_ADDRESS_RANGE = 0x08000000UL
       SHLIB_ADDRESS_RANGE  = 0xFE000000UL
       STACK_ADDRESS_RANGE  = 0xFE000000UL
       There is room from 0x09000000UL to 0xFE000000UL, but let's keep some
       distance. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x10000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0xEFFFFFFFUL
  #endif
  #if defined(UNIX_SUNOS5) && defined(SPARC)
    /* On Solaris 10/sparc64 with 32-bit ABI:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0x00000000UL
       SHLIB_ADDRESS_RANGE  = 0xFF000000UL
       STACK_ADDRESS_RANGE  = 0xFF000000UL
       There is room from 0x01000000UL to 0xFE000000UL, but let's keep some
       distance. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x10000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0xEFFFFFFFUL
  #endif
  #if defined(UNIX_HAIKU) && defined(I80386)
    /* On Haiku/i386:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x00000000UL ... 0x02000000UL
       MALLOC_ADDRESS_RANGE = 0x18000000UL ... 0x19000000UL
       SHLIB_ADDRESS_RANGE  = 0x00000000UL ... 0x02000000UL
       STACK_ADDRESS_RANGE  = 0x70000000UL ... 0x73000000UL
       There is room from 0x1A000000UL to 0x60000000UL, but let's keep some
       distance. */
    /* Force the same CODE_ADDRESS_RANGE across platforms. */
    #if (CODE_ADDRESS_RANGE == 0x00000000UL || CODE_ADDRESS_RANGE == 0x01000000UL || CODE_ADDRESS_RANGE == 0x02000000UL)
      #undef CODE_ADDRESS_RANGE
      #define CODE_ADDRESS_RANGE 0x03000000UL
    #endif
    #define MAPPABLE_ADDRESS_RANGE_START 0x20000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x5FFFFFFFUL
  #endif
  #if defined(UNIX_MINIX) && defined(I80386)
    /* On Minix/i386:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 30
       CODE_ADDRESS_RANGE   = 0x08000000UL
       MALLOC_ADDRESS_RANGE = 0x08000000UL
       SHLIB_ADDRESS_RANGE  = 0x08000000UL
       STACK_ADDRESS_RANGE  = 0xEF000000UL
       There is room from 0x09000000UL to 0xEF000000UL, but let's keep some
       distance. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x10000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0xDFFFFFFFUL
  #endif
  #if defined(UNIX_CYGWIN) && defined(I80386)
    /* On Cygwin, running on Windows 10 (64-bit):
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0x20000000UL
       SHLIB_ADDRESS_RANGE  = 0x00000000UL
       STACK_ADDRESS_RANGE  = 0x00000000UL
       There is room from 0x21000000UL to 0x5D000000UL
       (from which addresses < 0x38000000UL cannot be allocated)
       and from 0x80000000UL to 0xFF000000UL. */
    #if 0 /* both are possible */
      #define MAPPABLE_ADDRESS_RANGE_START 0x38000000UL
      #define MAPPABLE_ADDRESS_RANGE_END   0x57FFFFFFUL
    #else
      #define MAPPABLE_ADDRESS_RANGE_START 0x80000000UL
      #define MAPPABLE_ADDRESS_RANGE_END   0xFEFFFFFFUL
    #endif
  #endif
  #if defined(WIN32_NATIVE) && defined(I80386)
    /* On mingw, running on Windows 10 (64-bit):
       CODE_ADDRESS_RANGE   = 0x00000000UL
       MALLOC_ADDRESS_RANGE = 0x00000000UL
       SHLIB_ADDRESS_RANGE  = 0x00000000UL
       STACK_ADDRESS_RANGE  = 0x00000000UL
       Addresses >= 0x80000000UL are not mmapable.
       There is room from 0x03000000UL to 0x57000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x03000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x4FFFFFFFUL
  #endif
#else
  /* 64-bit platforms */
  #if defined(UNIX_LINUX) && defined(AMD64)
    /* On Linux/x86_64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 46
       CODE_ADDRESS_RANGE   = 0x0000000000000000UL or 0x000055xxxx000000UL or 0x000056xxxx000000UL (in Linux 4.13)
       MALLOC_ADDRESS_RANGE = 0x000000000x000000UL or 0x000055xxxx000000UL (in Linux 4.13)
       SHLIB_ADDRESS_RANGE  = 0x00000034F5000000UL or 0x0000003844000000UL or 0x00007Fxxxx000000UL
                              or 0x0000002Axx000000UL or 0x0000002Bxx000000UL (in Linux 3.2, when invoked by 'make')
                              or 0x00007Fxxxx000000UL (in WSL)
       STACK_ADDRESS_RANGE  = 0x0000007FBF000000UL or 0x00007FFDxx000000UL or 0x00007FFEBF000000UL or 0x00007FFECF000000UL
       On Linux/x86_64 (Alpine 3.7):
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 46
       CODE_ADDRESS_RANGE   = 0x000000xxxx000000UL ... 0x00000Fxxxx000000UL
       MALLOC_ADDRESS_RANGE = 0x000000xxxx000000UL ... 0x00000Fxxxx000000UL
       SHLIB_ADDRESS_RANGE  = 0x000060xxxx000000UL ... 0x00007Fxxxx000000UL
       STACK_ADDRESS_RANGE  = 0x000070xxxx000000UL ... 0x00007Fxxxx000000UL
       There is room from 0x100000000000UL to 0x2A0000000000UL
       and           from 0x400000000000UL to 0x540000000000UL
       and           from 0x580000000000UL to 0x600000000000UL. */
    /* Force the same CODE_ADDRESS_RANGE across platforms. */
    #if ((CODE_ADDRESS_RANGE >= 0x0000000000000000UL && CODE_ADDRESS_RANGE < 0x0000100000000000UL) || (CODE_ADDRESS_RANGE >= 0x0000550000000000UL && CODE_ADDRESS_RANGE < 0x0000570000000000UL))
      #undef CODE_ADDRESS_RANGE
      #define CODE_ADDRESS_RANGE 0x00005FFF00000000UL
    #endif
    #define MAPPABLE_ADDRESS_RANGE_START 0x100000000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x1FFFFFFFFFFFUL
  #endif
  #if defined(UNIX_LINUX) && defined(ARM64)
    /* On Linux/arm64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 38
       CODE_ADDRESS_RANGE   = 0x0000000000000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000009000000UL or 0x000000000E000000UL or 0x000000001E000000UL or 0x0000000021000000UL
       SHLIB_ADDRESS_RANGE  = 0x0000002000000000UL or 0x0000007F82000000UL
       STACK_ADDRESS_RANGE  = 0x0000007FC8000000UL or 0x0000007F7A000000UL or 0x0000007FF0000000UL or 0x0000007FFC000000UL
       On Linux/arm64 build.opensuse.org or Debian build machines:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 47
       CODE_ADDRESS_RANGE   = 0x0000000000000000UL or 0x0000AAAAxx000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000000000000UL or 0x0000AAAAxx000000UL
       SHLIB_ADDRESS_RANGE  = 0x0000400000000000UL or 0x0000FFFFxx000000UL
       STACK_ADDRESS_RANGE  = 0x0000FFFFFF000000UL or 0x0000FFFFxx000000UL
       There is room from 0x002100000000UL to 0x007F00000000UL. */
    /* Force the same CODE_ADDRESS_RANGE across platforms. */
    #if (CODE_ADDRESS_RANGE == 0x0000000000000000UL || (CODE_ADDRESS_RANGE >= 0x0000AAAA00000000UL && CODE_ADDRESS_RANGE < 0x0000AAAB00000000UL))
      #undef CODE_ADDRESS_RANGE
      #define CODE_ADDRESS_RANGE 0x0000AAAAFF000000UL
    #endif
    #define MAPPABLE_ADDRESS_RANGE_START 0x002100000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x007EFFFFFFFFUL
  #endif
  #if defined(UNIX_LINUX) && defined(DECALPHA)
    /* On Linux/alpha:
       Virtual address limit: 0x0000040000000000UL
       CODE_ADDRESS_RANGE   = 0x0000000120000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000120000000UL or 0x0000015555000000UL
       SHLIB_ADDRESS_RANGE  = 0x0000015555000000UL or 0x0000020000000000UL
       STACK_ADDRESS_RANGE  = 0x000000011F000000UL
       There is room from 0x000200000000UL to 0x015000000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x000200000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x014FFFFFFFFFUL
  #endif
  #if defined(UNIX_LINUX) && defined(IA64)
    /* On Linux/ia64:
       Bits 63..61 = region code,
       Bits 60..39 all zero or all one,
       Virtual address limit: R*2^61..R*2^61+2^39, (R+1)*2^61-2^39..(R+1)*2^61.
       CODE_ADDRESS_RANGE   = 0x4000000000000000UL
       MALLOC_ADDRESS_RANGE = 0x6000000000000000UL
       SHLIB_ADDRESS_RANGE  = 0x2000000000000000UL
       STACK_ADDRESS_RANGE  = 0x60000FFFFF000000UL or 0x9FFFFFFFFF000000UL
       A vdso at              0xA000000000000000UL
       There is room from 0x6000000100000000UL to 0x600007FF00000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x6000000100000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x600007FEFFFFFFFFUL
  #endif
  #if defined(UNIX_LINUX) && defined(MIPS64)
    /* On Linux/mips64eb and Linux/mips64el with 64-bit ABI:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 39
       CODE_ADDRESS_RANGE   = 0x0000000120000000UL
       MALLOC_ADDRESS_RANGE = 0x000000012x000000UL
       SHLIB_ADDRESS_RANGE  = 0x000000555F000000UL or 0x000000FFEF000000UL..0x000000FFF2000000UL
       STACK_ADDRESS_RANGE  = 0x000000FFFF000000UL
       On some Linux/mips64 Debian build machines:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 39
       CODE_ADDRESS_RANGE   = 0x000000AABx000000UL
       MALLOC_ADDRESS_RANGE = 0x000000AABx000000UL
       SHLIB_ADDRESS_RANGE  = 0x000000FFFx000000UL
       STACK_ADDRESS_RANGE  = 0x000000FFFF000000UL
       There is room from 0x005600000000UL to 0x00AA00000000UL. */
    /* Force the same CODE_ADDRESS_RANGE across platforms. */
    #if (CODE_ADDRESS_RANGE == 0x0000000000000000UL || (CODE_ADDRESS_RANGE >= 0x000000AA00000000UL && CODE_ADDRESS_RANGE < 0x000000AB00000000UL))
      #undef CODE_ADDRESS_RANGE
      #define CODE_ADDRESS_RANGE 0x000000AAFF000000UL
    #endif
    #define MAPPABLE_ADDRESS_RANGE_START 0x005600000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x00A9FFFFFFFFUL
  #endif
  #if defined(UNIX_LINUX) && defined(POWERPC64)
    /* On Linux/powerpc64 and Linux/powerpc64le:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 45
       CODE_ADDRESS_RANGE   = 0x0000000010000000UL or 0x0000000102000000UL or 0x0000000124000000UL
       MALLOC_ADDRESS_RANGE = 0x00000100xx000000UL
       SHLIB_ADDRESS_RANGE  = 0x00003FFF79000000UL ... 0x00003FFFA0000000UL
       STACK_ADDRESS_RANGE  = 0x00003FFFD1000000UL ... 0x00003FFFFF000000UL
       On some Linux/powerpc64 Debian build machines:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 48
       CODE_ADDRESS_RANGE   = 0x000000012x000000UL
       MALLOC_ADDRESS_RANGE = 0x00000100xx000000UL
       SHLIB_ADDRESS_RANGE  = 0x00007FFF82000000UL ... 0x00007FFFA4000000UL
       STACK_ADDRESS_RANGE  = 0x00007FFFD6000000UL ... 0x00007FFFE5000000UL
       There is room from 0x011000000000UL to 0x3FF000000000UL. */
    /* Force the same CODE_ADDRESS_RANGE across platforms. */
    #if (CODE_ADDRESS_RANGE < 0x0000000200000000UL)
      #undef CODE_ADDRESS_RANGE
      #define CODE_ADDRESS_RANGE 0x00000001FF000000UL
    #endif
    #define MAPPABLE_ADDRESS_RANGE_START 0x011000000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x3FEFFFFFFFFFUL
  #endif
  #if defined(UNIX_LINUX) && defined(RISCV64)
    /* On Linux/riscv64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 37
       CODE_ADDRESS_RANGE   = 0x0000000000000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000000000000UL
       SHLIB_ADDRESS_RANGE  = 0x0000002000000000UL
       STACK_ADDRESS_RANGE  = 0x0000003FFF000000UL
       There is room from 0x000080000000UL to 0x002000000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x000080000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x001FFFFFFFFFUL
  #endif
  #if defined(UNIX_LINUX) && defined(S390_64)
    /* On Linux/s390x:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 52
       CODE_ADDRESS_RANGE   = 0x0000000080000000UL or 0x0000000106000000UL or 0x0000000119000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000081000000UL ... 0x00000000BE000000UL or 0x000000012x000000UL
       SHLIB_ADDRESS_RANGE  = 0x000003FF81000000UL ... 0x000003FFFD000000UL
       STACK_ADDRESS_RANGE  = 0x000003FFC2000000UL ... 0x000003FFFF000000UL
       On Linux/s390x build.opensuse.org machines:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 62
       CODE_ADDRESS_RANGE   = 0x0000000001000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000001000000UL
       SHLIB_ADDRESS_RANGE  = 0x0000020000000000UL
       STACK_ADDRESS_RANGE  = 0x000003FFFF000000UL
       There is room from 0x000200000000UL to 0x020000000000UL. */
    /* Force the same CODE_ADDRESS_RANGE across platforms. */
    #if (CODE_ADDRESS_RANGE < 0x0000000200000000UL)
      #undef CODE_ADDRESS_RANGE
      #define CODE_ADDRESS_RANGE 0x00000001FF000000UL
    #endif
    #define MAPPABLE_ADDRESS_RANGE_START 0x000200000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x01FFFFFFFFFFUL
  #endif
  #if defined(UNIX_LINUX) && defined(SPARC64)
    /* On Linux 3.2/sparc64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 42
       CODE_ADDRESS_RANGE   = 0x0000000000000000UL or 0x0000010000000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000000000000UL or 0x0000010000000000UL
       SHLIB_ADDRESS_RANGE  = 0xFFFFF80100000000UL
       STACK_ADDRESS_RANGE  = 0x000007FEFF000000UL
       One some Linux/sparc64 Debian build machines:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 46
       CODE_ADDRESS_RANGE   = 0x0000010000000000UL
       MALLOC_ADDRESS_RANGE = 0x0000010000000000UL
       SHLIB_ADDRESS_RANGE  = 0xFFFF800100000000UL
       STACK_ADDRESS_RANGE  = 0x000007FEFF000000UL
       On Linux 4.13/sparc64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 50
       CODE_ADDRESS_RANGE   = 0x0000010000000000UL
       MALLOC_ADDRESS_RANGE = 0x0000010000000000UL
       SHLIB_ADDRESS_RANGE  = 0xFFF8000100000000UL
       STACK_ADDRESS_RANGE  = 0x000007FEFF000000UL
       There is room from 0x0000018000000000UL to 0x000007FE00000000UL. */
    /* Force the same CODE_ADDRESS_RANGE across platforms. */
    #if (CODE_ADDRESS_RANGE == 0x0000000000000000UL || CODE_ADDRESS_RANGE == 0x0000010000000000UL)
      #undef CODE_ADDRESS_RANGE
      #define CODE_ADDRESS_RANGE 0x0000010000000000UL
    #endif
    #define MAPPABLE_ADDRESS_RANGE_START 0x0000018000000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x000007FDFFFFFFFFUL
  #endif
  #if (defined(UNIX_FREEBSD) || defined(UNIX_GNU_FREEBSD)) && defined(AMD64)
    /* On FreeBSD/x86_64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 46
       CODE_ADDRESS_RANGE   = 0x0000000000000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000000000000UL or 0x0000000801000000UL
       SHLIB_ADDRESS_RANGE  = 0x0000000800000000UL
       STACK_ADDRESS_RANGE  = 0x00007FFFFF000000UL
       On GNU/kFreeBSD/x86_64:
       CODE_ADDRESS_RANGE   = 0x0000000000000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000000000000UL
       SHLIB_ADDRESS_RANGE  = 0x0000000800000000UL
       STACK_ADDRESS_RANGE  = 0x00007FFFFF000000UL
       There is room from 0x000900000000UL to 0x7FFF00000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x000900000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x7FFEFFFFFFFFUL
  #endif
  #if defined(UNIX_FREEBSD) && defined(ARM64)
    /* On FreeBSD/arm64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 47
       CODE_ADDRESS_RANGE   = 0x0000000000000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000040000000UL
       SHLIB_ADDRESS_RANGE  = 0x0000000040000000UL
       STACK_ADDRESS_RANGE  = 0x0000FFFFFF000000UL
       There is room from 0x000100000000UL to 0xFFFF00000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x000100000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0xFFFEFFFFFFFFUL
  #endif
  #if defined(UNIX_FREEBSD) && defined(DECALPHA)
    /* On FreeBSD/alpha:
       CODE_ADDRESS_RANGE   = 0x0000000120000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000120000000UL
       SHLIB_ADDRESS_RANGE  = 0x0000000160000000UL
       STACK_ADDRESS_RANGE  = 0x0000000011000000UL
       There is room from 0x000200000000UL to 0x03FF00000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x000200000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x03FF00000000UL
  #endif
  #if defined(UNIX_NETBSD) && defined(AMD64)
    /* On NetBSD/x86_64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 46
       CODE_ADDRESS_RANGE   = 0x0000000000000000UL
       MALLOC_ADDRESS_RANGE = 0x00007F7FF7000000UL
       SHLIB_ADDRESS_RANGE  = 0x00007F7FF7000000UL
       STACK_ADDRESS_RANGE  = 0x00007F7FFF000000UL
       There is room from 0x000100000000UL to 0x7F7000000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x000100000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x7F6FFFFFFFFFUL
  #endif
  #if defined(UNIX_NETBSD) && defined(DECALPHA)
    /* On NetBSD/alpha:
       CODE_ADDRESS_RANGE   = 0x0000000120000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000120000000UL
       SHLIB_ADDRESS_RANGE  = 0x0000000160000000UL
       STACK_ADDRESS_RANGE  = 0x00000001FF000000UL
       There is room from 0x000200000000UL to 0x03FF00000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x000200000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x03FF00000000UL
  #endif
  #if defined(UNIX_NETBSD) && defined(SPARC64)
    /* On NetBSD/sparc64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 42
       CODE_ADDRESS_RANGE   = 0x0000000000000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000040000000UL
       SHLIB_ADDRESS_RANGE  = 0x0000000040000000UL
       STACK_ADDRESS_RANGE  = 0xFFFFFFFFFF000000UL
       Addresses >= 0x07FFFFFFE000UL are not mmapable.
       There is room from 0x000048000000UL to 0x07F000000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x000048000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x07EFFFFFFFFFUL
  #endif
  #if defined(UNIX_OPENBSD) && defined(AMD64)
    /* On OpenBSD/x86_64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 46
       CODE_ADDRESS_RANGE   = 0x000000xxxx000000UL ... 0x00001Fxxxx000000UL
       MALLOC_ADDRESS_RANGE = 0x000000xxxx000000UL ... 0x00001Fxxxx000000UL
       SHLIB_ADDRESS_RANGE  = 0x000001xxxx000000UL ... 0x00001Fxxxx000000UL
       STACK_ADDRESS_RANGE  = 0x00007F7FFx000000UL
       The allocated ranges are randomized across the range
       from 0x000000000000UL to 0x200000000000UL.
       There is room from 0x200000000000UL to 0x7F0000000000UL. */
    /* Force the same CODE_ADDRESS_RANGE across platforms. */
    #if (CODE_ADDRESS_RANGE <= 0x00001FFFFF000000UL)
      #undef CODE_ADDRESS_RANGE
      #define CODE_ADDRESS_RANGE 0x00001FFFFF000000UL
    #endif
    #define MAPPABLE_ADDRESS_RANGE_START 0x200000000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x7EFFFFFFFFFFUL
  #endif
  #if defined(UNIX_MACOSX) && defined(AMD64)
    /* On Mac OS X 10.5.8/x86_64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 46
       CODE_ADDRESS_RANGE   = 0x0000000100000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000100000000UL
       SHLIB_ADDRESS_RANGE  = 0x00007FFF70000000UL
       STACK_ADDRESS_RANGE  = 0x00007FFF5F000000UL
       On Mac OS X 10.12.4/x86_64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 46
       CODE_ADDRESS_RANGE   = 0x0000000105000000UL
       MALLOC_ADDRESS_RANGE = 0x00007FAB97000000UL
       SHLIB_ADDRESS_RANGE  = 0x00007FAAF7000000UL
       STACK_ADDRESS_RANGE  = 0x00007FFF5E000000UL
       There is room from 0x000200000000UL to 0x7F0000000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x000200000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x7EFFFFFFFFFFUL
    #if 0 /* old */
      /* On MacOS X 10.5 in 64-bit mode, the available addresses for mmap and
         mach_vm_allocate are in the range 2^33...2^47. */
      #define MAPPABLE_ADDRESS_RANGE_START 0x000200000000UL
      #define MAPPABLE_ADDRESS_RANGE_END   0x3FFFFFFFFFFFUL
    #endif
  #endif
  #if defined(UNIX_MACOSX) && defined(ARM64)
    /* On macOS 11 (Darwin 20.4.0)/arm64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 46
       CODE_ADDRESS_RANGE   = 0x0000000102000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000134000000UL
       SHLIB_ADDRESS_RANGE  = 0x0000000136000000UL
       STACK_ADDRESS_RANGE  = 0x000000016B000000UL
       There is room from 0x000300000000UL to 0x000F00000000UL
       and from           0x008000000000UL to 0x400000000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x010000000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x01FFFFFFFFFFUL
  #endif
  #if defined(UNIX_AIX) && defined(POWERPC64)
    /* On AIX/POWER with 64-bit ABI:
       CODE_ADDRESS_RANGE   = 0x0000000100000000UL (r-x) or 0x0000000110000000UL (rw-)
       MALLOC_ADDRESS_RANGE = 0x0000000110000000UL
       SHLIB_ADDRESS_RANGE  = 0x0900000xx0000000UL (r-x) or 0x09001000x0000000UL (rw-)
       STACK_ADDRESS_RANGE  = 0x0FFFFFFFFF000000UL or 0x1000000000000000UL
       Addresses >= 0x0800000000000000UL are not mmapable.
       There is room from 0x0000000200000000UL to 0x0800000000000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x0000000200000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x07FFFFFFFFFFFFFFUL
  #endif
  #if defined(UNIX_SUNOS5) && defined(AMD64)
    /* On Solaris 10/x86_64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 46
       CODE_ADDRESS_RANGE   = 0x0000000000000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000000000000UL
       SHLIB_ADDRESS_RANGE  = 0xFFFFFD7FFF000000UL
       STACK_ADDRESS_RANGE  = 0xFFFFFD7FFF000000UL
       On Solaris 11/x86_64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 46
       CODE_ADDRESS_RANGE   = 0x0000000000000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000000000000UL
       SHLIB_ADDRESS_RANGE  = 0xFFFF80FFBF000000UL
       STACK_ADDRESS_RANGE  = 0xFFFF80FFBF000000UL
       There is room from 0x0000000100000000UL to 0x00007FFF00000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x0000000100000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x00007FFEFFFFFFFFUL
  #endif
  #if defined(UNIX_SUNOS5) && defined(SPARC64)
    /* On Solaris 10/sparc64:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 46
       CODE_ADDRESS_RANGE   = 0x0000000100000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000100000000UL
       SHLIB_ADDRESS_RANGE  = 0xFFFFFFFF7E000000UL
       STACK_ADDRESS_RANGE  = 0xFFFFFFFF7F000000UL
       There is room from 0x0000000200000000UL to 0x00007FFF00000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x0000000200000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x00007FFEFFFFFFFFUL
  #endif
  #if defined(UNIX_CYGWIN) && defined(AMD64)
    /* On Cygwin, running on Windows 10:
       MMAP_FIXED_ADDRESS_HIGHEST_BIT = 46
       CODE_ADDRESS_RANGE   = 0x0000000100000000UL
       MALLOC_ADDRESS_RANGE = 0x0000000600000000UL
       SHLIB_ADDRESS_RANGE  = 0x00000000FF000000UL
       STACK_ADDRESS_RANGE  = 0x00000000FF000000UL
       There is room from 0x000700000000UL to 0x06FF00000000UL. */
    #define MAPPABLE_ADDRESS_RANGE_START 0x000700000000UL
    #define MAPPABLE_ADDRESS_RANGE_END   0x05FFFFFFFFFFUL
  #endif
#endif
/* When changed: do nothing */

/* Whether the operating system is capable of sending interruptions
 (Ctrl-C and others) as signal: */
#if defined(UNIX)
  #define HAVE_SIGNALS
#endif
/* Whether we can even react to asynchronous signals:
 (If WIDE && !WIDE_HARD, writing a pointer is usually no elementary
 operation anymore!) */
#if (defined(WIDE) && !defined(WIDE_HARD)) && !(defined(GNU) && defined(SPARC))
  #define NO_ASYNC_INTERRUPTS
#endif
#if defined(NO_ASYNC_INTERRUPTS) && defined(MULTITHREAD)
  #error No multithreading possible with this memory model!
#endif
/* When changed: extend SPVW, write a interruptp(). */

/* Flavors of Pathname-management: */
#ifdef UNIX
  #define PATHNAME_UNIX
#endif
#ifdef WIN32
  #define PATHNAME_WIN32
#endif
/* Components of pathnames: */
#ifdef PATHNAME_WIN32
  #define HAS_HOST      1
  #define HAS_DEVICE    1
#endif
#ifdef PATHNAME_UNIX
  #define HAS_HOST      0
  #define HAS_DEVICE    0
#endif
/* Handling of the file "extension" (pathname-type): */
#if 0
  #define PATHNAME_EXT  /* Name and Type are separated, so no limitation of the length */
#endif
#if defined(PATHNAME_UNIX) || defined(PATHNAME_WIN32)
  #define PATHNAME_NOEXT  /* no explicit extension. */
#endif
/* Whether "//" at the beginning of a pathname has to remain (and not to be shortened to "/"): */
#ifdef UNIX_CYGWIN
  #define PATHNAME_UNIX_UNC
#endif
/* When changed: extend pathname.d */

/* Whether there is a type FOREIGN (a wrapper for various kinds of pointers): */
#if defined(UNIX) || defined(DYNAMIC_FFI) || defined(WIN32_NATIVE)
  /* (Used by FFI and by CLX.) */
  #define FOREIGN  void*
#endif
/* When changed: do nothing */
%% #ifdef FOREIGN
%%   export_def(FOREIGN);
%% #endif

/* Whether the STACK is checked at certain key points: */
#define STACKCHECKS  (SAFETY >= 1) /* when SUBRs and FSUBRs are called */
#define STACKCHECKC  (SAFETY >= 1) /* when compiled closures are interpreted */
#define STACKCHECKR  (SAFETY >= 1) /* in the reader */
#define STACKCHECKP  (SAFETY >= 1) /* in the printer */
#define STACKCHECKB  (SAFETY >= 1) /* in the bindings */
/* When changed: do nothing */


/* Feature dependent include files. */

#ifdef HAVE_ICONV
  #include <iconv.h>
  #if _LIBICONV_VERSION
    /* We use GNU libiconv. */
    #define GNU_LIBICONV
    #define HAVE_GOOD_ICONV
  #elif (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 2))
    /* glibc-2.2 iconv is also very reliable. Use it. */
    #define HAVE_GOOD_ICONV
  #else
    /* Other iconv implementations are too unreliable.
     Don't define HAVE_GOOD_ICONV. */
  #endif
#endif


/* ############### List of implemented CLtL2-features ############### */

#define X3J13_005  /* 18.5.1993 */
#define X3J13_014  /* 22.1.1995 */
#define X3J13_149  /* 22.7.1993 */
#define X3J13_175  /* 25.7.1993 */


/* ##################### Memory representation of objects ###################

Memory Representation and the Type Code of the various data types
=================================================================

1. The type code
----------------

An object consists of - in the same word - some type information and, for
immediate types, a couple of data bits, or, for heap allocated types,
a pointer to memory. There are many models of mixing type and pointer.
In the standard model, 6 to 8 bits (the word's high bits) are used for the
type. In the WIDE_HARD and WIDE_SOFT models, type and pointer are each 32
bits. In the HEAPCODES model, there are only 2 to 6 bits.

One bit (normally bit 31) is used as mark bit by the garbage collector.
Outside of GC, it is always cleared. (Except for the get_circularities and
subst_circ routines, and in the STACK, the GC bit is used for marking frames.)

2. Memory formats
-----------------

2.1. Immediate objects

2.1.1. Machine pointers

Machine pointers are immediate objects. They may point to the code area
(.text segment), to data areas (.bss, .data segments, malloc'ed areas).
Other values (e.g. pointers to text/data in shared libraries) are not
allowed, because they may contain bits which are interpreted as a type code.
To use such machine addresses, you must wrap them in foreign-pointers or
simple-bit-vectors.

2.1.2. Other immediate objects

Character, Fixnum, Short-Float, and, if IMMEDIATE_FFLOAT, Single-Float.
Furthermore: Frame-Pointer, Small-Read-Label, System. (System means some
finite number of special values, such as #<UNBOUND>.)

2.2. SUBRs

They are immediate in the sense that they do not move (they do not need to,
because they are allocated statically), but they have to be traversed by GC.

2.3. Pairs

These are heap objects containing just two pointers: Cons and, if SPVW_PURE,
Ratio and Complex.

2.4. Varobjects

These are heap objects of varying size. GC needs a header word at the
beginning of the object.

2.4.1. Records

These are varobjects which have additional type information and flags
in the second header word. Closure, Structure, Stream, Instance are always
records. Depending on the memory model, arrays, symbols etc. may or may
not be records.

2.4.2. Arrays

Simple-Bit-Vector, Simple-String, Simple-Vector are the "simple" arrays.
The non-simple ones are represented by a Iarray, yet the type code gives
some information about the rank, the representation and the element type:

                                |    "simple"     |  "not simple"  |
                                |    Sarray       |     Iarray     |
  ------------------------------+-----------------+----------------+
   (vector bit)                 | sbvector_type   | bvector_type   |
  ------------------------------+-----------------+----------------+
   (vector (unsigned-byte 2))   | sb2vector_type  | b2vector_type  |
  ------------------------------+-----------------+----------------+
   (vector (unsigned-byte 4))   | sb4vector_type  | b4vector_type  |
  ------------------------------+-----------------+----------------+
   (vector (unsigned-byte 8))   | sb8vector_type  | b8vector_type  |
  ------------------------------+-----------------+----------------+
   (vector (unsigned-byte 16))  | sb16vector_type | b16vector_type |
  ------------------------------+-----------------+----------------+
   (vector (unsigned-byte 32))  | sb32vector_type | b32vector_type |
  ------------------------------+-----------------+----------------+
   (vector character)           | sstring_type    | string_type    |
  ------------------------------+-----------------+----------------+
   (vector t)                   | svector_type    | vector_type    |
  ------------------------------+-----------------+----------------+
   array of dimension /= 1      |       --        |  mdarray_type  |
  ------------------------------+-----------------+----------------+

2.4.3. Other varobjects

Symbol has some special flags (keyword, constant, special) in the header,
if possible.

FSUBR, Bignum, Single-Float (unless IMMEDIATE_FFLOAT), Double-Float,
Long-Float, Ratio and Complex (only if SPVW_MIXED).

 ######################## LISP-objects in general ######################## */

/* Other acronyms like 'oint', 'tint', 'aint', 'cint' will be used
   for the corresponding integer types:
   Integer type      contains information equivalent to
      oint           LISP object
      tint           type code of a LISP object
      aint           address of a LISP object
      cint           LISP character

 Usually sizeof(oint) = sizeof(aint) = sizeof(uintL) = 32 Bit.
 Under the model WIDE sizeof(oint) is > sizeof(uintL).
 Model WIDE_HARD stands for sizeof(aint) > sizeof(uintL).
   This model is to be chosen if the following holds true:
   sizeof(void*) > sizeof(uintL) = 32 bit. It also requires that
   sizeof(long) = sizeof(void*) = 64 bit, because some 64-bit numbers
   appear as pre-processor constants.
 Model WIDE_SOFT stands for sizeof(oint) = 64 bit and sizeof(aint) = 32 bit.
   This model can be chosen on any 32-Bit-Machine, if the
   compiler has 64-bit numbers (in software or hardware).
   You will also need to choose it, if there would not be enough space
   for the type-bits in a 32-bit pointer.
 Model HEAPCODES stands for sizeof(oint) = sizeof(aint), and only minimal
   type information is stored in a pointer. All heap allocated objects
   (except conses) must contain the complete type and a length field in the
   first word. The heap gets somewhat bigger by this, and type tests require
   more memory accesses on average, but this model is portable even to
   systems whose memory map looks like Swiss Cheese. */

%% #if notused
%% #ifdef WIDE_HARD
%%   puts("#define WIDE_HARD");
%% #endif
%% #ifdef WIDE_SOFT
%%   puts("#define WIDE_SOFT");
%% #endif
%% #ifdef WIDE
%%   puts("#define WIDE");
%% #endif
%% #endif

/* These are the parameters for using SINGLEMAP_MEMORY. They depend heavily
   on the address space layout. */
/* When using SINGLEMAP_MEMORY:

   The type bits are part of the address that we send on the addressbus.
   Cf. macros pointable_unchecked and pointable_address_unchecked.
   Therefore we must consider MMAP_FIXED_ADDRESS_HIGHEST_BIT.

   In order to respect the constraints given by
   MAPPABLE_ADDRESS_RANGE_START and MAPPABLE_ADDRESS_RANGE_END,
   we use the addresses
     (typecode << oint_type_shift) | SINGLEMAP_ADDRESS_BASE
   where typecode consists of at most 6 or 7 bits and is > 0.
   oint_type_shift is usually around (MMAP_FIXED_ADDRESS_HIGHEST_BIT-6).
   SINGLEMAP_ADDRESS_BASE is chosen so that it contains only few bits.
   SINGLEMAP_TYPE_MASK contains the bits that can be used as type
   bits (excluding the garcol_bit_o). It consists of 6 or 7 bits.
   It must be disjoint to SINGLEMAP_ADDRESS_BASE.
   So, oint_type_shift is the smallest bit set in SINGLEMAP_TYPE_MASK.
   Trade-offs:
     - On 32-bit platforms, oint_type_shift should be as large as possible.
     - On 64-bit platforms, for SINGLEMAP_TYPE_MASK, 7 bits is better
       than 6 bits. */
/* Sort order: Keep this list sorted by
     1. word size (32-bit before 64-bit),
     2. operating system (Linux, *BSD, macOS, proprietary Unices, Windows)
     3. CPU and ABI (alphabetically) */
#if !defined(WIDE_HARD)
  /* 32-bit platforms */
  /* We can nowadays assume that MMAP_FIXED_ADDRESS_HIGHEST_BIT is 30. */
  /* On some platforms, strictly respecting the MAPPABLE_ADDRESS_RANGE
     would lead to a value of oint_type_shift < 24, which is unusable.
     Define IGNORE_MAPPABLE_ADDRESS_RANGE for these platforms. */
  #if defined(UNIX_LINUX) && defined(AMD64) /* Linux/x86_64 with 32-bit x32 ABI */
    #if 1 /* arbitrary choice */
      #define SINGLEMAP_ADDRESS_BASE 0x10000000UL
      #define SINGLEMAP_TYPE_MASK    0x6F000000UL
    #elif 1 /* arbitrary choice */
      #define SINGLEMAP_ADDRESS_BASE 0x20000000UL
      #define SINGLEMAP_TYPE_MASK    0x5F000000UL
    #else
      #define SINGLEMAP_ADDRESS_BASE 0x40000000UL
      #define SINGLEMAP_TYPE_MASK    0x3F000000UL
    #endif
    #define SINGLEMAP_oint_type_shift 24
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_LINUX) && defined(ARM) /* Linux/arm */
    #define SINGLEMAP_ADDRESS_BASE 0x40000000UL
    #define SINGLEMAP_TYPE_MASK    0x3F000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* This configuration allocates memory outside the MAPPABLE_ADDRESS_RANGE. */
    #define IGNORE_MAPPABLE_ADDRESS_RANGE
    #define SINGLEMAP_WORKS 0 /* does not work on build.opensuse.org machines */
  #endif
  #if defined(UNIX_LINUX) && defined(HPPA) /* Linux/hppa */
    #define SINGLEMAP_ADDRESS_BASE 0x08000000UL
    #define SINGLEMAP_TYPE_MASK    0x77000000UL
    #define SINGLEMAP_oint_type_shift 24
    #define SINGLEMAP_WORKS 1 /* but only without GENERATIONAL_GC */
  #endif
  #if defined(UNIX_LINUX) && defined(I80386) /* Linux/i386, Linux/x86_64 with 32-bit i386 ABI */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x77000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* This configuration allocates memory outside the MAPPABLE_ADDRESS_RANGE. */
    #define IGNORE_MAPPABLE_ADDRESS_RANGE
    /* Actually no such configuration works, because the CODE_ADDRESS_RANGE
       consumes so many bits that we have at most 1+1 bits for the typecode. */
    #define SINGLEMAP_WORKS 0
  #endif
  #if defined(UNIX_LINUX) && defined(M68K) /* Linux/m68k */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x3F000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* garcol_bit_o must be 30, because bit 31 is set in CODE_ADDRESS_RANGE. */
    #define SINGLEMAP_garcol_bit_o 30
    #define SINGLEMAP_WORKS 1 /* but only without GENERATIONAL_GC */
  #endif
  #if defined(UNIX_LINUX) && (defined(MIPS) || defined(MIPS64)) /* Linux/mips with o32 or n32 ABI */
    #if !(_MIPS_SIM == _ABIN32) /* Linux/mips with o32 ABI */
      #define SINGLEMAP_ADDRESS_BASE 0x40000000UL
      #define SINGLEMAP_TYPE_MASK    0x3F000000UL
      #define SINGLEMAP_oint_type_shift 24
      /* Actually no such configuration works, because the CODE_ADDRESS_RANGE
         consumes so many bits that we have at most 3+1 bits for the typecode. */
      #define SINGLEMAP_WORKS 0
    #else /* Linux/mips with n32 ABI */
      #define SINGLEMAP_ADDRESS_BASE 0x10000000UL
      #define SINGLEMAP_TYPE_MASK    0x6F000000UL
      #define SINGLEMAP_oint_type_shift 24
      /* This configuration allocates memory outside the MAPPABLE_ADDRESS_RANGE. */
      #define IGNORE_MAPPABLE_ADDRESS_RANGE
      #define SINGLEMAP_WORKS 1
    #endif
  #endif
  #if defined(UNIX_LINUX) && defined(POWERPC) /* Linux/powerpc64 with 32-bit ABI */
    #define SINGLEMAP_ADDRESS_BASE 0x10000000UL
    #define SINGLEMAP_TYPE_MASK    0x6F000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* This configuration allocates memory outside the MAPPABLE_ADDRESS_RANGE. */
    #define IGNORE_MAPPABLE_ADDRESS_RANGE
    /* Actually no such configuration works, because the CODE_ADDRESS_RANGE
       consumes so many bits that we have at most 5+1 bits for the typecode. */
    #define SINGLEMAP_WORKS 0
  #endif
  #if defined(UNIX_LINUX) && defined(S390) /* Linux/s390x with 32-bit ABI */
    #define SINGLEMAP_ADDRESS_BASE 0x08000000UL
    #define SINGLEMAP_TYPE_MASK    0x77000000UL
    #define SINGLEMAP_oint_type_shift 24
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_LINUX) && defined(SPARC) /* Linux/sparc64 with 32-bit ABI */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x77000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* Set garcol_bit_o to 27. */
    #define SINGLEMAP_garcol_bit_o 27
    /* Actually no such configuration works, because the CODE_ADDRESS_RANGE
       consumes so many bits that we have at most 4+1 bits for the typecode. */
    #define SINGLEMAP_WORKS 0
  #endif
  #if defined(UNIX_HURD) && defined(I80386) /* Hurd/i386 */
    #define SINGLEMAP_ADDRESS_BASE 0x08000000UL
    #define SINGLEMAP_TYPE_MASK    0x77000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* This configuration allocates memory outside the MAPPABLE_ADDRESS_RANGE. */
    #define IGNORE_MAPPABLE_ADDRESS_RANGE
    /* This configuration does not work, because it conflicts with the system's
       use of the memory region at 0x09000000UL. */
    #define SINGLEMAP_WORKS 0
  #endif
  #if (defined(__FreeBSD__) || defined(UNIX_GNU_FREEBSD) || defined(__DragonFly__)) && defined(I80386) /* FreeBSD/i386, GNU/kFreeBSD/i386, DragonFly/i386 */
    #define SINGLEMAP_ADDRESS_BASE 0x08000000UL
    #define SINGLEMAP_TYPE_MASK    0x77000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* This configuration allocates memory outside the MAPPABLE_ADDRESS_RANGE. */
    #define IGNORE_MAPPABLE_ADDRESS_RANGE
    /* This configuration works, but it conflicts with the system's use of
       the memory region at 0x28000000UL and is therefore too dangerous
       for general use. */
    #define SINGLEMAP_WORKS 0
  #endif
  #if defined(UNIX_NETBSD) && defined(I80386) /* NetBSD/i386 */
    #define SINGLEMAP_ADDRESS_BASE 0x08000000UL
    #define SINGLEMAP_TYPE_MASK    0x77000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* This configuration allocates memory outside the MAPPABLE_ADDRESS_RANGE. */
    #define IGNORE_MAPPABLE_ADDRESS_RANGE
    #define SINGLEMAP_WORKS 1 /* but only without GENERATIONAL_GC */
  #endif
  #if defined(UNIX_NETBSD) && defined(SPARC) /* NetBSD/sparc */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x5F000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* This configuration allocates memory outside the MAPPABLE_ADDRESS_RANGE:
       from 0x01000000UL to 0x1FFFFFFFUL and from 0x44000000UL to 0x5FFFFFFFUL. */
    #define IGNORE_MAPPABLE_ADDRESS_RANGE
    #define SINGLEMAP_WORKS 1 /* but only without GENERATIONAL_GC */
  #endif
  #if defined(UNIX_OPENBSD) && defined(I80386) /* OpenBSD/i386 */
    #define SINGLEMAP_ADDRESS_BASE 0x40000000UL
    #define SINGLEMAP_TYPE_MASK    0x3F000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* This configuration allocates memory outside the MAPPABLE_ADDRESS_RANGE. */
    #define IGNORE_MAPPABLE_ADDRESS_RANGE
    /* Actually no such configuration works, because the CODE_ADDRESS_RANGE
       consumes so many bits that we have at most 3+1 bits for the typecode. */
    #define SINGLEMAP_WORKS 0
  #endif
  #if defined(UNIX_MACOSX) && defined(I80386) /* Mac OS X/x86_64 with 32-bit ABI */
    #define SINGLEMAP_ADDRESS_BASE 0x10000000UL
    #define SINGLEMAP_TYPE_MASK    0x6F000000UL
    #define SINGLEMAP_oint_type_shift 24
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_MACOSX) && defined(POWERPC) /* Mac OS X/PowerPC */
    #define SINGLEMAP_ADDRESS_BASE 0x10000000UL
    #define SINGLEMAP_TYPE_MASK    0x6F000000UL
    #define SINGLEMAP_oint_type_shift 24
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_AIX) && defined(POWERPC) /* AIX/POWER with 32-bit ABI */
    #define SINGLEMAP_ADDRESS_BASE 0x40000000UL
    #define SINGLEMAP_TYPE_MASK    0x3F000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* Actually no such configuration works, because the CODE_ADDRESS_RANGE
       consumes so many bits that we have at most 5+1 bits for the typecode. */
    #define SINGLEMAP_WORKS 0
  #endif
  #if defined(UNIX_SUNOS5) && defined(I80386) /* Solaris/x86_64 with 32-bit ABI */
    #define SINGLEMAP_ADDRESS_BASE 0x08000000UL
    #define SINGLEMAP_TYPE_MASK    0x77000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* This configuration allocates memory outside the MAPPABLE_ADDRESS_RANGE. */
    #define IGNORE_MAPPABLE_ADDRESS_RANGE
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_SUNOS5) && defined(SPARC) /* Solaris/sparc64 with 32-bit ABI */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x7E000000UL
    #define SINGLEMAP_oint_type_shift 25
    /* This configuration allocates memory outside the MAPPABLE_ADDRESS_RANGE. */
    #define IGNORE_MAPPABLE_ADDRESS_RANGE
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_HAIKU) && defined(I80386) /* Haiku/i386 */
    #define SINGLEMAP_ADDRESS_BASE 0x40000000UL
    #define SINGLEMAP_TYPE_MASK    0x3F000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* This configuration allocates memory outside the MAPPABLE_ADDRESS_RANGE. */
    #define IGNORE_MAPPABLE_ADDRESS_RANGE
    /* Actually no such configuration works, because the CODE_ADDRESS_RANGE
       consumes so many bits that we have at most 5+1 bits for the typecode. */
    #define SINGLEMAP_WORKS 0
  #endif
  #if defined(UNIX_MINIX) && defined(I80386) /* Minix/i386 */
    #define SINGLEMAP_ADDRESS_BASE 0x08000000UL
    #define SINGLEMAP_TYPE_MASK    0x77000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* This configuration allocates memory outside the MAPPABLE_ADDRESS_RANGE. */
    #define IGNORE_MAPPABLE_ADDRESS_RANGE
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_CYGWIN) && defined(I80386) /* Cygwin, running on Windows 10 */
    #define SINGLEMAP_ADDRESS_BASE 0x80000000UL
    #define SINGLEMAP_TYPE_MASK    0x3F000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* Set garcol_bit_o to 30. */
    #define SINGLEMAP_garcol_bit_o 30
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(WIN32_NATIVE) && defined(I80386) /* mingw, running on Windows 10 */
    #define SINGLEMAP_ADDRESS_BASE 0x04000000UL
    #define SINGLEMAP_TYPE_MASK    0x7B000000UL
    #define SINGLEMAP_oint_type_shift 24
    /* This configuration allocates memory outside the MAPPABLE_ADDRESS_RANGE.
       It is risky to define IGNORE_MAPPABLE_ADDRESS_RANGE on this platform. */
    #define IGNORE_MAPPABLE_ADDRESS_RANGE
    #define SINGLEMAP_WORKS 1 /* but risky */
  #endif
  #ifndef SINGLEMAP_garcol_bit_o
    #define SINGLEMAP_garcol_bit_o 31
  #endif
  #define SINGLEMAP_oint_type_len (32-SINGLEMAP_oint_type_shift)
  #define SINGLEMAP_oint_type_mask (SINGLEMAP_TYPE_MASK | (1UL<<SINGLEMAP_garcol_bit_o))
#else
  /* 64-bit platforms */
  #if defined(UNIX_LINUX) && defined(AMD64) /* Linux/x86_64 */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x1F8000000000UL
    #define SINGLEMAP_oint_type_shift 39
    /* Actually no such configuration works, because the CODE_ADDRESS_RANGE
       consumes so many bits that we have at most 1+1 bits for the typecode. */
    #define SINGLEMAP_WORKS 0
  #endif
  #if defined(UNIX_LINUX) && defined(ARM64) /* Linux/arm64 */
    #if 1 /* arbitrary choice */
      #define SINGLEMAP_ADDRESS_BASE 0x002000000000UL
      #define SINGLEMAP_TYPE_MASK    0x005F00000000UL
    #else
      #define SINGLEMAP_ADDRESS_BASE 0x004000000000UL
      #define SINGLEMAP_TYPE_MASK    0x003F00000000UL
    #endif
    #define SINGLEMAP_oint_type_shift 32
    /* Actually no such configuration works, because the CODE_ADDRESS_RANGE
       consumes so many bits that we have at most 3+1 bits for the typecode. */
    #define SINGLEMAP_WORKS 0
  #endif
  #if defined(UNIX_LINUX) && defined(DECALPHA) /* Linux/alpha */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x00FE00000000UL
    #define SINGLEMAP_oint_type_shift 33
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_LINUX) && defined(IA64) /* Linux/ia64 */
    #define SINGLEMAP_ADDRESS_BASE 0x6000000000000000UL
    #define SINGLEMAP_TYPE_MASK    0x000007F000000000UL
    #define SINGLEMAP_oint_type_shift 36
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_LINUX) && defined(MIPS64) /* Linux/mips with 64-bit ABI */
    #define SINGLEMAP_ADDRESS_BASE 0x008000000000UL
    #define SINGLEMAP_TYPE_MASK    0x007E00000000UL
    #define SINGLEMAP_oint_type_shift 33
    /* Actually no such configuration works, because the CODE_ADDRESS_RANGE
       consumes so many bits that we have at most 4+1 bits for the typecode. */
    #define SINGLEMAP_WORKS 0
  #endif
  #if defined(UNIX_LINUX) && defined(POWERPC64) /* Linux/powerpc64, Linux/powerpc64le */
    #define SINGLEMAP_ADDRESS_BASE 0x010000000000UL
    #define SINGLEMAP_TYPE_MASK    0x3E8000000000UL
    #define SINGLEMAP_oint_type_shift 39
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_LINUX) && defined(RISCV64) /* Linux/riscv64 */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x001F80000000UL
    #define SINGLEMAP_oint_type_shift 31
    #define SINGLEMAP_WORKS 0 /* even without GENERATIONAL_GC */
  #endif
  #if defined(UNIX_LINUX) && defined(S390_64) /* Linux/s390x */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x01FC00000000UL
    #define SINGLEMAP_oint_type_shift 34
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_LINUX) && defined(SPARC64) /* Linux/sparc64 */
    #define SINGLEMAP_ADDRESS_BASE 0x0000010000000000UL
    #define SINGLEMAP_TYPE_MASK    0x000006F800000000UL
    #define SINGLEMAP_oint_type_shift 35
    /* This configuration allocates memory outside the MAPPABLE_ADDRESS_RANGE. */
    #define IGNORE_MAPPABLE_ADDRESS_RANGE
    #define SINGLEMAP_WORKS 1
  #endif
  #if (defined(UNIX_FREEBSD) || defined(UNIX_GNU_FREEBSD)) && defined(AMD64) /* FreeBSD/x86_64, GNU/kFreeBSD/x86_64 */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x7F0000000000UL
    #define SINGLEMAP_oint_type_shift 40
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_FREEBSD) && defined(ARM64) /* FreeBSD/arm64 */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0xFE0000000000UL
    #define SINGLEMAP_oint_type_shift 41
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_NETBSD) && defined(AMD64) /* NetBSD/x86_64 */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x7F0000000000UL
    #define SINGLEMAP_oint_type_shift 40
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_NETBSD) && defined(SPARC64) /* NetBSD/sparc64 */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x07F000000000UL
    #define SINGLEMAP_oint_type_shift 36
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_OPENBSD) && defined(AMD64) /* OpenBSD/x86_64 */
    #define SINGLEMAP_ADDRESS_BASE 0x200000000000UL
    #define SINGLEMAP_TYPE_MASK    0x5F0000000000UL
    #define SINGLEMAP_oint_type_shift 40
    /* Actually no such configuration works, because the CODE_ADDRESS_RANGE
       consumes so many bits that we have at most 2+1 bits for the typecode. */
    #define SINGLEMAP_WORKS 0
  #endif
  #if defined(UNIX_MACOSX) && defined(AMD64) /* Mac OS X/x86_64 */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x7F0000000000UL
    #define SINGLEMAP_oint_type_shift 40
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_MACOSX) && defined(ARM64) /* macOS 11/arm64 */
    #define SINGLEMAP_ADDRESS_BASE 0x010000000000UL
    #define SINGLEMAP_TYPE_MASK    0x00FE00000000UL
    #define SINGLEMAP_oint_type_shift 33
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_AIX) && defined(POWERPC64) /* AIX/POWER with 64-bit ABI */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x07F0000000000000UL
    #define SINGLEMAP_oint_type_shift 52
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_SUNOS5) && defined(AMD64) /* Solaris/x86_64 */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x00007F0000000000UL
    #define SINGLEMAP_oint_type_shift 40
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_SUNOS5) && defined(SPARC64) /* Solaris/sparc64 */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x00007F0000000000UL
    #define SINGLEMAP_oint_type_shift 40
    #define SINGLEMAP_WORKS 1
  #endif
  #if defined(UNIX_CYGWIN) && defined(AMD64) /* Cygwin */
    #define SINGLEMAP_ADDRESS_BASE 0UL
    #define SINGLEMAP_TYPE_MASK    0x03F800000000UL
    #define SINGLEMAP_oint_type_shift 35
    /* This configuration does not work: Compilation of compiler.lisp
       fails with "Cannot map memory to address 0x4800080000" or
       "Cannot map memory to address 0xb0000e0000". */
    #define SINGLEMAP_WORKS 0
  #endif
  #ifndef SINGLEMAP_garcol_bit_o
    #define SINGLEMAP_garcol_bit_o 63
  #endif
  #define SINGLEMAP_oint_type_len (64-SINGLEMAP_oint_type_shift)
  #define SINGLEMAP_oint_type_mask (SINGLEMAP_TYPE_MASK | (1UL<<SINGLEMAP_garcol_bit_o))
#endif
#if defined(SINGLEMAP_ADDRESS_BASE) || defined(SINGLEMAP_TYPE_MASK) || defined(SINGLEMAP_WORKS)
  /* Verify that SINGLEMAP_ADDRESS_BASE, SINGLEMAP_TYPE_MASK, SINGLEMAP_oint_type_shift,
     SINGLEMAP_garcol_bit_o, SINGLEMAP_oint_type_len, SINGLEMAP_oint_type_mask
     are now defined. */
  #if !(defined(SINGLEMAP_ADDRESS_BASE) && defined(SINGLEMAP_TYPE_MASK) \
        && defined(SINGLEMAP_oint_type_shift) \
        && defined(SINGLEMAP_garcol_bit_o) \
        && defined(SINGLEMAP_oint_type_len) && defined(SINGLEMAP_oint_type_mask))
    #error SINGLEMAP_ADDRESS_BASE, SINGLEMAP_TYPE_MASK, SINGLEMAP_oint_type_shift, SINGLEMAP_garcol_bit_o, SINGLEMAP_oint_type_len, SINGLEMAP_oint_type_mask are not defined for this platform!
  #endif
  /* Verify that SINGLEMAP_ADDRESS_BASE is a multiple of 0x10000
     and therefore is guaranteed to be page-aligned. */
  #if (SINGLEMAP_ADDRESS_BASE & 0xFFFFUL) != 0
    #error SINGLEMAP_ADDRESS_BASE is not page-aligned!
  #endif
  /* Verify that SINGLEMAP_TYPE_MASK is disjoint to SINGLEMAP_ADDRESS_BASE. */
  #if (SINGLEMAP_TYPE_MASK & SINGLEMAP_ADDRESS_BASE) != 0
    #error SINGLEMAP_TYPE_MASK is not disjoint to SINGLEMAP_ADDRESS_BASE!
  #endif
  /* Verify that SINGLEMAP_TYPE_MASK contains only bits >= SINGLEMAP_oint_type_shift. */
  #if (SINGLEMAP_TYPE_MASK & ((1UL<<(SINGLEMAP_oint_type_shift))-1UL)) != 0
    #error SINGLEMAP_TYPE_MASK contains bits < SINGLEMAP_oint_type_shift!
  #endif
  /* Verify that SINGLEMAP_TYPE_MASK contains the bit SINGLEMAP_oint_type_shift. */
  #if (SINGLEMAP_TYPE_MASK & (1UL<<(SINGLEMAP_oint_type_shift))) == 0
    #error You can increase SINGLEMAP_oint_type_shift!
  #endif
#endif

/* Verify the flags that determine the object representation that were defined
   by the user. */

#if defined(SINGLEMAP_MEMORY)
  /* SINGLEMAP_MEMORY implies TYPECODES. */
  #ifndef TYPECODES
    #define TYPECODES
  #endif
#endif

#if defined(KERNELVOID32A_HEAPCODES) || defined(KERNELVOID32B_HEAPCODES)
  #ifndef KERNELVOID32_HEAPCODES
    #define KERNELVOID32_HEAPCODES
  #endif
#endif

#if defined(GENERIC64A_HEAPCODES) || defined(GENERIC64B_HEAPCODES) || defined(GENERIC64C_HEAPCODES)
  #ifndef GENERIC64_HEAPCODES
    #define GENERIC64_HEAPCODES
  #endif
#endif

#if defined(ONE_FREE_BIT_HEAPCODES) || defined(KERNELVOID32_HEAPCODES) || defined(GENERIC64_HEAPCODES)
  #ifndef HEAPCODES
    #define HEAPCODES
  #endif
#endif

#if defined(WIDE_SOFT) && defined(HEAPCODES)
  #error WIDE_SOFT and HEAPCODES make no sense together, no need for WIDE_SOFT
#endif

#if defined(TYPECODES) && defined(HEAPCODES)
  #error TYPECODES and HEAPCODES make no sense together
#endif

/* Determine early whether to use SINGLEMAP_MEMORY, because the OS has
   restrictions on the mmapable addresses, and these restrictions have
   an influence on oint_type_shift and oint_type_len.  */
#if !defined(HEAPCODES)                                                        \
    && !defined(WIDE_SOFT)                                                     \
    && (defined(HAVE_MMAP_ANON)                                                \
        || defined(HAVE_MACH_VM) || defined(HAVE_WIN32_VM))                    \
    && !defined(NO_ADDRESS_SPACE_ASSUMPTIONS)                                  \
    && !defined(ADDRESS_RANGE_RANDOMIZED)                                      \
    && defined(WIDE_HARD)                                                      \
    && defined(SINGLEMAP_WORKS) && SINGLEMAP_WORKS                             \
    && !defined(NO_SINGLEMAP)
/* If we have not already excluded TYPECODES, and not WIDE_SOFT,
   and the OS has support for mmap or equivalent,
   and the OS does not use address space layout randomization [1]
   and we are on a 64-bit platform [2]
   and SINGLEMAP_MEMORY has been verified to work [3],
   then pick SINGLEMAP_MEMORY.
   [1] It does not work reliably when address space layout randomization
   is in effect: SINGLEMAP_MEMORY assumes that one can extend an existing
   memory region by mmapping the pages after it; but this might overwrite
   some small malloc regions that have been put there by the system.
   [2] On 32-bit platforms the resulting limit of 16 MB objects for each type
   is ridiculous today.
   [3] This is best tested by running one of
         make -f Makefile.devel build-porting64-gcc-spvw_pure_blocks
         make -f Makefile.devel build-porting64-cc-spvw_pure_blocks
 */
  /* Access to LISP-objects is made easier by putting each LISP-object
     to an address that already contains its type information. */
  #ifndef SINGLEMAP_MEMORY
    #define SINGLEMAP_MEMORY
  #endif
  /* SINGLEMAP_MEMORY implies TYPECODES. */
  #ifndef TYPECODES
    #define TYPECODES
  #endif
#endif

/* Determine early whether to use TRIVIALMAP_MEMORY, because TRIVIALMAP_MEMORY
   (and, with it, TRIVIALMAP_MEMORY_STACK) constrain the addresses used for
   heap objects and for the stack and thus give more freedom for choosing
   oint_type_shift and oint_type_len.  */
#if (defined(HAVE_MMAP_ANON)                                                   \
     || defined(HAVE_MACH_VM) || defined(HAVE_WIN32_VM))                       \
    && !defined(SINGLEMAP_MEMORY)                                              \
    && defined(MAPPABLE_ADDRESS_RANGE_START)                                   \
    && defined(MAPPABLE_ADDRESS_RANGE_END)                                     \
    && !defined(NO_ADDRESS_SPACE_ASSUMPTIONS)                                  \
    && !(defined(UNIX_LINUX) && defined(M68K) && (defined(HEAPCODES) || defined(ONE_FREE_BIT_HEAPCODES))) \
    && !defined(UNIX_HAIKU)                                                    \
    && !defined(UNIX_CYGWIN)                                                   \
    && !defined(NO_TRIVIALMAP)
  /* mmap() allows for a more flexible way of memory management than malloc().
     It's not really memory-mapping, but a more comfortable way to manage two
     large memory blocks.
     But it requires that we know where to map the large memory blocks in the
     address range. It does not work reliably when address space layout
     randomization across the *entire* address space is in effect:
     TRIVIALMAP_MEMORY assumes that one can extend an existing memory region
     by mmapping the pages after it; but this might overwrite some small malloc
     regions that have been put there by the system.
     Also, it does not work on Linux/m68k when HEAPCODES is requested.
     Also, it does not work well on Haiku, where it sometimes produces messages
     such as "Cannot map memory to address 0x202a8000 ... Invalid Argument".
     Also, it does not work well on Cygwin, where it sometimes produces messages
     "Cannot map memory to address ...". */
  #ifndef TRIVIALMAP_MEMORY
    #define TRIVIALMAP_MEMORY
  #endif
#endif


#if !(defined(TYPECODES) || defined(HEAPCODES))
  /* Heuristic for choosing TYPECODES or HEAPCODES.
     The SINGLEMAP_MEMORY case is already handled above. */
  #if defined(WIDE_SOFT)
    #define TYPECODES
  #else
    #if !defined(WIDE_HARD)
      /* On 32-bit platforms:
         - TYPECODES: the resulting limit of 16 MB objects for each type,
           especially for conses, is ridiculous today.
         - HEAPCODES: a per-platform decision whether to use
           ONE_FREE_BIT_HEAPCODES or KERNELVOID32_HEAPCODES can be made
           (below).
         - Except that on ARM in -mthumb mode HEAPCODES produces the error
           "PSEUDOCODE_ALIGNMENT is not fulfilled". (-falign-functions has no
           effect on ARM.)
         - On Linux/m68k (with gcc-5.4), nearly all HEAPCODES variants crash.
         Choose HEAPCODES whenever it will work. */
      #if (defined(ARM) && defined(__thumb__)) \
          || (defined(UNIX_LINUX) && defined(M68K))
        /* On these platforms, HEAPCODES does not work. */
        #define TYPECODES
      #else
        #define HEAPCODES
      #endif
    #else
      /* On 64-bit platforms:
         - TYPECODES: There's enough room for type bits, unless the OS allocates
           memory at addresses >= 0x0100000000000000. This correlates with the
           expression  MMAP_FIXED_ADDRESS_HIGHEST_BIT <= 55. In fact, the highest
           value of MMAP_FIXED_ADDRESS_HIGHEST_BIT seen so far is 52.
         - HEAPCODES: Both ONE_FREE_BIT_HEAPCODES and GENERIC64_HEAPCODES have
           alignment restrictions:
           ONE_FREE_BIT_HEAPCODES will normally not work if
           alignof(subr_t) = alignof(long) < 4, but with GCC we can force
           alignof(subr_t) = 4.
           GENERIC64_HEAPCODES likewise with alignment 8.
           Additionally some C functions must be aligned as well, see
           PSEUDOCODE_ALIGNMENT; this can be harder to achieve.
         TYPECODES is typically a few percent slower than HEAPCODES.
         So, use the following choice:
         - On platforms where compilers other than GCC (or clang, which is like
           GCC here) may be used, it is hard to fulfil the alignment constraint
           needed by HEAPCODES. Therefore favour TYPECODES on these platforms.
           Except where TYPECODES does not work, namely on AIX, Solaris/x86_64
           with cc, and Solaris/sparc64.
         - On platforms where we can assume GCC, both ONE_FREE_BIT_HEAPCODES and
           GENERIC64_HEAPCODES generally work well, with few exception. The
           choice between these two is done below. */
      #if defined(UNIX_AIX) || defined(UNIX_SUNOS5)
        /* A compiler other than GCC may be used. */
        #if (defined(UNIX_AIX) && defined(POWERPC64)) || (defined(UNIX_SUNOS5) && defined(AMD64) && !defined(GNU)) || (defined(UNIX_SUNOS5) && defined(SPARC64))
          /* On these platforms, TYPECODES (without SINGLEMAP_MEMORY) does not work. */
          #define HEAPCODES
        #else
          #define TYPECODES
        #endif
      #else
        #define HEAPCODES
      #endif
    #endif
  #endif
#endif
%% #ifdef HEAPCODES
%%   puts("#define HEAPCODES");
%% #endif

#ifdef WIDE_SOFT
  #if defined(GNU) && !defined(WIDE_SOFT_LARGEFIXNUM)
    /* Use the GNU-C extensions, to regard the wide oints as structs. */
    #define WIDE_STRUCT
  #endif
  /* defines the arrangement of an oint's elements: */
  #define WIDE_ENDIANNESS true  /* more efficient this way */
#endif

#if defined(GNU) && (SAFETY >= 3)
  /* Typechecking by the C-compiler */
  #define OBJECT_STRUCT
  #if !(defined(M68K) || defined(ARM)) && !(defined(__GNUG__) && (__GNUC__ == 3) && (__GNUC_MINOR__ == 3)) /* only if struct_alignment==1, and not with g++ 3.3 */
    #define CHART_STRUCT
  #endif
#endif

#if defined(DEBUG_GCSAFETY)
  #ifndef __cplusplus
    #error DEBUG_GCSAFETY works only with a C++ compiler! Reconfigure with CC=g++.
  #endif
  #if defined(WIDE_SOFT)
    #error DEBUG_GCSAFETY cannot be used together with WIDE_SOFT (not yet implemented)!
  #endif
  /* The 'gcv_object_t' and 'object' types share the major part of their innards. */
  #ifndef OBJECT_STRUCT
    #define OBJECT_STRUCT
  #endif
#endif

/* The type 'object' denotes an object in registers or in memory that is
 not seen by the GC.

 The type `gcv_object_t' denotes a GC visible object, i.e. a slot inside
 a heap-allocated object or a STACK slot. If its value is not an immediate
 object, any call that can trigger GC can modify the pointer value.
 NEVER write "var gcv_object_t foo;" - this is forbidden!
 You can write "var gcunsafe_object_t foo;" instead - but then you must not
 trigger GC during the entire lifetime of the variable 'foo'! */

%% #if (defined(OBJECT_STRUCT) || defined(WIDE_STRUCT)) && defined(WIDE) && !defined(WIDE_HARD) && defined(GENERATIONAL_GC)
%%   #define attribute_aligned_object " __attribute__ ((aligned(8)))"
%% #else
%%   #define attribute_aligned_object ""
%% #endif

#if !defined(WIDE_SOFT)

  #if defined(OBJECT_STRUCT)
    #define INNARDS_OF_GCV_OBJECT  \
      uintP one_o;
  #else
    typedef  void *  gcv_object_t;
  #endif
  /* There is an address and type bits in the representation. */

  typedef  uintP  oint;
  typedef  sintP  soint;

#else /* defined(WIDE_SOFT) */

  /* An object consists of a separated 32 bit address and a 32 bit type info. */
  typedef  uint64  oint;
  typedef  sint64  soint;
  #ifdef WIDE_STRUCT
    /* The struct around the union is needed to work around a gcc-2.95 bug. */
    #if BIG_ENDIAN_P==WIDE_ENDIANNESS
      #define INNARDS_OF_GCV_OBJECT                                      \
        union {                                                          \
          struct { /* tint */ uintL type; /* aint */ uintL addr; } both; \
          oint one_u _attribute_aligned_object_;                         \
        } u _attribute_aligned_object_;
    #else
      #define INNARDS_OF_GCV_OBJECT                                      \
        union {                                                          \
          struct { /* aint */ uintL addr; /* tint */ uintL type; } both; \
          oint one_u _attribute_aligned_object_;                         \
        } u _attribute_aligned_object_;
    #endif
    #define one_o  u.one_u
  #else
    typedef  oint  gcv_object_t;
  #endif

#endif

/* sizeof(gcv_object_t) = sizeof(oint) must hold true! */

%% #if !defined(WIDE_SOFT)
%%   #if defined(OBJECT_STRUCT)
%%     #ifdef DEBUG_GCSAFETY
%%       puts("struct object { uintP one_o; uintL allocstamp; };");
%%       puts("struct gcv_object_t { uintP one_o; operator object () const; gcv_object_t (object obj); gcv_object_t (struct fake_gcv_object obj); gcv_object_t (); };");
%%     #else
%%       emit_typedef("struct { uintP one_o; }","gcv_object_t");
%%     #endif
%%   #else
%%     emit_typedef("void *","gcv_object_t");
%%   #endif
%%   emit_typedef("uintP","oint");
%%   emit_typedef("sintP","soint");
%% #else
%%   emit_typedef("uint64","oint");
%%   emit_typedef("sint64","soint");
%%   #ifdef WIDE_STRUCT
%%     strcpy(buf,"struct { union {\n");
%%     #if BIG_ENDIAN_P==WIDE_ENDIANNESS
%%       strcat(buf,"  struct { /*tint*/ uintL type; /*aint*/ uintL addr; } both;\n");
%%     #else
%%       strcat(buf,"  struct { /*aint*/ uintL addr; /*tint*/ uintL type; } both;\n");
%%     #endif
%%     strcat(buf,"  oint one_u");
%%     strcat(buf,attribute_aligned_object);
%%     strcat(buf,"; } u");
%%     strcat(buf,attribute_aligned_object);
%%     strcat(buf,"; }");
%%     emit_typedef(buf,"gcv_object_t");
%%     emit_define("one_o","u.one_u");
%%   #else
%%     emit_typedef("oint","gcv_object_t");
%%   #endif
%% #endif

/* conversion between gcv_object_t/object and oint:
 as_oint(expr)   gcv_object_t/object --> oint
 as_object(x)    oint --> gcv_object_t
 The conversion  gcv_object_t --> object
 is implicit. */
#if defined(WIDE_STRUCT) || defined(OBJECT_STRUCT)
  #define as_oint(expr)  ((expr).one_o)
  #if defined(WIDE_STRUCT)
    #define as_object(o)  ((object){designated_init(u,{designated_init(one_u,(o))})INIT_ALLOCSTAMP})
  #elif defined(OBJECT_STRUCT)
    #define as_object(o)  ((object){designated_init(one_o,(o))INIT_ALLOCSTAMP})
  #else
    extern __inline__ object as_object (register oint o)
      { register object obj; obj.one_o = o; return obj; }
  #endif
#else
  #define as_oint(expr)  (oint)(expr)
  #define as_object(o)  (gcv_object_t)(o)
#endif
%% export_def(as_oint(expr));
%% export_def(as_object(o));

/* Separation of an oint in type bits and address:
   oint_type_mask  is always subset of  (2^oint_type_len-1)<<oint_type_shift
   and
   oint_addr_mask  is a superset of  (2^oint_addr_len-1)<<oint_addr_shift . */
#if !defined(TYPECODES)
  /* HEAPCODES model:
     For pointers, the address takes the full word (with type info in the
     lowest two bits). For immediate objects, we use 24 bits for the data
     (but exclude the highest available bit, which is the garcol_bit). */
  #if !(defined(ONE_FREE_BIT_HEAPCODES) || defined(KERNELVOID32_HEAPCODES) || defined(GENERIC64_HEAPCODES))
    /* Choose the appropriate HEAPCODES variant for the machine.
       On 32-bit platforms:
         - On platforms where compilers other than GCC (or clang, which is like
           GCC here) may be used, it is hard to fulfil the 8-byte alignment
           constraint needed for KERNELVOID32_HEAPCODES. Therefore favour
           ONE_FREE_BIT_HEAPCODES on these platforms.
         - On platforms where we can assume GCC, KERNELVOID32_HEAPCODES generally
           works well. There are few exceptions to this rule. (Whereas there are
           more platforms where ONE_FREE_BIT_HEAPCODES does not work, especially
           when not using TRIVIALMAP_MEMORY.)
       On 64-bit platforms:
         On most platforms, ONE_FREE_BIT_HEAPCODES and GENERIC64_HEAPCODES
         work equally well. I prefer ONE_FREE_BIT_HEAPCODES because it
         does not impose alignment restrictions on pointers.
         The exception are special cases where ONE_FREE_BIT_HEAPCODES is
         known to not work. */
    #if !defined(WIDE_HARD)
      /* 32-bit platforms */
      #if defined(UNIX_AIX) || defined(UNIX_SUNOS5)
        /* A compiler other than GCC may be used. */
        #define ONE_FREE_BIT_HEAPCODES
      #elif (defined(UNIX_CYGWIN) && defined(I80386))
        /* On these platforms, KERNELVOID32_HEAPCODES does not work. */
        #define ONE_FREE_BIT_HEAPCODES
      #else
        #define KERNELVOID32_HEAPCODES
      #endif
    #else
      /* 64-bit platforms */
      #if defined(NO_ADDRESS_SPACE_ASSUMPTIONS)
        /* With GENERIC64_HEAPCODES we don't need to make assumptions about the
           address range. */
        #define GENERIC64_HEAPCODES
      #elif (defined(UNIX_LINUX) && defined(S390_64))
        /* On these platforms, ONE_FREE_BIT_HEAPCODES does not generally work. */
        #define GENERIC64_HEAPCODES
      #else
        /* The general case. */
        #define ONE_FREE_BIT_HEAPCODES
      #endif
    #endif
  #endif
  #ifdef ONE_FREE_BIT_HEAPCODES
    /* The portable case. Assumes only that the GC bit can be chosen. */
    /* To determine HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS, run one of
         make -f Makefile.devel build-porting32-gcc-one_free_bit_heapcodes-trivialmap
         make -f Makefile.devel build-porting32-cc-one_free_bit_heapcodes-trivialmap
         make -f Makefile.devel build-porting64-gcc-one_free_bit_heapcodes-trivialmap
         make -f Makefile.devel build-porting64-cc-one_free_bit_heapcodes-trivialmap
       To determine HEAPCODES1BIT_WITH_MALLOC_WORKS, run one of
         make -f Makefile.devel build-porting32-gcc-one_free_bit_heapcodes-malloc
         make -f Makefile.devel build-porting32-cc-one_free_bit_heapcodes-malloc
         make -f Makefile.devel build-porting64-gcc-one_free_bit_heapcodes-malloc
         make -f Makefile.devel build-porting64-cc-one_free_bit_heapcodes-malloc
     */
    #if !defined(WIDE_HARD)
      /* 32-bit platforms */
      #if defined(UNIX_LINUX) && defined(AMD64) /* Linux/x86_64 with 32-bit x32 ABI */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #if !defined(TRIVIALMAP_MEMORY)
          /* Avoid error
             "STACK range (around 0xf6a94080) contains the bit used to identify frames". */
          #define garcol_bit_o 27
        #endif
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_LINUX) && defined(ARM) /* Linux/arm */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #if !defined(TRIVIALMAP_MEMORY)
          /* Avoid error
             "STACK range (around 0xb6c8a040) contains the bit used to identify frames". */
          #define garcol_bit_o 27
        #endif
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_LINUX) && defined(HPPA) /* Linux/hppa */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1 /* but only without GENERATIONAL_GC */
        #if !defined(TRIVIALMAP_MEMORY)
          /* Avoid error
             "STACK range (around 0xf65ff080) contains the bit used to identify frames". */
          #define garcol_bit_o 27
        #endif
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_LINUX) && defined(I80386) /* Linux/i386, Linux/x86_64 with 32-bit i386 ABI */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #if !defined(TRIVIALMAP_MEMORY)
          /* We get the error
             "STACK range (around 0xf71b2040) contains the bit used to identify frames"
             or warnings
             "Return value of malloc() = f71fe008 is not compatible with the choice of garcol_bit_o."
             The only way to avoid it would be to #define garcol_bit_o 27,
             but that conflicts with CODE_ADDRESS_RANGE. */
        #endif
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 0 /* sometimes 0, sometimes 1 */
      #endif
      #if defined(UNIX_LINUX) && defined(M68K) /* Linux/m68k */
        /* Crashes, regardless of garcol_bit_o and imm_type_shift values. */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 0 /* even without GENERATIONAL_GC */
        #if !defined(TRIVIALMAP_MEMORY)
          /* Avoid error
             "STACK range (around 0xf65ff040) contains the bit used to identify frames". */
          #define garcol_bit_o 27
        #endif
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_LINUX) && (defined(MIPS) || defined(MIPS64)) /* Linux/mips with o32 or n32 ABI */
        #if !(_MIPS_SIM == _ABIN32) /* Linux/mips with o32 ABI */
          #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
          #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
        #else /* Linux/mips with n32 ABI */
          #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
          #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
        #endif
      #endif
      #if defined(UNIX_LINUX) && defined(POWERPC) /* Linux/powerpc64 with 32-bit ABI */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #if !defined(TRIVIALMAP_MEMORY)
          /* Avoid error
             "STACK range (around 0xf7860040) contains the bit used to identify frames". */
          #define garcol_bit_o 27
        #endif
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_LINUX) && defined(S390) /* Linux/s390x with 32-bit ABI */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_LINUX) && defined(SPARC) /* Linux/sparc64 with 32-bit ABI */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #if !defined(TRIVIALMAP_MEMORY)
          /* Avoid error
             "STACK range (around 0xf7a9a080) contains the bit used to identify frames" */
          #define garcol_bit_o 27
        #endif
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_HURD) && defined(I80386) /* Hurd/i386 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if (defined(__FreeBSD__) || defined(UNIX_GNU_FREEBSD) || defined(__DragonFly__)) && defined(I80386) /* FreeBSD/i386, GNU/kFreeBSD/i386, DragonFly/i386 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_NETBSD) && defined(I80386) /* NetBSD/i386 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1 /* but only without GENERATIONAL_GC */
        #if !defined(TRIVIALMAP_MEMORY)
          /* Avoid error
             "STACK range (around 0xbb800000) contains the bit used to identify frames". */
          #define garcol_bit_o 30
        #endif
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_NETBSD) && defined(SPARC) /* NetBSD/sparc */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1 /* but only without GENERATIONAL_GC */
        #if !defined(TRIVIALMAP_MEMORY)
          /* Avoid error
             "STACK range (around 0xedb00000) contains the bit used to identify frames". */
          #define garcol_bit_o 28
        #endif
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_OPENBSD) && defined(I80386) /* OpenBSD/i386 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1 /* but only without GENERATIONAL_GC */
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 0
      #endif
      #if defined(UNIX_MACOSX) && defined(I80386) /* Mac OS X/x86_64 with 32-bit ABI */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_MACOSX) && defined(POWERPC) /* Mac OS X/PowerPC */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_AIX) && defined(POWERPC) /* AIX/POWER with 32-bit ABI */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_SUNOS5) && defined(I80386) /* Solaris/x86_64 with 32-bit ABI */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_SUNOS5) && defined(SPARC) /* Solaris/sparc64 with 32-bit ABI */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_HAIKU) && defined(I80386) /* Haiku/i386 */
        /* Sometimes we get repeated messages such as
           "Cannot map memory to address 0x202a8000 ... Invalid Argument" */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 0
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_MINIX) && defined(I80386) /* Minix/i386 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_CYGWIN) && defined(I80386) /* Cygwin, running on Windows 10 */
        /* Warns "clisp might crash later" and
           produces messages "Cannot map memory to address". */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 0
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(WIN32_NATIVE) && defined(I80386) /* mingw, running on Windows 10 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #ifndef garcol_bit_o
        #define garcol_bit_o 31
      #endif
      /* Choose imm_type_shift depending on garcol_bit_o. */
      #if garcol_bit_o >= 30
        #define imm_type_shift 3
      #elif garcol_bit_o >= 27 && garcol_bit_o <= 28
        #define imm_type_shift (garcol_bit_o+1)
      #else
        #error No way to define imm_type_shift, satisfying the constraints!
      #endif
    #else
      /* 64-bit platforms */
      #if defined(UNIX_LINUX) && defined(AMD64) /* Linux/x86_64 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_LINUX) && defined(ARM64) /* Linux/arm64 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_LINUX) && defined(DECALPHA) /* Linux/alpha */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_LINUX) && defined(IA64) /* Linux/ia64 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_LINUX) && defined(MIPS64) /* Linux/mips with 64-bit ABI */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_LINUX) && defined(POWERPC64) /* Linux/powerpc64, Linux/powerpc64le */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_LINUX) && defined(RISCV64) /* Linux/riscv64 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_LINUX) && defined(S390_64) /* Linux/s390x */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1 /* but only with(!) GENERATIONAL_GC */
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 0
      #endif
      #if defined(UNIX_LINUX) && defined(SPARC64) /* Linux/sparc64 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #if !defined(TRIVIALMAP_MEMORY)
          /* Avoid error
             "STACK range (around 0xfff800010098c080) contains the bit used to identify frames"
             or
             "STACK range (around 0xfffff8010049a080) contains the bit used to identify frames". */
          #define garcol_bit_o 42
          #define imm_type_shift 3
          #define oint_data_len 36
        #endif
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if (defined(UNIX_FREEBSD) || defined(UNIX_GNU_FREEBSD)) && defined(AMD64) /* FreeBSD/x86_64, GNU/kFreeBSD/x86_64 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_FREEBSD) && defined(ARM64) /* FreeBSD/arm64 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_NETBSD) && defined(AMD64) /* NetBSD/x86_64 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_NETBSD) && defined(SPARC64) /* NetBSD/sparc64 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS ?
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS ?
      #endif
      #if defined(UNIX_OPENBSD) && defined(AMD64) /* OpenBSD/x86_64 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_MACOSX) && defined(AMD64) /* Mac OS X/x86_64 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_MACOSX) && defined(ARM64) /* macOS 11/arm64 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_AIX) && defined(POWERPC64) /* AIX/POWER with 64-bit ABI */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_SUNOS5) && defined(AMD64) /* Solaris/x86_64 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_SUNOS5) && defined(SPARC64) /* Solaris/sparc64 */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 1
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 1
      #endif
      #if defined(UNIX_CYGWIN) && defined(AMD64) /* Cygwin */
        /* Produces messages "Cannot map memory to address". */
        #define HEAPCODES1BIT_WITH_TRIVIALMAP_WORKS 0
        /* Crashes. */
        #define HEAPCODES1BIT_WITH_MALLOC_WORKS 0
      #endif
      #ifndef garcol_bit_o
        #define garcol_bit_o 63
      #endif
    #endif
    #if 0 /* old */
      #if (defined(M68K) && defined(UNIX_LINUX)) || (defined(SPARC) && defined(UNIX_LINUX) && (__GLIBC__ < 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ < 2)))
        /* On Sparc-Linux with glibc 2.1 and older:
           malloc()ed addresses are of the form 0x0....... or 0xe........
           Bits 31..29 are therefore part of an address and cannot
           be used as garcol_bit. We therefore choose bit 28 as garcol_bit.
           Now, the 24 data bits of an immediate value must not intersect the
           garcol_bit, so we use bits 27..4 for that (we could use bits 26..3
           as well).
           On m68k-Linux, malloc()ed addresses are of the form 0x80...... or
           0xc0....... Bits 31..30 are therefore part of an address and cannot
           be used as garcol_bit. We therefore have three choices:
             data bits: bits 26..3, garcol_bit_o = 28/27
             data bits: bits 27..4, garcol_bit_o = 28/3
             data bits: bits 28..5, garcol_bit_o = 4/3 */
        #define garcol_bit_o 28
        #define imm_type_shift 29
      #endif
    #endif
    /* The first type field consists of bits 2,1,0. */
    #define oint_type_shift 0
    /* Immediate objects have a second type field,
       consisting of the bits imm_type_shift+2,...,imm_type_shift. */
    #ifndef imm_type_shift
      #define imm_type_shift 3
    #endif
    #if !((imm_type_shift >= 3) && (imm_type_shift+3 <= pointer_bitsize))
      #error Wrong choice of imm_type_shift!
    #endif
    /* Which bits to use for the type of an object. */
    #if (imm_type_shift == 3)
      #define oint_type_len 8
      #define oint_type_mask (bit(6)-1)
    #else
      #define oint_type_len pointer_bitsize
      #define oint_type_mask ((7UL << imm_type_shift) | 7UL)
    #endif
    /* Which bits to use for the data of an immediate object. */
    #ifndef oint_data_len
      #if (pointer_bitsize==64)
        #define oint_data_len 48
      #else
        #define oint_data_len 24
      #endif
    #endif
    #if (imm_type_shift == 3)
      #define oint_data_shift 6 /* or 7 */
    #else
      #define oint_data_shift 3
    #endif
    #define oint_data_mask (bit(oint_data_len+oint_data_shift)-bit(oint_data_shift))
    /* Some basic checks, */
    #if !(garcol_bit_o >= oint_data_len+oint_data_shift)
      #error Incompatible choices of garcol_bit_o and oint_data_len, oint_data_shift!
    #endif
    #if !((garcol_bit_o < imm_type_shift) || (garcol_bit_o >= imm_type_shift+3))
      #error Incompatible choices of garcol_bit_o and imm_type_shift!
    #endif
    #if !((oint_data_len+oint_data_shift <= imm_type_shift) || (oint_data_shift >= imm_type_shift+3))
      #error Incompatible choices of oint_data_shift and imm_type_shift!
    #endif
    /* subr_tab (= subr_tab_data) and symbol_tab (= symbol_tab_data) must be at
       addresses that don't contain the garcol_bit_o, otherwise the GC mark
       phase crashes because of the GCself pointers in the subrs and symbols. */
    #if ((CODE_ADDRESS_RANGE >> garcol_bit_o) & 1) != 0
      #error Wrong choice of garcol_bit_o: it conflicts with CODE_ADDRESS_RANGE!
    #endif
  #endif /* ONE_FREE_BIT_HEAPCODES */
  #ifdef KERNELVOID32_HEAPCODES
    #if !(defined(KERNELVOID32A_HEAPCODES) || defined(KERNELVOID32B_HEAPCODES))
      #if (defined(UNIX_LINUX) && defined(ARM)) \
          || (defined(UNIX_LINUX) && defined(I80386)) \
          || (defined(UNIX_SUNOS5) && defined(I80386))
        /* On these platforms, STACK_ADDRESS_RANGE is not always in the same
           area. But it's always either < 0xC0000000UL or >= 0xE0000000UL. */
        #define KERNELVOID32A_HEAPCODES
      #else
        /* On all other platforms, STACK_ADDRESS_RANGE is always in the same
           area. */
        #if STACK_ADDRESS_RANGE < 0xC0000000UL || STACK_ADDRESS_RANGE >= 0xE0000000UL
          #define KERNELVOID32A_HEAPCODES
        #else
          #define KERNELVOID32B_HEAPCODES
        #endif
      #endif
    #endif
    #if defined(KERNELVOID32A_HEAPCODES) || defined(KERNELVOID32B_HEAPCODES)
      /* KERNELVOID32A_HEAPCODES:
           The Linux/32-bit case. Assumes
           1. that the virtual memory addresses end at 0xC0000000, or at least
              that we can put a black hole on the range 0xC0000000..0xDFFFFFFF,
           2. that the compiler and linker can enforce an 8-byte alignment of
              symbol_tab and subr_tab.
           Only bit 0 or 1 can be used as GC-bit.
         KERNELVOID32B_HEAPCODES:
           The OpenBSD/i386 case. Assumes
           1. that the virtual memory addresses end at 0xE0000000, or at least
              that we can put a black hole on the range 0xE0000000..0xFFFFFFFF,
           2. that the compiler and linker can enforce an 8-byte alignment of
              symbol_tab and subr_tab.
           Only bit 0 or 1 can be used as GC-bit. */
      #define oint_type_shift 0
      #define oint_type_len 32
      #define oint_type_mask 0xE000001FUL
      #define oint_data_shift 5
      #define oint_data_len 24
      #define oint_data_mask 0x1FFFFFE0UL
      #define garcol_bit_o 0
    #endif
    /* Here we note whether KERNELVOID32_HEAPCODES actually works,
       for each 32-bit platform.
       To determine KERNELVOID32_HEAPCODES_WORKS, run one of
         make -f Makefile.devel build-porting32-gcc-kernelvoid32_heapcodes
         make -f Makefile.devel build-porting32-cc-kernelvoid32_heapcodes
     */
    #if defined(UNIX_LINUX) && defined(AMD64) /* Linux/x86_64 with 32-bit x32 ABI */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_LINUX) && defined(ARM) /* Linux/arm */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_LINUX) && defined(HPPA) /* Linux/hppa */
      #define KERNELVOID32_HEAPCODES_WORKS 1 /* without GENERATIONAL_GC */
    #endif
    #if defined(UNIX_LINUX) && defined(I80386) /* Linux/i386, Linux/x86_64 with 32-bit i386 ABI */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_LINUX) && defined(M68K) /* Linux/m68k */
      #define KERNELVOID32_HEAPCODES_WORKS 1 /* without GENERATIONAL_GC */
    #endif
    #if defined(UNIX_LINUX) && (defined(MIPS) || defined(MIPS64)) /* Linux/mips with o32 or n32 ABI */
      #if !(_MIPS_SIM == _ABIN32) /* Linux/mips with o32 ABI */
        #define KERNELVOID32_HEAPCODES_WORKS 1
      #else /* Linux/mips with n32 ABI */
        #define KERNELVOID32_HEAPCODES_WORKS 1
      #endif
    #endif
    #if defined(UNIX_LINUX) && defined(POWERPC) /* Linux/powerpc64 with 32-bit ABI */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_LINUX) && defined(S390) /* Linux/s390x with 32-bit ABI */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_LINUX) && defined(SPARC) /* Linux/sparc64 with 32-bit ABI */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_HURD) && defined(I80386) /* Hurd/i386 */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
    #if (defined(__FreeBSD__) || defined(UNIX_GNU_FREEBSD) || defined(__DragonFly__)) && defined(I80386) /* FreeBSD/i386, GNU/kFreeBSD/i386, DragonFly/i386 */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_NETBSD) && defined(I80386) /* NetBSD/i386 */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_NETBSD) && defined(SPARC) /* NetBSD/sparc */
      #define KERNELVOID32_HEAPCODES_WORKS 1 /* without GENERATIONAL_GC */
    #endif
    #if defined(UNIX_OPENBSD) && defined(I80386) /* OpenBSD/i386 */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_MACOSX) && defined(I80386) /* Mac OS X/x86_64 with 32-bit ABI */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_MACOSX) && defined(POWERPC) /* Mac OS X/PowerPC */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_AIX) && defined(POWERPC) /* AIX/POWER with 32-bit ABI */
      #define KERNELVOID32_HEAPCODES_WORKS 1 /* 1 with gcc, 0 with xlc */
    #endif
    #if defined(UNIX_SUNOS5) && defined(I80386) /* Solaris/x86_64 with 32-bit ABI */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_SUNOS5) && defined(SPARC) /* Solaris/sparc64 with 32-bit ABI */
      #define KERNELVOID32_HEAPCODES_WORKS 0 /* 1 with gcc, 0 with cc */
    #endif
    #if defined(UNIX_HAIKU) && defined(I80386) /* Haiku/i386 */
      /* Works fine without TRIVIALMAP_MEMORY.
         With TRIVIALMAP_MEMORY, sometimes we get repeated messages such as
         "Cannot map memory to address 0x202a8000 ... Invalid Argument" */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_MINIX) && defined(I80386) /* Minix/i386 */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_CYGWIN) && defined(I80386) /* Cygwin, running on Windows 10 */
      /* Warns "clisp might crash later" and
         produces messages "Cannot map memory to address". */
      #define KERNELVOID32_HEAPCODES_WORKS 0
    #endif
    #if defined(WIN32_NATIVE) && defined(I80386) /* mingw, running on Windows 10 */
      #define KERNELVOID32_HEAPCODES_WORKS 1
    #endif
  #endif /* KERNELVOID32_HEAPCODES */
  #ifdef GENERIC64_HEAPCODES
    /* GENERIC64A_HEAPCODES is closely modeled on KERNELVOID32A_HEAPCODES.
       GENERIC64B_HEAPCODES shifts the immediate values by one more bit.
       GENERIC64C_HEAPCODES drops the necessity of an address range hole. */
    #if !(defined(GENERIC64A_HEAPCODES) || defined(GENERIC64B_HEAPCODES) || defined(GENERIC64C_HEAPCODES))
      #if (defined(UNIX_FREEBSD) && defined(ARM64) && defined(__clang__)) \
          || (defined(UNIX_SUNOS5) && defined(SPARC64) && !defined(GNU))
        /* On FreeBSD/arm64 with clang and on Solaris/sparc64 with Sun C,
           GENERIC64C_HEAPCODES leads to "PSEUDOCODE_ALIGNMENT is not fulfilled.",
           but GENERIC64B_HEAPCODES works. So choose that, even if
           NO_ADDRESS_SPACE_ASSUMPTIONS is defined; we wouldn't be able
           to fully honor NO_ADDRESS_SPACE_ASSUMPTIONS in these cases. */
        #define GENERIC64B_HEAPCODES
      #else
        #define GENERIC64C_HEAPCODES
      #endif
    #endif
    #ifdef GENERIC64A_HEAPCODES
      /* The generic 64-bit case, variant A.
       Assumes 1. that the virtual memory addresses end at 0xC000000000000000,
       or at least that we can put a black hole on the range
       0xC000000000000000..0xDFFFFFFFFFFFFFFF,
       2. an 8-byte alignment for symbol_tab, subr_tab,
       3. a 4-byte alignment for any pointer in memory.
       Only bit 0 or 1 or 2 can be used as GC-bit. */
      #define oint_type_shift 0
      #define oint_type_len 64
      #define oint_type_mask 0xE00000000000001FUL
      #define oint_data_shift 5
      #define oint_data_len 56
      #define oint_data_mask 0x1FFFFFFFFFFFFFE0UL
      #define garcol_bit_o 0
    #endif /* GENERIC64A_HEAPCODES */
    #ifdef GENERIC64B_HEAPCODES
      /* The generic 64-bit case, variant B.
       Assumes 1. that the virtual memory addresses end at 0xC000000000000000,
       or at least that we can put a black hole on the range
       0xC000000000000000..0xDFFFFFFFFFFFFFFF,
       2. an 8-byte alignment for symbol_tab, subr_tab,
       3. a 4-byte alignment for any pointer in memory.
       Only bit 0 or 1 or 2 can be used as GC-bit. */
      #define oint_type_shift 0
      #define oint_type_len 64
      #define oint_type_mask 0xE00000000000003FUL
      #define oint_data_shift 6
      #define oint_data_len 55
      #define oint_data_mask 0x1FFFFFFFFFFFFFC0UL
      #define garcol_bit_o 0
    #endif /* GENERIC64B_HEAPCODES */
    #ifdef GENERIC64C_HEAPCODES
      /* The generic 64-bit case, variant C.
       Assumes nothing about the virtual memory addresses.
       But assumes an 8-byte alignment for symbol_tab, subr_tab, and more
       generally any pointer in memory.
       Only bit 0 or 1 or 2 can be used as GC-bit. */
      #define oint_type_shift 0
      #define oint_type_len 64
      #define oint_type_mask 0x000000000000003FUL
      #define oint_data_shift 6
      #define oint_data_len 58
      #define oint_data_mask 0xFFFFFFFFFFFFFFC0UL
      #define garcol_bit_o 0
    #endif /* GENERIC64C_HEAPCODES */
    /* Here we note whether GENERIC64C_HEAPCODES actually works,
       for each 64-bit platform.
       To determine GENERIC64C_HEAPCODES_WORKS, run one of
         make -f Makefile.devel build-porting64-gcc-generic64_heapcodes
         make -f Makefile.devel build-porting64-cc-generic64_heapcodes
     */
    #if defined(UNIX_LINUX) && defined(AMD64) /* Linux/x86_64 */
      #define GENERIC64C_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_LINUX) && defined(ARM64) /* Linux/arm64 */
      #define GENERIC64C_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_LINUX) && defined(DECALPHA) /* Linux/alpha */
      #define GENERIC64C_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_LINUX) && defined(IA64) /* Linux/ia64 */
      #define GENERIC64C_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_LINUX) && defined(MIPS64) /* Linux/mips with 64-bit ABI */
      #define GENERIC64C_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_LINUX) && defined(POWERPC64) /* Linux/powerpc64, Linux/powerpc64le */
      #define GENERIC64C_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_LINUX) && defined(RISCV64) /* Linux/riscv64 */
      #define GENERIC64C_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_LINUX) && defined(S390_64) /* Linux/s390x */
      #define GENERIC64C_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_LINUX) && defined(SPARC64) /* Linux/sparc64 */
      #define GENERIC64C_HEAPCODES_WORKS 1
    #endif
    #if (defined(UNIX_FREEBSD) || defined(UNIX_GNU_FREEBSD)) && defined(AMD64) /* FreeBSD/x86_64, GNU/kFreeBSD/x86_64 */
      #define GENERIC64C_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_FREEBSD) && defined(ARM64) /* FreeBSD/arm64 */
      #define GENERIC64C_HEAPCODES_WORKS 0
      #define GENERIC64B_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_NETBSD) && defined(AMD64) /* NetBSD/x86_64 */
      #define GENERIC64C_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_NETBSD) && defined(SPARC64) /* NetBSD/sparc64 */
      #define GENERIC64C_HEAPCODES_WORKS ?
    #endif
    #if defined(UNIX_OPENBSD) && defined(AMD64) /* OpenBSD/x86_64 */
      #define GENERIC64C_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_MACOSX) && defined(AMD64) /* Mac OS X/x86_64 */
      #define GENERIC64C_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_AIX) && defined(POWERPC64) /* AIX/POWER with 64-bit ABI */
      #define GENERIC64C_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_SUNOS5) && defined(AMD64) /* Solaris/x86_64 */
      #define GENERIC64C_HEAPCODES_WORKS 1 /* 1 with gcc, 0 with cc */
      #define GENERIC64B_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_SUNOS5) && defined(SPARC64) /* Solaris/sparc64 */
      #define GENERIC64C_HEAPCODES_WORKS 1
    #endif
    #if defined(UNIX_CYGWIN) && defined(AMD64) /* Cygwin */
      /* Produces messages "Cannot map memory to address". */
      #define GENERIC64C_HEAPCODES_WORKS 0
    #endif
  #endif /* GENERIC64_HEAPCODES */
  #if defined(WIDE_HARD)
    #define oint_addr_shift 0
    #define oint_addr_len 64
    #define oint_addr_mask 0xFFFFFFFFFFFFFFFFUL
  #else
    #define oint_addr_shift 0
    #define oint_addr_len 32
    #define oint_addr_mask 0xFFFFFFFFUL
  #endif
#else /* TYPECODES */
  /* Now come the platforms with TYPECODES. oint_type_len should be >= 8,
     and oint_type_mask should have at least 8 bits set and at most one bit in
     common with oint_addr_mask. */
  #if defined(WIDE_SOFT)
    /* Separate 32-bit words for typecode and address. */
    #if defined(WIDE_SOFT_LARGEFIXNUM)
      /* Used to test large fixnums on 32-bit platforms.
         Bits 63..48 = type code, Bits 47..32 = zero, Bits 31..0 = address */
      #define oint_type_shift 48
      #define oint_type_len 16
      #define oint_type_mask ULL(0xFFFF000000000000)
      #define oint_addr_shift 0
      #define oint_addr_len 48
      #define oint_addr_mask ULL(0x0000FFFFFFFFFFFF)
    #elif WIDE_ENDIANNESS
      /* Bits 63..32 = type code, Bits 31..0 = address */
      #define oint_type_shift 32
      #define oint_type_len 32
      #define oint_type_mask ULL(0xFFFFFFFF00000000)
      #define oint_addr_shift 0
      #define oint_addr_len 32
      #define oint_addr_mask ULL(0x00000000FFFFFFFF)
    #else /* conversely it is a little slower: */
      /* Bits 63..32 = address, Bits 31..0 = type code */
      #define oint_type_shift 0
      #define oint_type_len 32
      #define oint_type_mask ULL(0x00000000FFFFFFFF)
      #define oint_addr_shift 32
      #define oint_addr_len 32
      #define oint_addr_mask ULL(0xFFFFFFFF00000000)
    #endif
  #else
    /* oint == uintP.
       Type code and address are in the same word. */
    #if defined(SINGLEMAP_MEMORY)
      /* The parameters are already determined above. */
      /* Verify that SINGLEMAP_ADDRESS_BASE, SINGLEMAP_TYPE_MASK, SINGLEMAP_oint_type_shift,
         SINGLEMAP_garcol_bit_o, SINGLEMAP_oint_type_len, SINGLEMAP_oint_type_mask
         are defined. */
      #if !(defined(SINGLEMAP_ADDRESS_BASE) && defined(SINGLEMAP_TYPE_MASK) \
            && defined(SINGLEMAP_oint_type_shift) \
            && defined(SINGLEMAP_garcol_bit_o) \
            && defined(SINGLEMAP_oint_type_len) && defined(SINGLEMAP_oint_type_mask))
        #error SINGLEMAP_ADDRESS_BASE, SINGLEMAP_TYPE_MASK, SINGLEMAP_oint_type_shift, SINGLEMAP_garcol_bit_o, SINGLEMAP_oint_type_len, SINGLEMAP_oint_type_mask are not defined for this platform!
      #endif
      #if !defined(WIDE_HARD)
        /* 32-bit platforms */
        /* Verify the assumption that MMAP_FIXED_ADDRESS_HIGHEST_BIT is 30. */
        #if defined(HAVE_WIN32_VM)
          #undef MMAP_FIXED_ADDRESS_HIGHEST_BIT
          #define MMAP_FIXED_ADDRESS_HIGHEST_BIT 30
        #else
          #if defined(MMAP_FIXED_ADDRESS_HIGHEST_BIT)
            #if !(MMAP_FIXED_ADDRESS_HIGHEST_BIT == 30)
              #error Unexpected value of MMAP_FIXED_ADDRESS_HIGHEST_BIT on 32-bit platform!
            #endif
          #else
            #define MMAP_FIXED_ADDRESS_HIGHEST_BIT 30
          #endif
        #endif
      #endif
      #define oint_type_shift SINGLEMAP_oint_type_shift
      #define garcol_bit_o SINGLEMAP_garcol_bit_o
      #define oint_type_len SINGLEMAP_oint_type_len
      #define oint_type_mask SINGLEMAP_oint_type_mask
      #define oint_addr_shift 0
      #define oint_addr_len oint_type_shift
      /* Not just ~oint_type_mask, because evaluation in preprocessor directives uses intmax_t. */
      #define oint_addr_mask ((bitm(pointer_bitsize)-1) & ~oint_type_mask)
      #define oint_data_shift oint_addr_shift
      #define oint_data_len oint_addr_len
      #define oint_data_mask ((1UL<<oint_data_len)-1)
      #if oint_data_len < 24
        #error oint_data_len is too small, short-floats need at least 24 data bits!
      #endif
    #else
      #if defined(TRIVIALMAP_MEMORY)
        /* Due to TRIVIALMAP_MEMORY, heap object and STACK addresses are in
           [MAPPABLE_ADDRESS_RANGE_START, MAPPABLE_ADDRESS_RANGE_END].
           CODE_ADDRESS_RANGE matters here. */
        /* To determine TYPECODES_WITH_TRIVIALMAP_WORKS, run one of
             make -f Makefile.devel build-porting32-gcc-typecodes-spvw_mixed_blocks-trivialmap
             make -f Makefile.devel build-porting32-cc-typecodes-spvw_mixed_blocks-trivialmap
             make -f Makefile.devel build-porting64-gcc-typecodes-spvw_mixed_blocks-trivialmap
             make -f Makefile.devel build-porting64-cc-typecodes-spvw_mixed_blocks-trivialmap
         */
        #if !defined(WIDE_HARD)
          /* 32-bit platforms */
          /* Try to accommodate 7 or 8 type bits.
             But a value of oint_type_shift < 24 is unusable. */
          #undef MAPPABLE_ADDRESS_RANGE_START
          #undef MAPPABLE_ADDRESS_RANGE_END
          #if CODE_ADDRESS_RANGE != 0
            #if MALLOC_ADDRESS_RANGE != 0 && SHLIB_ADDRESS_RANGE != 0
              #define MAPPABLE_ADDRESS_RANGE_START 0x00010000UL
            #else
              /* If we use 0x00xxxxxxUL, there is a collision with malloc or shlibs.
                 If we use CODE_ADDRESS_RANGE + 0x00xxxxxxUL, there is a collision with code.
                 If we use another location, there is room for at most 5+1 type bits, i.e. we
                 run into "Bogus oint_type_mask -- oint_type_mask has more than one extraneous bit!!" */
              #error No way to accommodate 7 type bits, because of CODE_ADDRESS_RANGE.
            #endif
          #endif
          #if defined(UNIX_LINUX) && defined(AMD64) /* Linux/x86_64 with 32-bit x32 ABI */
            #define MAPPABLE_ADDRESS_RANGE_START 0x04000000UL /* 0x04000000UL or 0x08000000UL or 0x10000000UL or 0x20000000UL or 0x40000000UL */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1 /* but only without GENERATIONAL_GC */
          #endif
          #if defined(UNIX_LINUX) && defined(ARM) /* Linux/arm */
            #define MAPPABLE_ADDRESS_RANGE_START 0x08000000UL /* or 0x04000000UL or 0x10000000UL or 0x20000000UL */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 0 /* works on some machines but not on others */
          #endif
          #if defined(UNIX_LINUX) && defined(HPPA) /* Linux/hppa */
            #define MAPPABLE_ADDRESS_RANGE_START 0x08000000UL /* or 0x01000000UL or 0x02000000UL or 0x04000000UL or 0x10000000UL or 0x20000000UL or 0x40000000UL */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1 /* but only without GENERATIONAL_GC */
          #endif
          #if defined(UNIX_LINUX) && defined(I80386) /* Linux/i386, Linux/x86_64 with 32-bit i386 ABI */
            #define MAPPABLE_ADDRESS_RANGE_START 0x00010000UL
            #error No way to accommodate 7 type bits, because of CODE_ADDRESS_RANGE.
          #endif
          #if defined(UNIX_LINUX) && defined(M68K) /* Linux/m68k */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1 /* but only without GENERATIONAL_GC */
          #endif
          #if defined(UNIX_LINUX) && (defined(MIPS) || defined(MIPS64)) /* Linux/mips with o32 or n32 ABI */
            #if !(_MIPS_SIM == _ABIN32) /* Linux/mips with o32 ABI */
              #define MAPPABLE_ADDRESS_RANGE_START 0x01000000UL /* or 0x02000000UL or 0x04000000UL or 0x08000000UL or 0x10000000UL or 0x20000000UL or 0x40000000UL */
              #define TYPECODES_WITH_TRIVIALMAP_WORKS 0
            #else /* Linux/mips with n32 ABI */
              #undef MAPPABLE_ADDRESS_RANGE_START
              #define MAPPABLE_ADDRESS_RANGE_START 0x01000000UL /* or 0x02000000UL or 0x04000000UL or 0x08000000UL or 0x10000000UL or 0x20000000UL or 0x40000000UL */
              #define TYPECODES_WITH_TRIVIALMAP_WORKS 0
            #endif
          #endif
          #if defined(UNIX_LINUX) && defined(POWERPC) /* Linux/powerpc64 with 32-bit ABI */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 0
          #endif
          #if defined(UNIX_LINUX) && defined(S390) /* Linux/s390x with 32-bit ABI */
            #define MAPPABLE_ADDRESS_RANGE_START 0x01000000UL /* or 0x02000000UL or 0x04000000UL or 0x08000000UL or 0x10000000UL or 0x20000000UL or 0x40000000UL */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 0
          #endif
          #if defined(UNIX_LINUX) && defined(SPARC) /* Linux/sparc64 with 32-bit ABI */
            #define MAPPABLE_ADDRESS_RANGE_START 0x00010000UL
            #error No way to accommodate 7 type bits, because of CODE_ADDRESS_RANGE.
          #endif
          #if defined(UNIX_HURD) && defined(I80386) /* Hurd/i386 */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 0
          #endif
          #if (defined(__FreeBSD__) || defined(UNIX_GNU_FREEBSD) || defined(__DragonFly__)) && defined(I80386) /* FreeBSD/i386, GNU/kFreeBSD/i386, DragonFly/i386 */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 0
          #endif
          #if defined(UNIX_NETBSD) && defined(I80386) /* NetBSD/i386 */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1 /* but only without GENERATIONAL_GC */
          #endif
          #if defined(UNIX_NETBSD) && defined(SPARC) /* NetBSD/sparc */
            #define MAPPABLE_ADDRESS_RANGE_START 0x10000000UL /* or 0x01000000UL or 0x02000000UL or 0x04000000UL or 0x08000000UL */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1 /* but only without GENERATIONAL_GC */
          #endif
          #if defined(UNIX_OPENBSD) && defined(I80386) /* OpenBSD/i386 */
            #define MAPPABLE_ADDRESS_RANGE_START 0x00010000UL
            #error No way to accommodate 7 type bits, because of CODE_ADDRESS_RANGE.
          #endif
          #if defined(UNIX_MACOSX) && defined(I80386) /* Mac OS X/x86_64 with 32-bit ABI */
            #define MAPPABLE_ADDRESS_RANGE_START 0x10000000UL /* or 0x02000000UL or 0x04000000UL or 0x08000000UL or 0x20000000UL or 0x40000000UL or 0x80000000UL */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_MACOSX) && defined(POWERPC) /* Mac OS X/PowerPC */
            #define MAPPABLE_ADDRESS_RANGE_START 0x10000000UL /* or 0x02000000UL or 0x04000000UL or 0x08000000UL or 0x20000000UL or 0x40000000UL or 0x80000000UL */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_AIX) && defined(POWERPC) /* AIX/POWER with 32-bit ABI */
            #define MAPPABLE_ADDRESS_RANGE_START 0x00010000UL
            #error No way to accommodate 7 type bits, because of CODE_ADDRESS_RANGE.
          #endif
          #if defined(UNIX_SUNOS5) && defined(I80386) /* Solaris/x86_64 with 32-bit ABI */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 0
          #endif
          #if defined(UNIX_SUNOS5) && defined(SPARC) /* Solaris/sparc64 with 32-bit ABI */
            #define MAPPABLE_ADDRESS_RANGE_START 0x08000000UL /* or 0x01000000UL or 0x02000000UL or 0x04000000UL or 0x10000000UL or 0x20000000UL or 0x40000000UL */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 0
          #endif
          #if defined(UNIX_HAIKU) && defined(I80386) /* Haiku/i386 */
            #define MAPPABLE_ADDRESS_RANGE_START 0x00010000UL
            #error No way to accommodate 7 type bits, because of CODE_ADDRESS_RANGE.
          #endif
          #if defined(UNIX_MINIX) && defined(I80386) /* Minix/i386 */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_CYGWIN) && defined(I80386) /* Cygwin, running on Windows 10 */
            #define MAPPABLE_ADDRESS_RANGE_START 0x40000000UL
            /* Warns "clisp might crash later" and
               produces messages "Cannot map memory to address". */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 0
          #endif
          #if defined(WIN32_NATIVE) && defined(I80386) /* mingw, running on Windows 10 */
            #define MAPPABLE_ADDRESS_RANGE_START 0x08000000UL /* or 0x04000000UL or 0x10000000UL or 0x20000000UL or 0x40000000UL */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 0 /* even without GENERATIONAL_GC */
          #endif
          #define MAPPABLE_ADDRESS_RANGE_END (MAPPABLE_ADDRESS_RANGE_START | 0x00FFFFFFUL)
          #define oint_type_shift 24
          #define oint_type_len 8
          #define oint_addr_shift 0
          #define oint_addr_len oint_type_shift
          #define oint_addr_mask (MAPPABLE_ADDRESS_RANGE_START | CODE_ADDRESS_RANGE | 0x00FFFFFFUL)
          /* Not just ~oint_addr_mask, because evaluation in preprocessor directives uses intmax_t. */
          #define oint_type_mask (0xFFFFFFFFUL & ~oint_addr_mask)
        #else
          /* 64-bit platforms */
          #if defined(UNIX_LINUX) && defined(AMD64) /* Linux/x86_64 */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_LINUX) && defined(ARM64) /* Linux/arm64 */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_LINUX) && defined(DECALPHA) /* Linux/alpha */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_LINUX) && defined(IA64) /* Linux/ia64 */
            #define oint_type_shift 53
            #define garcol_bit_o 60
            #define oint_addr_mask 0xE01FFFFFFFFFFFFFUL
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_LINUX) && defined(MIPS64) /* Linux/mips with 64-bit ABI */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_LINUX) && defined(POWERPC64) /* Linux/powerpc64, Linux/powerpc64le */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_LINUX) && defined(RISCV64) /* Linux/riscv64 */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_LINUX) && defined(S390_64) /* Linux/s390x */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_LINUX) && defined(SPARC64) /* Linux/sparc64 */
            #if 0 /* Does not help for the older machine. */
              #undef MAPPABLE_ADDRESS_RANGE_START
              #undef MAPPABLE_ADDRESS_RANGE_END
              #define MAPPABLE_ADDRESS_RANGE_START 0x0000040000000000UL
              #define MAPPABLE_ADDRESS_RANGE_END   0x00000403FFFFFFFFUL
              #define oint_type_shift 34
              #define garcol_bit_o 41
              #define oint_addr_mask 0xFFFFFD03FFFFFFFFUL
            #endif
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 0 /* 0 on an older machine (gcc-4.6.3), 1 on newer machines (gcc-6.4, gcc-7.2) */
          #endif
          #if (defined(UNIX_FREEBSD) || defined(UNIX_GNU_FREEBSD)) && defined(AMD64) /* FreeBSD/x86_64, GNU/kFreeBSD/x86_64 */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_FREEBSD) && defined(ARM64) /* FreeBSD/arm64 */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_NETBSD) && defined(AMD64) /* NetBSD/x86_64 */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_NETBSD) && defined(SPARC64) /* NetBSD/sparc64 */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS ?
          #endif
          #if defined(UNIX_OPENBSD) && defined(AMD64) /* OpenBSD/x86_64 */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_MACOSX) && defined(AMD64) /* Mac OS X/x86_64 */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 0
          #endif
          #if defined(UNIX_MACOSX) && defined(AMD64) /* macOS 11/arm64 */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_AIX) && defined(POWERPC64) /* AIX/POWER with 64-bit ABI */
            #undef MAPPABLE_ADDRESS_RANGE_END
            #define MAPPABLE_ADDRESS_RANGE_END 0x00FFFFFFFFFFFFFFUL
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 0
          #endif
          #if defined(UNIX_SUNOS5) && defined(AMD64) /* Solaris/x86_64 */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 1
          #endif
          #if defined(UNIX_SUNOS5) && defined(SPARC64) /* Solaris/sparc64 */
            /* Link error: "ld: fatal: relocation error: R_SPARC_H44: file spvw.o: symbol symbol_tab_data: value 0x1000000400 does not fit" */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 0
          #endif
          #if defined(UNIX_CYGWIN) && defined(AMD64) /* Cygwin */
            /* Produces messages "Cannot map memory to address". */
            #define TYPECODES_WITH_TRIVIALMAP_WORKS 0
          #endif
          #define oint_type_len 8
          #ifndef oint_type_shift
            #define oint_type_shift 56
          #endif
          #define oint_addr_shift 0
          #define oint_addr_len oint_type_shift
          #ifndef oint_addr_mask
            #define oint_addr_mask ((1UL<<oint_addr_len)-1)
          #endif
          /* Not just ~oint_addr_mask, because evaluation in preprocessor directives uses intmax_t. */
          #define oint_type_mask (0xFFFFFFFFFFFFFFFFUL & ~oint_addr_mask)
        #endif
      #else
        /* Heap object and STACK addresses are allocated through mymalloc.
           Therefore CODE_ADDRESS_RANGE and MALLOC_ADDRESS_RANGE matter here. */
        /* To determine TYPECODES_WITH_MALLOC_WORKS, run one of
             make -k -f Makefile.devel build-porting32-gcc-typecodes-spvw_mixed_blocks-malloc build-porting32-gcc-typecodes-spvw_mixed_pages build-porting32-gcc-typecodes-spvw_pure_pages
             make -k -f Makefile.devel build-porting32-cc-typecodes-spvw_mixed_blocks-malloc build-porting32-cc-typecodes-spvw_mixed_pages build-porting32-cc-typecodes-spvw_pure_pages
             make -k -f Makefile.devel build-porting64-gcc-typecodes-spvw_mixed_blocks-malloc build-porting64-gcc-typecodes-spvw_mixed_pages build-porting64-gcc-typecodes-spvw_pure_pages
             make -k -f Makefile.devel build-porting64-cc-typecodes-spvw_mixed_blocks-malloc build-porting64-cc-typecodes-spvw_mixed_pages build-porting64-cc-typecodes-spvw_pure_pages
         */
        #if !defined(WIDE_HARD)
          /* 32-bit platforms */
          /* It is not worth testing these configurations without TRIVIALMAP_MEMORY.
             Most of them would fail when starting lisp.run, with the error
             "Return value of malloc() = ... is not compatible with type code distribution." */
          #ifndef oint_addr_mask
            #error "This configuration is not supported without TRIVIALMAP_MEMORY. Add -DTRIVIALMAP_MEMORY to the CFLAGS in the Makefile."
          #endif
        #else
          /* 64-bit platforms */
          /* Nearly the same as with TRIVIALMAP_MEMORY. */
          #if defined(UNIX_LINUX) && defined(AMD64) /* Linux/x86_64 */
            #define TYPECODES_WITH_MALLOC_WORKS 1
          #endif
          #if defined(UNIX_LINUX) && defined(ARM64) /* Linux/arm64 */
            #define TYPECODES_WITH_MALLOC_WORKS 1
          #endif
          #if defined(UNIX_LINUX) && defined(DECALPHA) /* Linux/alpha */
            #define TYPECODES_WITH_MALLOC_WORKS 1
          #endif
          #if defined(UNIX_LINUX) && defined(IA64) /* Linux/ia64 */
            #define oint_type_shift 53
            #define garcol_bit_o 60
            #define oint_addr_mask 0xE01FFFFFFFFFFFFFUL
            #define TYPECODES_WITH_MALLOC_WORKS 0
          #endif
          #if defined(UNIX_LINUX) && defined(MIPS64) /* Linux/mips with 64-bit ABI */
            #define TYPECODES_WITH_MALLOC_WORKS 1
          #endif
          #if defined(UNIX_LINUX) && defined(POWERPC64) /* Linux/powerpc64, Linux/powerpc64le */
            #define TYPECODES_WITH_MALLOC_WORKS 1
          #endif
          #if defined(UNIX_LINUX) && defined(RISCV64) /* Linux/riscv64 */
            #define TYPECODES_WITH_MALLOC_WORKS 1
          #endif
          #if defined(UNIX_LINUX) && defined(S390_64) /* Linux/s390x */
            #define TYPECODES_WITH_MALLOC_WORKS 1
          #endif
          #if defined(UNIX_LINUX) && defined(SPARC64) /* Linux/sparc64 */
            #if 0 /* Does not help for the older machine. */
              /* Avoid error
                 "Return value of malloc() = 00000100004808e0 is not compatible with type code distribution."
                 as well as
                 "Return value of malloc() = fffff8010049a010 is not compatible with type code distribution." */
              #define oint_type_shift 35
              #define garcol_bit_o 42
              #define oint_addr_mask 0xFFFFF907FFFFFFFFUL
            #endif
            #define TYPECODES_WITH_MALLOC_WORKS 0 /* 0 on an older machine (gcc-4.6.3), 1 on newer machines (gcc-6.4, gcc-7.2), but with unexpected "No more room for LISP objects" */
          #endif
          #if (defined(UNIX_FREEBSD) || defined(UNIX_GNU_FREEBSD)) && defined(AMD64) /* FreeBSD/x86_64, GNU/kFreeBSD/x86_64 */
            #define TYPECODES_WITH_MALLOC_WORKS 1
          #endif
          #if defined(UNIX_FREEBSD) && defined(ARM64) /* FreeBSD/arm64 */
            #define TYPECODES_WITH_MALLOC_WORKS 1
          #endif
          #if defined(UNIX_NETBSD) && defined(AMD64) /* NetBSD/x86_64 */
            #define TYPECODES_WITH_MALLOC_WORKS 1
          #endif
          #if defined(UNIX_NETBSD) && defined(SPARC64) /* NetBSD/sparc64 */
            #define TYPECODES_WITH_MALLOC_WORKS ?
          #endif
          #if defined(UNIX_OPENBSD) && defined(AMD64) /* OpenBSD/x86_64 */
            #define TYPECODES_WITH_MALLOC_WORKS 1
          #endif
          #if defined(UNIX_MACOSX) && defined(AMD64) /* Mac OS X/x86_64 */
            #define TYPECODES_WITH_MALLOC_WORKS 0
          #endif
          #if defined(UNIX_MACOSX) && defined(ARM64) /* macOS 11/arm64 */
            #define TYPECODES_WITH_MALLOC_WORKS 1
          #endif
          #if defined(UNIX_AIX) && defined(POWERPC64) /* AIX/POWER with 64-bit ABI */
            #define TYPECODES_WITH_MALLOC_WORKS 0
          #endif
          #if defined(UNIX_SUNOS5) && defined(AMD64) /* Solaris/x86_64 */
            #define TYPECODES_WITH_MALLOC_WORKS 1
          #endif
          #if defined(UNIX_SUNOS5) && defined(SPARC64) /* Solaris/sparc64 */
            /* Link error: "ld: fatal: relocation error: R_SPARC_H44: file spvw.o: symbol symbol_tab_data: value 0x1000000400 does not fit" */
            #define TYPECODES_WITH_MALLOC_WORKS 0
          #endif
          #if defined(UNIX_CYGWIN) && defined(AMD64) /* Cygwin */
            #define TYPECODES_WITH_MALLOC_WORKS 1
          #endif
          #ifndef oint_type_shift
            #define oint_type_shift 56
          #endif
          #ifndef oint_type_len
            #define oint_type_len 8
          #endif
          #define oint_addr_shift 0
          #define oint_addr_len oint_type_shift
          #ifndef oint_addr_mask
            #define oint_addr_mask ((1UL<<oint_addr_len)-1)
          #endif
          /* Not just ~oint_addr_mask, because evaluation in preprocessor directives uses intmax_t. */
          #define oint_type_mask (0xFFFFFFFFFFFFFFFFUL & ~oint_addr_mask)
        #endif
      #endif
      #define oint_data_shift oint_addr_shift
      #define oint_data_len oint_addr_len
      #define oint_data_mask ((1UL<<oint_data_len)-1)
    #endif
  #endif
#endif
%% #if notused
%%  export_def(oint_type_shift);
%%  export_def(oint_type_len);
%%  export_def(oint_type_mask);
%%  export_def(oint_addr_shift);
%%  export_def(oint_addr_len);
%%  export_def(oint_addr_mask);
%% #endif
#ifndef oint_type_len
#error CLISP has not been ported to this platform - oint_type_len undefined
#endif

/* Generally we use all of the space of an address for the data of Fixnums etc.
 Always     [oint_data_shift..oint_data_shift+oint_data_len-1] is subset of
            [oint_addr_shift..oint_addr_shift+oint_addr_len-1],
 thus       oint_data_len <= oint_addr_len. */
#ifndef oint_data_len
  #define oint_data_shift oint_addr_shift
  #define oint_data_len oint_addr_len
  #define oint_data_mask oint_addr_mask
#endif
%% #if notused
%%  export_def(oint_data_shift);
%%  export_def(oint_data_len);
%%  export_def(oint_data_mask);
%% #endif

/* Integer type for typebits: */
/* Use a #if cascade because oint_type_len may expand to an parenthesized expression. */
#if oint_type_len <= 8
typedef uint8  tint;
#elif oint_type_len <= 16
typedef uint16  tint;
#elif oint_type_len <= 32
typedef uint32  tint;
#elif oint_type_len <= 64
typedef uint64  tint;
#else
typedef unsigned_int_with_n_bits(oint_type_len)  tint;
#endif
%% #if oint_type_len > 32 && oint_type_len <= 64
%% sprintf(buf,"uint64"); emit_typedef(buf,"tint");
%% #else
%% sprintf(buf,"uint%d",oint_type_len); emit_typedef(buf,"tint");
%% #endif

/* Integer type for addresses: */
/* Use a #if cascade because oint_addr_len may expand to an parenthesized expression. */
#if defined(WIDE_HARD)
typedef uint64  aint;
typedef sint64  saint;
#elif oint_addr_len <= 8
typedef uint8  aint;
typedef sint8  saint;
#elif oint_addr_len <= 16
typedef uint16  aint;
typedef sint16  saint;
#elif oint_addr_len <= 32
typedef uint32  aint;
typedef sint32  saint;
#elif oint_addr_len <= 64
typedef uint64  aint;
typedef sint64  saint;
#else
typedef unsigned_int_with_n_bits(oint_addr_len)  aint;
typedef signed_int_with_n_bits(oint_addr_len)  saint;
#endif
%% #if defined(WIDE_HARD) || (oint_addr_len > 32 && oint_addr_len <= 64)
%% sprintf(buf,"uint64"); emit_typedef(buf,"aint");
%% #if notused
%% sprintf(buf,"sint64"); emit_typedef(buf,"saint");
%% #endif
%% #else
%% sprintf(buf,"uint%d",oint_addr_len); emit_typedef(buf,"aint");
%% #if notused
%% sprintf(buf,"sint%d",oint_addr_len); emit_typedef(buf,"saint");
%% #endif
%% #endif

/* Integer type for immediate values:
 Always 32 = intLsize <= intVsize <= 64. */
#if (oint_data_len <= 32)
  #define intVsize 32
#else
  #define intVsize 64
#endif
typedef unsigned_int_with_n_bits(intVsize)  uintV;
typedef signed_int_with_n_bits(intVsize)  sintV;
%% sprintf(buf,"uint%d",intVsize); emit_typedef(buf,"uintV");
%% sprintf(buf,"sint%d",intVsize); emit_typedef(buf,"sintV");

/* Integer type used to represent an amount of memory:
 (This may be larger than size_t or ptrdiff_t: size_t is required by ISO C to
 be enough for the size of a single memory block; ptrdiff_t is required by
 ISO C to be enough for the size of a single memory block plus room for a sign
 bit. But on segmented architectures which allow many medium-sized memory
 blocks, like the 80286 was, the total available memory size may be bigger.
 Also, we avoid size_t because it's likely to be wrong on 64-bit Woe32.) */
#if !defined(WIDE_HARD) || ((oint_addr_mask & ~0xFFFFFFFFUL) == 0)
  /* A 32-bit integer is sufficient. */
  #define intMsize  intLsize
  typedef uintL uintM;
  typedef sintL sintM;
#else
  /* An integer as wide as a pointer may be required. */
  #define intMsize  pointer_bitsize
  typedef uintP uintM;
  typedef sintP sintM;
#endif

/* Number of bits by which an address is finally being shifted: */
#ifndef addr_shift
  #define addr_shift 0
#endif
%% #if notused
%%  export_def(addr_shift);
%% #endif

/* Verify the values w.r.t. the earlier configured SINGLEMAP_MEMORY. */
#if defined(SINGLEMAP_MEMORY)
  #if oint_addr_shift != 0
    #error oint_addr_shift must be 0 with SINGLEMAP_MEMORY !!
  #endif
  #if addr_shift != 0
    #error addr_shift must be 0 with SINGLEMAP_MEMORY !!
  #endif
#endif


/* Flavor of the garbage collection: normal or generational. */
#if /* Generational GC requires virtual memory. */                            \
    defined(VIRTUAL_MEMORY)                                                   \
    && /* It requires memory mapping so that every object is visible at a     \
          single address. */                                                  \
       (defined(SINGLEMAP_MEMORY) || defined(TRIVIALMAP_MEMORY))              \
    && /* It requires a working mprotect(). */                                \
       defined(HAVE_WORKING_MPROTECT)                                         \
    && /* It requires SIGSEGV recovery, provided by libsigsegv. */            \
       defined(HAVE_SIGSEGV_RECOVERY)                                         \
    && /* Not worth spending effort on making it work with                    \
          WIDE_SOFT_LARGEFIXNUM. */                                           \
       !defined(WIDE_SOFT_LARGEFIXNUM)                                        \
    && /* It does not work on EdgeRouter Pro (Cavium Octeon II) hardware. */  \
       !(defined(UNIX_LINUX) && (defined(MIPS) || defined(MIPS64)))           \
    && /* It does not work on Linux/riscv64 so far (Linux bug with PROT_NONE). */\
       !(defined(UNIX_LINUX) && defined(RISCV64))                             \
    && /* It does not work on NetBSD 7 (both NetBSD/i386 and NetBSD/sparc). */\
       !defined(UNIX_NETBSD)                                                  \
    && /* It does not work in QEMU user-mode. */                              \
       !((defined(UNIX_LINUX) && defined(HPPA)) || (defined(UNIX_LINUX) && defined(M68K))) \
    && /* Generational GC is tricky stuff. Turn it off at safety 3. */        \
       (SAFETY < 3)                                                           \
    && /* The user can also turn off generational GC explicitly. */           \
       !defined(NO_GENERATIONAL_GC)
  #ifndef GENERATIONAL_GC
    #define GENERATIONAL_GC
  #endif
#endif


/* Put subr_tab and symbol_tab to given addresses through memory-mapping. */
#if defined(SINGLEMAP_MEMORY) && !defined(WIDE_SOFT)
  #define MAP_MEMORY_TABLES
#endif


#ifdef SINGLEMAP_MEMORY
  /* Some type-bit combinations might not be allowed */
  #ifdef vm_addr_mask
    #define tint_allowed_type_mask  ((oint_type_mask & vm_addr_mask) >> oint_type_shift)
  #endif
#endif


/* Complete the definition of the type 'gcv_object_t'. */
#if defined(OBJECT_STRUCT) || defined(WIDE_STRUCT)
  #if defined(WIDE) && !defined(WIDE_HARD)
    #ifdef GENERATIONAL_GC
      /* The generational GC can't deal with an object-pointer that points
       towards two memory pages.
       Thus we enforce alignof(gcv_object_t) = sizeof(gcv_object_t). */
      #define _attribute_aligned_object_  __attribute__ ((aligned(8)))
    #else
      #define _attribute_aligned_object_
    #endif
  #endif
  #ifdef DEBUG_GCSAFETY
    struct object;
    struct gcv_object_t {
      INNARDS_OF_GCV_OBJECT
      /* Conversion to object. */
      operator object () const;
      /* Conversion from object. */
      gcv_object_t (object obj);
      /* Conversion from fake_gcv_object. */
      gcv_object_t (struct fake_gcv_object obj);
      /* Uninitialized object. */
      gcv_object_t ();
    };
  #else
    typedef struct { INNARDS_OF_GCV_OBJECT } gcv_object_t;
  #endif
#endif
#ifndef _attribute_aligned_object_
  #define _attribute_aligned_object_
#endif


/* Define the type 'object'. */
#ifdef DEBUG_GCSAFETY
  /* A counter that is incremented each time an allocation occurs that could
   trigger GC. */
  #if defined(MULTITHREAD)
    /* VTZ: this is slow but there will be need of many forward declarations
     in order to compile. Also in GCSAFETY we do not care about peformance. */
    extern uintL* current_thread_alloccount (void);
    #define alloccount (*current_thread_alloccount())
  #else
    extern uintL alloccount;
  #endif
  /* A register-allocated object contains, if not GC-invariant, the timestamp
   of when it was fetched from a GC-visible location. */
  struct object {
    INNARDS_OF_GCV_OBJECT
    uintL allocstamp;
  };
  /* Always initialize allocstamp with the current(!) value of alloccount. */
  #define INIT_ALLOCSTAMP  , designated_init(allocstamp,alloccount)
#else
  typedef gcv_object_t object;
  #define INIT_ALLOCSTAMP
#endif
%% #ifdef DEBUG_GCSAFETY
%%   #if defined(MULTITHREAD)
%%     exportF(uintL*,current_thread_alloccount,(void));
%%     export_def(alloccount);
%%   #else
%%     exportV(uintL,alloccount);
%%   #endif
%% #else
%%   emit_typedef("gcv_object_t","object");
%% #endif

/* fake_gcv_object(value)
 creates a gcv_object that is actually not seen by GC,
 for use as second word in SKIP2 frames. */
#ifdef DEBUG_GCSAFETY
  struct fake_gcv_object {
    oint fake_value;
    fake_gcv_object (oint value) : fake_value (value) {}
  };
#else
  #define fake_gcv_object(value)  as_object((oint)(value))
#endif
%% #ifdef export_unwind_protect_macros
%%  #ifdef DEBUG_GCSAFETY
%%   puts("struct fake_gcv_object { oint fake_value; fake_gcv_object (oint value) : fake_value (value) {} };");
%%  #else
%%   export_def(fake_gcv_object(value));
%%  #endif
%% #endif

/* Hack for use only in areas where no GC can be triggered. */
#ifdef DEBUG_GCSAFETY
  struct gcunsafe_object_t : gcv_object_t {
    uintL allocstamp;
    /* Conversion from object. */
    gcunsafe_object_t (object obj);
    /* Conversion from gcv_object_t. */
    gcunsafe_object_t (gcv_object_t obj);
    /* Verification that no GC has been triggered. */
    ~gcunsafe_object_t (void);
  };
#else
  typedef gcv_object_t gcunsafe_object_t;
#endif


/* mask of those bits of a tint, which really belong to the type:
 tint_type_mask = oint_type_mask >> oint_type_shift
 (a constant expression, without any 'long long's in it!) */
#ifdef WIDE_SOFT
  #define tint_type_mask  (bitm(oint_type_len)-1)
#else
  #define tint_type_mask  (oint_type_mask >> oint_type_shift)
#endif
%% #if notused
%% export_def(tint_type_mask);
%% #endif

/* To add something to an object/oint:
 objectplus(obj,offset) */
#if !(defined(WIDE_SOFT) || defined(OBJECT_STRUCT))
  #define objectplus(obj,offset)  ((object)pointerplus(obj,offset))
#else /* defined(WIDE_SOFT) || defined(OBJECT_STRUCT) */
  #define objectplus(obj,offset)  as_object(as_oint(obj)+(soint)(offset))
#endif
%% #if !(defined(WIDE_SOFT) || defined(OBJECT_STRUCT))
%%   emit_define("objectplus(obj,offset)","((object)pointerplus(obj,offset))");
%% #else
%%   emit_define("objectplus(obj,offset)","as_object(as_oint(obj)+(soint)(offset))");
%% #endif

/* Bit operations on entities of type uintV:
 ...vbit... instead of ...bit..., "v" = "value". */
#if (intVsize > 32)
  #define vbit(n)  (LL(1)<<(n))
  #define vbitm(n)  (LL(2)<<((n)-1))
  #define vbit_test(x,n)  ((x) & vbit(n))
  #define minus_vbit(n)  (-LL(1)<<(n))
#else
  #define vbit  bit
  #define vbitm  bitm
  #define vbit_test  bit_test
  #define minus_vbit  minus_bit
#endif
%% #if notused
%%  export_def(vbit(n));
%%  export_def(vbitm(n));
%%  export_def(vbit_test(x,n));
%%  export_def(minus_vbit(n));
%% #endif

/* Bit operations on entities of type oint:
 ...wbit... instead of ...bit..., "w" = "wide". */
#if defined(WIDE_SOFT)
  #define wbit(n)  (LL(1)<<(n))
  #define wbitm(n)  (LL(2)<<((n)-1))
  #define wbit_test(x,n)  ((x) & wbit(n))
  #define minus_wbit(n)  (-LL(1)<<(n))
#else
  #define wbit  bit
  #define wbitm  bitm
  #define wbit_test  bit_test
  #define minus_wbit  minus_bit
#endif
%% export_def(wbit);
%% #if notused
%%  export_def(wbitm);
%% #endif
%% export_def(wbit_test);
%% export_def(minus_wbit);

#ifdef TYPECODES

  /* Type info:
   typecode(object) and mtypecode(object) yield the type code of
   an object obj. For mtypecode it has to be in memory. */
  #if !(exact_uint_size_p(oint_type_len) && (tint_type_mask == bit(oint_type_len)-1))
    #define typecode(expr)  \
      ((tint)(as_oint(expr) >> oint_type_shift) & (oint_type_mask >> oint_type_shift))
    #define mtypecode(expr)  typecode(expr)
  #else
    /* The type 'tint' has exactly oint_type_len bits,
     and tint_type_mask = 2^oint_type_len-1.
     So it's not necessary for you to AND.
     On the other hand on a 68000 a ROL.L #8 is faster,
     as is a shift on a SPARC. */
    #define typecode(expr)  ((tint)(as_oint(expr) >> oint_type_shift))
    #if defined(SPARC) && !defined(WIDE)
      #undef typecode
      #define typecode(expr)  \
        ((as_oint(expr) << (32-oint_type_len-oint_type_shift)) >> (32-oint_type_len))
    #elif defined(WIDE) && defined(WIDE_STRUCT)
      #undef typecode
      #define typecode(expr)  ((expr).u.both.type)
    #endif
    /* Furthermore you can do accesses in memory without shift: */
    #if !defined(WIDE) && (oint_type_len==8) && (((oint_type_shift==24) && BIG_ENDIAN_P) || ((oint_type_shift==0) && !BIG_ENDIAN_P))
      #define mtypecode(expr)  (*(tint*)&(expr))
      #define fast_mtypecode
    #elif !defined(WIDE) && (oint_type_len==8) && (((oint_type_shift==24) && !BIG_ENDIAN_P) || ((oint_type_shift==0) && BIG_ENDIAN_P))
      #define mtypecode(expr)  (*((tint*)&(expr)+3))
      #define fast_mtypecode
    #elif defined(WIDE)
      #ifdef WIDE_STRUCT
        #define mtypecode(expr)  ((expr).u.both.type)
      #elif (oint_type_len==8)
        #if ((oint_type_shift==56) && BIG_ENDIAN_P) || ((oint_type_shift==0) && !BIG_ENDIAN_P)
          #define mtypecode(expr)  (*(tint*)&(expr))
        #elif ((oint_type_shift==0) && BIG_ENDIAN_P) || ((oint_type_shift==56) && !BIG_ENDIAN_P)
          #define mtypecode(expr)  (*((tint*)&(expr)+7))
        #endif
      #elif (oint_type_len==16)
        #if ((oint_type_shift==48) && BIG_ENDIAN_P) || ((oint_type_shift==0) && !BIG_ENDIAN_P)
          #define mtypecode(expr)  (*(tint*)&(expr))
        #elif ((oint_type_shift==0) && BIG_ENDIAN_P) || ((oint_type_shift==48) && !BIG_ENDIAN_P)
          #define mtypecode(expr)  (*((tint*)&(expr)+3))
        #endif
      #elif (oint_type_len==32)
        #if ((oint_type_shift==32) && BIG_ENDIAN_P) || ((oint_type_shift==0) && !BIG_ENDIAN_P)
          #define mtypecode(expr)  (*(tint*)&(expr))
        #elif ((oint_type_shift==0) && BIG_ENDIAN_P) || ((oint_type_shift==32) && !BIG_ENDIAN_P)
          #define mtypecode(expr)  (*((tint*)&(expr)+1))
        #endif
      #endif
      #define fast_mtypecode
    #endif
    #ifndef mtypecode
      /* no optimization is possible */
      #undef fast_mtypecode
      #define mtypecode(expr)  typecode(expr)
    #endif
  #endif

  /* Extraction of the address field without type info.
   untype(obj) */
  #if defined(WIDE) && defined(WIDE_STRUCT)
    #define untype(expr)  ((expr).u.both.addr)
  #elif !(defined(SPARC) && (oint_addr_mask==bitm(oint_addr_len)-1))
    #define untype(expr)    \
      ((aint)(as_oint(expr) >> oint_addr_shift) & (aint)(oint_addr_mask >> oint_addr_shift))
  #else
    /* On a SPARC processor long constants are slower than shifts.
       Therefore, one does not need to use AND here. */
    #if (oint_addr_len+oint_addr_shift<=pointer_bitsize)
      #define untype(expr)  \
        (((uintP)as_oint(expr) << (pointer_bitsize-oint_addr_len-oint_addr_shift)) >> (pointer_bitsize-oint_addr_len))
    #else /* oint must be 64 bits wide. */
      #define untype(expr)  \
        ((aint)((as_oint(expr) << (64-oint_addr_len-oint_addr_shift)) >> (64-oint_addr_len)))
    #endif
  #endif

  /* Object from type info and address field:
   type_untype_object(type,address) */
  #if defined(WIDE) && defined(WIDE_STRUCT)
    #if BIG_ENDIAN_P==WIDE_ENDIANNESS
      #define type_untype_object(type,address)  ((object){{(tint)(type),(aint)(address)}INIT_ALLOCSTAMP})
    #else
      #define type_untype_object(type,address)  ((object){{(aint)(address),(tint)(type)}INIT_ALLOCSTAMP})
    #endif
  #elif !(oint_addr_shift==0)
    #define type_untype_object(type,address)  \
      (as_object(  ((oint)(tint)(type) << oint_type_shift) + \
                   ((oint)(aint)(address) << oint_addr_shift) ))
  #else /* you don't have to shift if oint_addr_shift=0: */
    #if defined(WIDE_SOFT)
      /* Beware: Conversion of  address  to oint by Zero-Extend! */
      #define type_untype_object(type,address)              \
        objectplus((oint)(aint)(address),(oint)(tint)(type)<<oint_type_shift)
    #elif defined(OBJECT_STRUCT)
      #define type_untype_object(type,address)              \
        as_object((oint)pointerplus((address),(oint)(tint)(type)<<oint_type_shift))
    #else /* normal case */
      /* In order for this (NIL_IS_CONSTANT) to be a valid initializer
       under gcc-2.5.8, you must not cast from pointer to oint and then
       back to pointer, but you'll have to stay in the pointer's range.. */
      #define type_untype_object(type,address)              \
        as_object(pointerplus((address),(oint)(tint)(type)<<oint_type_shift))
    #endif
  #endif

  /* Object from type info and direct data (as "address"):
   type_data_object(type,data) */
  #if defined(WIDE) && defined(WIDE_STRUCT)
    #if BIG_ENDIAN_P==WIDE_ENDIANNESS
      #define type_data_object(type,data)  ((object){{(tint)(type),(aint)(data)}INIT_ALLOCSTAMP})
    #else
      #define type_data_object(type,data)  ((object){{(aint)(data),(tint)(type)}INIT_ALLOCSTAMP})
    #endif
  #elif !(oint_addr_shift==0)
    #define type_data_object(type,data)  \
      (as_object(  ((oint)(tint)(type) << oint_type_shift) + \
                   ((oint)(aint)(data) << oint_addr_shift) ))
  #else /* if oint_addr_shift=0, you don't have to shift: */
    #define type_data_object(type,data)  \
      (as_object( ((oint)(tint)(type) << oint_type_shift) + (oint)(aint)(data) ))
  #endif

  /* Extraction of the address without type info:
   upointer(obj)
   (upointer means "untyped pointer".) */
  #if (addr_shift==0)
    #define upointer  untype
  #else
    #define optimized_upointer(obj)  \
      ((aint)((as_oint(obj) << (32-oint_addr_len-oint_addr_shift)) >> (32-oint_addr_len-addr_shift)))
    #define upointer(obj)  (untype(obj)<<addr_shift)
  #endif

  /* Object from type info and address:
   type_pointer_object(type,address) */
  #if defined(WIDE_SOFT) && !defined(WIDE_STRUCT)
    /* Cast to uintP, so that conversion of  address  to aint is done by Zero-Extend! */
    #define type_pointer_object(type,address)  \
      type_untype_object(type,(aint)(uintP)(address)>>addr_shift)
  #elif (addr_shift==0)
    /* (No cast to aint, so NIL can be used to initialize.) */
    #define type_pointer_object(type,address)  \
      type_untype_object(type,address)
  #else /* more efficient, */
    /* but this requires address to be divisible by 2^addr_shift: */
    #define type_pointer_object(type,address)  \
      (as_object(((oint)(tint)(type) << oint_type_shift) + \
                 ((oint)(aint)(address) << (oint_addr_shift-addr_shift))))
  #endif

  /* Object from constant type info and constant address:
   type_constpointer_object(type,address) */
  #define type_constpointer_object(type,address)  type_pointer_object(type,address)

  /* oint from constant type info and address = 0:
   type_zero_oint(type) */
  #if defined(WIDE_SOFT) && defined(WIDE_STRUCT)
    #define type_zero_oint(type)  as_oint(type_untype_object(type,0))
  #else
    #define type_zero_oint(type)  ((oint)(tint)(type) << oint_type_shift)
  #endif

#else /* HEAPCODES */

  #ifdef ONE_FREE_BIT_HEAPCODES

    /* We can assume a general alignment of 4 bytes, and thus have the low 2
     bits for encoding type. Here's how we divide the address space:
       machine, frame_pointer  1/4
       subr                    1/4
       cons                    1/8
       varobject               1/4 (not 1/8 because symbol_tab is not 8-aligned)
       immediate               > 0 (anything >= 7/256 does it).
     Note that cons and varobject cannot have the same encoding mod 8
     (otherwise gc_mark:up wouldn't work).
     So, here are the encodings.
       machine             ... .00   encodes pointers, offset 0
       subr                ... .10   encodes pointers, offset 2
       varobject           ... .01   offset 1, the pointers are == 0 mod 4
       cons                ... 011   offset 3, the pointers are == 0 mod 8
       immediate           ... 111
         fixnum            00s 111   s = sign bit
         sfloat            01s 111   s = sign bit
         char              100 111
         small-read-label  110 111
         system            111 111
     Varobjects all start with a word containing the type (1 byte) and a
     length field (up to 24 bits). */

    /* These are the biases, mod 8. */
      #define machine_bias    0UL  /* mod 4 */
      #define subr_bias       2UL  /* mod 4 */
      #define varobject_bias  1UL  /* mod 4 */
      #define cons_bias       3UL  /* mod 8 */
      #define immediate_bias  7UL  /* mod 8 */

    /* Immediate objects have a second type field,
       consisting of the bits imm_type_shift+2,...,imm_type_shift. */

    /* Distinction between fixnums and bignums. */
      #define bignum_bit_o  1
      #define NUMBER_BITS_INVERTED
    /* Distinction between fixnums, short-floats and other kinds of numbers.
     (NB: IMMEDIATE_FFLOAT is not defined for HEAPCODES.) */
      #define number_immediatep(obj)  ((as_oint(obj) & wbit(1)) != 0)

    /* For masking out the nonimmediate biases.
     This must be 3, not 7, otherwise gc_mark won't work. */
      #define nonimmediate_bias_mask  3
      #define nonimmediate_heapcode_mask  3

    /* Combine an object from type info and immediate data.
     type_data_object(type,data) */
      #define type_data_object(type,data)  \
          (as_object(  ((oint)(tint)(type) << oint_type_shift) + \
                       ((oint)(aint)(data) << oint_data_shift) ))

    /* An oint made up with a given type info, and address = 0.
     type_zero_oint(type) */
      #define type_zero_oint(type)  ((oint)(tint)(type) << oint_type_shift)

    /* The GC bit. Addresses may not have this bit set. */
      /* define garcol_bit_o  (already defined above)  # only set during garbage collection */

    /* A bit mask used for testing for immediate objects types:
       #define immediate_object_p(obj)  \
         ((as_oint(obj) & immediate_bias_mask) == immediate_bias)
       It must contain all the bits of immediate_bias. */
      #define immediate_bias_mask  7

    /* Test for gc-invariant object. (This includes immediate, machine, subr.)
     gcinvariant_object_p(obj) */
      #define gcinvariant_object_p(obj)  \
        (((as_oint(obj) & 1) == 0) || immediate_object_p(obj))
      #define gcinvariant_oint_p(obj_o)  \
        ((((obj_o) & 1) == 0) || ((7 & ~(obj_o)) == 0))

    /* Test for gc-invariant object, given only the bias. */
      #define gcinvariant_bias_p(bias)  \
        ((((bias) & 1) == 0) || ((7 & ~(bias)) == 0))

    /* The heap of a heap allocated object. 0 for varobjects, 1 for conses. */
      #define nonimmediate_heapnr(obj)  \
        ((as_oint(obj) >> 1) & 1)

  #endif /* ONE_FREE_BIT_HEAPCODES */

  #ifdef KERNELVOID32_HEAPCODES

    /* We must assume a general alignment of 4 bytes and an enforced alignment
     of 8 bytes for Lisp objects, and thus have the low 2 to 3 bits for
     encoding heap and the garcol_bit. Here's how we divide the address space:
       machine, frame_pointer  1/4 * 7/8
       immediate               1/4 * 1/8
       cons                    1/8
       varobject               1/8
     Note that cons and varobject cannot have the same encoding mod 8
     (otherwise gc_mark:up wouldn't work).
     Immediates look like pointers in the range 0xC0000000..0xFFFFFFFF.
     We know that the Linux kernel never assigns virtual memory in this area.
     So, here are the encodings. Bit 0 is used as the garcol_bit.
     With KERNELVOID32A_HEAPCODES:
       machine                 ... ... .00   encodes pointers, offset 0
       cons                    ... ... 010   offset 2, the pointers are == 0 mod 8
       varobject               ... ... 110   offset 6, the pointers are == 4 mod 8
       immediate           110 ... ...  00
         fixnum            110 ... 00s  00   s = sign bit
         sfloat            110 ... 01s  00   s = sign bit
         char              110 ... 100  00
         small-read-label  110 ... 110  00
         system            110 ... 111  00
     With KERNELVOID32B_HEAPCODES:
       machine                 ... ... .00   encodes pointers, offset 0
       cons                    ... ... 010   offset 2, the pointers are == 0 mod 8
       varobject               ... ... 110   offset 6, the pointers are == 4 mod 8
       immediate           111 ... ...  00
         fixnum            111 ... 00s  00   s = sign bit
         sfloat            111 ... 01s  00   s = sign bit
         char              111 ... 100  00
         small-read-label  111 ... 110  00
         system            111 ... 111  00
     Varobjects all start with a word containing the type (1 byte) and a
     length field (up to 24 bits). */

    /* These are the biases. */
      #define machine_bias    0  /* + 0 mod 4 */
      #define varobject_bias  2  /* + 4 mod 8 */
      #define cons_bias       2  /* + 0 mod 8 */
      #ifdef KERNELVOID32A_HEAPCODES
        #define immediate_bias  0xC0000000  /* + 0 mod 4 */
      #endif
      #ifdef KERNELVOID32B_HEAPCODES
        #define immediate_bias  0xE0000000  /* + 0 mod 4 */
      #endif
      #define subr_bias       varobject_bias

    /* Immediate objects have a second type field. */
      #define imm_type_shift  2  /* could also be 3, if oint_data_shift == 6 */

    /* Distinction between fixnums and bignums. */
      #define bignum_bit_o  1
    /* Distinction between fixnums, short-floats and other kinds of numbers.
     (NB: IMMEDIATE_FFLOAT is not defined for HEAPCODES.) */
      #define number_immediatep(obj)  ((as_oint(obj) & wbit(1)) == 0)

    /* The misalignment of varobjects, modulo varobject_alignment. */
      #define varobjects_misaligned  4

    /* For masking out the nonimmediate biases.
       nonimmediate_bias_mask must be <= sizeof(gcv_object_t)-1, due to the way
       gc_mark works. */
      #define nonimmediate_bias_mask  3
      #define nonimmediate_heapcode_mask  7

    /* Combine an object from type info and immediate data.
     type_data_object(type,data) */
      #define type_data_object(type,data)  \
          (as_object(  ((oint)(tint)(type) << oint_type_shift) + \
                       ((oint)(aint)(data) << oint_data_shift) ))

    /* An oint made up with a given type info, and address = 0.
     type_zero_oint(type) */
      #define type_zero_oint(type)  ((oint)(tint)(type) << oint_type_shift)

    /* The GC bit. Addresses may not have this bit set. */
      /* define garcol_bit_o  (already defined above)  # only set during garbage collection */

    /* A bit mask used for testing for immediate objects types:
       #define immediate_object_p(obj)  \
         ((as_oint(obj) & immediate_bias_mask) == immediate_bias)
       It must contain all the bits of immediate_bias. */
      #define immediate_bias_mask  0xE0000003

    /* Test for gc-invariant object. (This includes immediate, machine.)
     gcinvariant_object_p(obj) */
      #define gcinvariant_object_p(obj)  \
        ((as_oint(obj) & bit(1)) == 0)
      #define gcinvariant_oint_p(obj_o)  \
        (((obj_o) & bit(1)) == 0)
    /* NB: Subrs are not included in this test, because subrp(obj) require a
     memory access. */

    /* Test for gc-invariant object, given only the bias. */
      #define gcinvariant_bias_p(bias)  \
        (((bias) & 2) == 0)

    /* The heap of a heap allocated object. 0 for varobjects, 1 for conses. */
      #define nonimmediate_heapnr(obj)  \
        (1 & ~(as_oint(obj) >> 2))

  #endif /* KERNELVOID32_HEAPCODES */

  #ifdef GENERIC64_HEAPCODES

    #ifdef GENERIC64A_HEAPCODES

      /* We must assume a general alignment of 8 bytes for Lisp objects, and thus
       have the low 3 bits for encoding heap and the garcol_bit. Here's how we
       divide the address space:
         machine, frame_pointer  1/4 * 7/8
         immediate               1/4 * 1/8
         cons                    1/8
         varobject               1/8
       Note that cons and varobject cannot have the same encoding mod 8
       (otherwise gc_mark:up wouldn't work).
       Immediates look like pointers in the range
       0xC000000000000000..0xFFFFFFFFFFFFFFFF.
       We know that the Linux kernel never assigns virtual memory in this area.
       So, here are the encodings. Bit 0 is used as the garcol_bit.
         machine                 ... ...  00   encodes pointers, offset 0
         cons                    ... ... 010   offset 2, the pointers are == 0 mod 8
         varobject               ... ... 110   offset 6, the pointers are == 0 mod 8
         immediate           110 ... ...  00
           fixnum            110 ... 00s  00   s = sign bit
           sfloat            110 ... 01s  00   s = sign bit
           char              110 ... 100  00
           small-read-label  110 ... 110  00
           system            110 ... 111  00
       Varobjects all start with a word containing the type (1 byte) and a
       length field (up to 24 bits). */

      /* These are the biases. */
        #define machine_bias    0  /* + 0 mod 4 */
        #define varobject_bias  6  /* + 0 mod 8 */
        #define cons_bias       2  /* + 0 mod 8 */
        #define immediate_bias  0xC000000000000000UL  /* + 0 mod 8 */
        #define subr_bias       varobject_bias

      /* Immediate objects have a second type field. */
        #define imm_type_shift  2  /* could also be 3, if oint_data_shift == 6 */

    #endif /* GENERIC64A_HEAPCODES */

    #ifdef GENERIC64B_HEAPCODES

      /* We must assume a general alignment of 8 bytes for Lisp objects, and thus
       have the low 3 bits for encoding heap and the garcol_bit. Here's how we
       divide the address space:
         machine, frame_pointer  1/4 * 7/8
         immediate               1/8 * 1/8
         cons                    1/8
         varobject               1/8
       Note that cons and varobject cannot have the same encoding mod 8
       (otherwise gc_mark:up wouldn't work).
       So, here are the encodings. Bit 0 is used as the garcol_bit.
         machine                 ... ...  00   encodes pointers, offset 0
         cons                    ... ... 010   offset 2, the pointers are == 0 mod 8
         varobject               ... ... 110   offset 6, the pointers are == 0 mod 8
         immediate           110 ... ... 100
           fixnum            110 ... 00s 100   s = sign bit
           sfloat            110 ... 01s 100   s = sign bit
           char              110 ... 100 100
           small-read-label  110 ... 110 100
           system            110 ... 111 100
       Varobjects all start with a word containing the type (1 byte) and a
       length field (up to 24 bits). */

      /* These are the biases. */
        #define machine_bias    0  /* + 0 mod 4 */
        #define varobject_bias  6  /* + 0 mod 8 */
        #define cons_bias       2  /* + 0 mod 8 */
        #define immediate_bias  0xC000000000000004UL  /* + 0 mod 8 */
        #define subr_bias       varobject_bias

      /* Immediate objects have a second type field. */
        #define imm_type_shift  3  /* relies on oint_data_shift == 6 */

    #endif /* GENERIC64B_HEAPCODES */

    #ifdef GENERIC64C_HEAPCODES

      /* We must assume a general alignment of 8 bytes for Lisp objects, and thus
       have the low 3 bits for encoding heap and the garcol_bit. Here's how we
       divide the address space:
         machine, frame_pointer  1/8
         immediate               1/8
         cons                    1/8
         varobject               1/8
       Note that cons and varobject cannot have the same encoding mod 8
       (otherwise gc_mark:up wouldn't work).
       So, here are the encodings. Bit 0 is used as the garcol_bit.
         machine                ... ... 000   encodes pointers, offset 0
         cons                   ... ... 010   offset 2, the pointers are == 0 mod 8
         varobject              ... ... 110   offset 6, the pointers are == 0 mod 8
         immediate              ... ... 100
           fixnum               ... 00s 100   s = sign bit
           sfloat               ... 01s 100   s = sign bit
           char                 ... 100 100
           small-read-label     ... 110 100
           system               ... 111 100
       Varobjects all start with a word containing the type (1 byte) and a
       length field (up to 24 bits). */

      /* These are the biases. */
        #define machine_bias    0  /* + 0 mod 8 */
        #define varobject_bias  6  /* + 0 mod 8 */
        #define cons_bias       2  /* + 0 mod 8 */
        #define immediate_bias  4  /* + 0 mod 8 */
        #define subr_bias       varobject_bias

      /* Immediate objects have a second type field. */
        #define imm_type_shift  3  /* relies on oint_data_shift == 6 */

    #endif /* GENERIC64C_HEAPCODES */

    /* Distinction between fixnums and bignums. */
      #define bignum_bit_o  1
    /* Distinction between fixnums, short-floats and other kinds of numbers.
     (NB: IMMEDIATE_FFLOAT is not defined for HEAPCODES.) */
      #define number_immediatep(obj)  ((as_oint(obj) & wbit(1)) == 0)

    /* For masking out the nonimmediate biases. */
      #define nonimmediate_bias_mask  7
      #define nonimmediate_heapcode_mask  7

    /* Combine an object from type info and immediate data.
     type_data_object(type,data) */
      #define type_data_object(type,data)  \
          (as_object(  ((oint)(tint)(type) << oint_type_shift) + \
                       ((oint)(aint)(data) << oint_data_shift) ))

    /* An oint made up with a given type info, and address = 0.
     type_zero_oint(type) */
      #define type_zero_oint(type)  ((oint)(tint)(type) << oint_type_shift)

    /* The GC bit. Addresses may not have this bit set. */
      /* define garcol_bit_o  (already defined above)  # only set during garbage collection */

    /* A bit mask used for testing for immediate objects types:
       #define immediate_object_p(obj)  \
         ((as_oint(obj) & immediate_bias_mask) == immediate_bias)
       It must contain all the bits of immediate_bias. */
    #ifdef GENERIC64A_HEAPCODES
      #define immediate_bias_mask  0xE000000000000003UL
    #endif
    #ifdef GENERIC64B_HEAPCODES
      #define immediate_bias_mask  0xE000000000000007UL
    #endif
    #ifdef GENERIC64C_HEAPCODES
      #define immediate_bias_mask  7
    #endif

    /* Test for gc-invariant object. (This includes immediate, machine.)
     gcinvariant_object_p(obj) */
      #define gcinvariant_object_p(obj)  \
        ((as_oint(obj) & bit(1)) == 0)
      #define gcinvariant_oint_p(obj_o)  \
        (((obj_o) & bit(1)) == 0)
    /* NB: Subrs are not included in this test, because subrp(obj) require a
     memory access. */

    /* Test for gc-invariant object, given only the bias. */
      #define gcinvariant_bias_p(bias)  \
        (((bias) & 2) == 0)

    /* The heap of a heap allocated object. 0 for varobjects, 1 for conses. */
      #define nonimmediate_heapnr(obj)  \
        (1 & ~(as_oint(obj) >> 2))

  #endif /* GENERIC64_HEAPCODES */

  /* The types of immediate objects. */
    #define fixnum_type            ((0 << imm_type_shift) + immediate_bias)
    #define sfloat_type            ((2 << imm_type_shift) + immediate_bias)
    #define char_type              ((4 << imm_type_shift) + immediate_bias)
    #define small_read_label_type  ((6 << imm_type_shift) + immediate_bias)
    #define system_type            ((7 << imm_type_shift) + immediate_bias)

  /* The sign bit, for immediate numbers only. */
    #define sign_bit_t  (0 + imm_type_shift)
    #define sign_bit_o  (sign_bit_t+oint_type_shift)

  /* Test for immediate object.
   immediate_object_p(obj) */
    #if (immediate_bias_mask == immediate_bias)
      /* Small optimization */
      #define immediate_object_p(obj)  \
        ((immediate_bias_mask & ~as_oint(obj)) == 0)
    #else
      #define immediate_object_p(obj)  \
        ((as_oint(obj) & immediate_bias_mask) == immediate_bias)
    #endif

#endif /* TYPECODES */
%% #ifdef TYPECODES
%%  export_def(typecode(expr));
%%  export_def(mtypecode(expr));
%%  export_def(type_untype_object(type,address));
%%  export_def(upointer(obj));
%%  export_def(type_pointer_object(type,address));
%%  export_def(type_constpointer_object(type,address));
%% #else
%%  export_def(number_immediatep(obj));
%%  export_def(immediate_object_p(obj));
%%  export_def(gcinvariant_object_p(obj));
%%  export_def(gcinvariant_oint_p(obj_o));
%%  export_def(gcinvariant_bias_p(bias));
%% #endif
%% export_def(type_data_object(type,address));
%% export_def(type_zero_oint(obj));

/* The misalignment of varobjects, modulo varobject_alignment. */
#ifndef varobjects_misaligned
  #define varobjects_misaligned  0
#endif
#if varobjects_misaligned
  #define VAROBJECTS_ALIGNMENT_DUMMY_DECL  char alignment_dummy[varobjects_misaligned];
#else
  #define VAROBJECTS_ALIGNMENT_DUMMY_DECL
#endif
%% export_def(varobjects_misaligned);
%% export_def(VAROBJECTS_ALIGNMENT_DUMMY_DECL);
/* If varobjects are misaligned, fields of varobjects must be aligned on 4-byte
   boundaries only, even if they are of type 'double' or 'uint64'. This is
   necessary for KERNELVOID32_HEAPCODES on Linux/sparc and NetBSD/sparc. */
#if defined(GNU) && varobjects_misaligned
  #define _attribute_in_misaligned_varobjects_ __attribute__ ((packed, aligned (4)))
#else
  #define _attribute_in_misaligned_varobjects_
#endif
%% #if defined(GNU) && varobjects_misaligned
%%   #define attribute_in_misaligned_varobjects " __attribute__ ((packed, aligned (4)))"
%% #else
%%   #define attribute_in_misaligned_varobjects ""
%% #endif

/* The misalignment of conses, modulo 2*sizeof(gcv_object_t).
   Note: It is more efficient w.r.t. to the data cache to misalign the
   varobjects than the conses. */
#ifndef conses_misaligned
  #define conses_misaligned  0
#endif


/* Pointers that are arguments of make_machine() must have a certain
   alignment. */
#ifdef TYPECODES
  #define PSEUDODATA_ALIGNMENT 1
  #define log2_PSEUDODATA_ALIGNMENT 0
#else /* HEAPCODES */
  #ifdef GENERIC64C_HEAPCODES
    /* See above:  machine                 ... ... 000 */
    #define PSEUDODATA_ALIGNMENT 8
    #define log2_PSEUDODATA_ALIGNMENT 3
  #else
    /* See above:  machine                 ... ...  00 */
    #define PSEUDODATA_ALIGNMENT 4
    #define log2_PSEUDODATA_ALIGNMENT 2
  #endif
#endif


/* Objects with variable length must reside at addresses that are divisible
   by 2 or 4 or 8. This alignment should be so large as to avoid misaligned
   accesses to 'long' and especially 'double' fields in varobjects.
   Look
   1. at alignof(long), alignof(double) (matters only if FAST_DOUBLE),
   2. at the definition of BIGGEST_ALIGNMENT and BIGGEST_FIELD_ALIGNMENT in
      gcc-5.4.0/gcc/config/<cpu>/<cpu>.h, divided by 8. */
#if defined(M68K)
  #if addr_shift!=0
    #define varobject_alignment  bit(addr_shift)  /* because of the condensed distribution of typecodes */
  #else
    #define varobject_alignment  2
  #endif
#endif
#if defined(I80386) || defined(POWERPC) || defined(ARM) || defined(S390)
  #define varobject_alignment  4
#endif
#if defined(SPARC) || defined(HPPA) || defined(MIPS) || defined(DECALPHA) || defined(IA64) || defined(AMD64) || defined(ARM64) || defined(RISCV64)
  #define varobject_alignment  8
#endif
#if (!defined(TYPECODES) || defined(GENERATIONAL_GC)) && (varobject_alignment < 4)
  #undef varobject_alignment
  #define varobject_alignment  4
#endif
#if ((defined(GENERATIONAL_GC) && defined(WIDE)) || defined(KERNELVOID32_HEAPCODES) || defined(GENERIC64_HEAPCODES)) && (varobject_alignment < 8)
  #undef varobject_alignment
  #define varobject_alignment  8
#endif
/* varobject_alignment should be defined: */
#ifndef varobject_alignment
  #error varobject_alignment depends on CPU -- readjust varobject_alignment!!
#endif
/* varobject_alignment should be a power of 2: */
#if !((varobject_alignment & (varobject_alignment-1)) ==0)
  #error Bogus varobject_alignment -- readjust varobject_alignment!!
#endif
/* varobject_alignment should be a multiple of 2^addr_shift : */
#if (varobject_alignment % bit(addr_shift))
  #error Bogus varobject_alignment -- readjust varobject_alignment!!
#endif
%% export_def(varobject_alignment);

/* In some cases it is required that sizeof(symbol_) is a multiple of varobject_alignment.
   - When KERNELVOID32_HEAPCODES || GENERIC64_HEAPCODES.
   - When MAP_MEMORY_TABLES, because symbol_tab_data (with elements of size sizeof(symbol_))
     gets copied to symbol_tab (into the heap, where varobject_alignment is a requirement). */
#if defined(KERNELVOID32_HEAPCODES) || defined(GENERIC64_HEAPCODES) || defined(MAP_MEMORY_TABLES)
 #define sizeof_symbol_is_multiple_of_varobject_alignment
#endif


#ifdef TYPECODES

/* Now we'll define the various type bits and type codes. */

/* Single-floats can be immediate objects, like short-floats, if there are
 enough bits in a 'gcv_object_t'. */
#if defined(WIDE_HARD) || defined(WIDE_SOFT)
  #define IMMEDIATE_FFLOAT
#endif

/* Determine whether a type isn't changed by the GC
 (ie. if it's not a pointer): */
  #if 0 && (defined(GNU) || defined(INTEL))
    #define gcinvariant_type_p(type)  \
      ({var bool _erg;                         \
        switch (type)                          \
          { case_machine:                      \
            case_char: case_subr: case_system: \
            case_fixnum: case_sfloat:          \
            /* with WIDE also: case_ffloat: */ \
              _erg = true; break;              \
            default: _erg = false; break;      \
          }                                    \
        _erg;                                  \
       })
  #endif

#ifndef tint_allowed_type_mask
  #define tint_allowed_type_mask  tint_type_mask
#endif

/* There are 7 to 8 type bits available: TB7, [TB6,] TB5, TB4, ..., TB0.
 All of them have to be set in tint_allowed_type_mask and thus in tint_type_mask as well
 We distribute them under the assumption that only one bit is missing in tint_type_mask.
 TB6 will be set to -1, if it can't be used. */
#if defined(SINGLEMAP_MEMORY)
  /* TB7 will be used as garcol_bit.
     bit(TB7)|SINGLEMAP_ADDRESS_BASE does not need to be a mappable address. */
  #if defined(garcol_bit_o)
    #define TB7 (garcol_bit_o-oint_type_shift)
  #else
    #if ((oint_type_mask >> (oint_type_len-1 + oint_type_shift)) & 1) != 0
      #define TB7 (oint_type_len-1)
    #elif ((oint_type_mask >> (oint_type_len-2 + oint_type_shift)) & 1) != 0
      #define TB7 (oint_type_len-2)
    #else
      #error What is the right value for TB7?
    #endif
  #endif
  /* The other bits must be allocated in tint_type_mask.
     Assume tint_type_mask has 6..7 bits and up to 3 "holes". */
  #if (((tint_type_mask >> 0) & 1) != 0) && (0 != TB7)
    #define TB0 0
  #endif
  #if (((tint_type_mask >> 1) & 1) != 0) && (1 != TB7)
    #if !defined(TB0)
      #define TB0 1
    #else
      #define TB1 1
    #endif
  #endif
  #if (((tint_type_mask >> 2) & 1) != 0) && (2 != TB7)
    #if !defined(TB0)
      #define TB0 2
    #elif !defined(TB1)
      #define TB1 2
    #else
      #define TB2 2
    #endif
  #endif
  #if (((tint_type_mask >> 3) & 1) != 0) && (3 != TB7)
    #if !defined(TB0)
      #define TB0 3
    #elif !defined(TB1)
      #define TB1 3
    #elif !defined(TB2)
      #define TB2 3
    #else
      #define TB3 3
    #endif
  #endif
  #if (((tint_type_mask >> 4) & 1) != 0) && (4 != TB7)
    #if !defined(TB1)
      #define TB1 4
    #elif !defined(TB2)
      #define TB2 4
    #elif !defined(TB3)
      #define TB3 4
    #else
      #define TB4 4
    #endif
  #endif
  #if (((tint_type_mask >> 5) & 1) != 0) && (5 != TB7)
    #if !defined(TB2)
      #define TB2 5
    #elif !defined(TB3)
      #define TB3 5
    #elif !defined(TB4)
      #define TB4 5
    #else
      #define TB5 5
    #endif
  #endif
  #if (((tint_type_mask >> 6) & 1) != 0) && (6 != TB7)
    #if !defined(TB3)
      #define TB3 6
    #elif !defined(TB4)
      #define TB4 6
    #elif !defined(TB5)
      #define TB5 6
    #else
      #define TB6 6
    #endif
  #endif
  #if (((tint_type_mask >> 7) & 1) != 0) && (7 != TB7)
    #if !defined(TB4)
      #define TB4 7
    #elif !defined(TB5)
      #define TB5 7
    #elif !defined(TB6)
      #define TB6 7
    #endif
  #endif
  #if (((tint_type_mask >> 8) & 1) != 0) && (8 != TB7)
    #if !defined(TB5)
      #define TB5 8
    #elif !defined(TB6)
      #define TB6 8
    #endif
  #endif
  #if (((tint_type_mask >> 9) & 1) != 0) && (9 != TB7)
    #if !defined(TB6)
      #define TB6 9
    #endif
  #endif
  #if !defined(TB6)
    #define TB6 -1
  #endif
#else
  /* In the normal TYPECODES model, the type bits are packed into 8 bits. */
  #if ((0xFF & ~tint_allowed_type_mask) == 0)
    #define TB7 7
    #define TB6 6
    #define TB5 5
    #define TB4 4
    #define TB3 3
    #define TB2 2
    #define TB1 1
    #define TB0 0
  #elif (oint_type_len==7)
    #define TB7 6
    #define TB6 -1
    #define TB5 5
    #define TB4 4
    #define TB3 3
    #define TB2 2
    #define TB1 1
    #define TB0 0
  #else
    /* Some bits have to be avoided */
    #define tint_avoid  ((bitm(oint_type_len)-1) & ~tint_allowed_type_mask)
    /* tint_avoid must only contain one bit: */
    #if (tint_avoid & (tint_avoid-1))
      #error Bogus oint_type_mask -- oint_type_mask has more than one extraneous bit!!
    #endif
    /* tint_avoid consists of exactly one bit that has to be avoided. */
    #if (tint_avoid > bit(0))
      #define TB0 0
    #else
      #define TB0 1
    #endif
    #if (tint_avoid > bit(1))
      #define TB1 1
    #else
      #define TB1 2
    #endif
    #if (tint_avoid > bit(2))
      #define TB2 2
    #else
      #define TB2 3
    #endif
    #if (tint_avoid > bit(3))
      #define TB3 3
    #else
      #define TB3 4
    #endif
    #if (tint_avoid > bit(4))
      #define TB4 4
    #else
      #define TB4 5
    #endif
    #if (tint_avoid > bit(5))
      #define TB5 5
    #else
      #define TB5 6
    #endif
    #if ((tint_allowed_type_mask & ~0xFF) == 0)
      #define TB6 -1
      #if (tint_avoid > bit(6))
        #define TB7 6
      #else
        #define TB7 7
      #endif
    #else
      #if (tint_avoid > bit(6))
        #define TB6 6
      #else
        #define TB6 7
      #endif
      #if (tint_avoid > bit(7))
        #define TB7 7
      #else
        #define TB7 8
      #endif
    #endif
  #endif
#endif

/* bit masks for the type bits: */
  #define BTB0  bit(TB0)
  #define BTB1  bit(TB1)
  #define BTB2  bit(TB2)
  #define BTB3  bit(TB3)
  #define BTB4  bit(TB4)
  #define BTB5  bit(TB5)
  #define BTB6  bit(TB6)
  #define BTB7  bit(TB7)

#define STANDARD_8BIT_TYPECODES

#ifdef STANDARD_8BIT_TYPECODES

#if defined(I80386) && defined(UNIX_LINUX) && (CODE_ADDRESS_RANGE == 0)
  /* At 0x60000000 there are the shared-libraries.
   At 0x50000000 (Linux 1.2) resp. 0x40000000 (Linux 2.0) there are several
   mmap-pages,for example ones allocated  by setlocale() or gettext().
   Therefore we only have to do a few changes to the distribution of the type codes. */
#endif

#if defined(I80386) && defined(UNIX_LINUX) && (CODE_ADDRESS_RANGE != 0)
  /* Code and malloc memory is at 0x08000000.
   Therefore avoid allocating typecode 0x08 for the moment. */
#endif

#if (defined(M68K) || defined(SPARC)) && defined(UNIX_LINUX)
  /* At 0x50000000 there are shared libraries located.
   But this doesn't mean we have to change the type code distribution. */
#endif

#if (defined(MIPS) || defined(POWERPC)) && defined(UNIX_LINUX)
  /* At 0x2AAAB000 there are shared libraries located.
   But this doesn't mean we have to change the type code distribution. */
#endif

#if defined(SPARC64) && defined(UNIX_LINUX)
  /* At 0x70000000 there are shared libraries located.
   But this doesn't mean we have to change the type code distribution. */
#endif

/* Type bits:
 in Typcodes (tint): */
  #define garcol_bit_t     TB7  /* only set during GC */
  #if (TB6 >= 0)
    #define cons_bit_t     TB6  /* only set for CONS */
  #endif
  #define number_bit_t     TB5  /* only set for numbers */
  #define notsimple_bit_t  TB3  /* for arrays: deleted for simple arrays */
  #define sign_bit_t       TB0  /* Sign for real numbers (set <==> number <0) */
  #define float_bit_t      TB1
  #define float1_bit_t     TB3
  #define float2_bit_t     TB2
  #define ratio_bit_t      TB3
  #define bignum_bit_t     TB2
/* in Objects (oint): */
  #if !defined(garcol_bit_o)
    #define garcol_bit_o   (garcol_bit_t+oint_type_shift)    /* only set during the garbage collection! */
  #else
    /* Verify garcol_bit_o has the expected value. */
    #if !(garcol_bit_o == (garcol_bit_t+oint_type_shift))
      #error garcol_bit_o already defined, but is not consistent with TB7!
    #endif
  #endif
  #if (TB6 >= 0)
    #define cons_bit_o     (cons_bit_t+oint_type_shift)      /* only set for cons CONS */
  #endif
  #define number_bit_o     (number_bit_t+oint_type_shift)    /* only set for numbers */
  #define notsimple_bit_o  (notsimple_bit_t+oint_type_shift) /* for arrays: deleted for simple arrays */
  #define sign_bit_o       (sign_bit_t+oint_type_shift)      /* Sign for real numbers */
  #define float_bit_o      (float_bit_t+oint_type_shift)
  #define float1_bit_o     (float1_bit_t+oint_type_shift)
  #define float2_bit_o     (float2_bit_t+oint_type_shift)
  #define ratio_bit_o      (ratio_bit_t+oint_type_shift)
  #define bignum_bit_o     (bignum_bit_t+oint_type_shift)

/* constant type codes: */
  #define machine_type    (0)                                  /* 0x00  # %00000000  ; machine pointer */
  #define subr_type       (                              BTB0) /* 0x01  # %00000001  ; SUBR */
  #define char_type       (                         BTB1     ) /* 0x02  # %00000010  ; character */
  #define system_type     (                         BTB1|BTB0) /* 0x03  # %00000011  ; frame-pointer, small-read-label, system */
  #define symbol_type     (                    BTB2          ) /* 0x04  # %000001xx  ; symbol */
          /* bits for symbols in the GCself pointer: */
          #define var_bit0_t  TB0  /* set if the symbol is proclaimed SPECIAL or constant */
          #define var_bit1_t  TB1  /* set if the symbol is a symbol-macro or constant */
  #if (TB6 < 0)
  #define cons_type       (               BTB3               ) /* 0x08  # %00001000  ; cons */
  #endif
  #define closure_type    (               BTB3          |BTB0) /* 0x09  # %00001001  ; closure */
  #define structure_type  (               BTB3     |BTB1     ) /* 0x0A  # %00001010  ; structure */
  #define stream_type     (               BTB3     |BTB1|BTB0) /* 0x0B  # %00001011  ; stream */
  #define orecord_type    (               BTB3|BTB2          ) /* 0x0C  # %00001100  ; OtherRecord (Package, Byte, ...) */
  #define instance_type   (               BTB3|BTB2     |BTB0) /* 0x0D  # %00001101  ; CLOS instance */
  #define lrecord_type    (               BTB3|BTB2|BTB1     ) /* 0x0E  # %00001110  ; LongRecord (WeakList, WeakAlist, ...) */
  #define mdarray_type    (               BTB3|BTB2|BTB1|BTB0) /* 0x0F  # %00001111  ; other array (rank/=1 or other eltype) */
  #define sbvector_type   (          BTB4                    ) /* 0x10  # %00010000  ; simple-bit-vector */
  #define sb2vector_type  (          BTB4               |BTB0) /* 0x11  # %00010001  ; simple (VECTOR (UNSIGNED-BYTE 2)) */
  #define sb4vector_type  (          BTB4          |BTB1     ) /* 0x12  # %00010010  ; simple (VECTOR (UNSIGNED-BYTE 4)) */
  #define sb8vector_type  (          BTB4          |BTB1|BTB0) /* 0x13  # %00010011  ; simple (VECTOR (UNSIGNED-BYTE 8)) */
  #define sb16vector_type (          BTB4     |BTB2          ) /* 0x14  # %00010100  ; simple (VECTOR (UNSIGNED-BYTE 16)) */
  #define sb32vector_type (          BTB4     |BTB2     |BTB0) /* 0x15  # %00010101  ; simple (VECTOR (UNSIGNED-BYTE 32)) */
  #define sstring_type    (          BTB4     |BTB2|BTB1     ) /* 0x16  # %00010110  ; simple-string */
  #define svector_type    (          BTB4     |BTB2|BTB1|BTB0) /* 0x17  # %00010111  ; simple-vector */
  #define bvector_type    (          BTB4|BTB3               ) /* 0x18  # %00011000  ; non-simple bit-vector */
  #define b2vector_type   (          BTB4|BTB3          |BTB0) /* 0x19  # %00011001  ; non-simple (VECTOR (UNSIGNED-BYTE 2)) */
  #define b4vector_type   (          BTB4|BTB3     |BTB1     ) /* 0x1A  # %00011010  ; non-simple (VECTOR (UNSIGNED-BYTE 4)) */
  #define b8vector_type   (          BTB4|BTB3     |BTB1|BTB0) /* 0x1B  # %00011011  ; non-simple (VECTOR (UNSIGNED-BYTE 8)) */
  #define b16vector_type  (          BTB4|BTB3|BTB2          ) /* 0x1C  # %00011100  ; non-simple (VECTOR (UNSIGNED-BYTE 16)) */
  #define b32vector_type  (          BTB4|BTB3|BTB2     |BTB0) /* 0x1D  # %00011101  ; non-simple (VECTOR (UNSIGNED-BYTE 32)) */
  #define string_type     (          BTB4|BTB3|BTB2|BTB1     ) /* 0x1E  # %00011110  ; non-simple string */
  #define vector_type     (          BTB4|BTB3|BTB2|BTB1|BTB0) /* 0x1F  # %00011111  ; non-simple (VECTOR T) */
  #define fixnum_type     (     BTB5                         ) /* 0x20  # %00100000  ; fixnum */
  #define sfloat_type     (     BTB5               |BTB1     ) /* 0x22  # %00100010  ; short-float */
  #define bignum_type     (     BTB5          |BTB2          ) /* 0x24  # %00100100  ; bignum */
  #define ffloat_type     (     BTB5          |BTB2|BTB1     ) /* 0x26  # %00100110  ; single-float */
  #define ratio_type      (     BTB5     |BTB3               ) /* 0x28  # %00101000  ; ratio */
  #define dfloat_type     (     BTB5     |BTB3     |BTB1     ) /* 0x2A  # %00101010  ; double-float */
  #define complex_type    (     BTB5     |BTB3|BTB2          ) /* 0x2C  # %00101100  ; complex */
  #define lfloat_type     (     BTB5     |BTB3|BTB2|BTB1     ) /* 0x2E  # %00101110  ; long-float */
  #if (TB6 >= 0)
  #define cons_type       (BTB6                              ) /* 0x40  # %01000000  ; cons */
  #endif

/* Bits for symbols in VAR/FUN-Frames (in LISP-Stack): */
  #define active_bit  0  /* set: binding is active */
  #define dynam_bit   1  /* set: binding is dynamic */
  #define svar_bit    2  /* set: next parameter is supplied-p-parameter for this */
/* If symbols are always on addresses divisible by 8, we can store
   these bits in the symbol pointer.
   There are
     1) symbols in the heap - these have the alignment varobject_alignment.
     2) symbols in the symbol_tab_data - their alignment is:
        if sizeof_symbol_is_multiple_of_varobject_alignment is defined:
          varobject_alignment
        otherwise:
          alignof(symbol_) = alignment_long. */
#if (varobject_alignment >= bit(3)) \
    && ((defined(sizeof_symbol_is_multiple_of_varobject_alignment) ? varobject_alignment : alignment_long) >= bit(3)) \
    && !defined(NO_SYMBOLFLAGS)
  /* Store them in the oint_addr part, not in the oint_type part. */
  #define oint_symbolflags_shift  oint_addr_shift
#else
  /* There's no space in the symbol for active_bit, dynam_bit, svar_bit.
     Use an extra word on the LISP-stack. */
  #define NO_symbolflags
#endif

#ifndef IMMEDIATE_FFLOAT
  /* type is GC-invariant, if
   type-info-byte >=0, <= system_type or >= fixnum_type, < bignum_type. */
    #define gcinvariant_type_p(type)  \
      (((type) & ~(BTB5|BTB1|BTB0)) == 0)
#else
  /* type is GC-invariant, if
   type-info-byte is one of 0x00..0x03,0x20..0x23,0x26..0x27 ist. */
  #if (TB1==TB0+1) && (TB2==TB0+2) && (TB3==TB0+3) && (TB4==TB0+4) && (TB5==TB0+5)
    #define gcinvariant_type_p(type)  \
      ((((type)>>(TB0+1))<0x14) && ((bit((type)>>(TB0+1)) & 0xFFF4FFFCUL) == 0))
  #else
    /* Test whether ((type)>>TB1) is one of
       0, 1, bit(TB5-TB1), bit(TB5-TB1) | 1, bit(TB5-TB1) | bit(TB2-TB1) | 1. */
    #define gcinvariant_type_p(type)  gcinvariant_type_aux((type)>>TB1)
    #define gcinvariant_type_sum(type)  \
      (((type) | ((type)>>(TB5-(TB2+1)))) & (((BTB2<<1)+BTB2+BTB1)>>TB1))
    #define gcinvariant_type_aux(type)                         \
      (((type) < ((BTB5+(BTB2<<1))>>TB1))                      \
       && ((type) & ~((BTB5|BTB2|BTB1)>>TB1)) == 0             \
       && (bit(gcinvariant_type_sum(type))                     \
           & (  bit(0)                                         \
              | bit(1)                                         \
              | bit(bit(TB2+1-TB1))                            \
              | bit(bit(TB2+1-TB1) | 1)                        \
              | bit(bit(TB2+1-TB1) | bit(TB2-TB1) | 1))) != 0)
  #endif
#endif

#endif /* STANDARD_8BIT_TYPECODES */

#if !(gcinvariant_type_p(ffloat_type) == defined(IMMEDIATE_FFLOAT))
  #error gcinvariant_type_p() incorrectly implemented!
#endif

/* Test for gc-invariant object. (This includes immediate, machine, subr.)
 gcinvariant_object_p(obj) */
  #define gcinvariant_object_p(obj)  \
    gcinvariant_type_p(typecode(obj))
  #define gcinvariant_oint_p(obj_o)  \
    gcinvariant_type_p((tint)((obj_o) >> oint_type_shift) & (oint_type_mask >> oint_type_shift))

#else /* no TYPECODES */

/* Bits for symbols in VAR/FUN-Frames (on LISP-Stack):
 are not located in the oint_type-part, but in the oint_data-part. */
  #define active_bit  0  /* set: binding is active */
  #define dynam_bit   1  /* set: binding is dynamic */
  #define svar_bit    2  /* set: next parameter is supplied-p-parameter for this one */
#define NO_symbolflags /* there's no space in the symbol for active_bit, dynam_bit, svar_bit */

/* Bits for symbols in the flags: */
  #define var_bit0_f  0  /* set if the symbol is proclaimed SPECIAL or constant */
  #define var_bit1_f  1  /* set if the symbol is a symbol-macro or constant */

#endif /* TYPECODES */
%% #ifdef TYPECODES
%%  export_def(gcinvariant_type_p(type));
%%  export_def(gcinvariant_type_sum(type));
%%  export_def(gcinvariant_type_aux(type));
%%  export_def(gcinvariant_object_p(obj));
%%  export_def(gcinvariant_oint_p(obj_o));
%% #endif


/* What's really being sent from an address to the address-bus */
#define hardware_addressbus_mask  ~0UL  /* Default: nothing is dropped */
/* Clever memory-mapping spares us from masking out of certain
 bits before one accesses the address */
#define addressbus_mask  hardware_addressbus_mask
#ifdef SINGLEMAP_MEMORY
  #if !defined(WIDE_SOFT)
    /* Memory-mapping makes the bits 31..24 of an address redundant now. */
    #undef addressbus_mask
    #define addressbus_mask  oint_addr_mask  /* most of the time it's = 0x00FFFFFFUL */
  #endif
#endif
%% #if notused
%% export_def(addressbus_mask);
%% #endif


#if defined(SINGLEMAP_MEMORY) && 1
  /* The STACK resides in a singlemap-area as well, Typinfo system_type. */
  #define SINGLEMAP_MEMORY_STACK
#endif

#if defined(TRIVIALMAP_MEMORY) && 1
  /* The STACK region is allocated through mmap() or similar.
     This makes it possible to control the bits that are set in a STACK pointer
     and avoid collisions with frame_bit_o and (if TYPECODES) the type bits. */
  #define TRIVIALMAP_MEMORY_STACK
#endif


/* Verify the oint_addr_shift value w.r.t. the autoconfigured CODE_ADDRESS_RANGE
   and MALLOC_ADDRESS_RANGE values. */
#if !defined(WIDE_SOFT)
  /* The CODE_ADDRESS_RANGE needs to be checked because we store code
     pointers in Lisp objects (cf. macro ThePseudofun).
     In case of TYPECODES, the typecode() of such pointers must be machine_type,
     otherwise gc_mark() gets confused and crashes. */
  #if (CODE_ADDRESS_RANGE >> addr_shift) & ~(oint_addr_mask >> oint_addr_shift)
    #error oint_addr_mask does not cover CODE_ADDRESS_RANGE !!
  #endif
  /* The MALLOC_ADDRESS_RANGE needs to be checked because
     1) if !defined(SINGLEMAP_MEMORY) && !defined(TRIVIALMAP_MEMORY),
        Lisp objects reside in memory allocated through mymalloc,
     2) if !defined(SINGLEMAP_MEMORY_STACK) && !defined(TRIVIALMAP_MEMORY_STACK),
        the STACK is allocated through mymalloc, and pointers into the STACK
        occur in frames (cf. macro framebottomword and function make_variable_frame)
        and in environments (cf. type environment_t). */
  #if !((defined(SINGLEMAP_MEMORY) && defined(SINGLEMAP_MEMORY_STACK)) \
        || (defined(TRIVIALMAP_MEMORY) && defined(TRIVIALMAP_MEMORY_STACK)))
    #if (MALLOC_ADDRESS_RANGE >> addr_shift) & ~(oint_addr_mask >> oint_addr_shift)
       #error oint_addr_mask does not cover MALLOC_ADDRESS_RANGE !!
    #endif
  #endif
#endif


#ifdef TYPECODES

/* You have to remove the typebits in order to access the components
 of an object.
 pointable(obj) does this, returning a void*.
 pointable_unchecked(obj) likewise, but without the DEBUG_GCSAFETY check.
 pointable_address_unchecked(obj_o) likewise, but takes an oint as argument
                                    and returns an aint. */
  #if !((oint_addr_shift==0) && (addr_shift==0))
    #define pointable_unchecked(obj)  ((void*)upointer(obj))
    #define pointable_address_unchecked(obj_o)  \
      (((aint)((obj_o) >> oint_addr_shift) & (aint)(oint_addr_mask >> oint_addr_shift)) << addr_shift)
  #else
    /* If oint_addr_shift=0 and addr_shift=0, you don't have to shift. */
    #define pointable_unchecked(obj)  \
      ((void*)pointable_address_unchecked(as_oint(obj)))
    #if defined(SINGLEMAP_MEMORY)
      /* Through memory mapping, type bits must be sent to the address bus,
         together with the address. */
      #define pointable_address_unchecked(obj_o)  (aint)(obj_o)
    #else
      #if !((tint_type_mask & (addressbus_mask>>oint_type_shift)) == 0)
        /* The general case. */
        #define pointable_address_unchecked(obj_o)  \
          ((aint)(obj_o) & ((aint)oint_addr_mask | ~addressbus_mask))
      #else
        /* Moreover if oint_type_mask and addressbus_mask are disjoint
           (((tint_type_mask<<oint_type_shift) & addressbus_mask) == 0),
           no typebits are being sent to the address bus anyway.
           So there's nothing to be done: */
        #define pointable_address_unchecked(obj_o)  (aint)(obj_o)
      #endif
    #endif
  #endif
  #ifdef DEBUG_GCSAFETY
    /* Check that obj has not been held in a GC-unsafe variable while a
     memory allocation was made. */
    static inline void* pointable (gcv_object_t obj) {
      return pointable_unchecked(obj);
    }
    static inline void* pointable (object obj) {
      return pointable_unchecked((gcv_object_t)obj); /* The cast does the check. */
    }
  #else
    #define pointable(obj)  pointable_unchecked(obj)
  #endif

/* If you want to access an object with a known type-info whose
 set typebits are being swallowed by the address bus (the
 typebits, that are =0 don't matter), you can do without 'untype': */
  #if defined(DEBUG_GCSAFETY)
    #define type_pointable(type,obj)  pointable(obj)
  #elif defined(WIDE_STRUCT)
    #define type_pointable(type,obj)  ((void*)((obj).u.both.addr))
  #elif !((oint_addr_shift==0) && (addr_shift==0) && (((tint_type_mask<<oint_type_shift) & addressbus_mask) == 0))
    #if (addr_shift==0)
      #define type_pointable(type,obj)  \
        ((oint_addr_shift==0) && ((type_zero_oint(type) & addressbus_mask) == 0) \
         ? (void*)(aint)as_oint(obj)                                             \
         : (void*)(aint)pointable(obj)                                           \
        )
    #elif !(addr_shift==0)
      /* Analogous, but here the macro 'optimized_upointer'
       assumes the role of the address bus: */
      #define type_pointable(type,obj)  \
        ((optimized_upointer(type_data_object(type,0)) == 0) \
         ? (void*)(aint)optimized_upointer(obj)              \
         : (void*)(aint)pointable(obj)                       \
        )
    #endif
  #else
    /* If pointable(obj) = obj, type_pointable() doesn't do anything as well: */
    #define type_pointable(type,obj)  ((void*)(aint)as_oint(obj))
  #endif

/* If you want to access an object that has one of several known
 type infos, you can probably omit the 'untype'.
 The  OR of the type infos is more authoritative. */
  #define types_pointable(ORed_types,obj)  type_pointable(ORed_types,obj)

#else /* HEAPCODES */

  #define pointable_address_unchecked(obj_o)  \
    (((aint)((obj_o) >> oint_addr_shift) & (aint)(oint_addr_mask >> oint_addr_shift)) << addr_shift)

#endif
%% #ifdef TYPECODES
%%  export_def(pointable_unchecked(obj));
%%  export_def(pointable_address_unchecked(obj_o));
%%  #ifdef DEBUG_GCSAFETY
%%   puts("static inline void* pointable (gcv_object_t obj) { return pointable_unchecked(obj); }");
%%   puts("static inline void* pointable (object obj) { return pointable_unchecked((gcv_object_t)obj); }");
%%  #else
%%   emit_define("pointable(obj)","pointable_unchecked(obj)");
%%  #endif
%% #else /* HEAPCODES */
%%  export_def(pointable_address_unchecked(obj_o));
%% #endif


#ifdef oint_symbolflags_shift
  #if defined(SINGLEMAP_MEMORY) && (oint_symbolflags_shift==oint_type_shift)
    /* Since we can't multimap the symbol_tab, we can't use extrabits in
     a symbol's typecode. */
    #undef oint_symbolflags_shift
    #define NO_symbolflags
  #endif
#endif
#ifdef NO_symbolflags
  #define oint_symbolflags_shift  -1 /* invalid value */
#endif


/* Whether we try to initialize subr_tab statically.
 (g++ 3.3 doesn't accept compound expressions as initializers:
 http://gcc.gnu.org/bugzilla/show_bug.cgi?id=12615
 g++ 3.4 similarly: http://gcc.gnu.org/bugzilla/show_bug.cgi?id=15180)
 With DEBUG_GCSAFETY, the initialization of subr_tab_data crashes in
 nonimmprobe. */
#if !(defined(WIDE_SOFT) && !defined(WIDE_STRUCT)) && !(defined(__GNUG__) && (__GNUC__ == 3) && (__GNUC_MINOR__ == 3 || __GNUC_MINOR__ == 4) && defined(OBJECT_STRUCT)) && !(defined(DEBUG_GCSAFETY) && defined(SINGLEMAP_MEMORY))
  #define INIT_SUBR_TAB
#endif
/* NB: This has to be defined so external modules can work.
 When changed: do nothing */

/* Whether we try to initialize symbol_tab statically.
 (Make initialization easier, but there is not enough space for the
 compilation of SPVWTABS on some systems.
 EMX 0.9c (gcc-2.7.2.1) says "Virtual memory exhausted".
 g++ 3.3 doesn't accept compound expressions as initializers:
 http://gcc.gnu.org/bugzilla/show_bug.cgi?id=12615
 g++ 3.4 similarly: http://gcc.gnu.org/bugzilla/show_bug.cgi?id=15180)
 g++ 4.8 on 32-bit AIX/PowerPC produces code with invalid displacements.
 With DEBUG_GCSAFETY, the initialization of symbol_tab_data crashes in
 nonimmprobe. */
#if !(defined(WIDE_SOFT) && !defined(WIDE_STRUCT)) && !(defined(__GNUG__) && (__GNUC__ == 3) && (__GNUC_MINOR__ == 3 || __GNUC_MINOR__ == 4) && defined(OBJECT_STRUCT)) && !(defined(__GNUG__) && defined(UNIX_AIX) && defined(POWERPC)) && !(defined(DEBUG_GCSAFETY) && defined(SINGLEMAP_MEMORY))
  #define INIT_SYMBOL_TAB
#endif
/* When changed: nothing to do */

/* Whether we try to initialize object_tab statically.
 (g++ 3.3 doesn't accept compound expressions as initializers:
 http://gcc.gnu.org/bugzilla/show_bug.cgi?id=12615
 g++ 3.4 similarly: http://gcc.gnu.org/bugzilla/show_bug.cgi?id=15180)
 With DEBUG_GCSAFETY, the initialization of object_tab crashes in
 nonimmprobe. */
#if !(defined(WIDE_SOFT) && !defined(WIDE_STRUCT)) && !(defined(__GNUG__) && (__GNUC__ == 3) && (__GNUC_MINOR__ == 3 || __GNUC_MINOR__ == 4) && defined(OBJECT_STRUCT)) && !(defined(DEBUG_GCSAFETY) && defined(SINGLEMAP_MEMORY))
  #define INIT_OBJECT_TAB
#endif
/* When changed: do nothing */


/* Set during the core of GC.
 When this is set, unexpected handle_fault() calls that can
 - if defined(MORRIS_GC) && defined(GENERATIONAL_GC) - copy
 Morris-chain backpointers from a cons cell to an old_new_pointer_t with set
 garcol_bit(!) into the heap, where they are guaranteed to lead to a crash
 later. So, uncontrolled memory accesses are forbidden while inside_gc. */
extern bool inside_gc;
%% exportV(bool,inside_gc);


#ifdef DEBUG_GCSAFETY

  /* Forward declarations. */
  static inline bool gcinvariant_symbol_p (object obj);
  #if defined(KERNELVOID32_HEAPCODES) || defined(GENERIC64_HEAPCODES)
  static inline bool nonimmsubrp (object obj);
  #else
  #define nonimmsubrp(obj)  false
  #endif

  /* Force a crash if a memory pointer points to nonexistent memory. */
  #define nonimmprobe(obj_o)  \
    do {                                                                       \
      /* Don't do probes inside GC. It leads to unexpected handle_fault()      \
         calls that can - if defined(MORRIS_GC) && defined(GENERATIONAL_GC) -  \
         copy Morris-chain backpointers from a cons cell to an old_new_pointer_t \
         with set garcol_bit(!) into the heap, where they are guaranteed to    \
         lead to a crash later. */                                             \
      if (!inside_gc)                                                          \
        if (((obj_o) & wbit(garcol_bit_o)) == 0) /* exclude frame words from the STACK */ \
          if (!gcinvariant_oint_p(obj_o)) /* exclude immediate objects */      \
            /* Access a single char, without needing to subtract the bias. */  \
            *(volatile char *)pointable_address_unchecked(obj_o);              \
    } while (0)

  /* When a gcv_object_t is fetched from a GC visible location (in the heap or
   on the STACK) we can assume that GC has updated it. */
  inline gcv_object_t::operator object () const {
    nonimmprobe(one_o);
    return (object){ designated_init(one_o,one_o) INIT_ALLOCSTAMP };
  }

  /* When an object is put into a GC visible location (in the heap or
   on the STACK) we check that it has not been held in a GC-unsafe variable
   while a memory allocation was made. */
  inline gcv_object_t::gcv_object_t (object obj) {
    if (!(gcinvariant_object_p(obj) || gcinvariant_symbol_p(obj)
          || obj.allocstamp == alloccount || nonimmsubrp(obj)))
      abort();
    one_o = as_oint(obj);
    nonimmprobe(one_o);
  }
  /* The only exception are fake gcv_objects. */
  inline gcv_object_t::gcv_object_t (fake_gcv_object obj) {
    one_o = obj.fake_value;
  }

  /* Uninitialized. */
  inline gcv_object_t::gcv_object_t () {
  }

  /* Start of an area where no GC can be triggered. */
  inline gcunsafe_object_t::gcunsafe_object_t (object obj)
    : gcv_object_t (obj), allocstamp (alloccount) {}
  inline gcunsafe_object_t::gcunsafe_object_t (gcv_object_t obj)
    : gcv_object_t (obj), allocstamp (alloccount) {}
  /* End of an area where no GC can be triggered. */
  inline gcunsafe_object_t::~gcunsafe_object_t () {
    if (!(allocstamp == alloccount))
      abort();
  }

#endif
%% #ifdef DEBUG_GCSAFETY
%%   puts("static inline bool gcinvariant_symbol_p (object obj);");
%%   #if defined(KERNELVOID32_HEAPCODES) || defined(GENERIC64_HEAPCODES)
%%     puts("static inline bool nonimmsubrp (object obj);");
%%   #else
%%     emit_define("nonimmsubrp(obj)","false");
%%   #endif
%%   export_def(nonimmprobe(obj_o));
%%   puts("inline gcv_object_t::operator object () const { nonimmprobe(one_o); return (object){ designated_init(one_o,one_o), designated_init(allocstamp,alloccount) }; }");
%%   puts("inline gcv_object_t::gcv_object_t (object obj) { if (!(gcinvariant_object_p(obj) || gcinvariant_symbol_p(obj) || obj.allocstamp == alloccount || nonimmsubrp(obj))) abort(); one_o = as_oint(obj); nonimmprobe(one_o); }");
%%   puts("inline gcv_object_t::gcv_object_t () {}");
%% #endif


/* Force a memory allocation for all functions which can trigger GC but
   sometimes do and sometimes don't. This makes DEBUG_GCSAFETY more efficient.
   GCTRIGGER()             does a no-op memory allocation
   GCTRIGGER1(obj1)        likewise, but saves obj1 temporarily
   GCTRIGGER2(obj1,obj2)   likewise, but saves obj1, obj2 temporarily
   ...
   GCTRIGGER_IF(condition, statement)
                           likewise, but only if the condition is fulfilled */
#ifdef DEBUG_GCSAFETY
  /* When these macros are used in C macros, obj1, obj2 etc. can sometimes be
   expressions of type 'object' and sometimes of type 'gcv_object_t'. Need
   two implementations of inc_allocstamp. */
  inline void inc_allocstamp (object& obj) { obj.allocstamp++; }
  inline void inc_allocstamp (gcv_object_t& obj) { }
  #define GCTRIGGER()  \
    (void)(alloccount++)
  #define GCTRIGGER1(obj1)  \
    (void)(inc_allocstamp(obj1), alloccount++)
  #define GCTRIGGER2(obj1,obj2)  \
    (void)(inc_allocstamp(obj1), inc_allocstamp(obj2), alloccount++)
  #define GCTRIGGER3(obj1,obj2,obj3)  \
    (void)(inc_allocstamp(obj1), inc_allocstamp(obj2), inc_allocstamp(obj3), alloccount++)
  #define GCTRIGGER4(obj1,obj2,obj3,obj4)  \
    (void)(inc_allocstamp(obj1), inc_allocstamp(obj2), inc_allocstamp(obj3), inc_allocstamp(obj4), alloccount++)
  #define GCTRIGGER5(obj1,obj2,obj3,obj4,obj5)  \
    (void)(inc_allocstamp(obj1), inc_allocstamp(obj2), inc_allocstamp(obj3), inc_allocstamp(obj4), inc_allocstamp(obj5), alloccount++)
  #define GCTRIGGER6(obj1,obj2,obj3,obj4,obj5,obj6)  \
    (void)(inc_allocstamp(obj1), inc_allocstamp(obj2), inc_allocstamp(obj3), inc_allocstamp(obj4), inc_allocstamp(obj5), inc_allocstamp(obj6), alloccount++)
  #define GCTRIGGER_IF(condition,statement)  \
    if (condition) statement
#else
  #define GCTRIGGER()  (void)0
  #define GCTRIGGER1(obj1)  (void)0
  #define GCTRIGGER2(obj1,obj2)  (void)0
  #define GCTRIGGER3(obj1,obj2,obj3)  (void)0
  #define GCTRIGGER4(obj1,obj2,obj3,obj4)  (void)0
  #define GCTRIGGER5(obj1,obj2,obj3,obj4,obj5)  (void)0
  #define GCTRIGGER6(obj1,obj2,obj3,obj4,obj5,obj6)  (void)0
  #define GCTRIGGER_IF(condition,statement)  (void)0
#endif


/* ################### Methods for memory management #####################

 SPVW_BLOCKS : Memory management with few memory blocks
 SPVW_PAGES  : Memory management with many memory blocks
 SPVW_MIXED  : Objects of mixed types are possible on the same page or block
 SPVW_PURE   : Every memory block/every memory page contains only objects
               of exactly one type */
#if defined(SINGLEMAP_MEMORY) || defined(TRIVIALMAP_MEMORY)
  /* Multimapping of single pages isn't implemented yet.??
   Singlemapping of single pages isn't implemented yet.??
   If you use mmap() as malloc()-replacement, single pages aren't needed. */
  #define SPVW_BLOCKS
#elif defined(VIRTUAL_MEMORY)
  /* On Unix-systems you can still fetch more memory afterwards,
   but you should concentrate the data - if possible - on few pages. */
  #define SPVW_PAGES
#else
  #define SPVW_BLOCKS
#endif
#if defined(SINGLEMAP_MEMORY)
  /* SINGLEMAP_MEMORY -> Ony pure pages/blocks make sense, since
   the address of a page determines the type of the objects it contains. */
  #define SPVW_PURE
#elif !defined(TYPECODES) || defined(SPVW_BLOCKS) || defined(TRIVIALMAP_MEMORY)
  /* !TYPECODES -> there aren't real typecodes, only Cons and Varobject.
   SPVW_BLOCKS -> SPVW_PURE_BLOCKS is only implemented for SINGLEMAP_MEMORY.
   TRIVIALMAP_MEMORY -> not many blocks available, small adress space. */
  #define SPVW_MIXED
#else
  /* The decision among SPVW_PURE_PAGES and SPVW_MIXED_PAGES is quite
     arbitrary. */
  #if defined(PREFER_PURE_PAGES)
    #define SPVW_PURE
  #else
    #define SPVW_MIXED
  #endif
#endif
#if !(defined(SPVW_BLOCKS) || defined(SPVW_PAGES))
  #error readjust SPVW_BLOCKS/SPVW_PAGES!
#endif
#if !(defined(SPVW_MIXED) || defined(SPVW_PURE))
  #error readjust SPVW_MIXED/SPVW_PURE!
#endif
#if (defined(SPVW_BLOCKS) && defined(SPVW_PURE)) != defined(SINGLEMAP_MEMORY)
  #error SINGLEMAP_MEMORY <==> SPVW_PURE_BLOCKS!
#endif
#if (defined(SPVW_BLOCKS) && defined(SPVW_MIXED)) < defined(TRIVIALMAP_MEMORY)
  #error TRIVIALMAP_MEMORY ==> SPVW_MIXED_BLOCKS!
#endif
#if defined(SPVW_PURE) && !defined(TYPECODES)
  #error SPVW_PURE ==> TYPECODES!
#endif
#if (defined(SPVW_BLOCKS) && (defined(SPVW_PURE) || defined(SPVW_MIXED))) < defined(GENERATIONAL_GC)
  #error GENERATIONAL_GC ==> SPVW_PURE_BLOCKS or SPVW_MIXED_BLOCKS_STAGGERED or SPVW_MIXED_BLOCKS_OPPOSITE!
#endif

/* The old GC algorithm does not support MULTITHREAD. */
#if defined(OLD_GC) && defined(MULTITHREAD)
  #error OLD_GC does not support MULTITHREAD!
#endif

/* Algorithm by Morris, that compacts Conses without mixing them up: */
#if defined(SPVW_BLOCKS) && defined(VIRTUAL_MEMORY) && !defined(NO_MORRIS_GC) /*  && !defined(MULTITHREAD) */
  /* Morris-GC is recommended, as it preserves the locality. */
  #define MORRIS_GC
#endif


/* ################# definitions by cases with respect to type codes ################# */

#ifdef TYPECODES

/* Has to start with switch (typecode(obj)), after that it's like a
 switch-statement with arbitrarily many case-labels.
 Example:  switch (typecode(arg)) { case_string: ...; break; ... } */
  #define case_machine    case machine_type   /* machine-pointer */
  #define case_sstring    case sstring_type   /* Simple-String */
  #define case_ostring    case string_type    /* Other String */
  #define case_sbvector   case sbvector_type   /* Simple-Bit-Vector */
  #define case_obvector   case bvector_type    /* Other Bit-Vector */
  #define case_sb2vector  case sb2vector_type  /* Simple-2Bit-Vector */
  #define case_ob2vector  case b2vector_type   /* Other 2Bit-Vector */
  #define case_sb4vector  case sb4vector_type  /* Simple-4Bit-Vector */
  #define case_ob4vector  case b4vector_type   /* Other 4Bit-Vector */
  #define case_sb8vector  case sb8vector_type  /* Simple-8Bit-Vector */
  #define case_ob8vector  case b8vector_type   /* Other 8Bit-Vector */
  #define case_sb16vector case sb16vector_type /* Simple-16Bit-Vector */
  #define case_ob16vector case b16vector_type  /* Other 16Bit-Vector */
  #define case_sb32vector case sb32vector_type /* Simple-32Bit-Vector */
  #define case_ob32vector case b32vector_type  /* Other 32Bit-Vector */
  #define case_svector    case svector_type    /* Simple-(General-)Vector */
  #define case_ovector    case vector_type    /* Other (General-)Vector */
  #define case_mdarray    case mdarray_type   /* other Array */
  #define case_string     case_sstring: case_ostring /* general string */
  #define case_bvector    case_sbvector: case_obvector /* general bit vector */
  #define case_b2vector   case_sb2vector: case_ob2vector /* general 2bit vector */
  #define case_b4vector   case_sb4vector: case_ob4vector /* general 4bit vector */
  #define case_b8vector   case_sb8vector: case_ob8vector /* general 8bit vector */
  #define case_b16vector  case_sb16vector: case_ob16vector /* general 16bit vector */
  #define case_b32vector  case_sb32vector: case_ob32vector /* general 32bit vector */
  #define case_vector     case_svector: case_ovector /* general vector */
  #define case_array      case_string: case_bvector: case_b2vector: case_b4vector: case_b8vector: case_b16vector: case_b32vector: case_vector: case_mdarray /* general Array */
  #define case_closure    case closure_type   /* Closure */
  #ifdef structure_type
  #define case_structure  case structure_type /* Structure */
  #define _case_structure case_structure:
  #else
  #define structure_type  orecord_type        /* Structures are OtherRecords */
  #define _case_structure
  #endif
  #ifdef stream_type
  #define case_stream     case stream_type    /* Stream */
  #define _case_stream    case_stream:
  #else
  #define stream_type     orecord_type        /* Streams are OtherRecords */
  #define _case_stream
  #endif
  #define case_orecord    case orecord_type   /* Other Record */
  #define case_instance   case instance_type  /* CLOS-Instance */
  #define case_lrecord    case lrecord_type   /* Long Record */
  #define case_char       case char_type      /* Character */
  #define case_subr       case subr_type      /* SUBR */
  #define case_system     case system_type    /* Frame-Pointer, Small-Read-Label, System */
  #define case_posfixnum  case fixnum_type    /* Fixnum >=0 */
  #define case_negfixnum  case fixnum_type|bit(sign_bit_t) /* Fixnum <0 */
  #define case_fixnum     case_posfixnum: case_negfixnum /* Fixnum */
  #define case_posbignum  case bignum_type    /* Bignum >0 */
  #define case_negbignum  case bignum_type|bit(sign_bit_t) /* Bignum <0 */
  #define case_bignum     case_posbignum: case_negbignum /* Bignum */
  #define case_integer    case_fixnum: case_bignum /* Integer */
  #define case_ratio      case ratio_type: case ratio_type|bit(sign_bit_t) /* Ratio */
  #ifdef SPVW_MIXED
  #define _case_ratio     case_ratio:
  #else
  #define _case_ratio
  #endif
  #define case_rational   case_integer: case_ratio /* Rational */
  #define case_sfloat     case sfloat_type: case sfloat_type|bit(sign_bit_t) /* Short-Float */
  #define case_ffloat     case ffloat_type: case ffloat_type|bit(sign_bit_t) /* Single-Float */
  #define case_dfloat     case dfloat_type: case dfloat_type|bit(sign_bit_t) /* Double-Float */
  #define case_lfloat     case lfloat_type: case lfloat_type|bit(sign_bit_t) /* Long-Float */
  #define case_float      case_sfloat: case_ffloat: case_dfloat: case_lfloat /* Float */
  #define case_real       case_rational: case_float /* Real */
  #define case_complex    case complex_type /* Complex */
  #ifdef SPVW_MIXED
  #define _case_complex   case_complex:
  #else
  #define _case_complex
  #endif
  #define case_number     case_real: case_complex /* Number */
  #define case_symbol     case symbol_type /* Symbol */
  #define case_sxrecord   case_closure: _case_structure _case_stream _case_ratio _case_complex case_orecord: case_instance /* Srecord/Xrecord general */
  #define case_record     case_sxrecord: case_lrecord /* Lrecord/Srecord/Xrecord general */
  #if /* !defined(NO_symbolflags) && */ (oint_symbolflags_shift==oint_type_shift)
  #define case_symbolflagged  /* Symbol with Flags                        */\
          case symbol_type:                                             \
          case symbol_type|bit(active_bit):                             \
          case symbol_type|bit(dynam_bit):                              \
          case symbol_type|bit(dynam_bit)|bit(active_bit):              \
          case symbol_type|bit(svar_bit):                               \
          case symbol_type|bit(svar_bit)|bit(active_bit):               \
          case symbol_type|bit(svar_bit)|bit(dynam_bit):                \
          case symbol_type|bit(svar_bit)|bit(dynam_bit)|bit(active_bit)
  #else
  #define case_symbolflagged  case_symbol /* Symbol with flags */
  #endif
  #define case_cons       case cons_type /* Cons */

#else

  #define _case_structure
  #define _case_stream

#endif


/* ################## Structure of memory of LISP objects ###################

 uintWC is the Integer type for the lengths of Bignum, Lfloat, Iarray.
 Subset relation: uintW <= uintWC <= uintC. */
#ifdef TYPECODES
  #define intWCsize intCsize
  typedef uintC uintWC;
  typedef sintC sintWC;
#else
  /* Type and sign are stored in the heap - only 16 bits for the length. */
  #define intWCsize intWsize
  typedef uintW uintWC;
  typedef sintW sintWC;
#endif
/* uintWCoverflow(x) checks, if there has been an overflow after the execution
 of an x++. */
#define uintWCoverflow(x)  ((intWCsize<intLsize) && ((uintWC)(x)==0))

/* ---------------------- Objects with two pointers ---------------------- #
 They contain just the two pointers, no header. The type must already be
 known when the object is accessed.

 Normally, Cons, Ratio, Complex can all be considered as pairs. But if
 SPVW_MIXED, the heap statistics are a little unspecific if we mix the
 three types; therefore in that case we let Ratio and Complex be Varobjects. */
#ifdef SPVW_MIXED
  #define case_pair  case_cons
#else
  #define case_pair  case_cons: case_ratio: case_complex
#endif

/* ---------------------- Objects of varying length ---------------------- #
 The first word is reserved for garbage collection. Outside of garbage
 collection, it contains a pointer to the object itself. Note that the
 GC, when it moves an object, takes care not to modify the typecode of
 this first word (except the GC bit, which it temporarily uses).

 Type of the header flags: */
#if (oint_type_len<=8) && (oint_type_shift/8 == (oint_type_shift+oint_type_len-1)/8) && !defined(DEBUG_GCSAFETY)
  /* Access to an individual byte is possible */
  #define hfintsize  intBsize
  typedef uintB  hfint;
#else
  /* access to a full word */
  #define hfintsize  pointer_bitsize
  typedef uintP  hfint;
#endif
%% #if (oint_type_len<=8) && (oint_type_shift/8 == (oint_type_shift+oint_type_len-1)/8) && !defined(DEBUG_GCSAFETY)
%%   emit_typedef("uintB","hfint");
%% #else
%%   emit_typedef("uintP","hfint");
%% #endif

/* Objects with variable length */
#ifdef TYPECODES
  #ifdef DEBUG_GCSAFETY
    #define VAROBJECT_HEADER  \
               gcv_object_t _GCself;  /* Self pointer for GC, contains flags */
  #else
    #define VAROBJECT_HEADER  \
               union {                                                    \
                 gcv_object_t _GCself;  /* Self pointer for GC              */\
                 hfint flags[sizeof(gcv_object_t)/sizeof(hfint)]; /* Flags  */\
               } header;
  #endif
#else
  #define VAROBJECT_HEADER  \
               gcv_object_t GCself;  /* Self pointer for GC  */\
               uintL tfl;            /* type, flags, length */
#endif
typedef struct {
  VAROBJECT_HEADER
} varobject_;
typedef varobject_ *  Varobject;
#ifdef TYPECODES
  #ifdef DEBUG_GCSAFETY
    #define GCself  _GCself
    #define header_flags  _GCself.one_o
  #else
    #define GCself  header._GCself
    /* The typecode can be found in the byte ((Varobject)p)->header_flags. */
    #if !(oint_type_len>=hfintsize ? oint_type_shift%hfintsize==0 : floor(oint_type_shift,hfintsize)==floor(oint_type_shift+oint_type_len-1,hfintsize))
      #error Bogus header_flags -- redefine header_flags!
    #endif
    #if BIG_ENDIAN_P
      #define header_flags  header.flags[sizeof(gcv_object_t)/sizeof(hfint)-1-floor(oint_type_shift,hfintsize)]
    #else
      #define header_flags  header.flags[floor(oint_type_shift,hfintsize)]
    #endif
  #endif
  /* it applies  mtypecode(((Varobject)p)->GCself) =
   (((Varobject)p)->header_flags >> (oint_type_shift%hfintsize)) & tint_type_mask
   Bits for Symbols in the self pointer (see above):
   define var_bit0_t  ...  # set if the symbol is proclaimed SPECIAL or constant
   define var_bit1_t  ...  # set if the symbol is a symbol-macro or constant */
  #define var_bit0_hf  (var_bit0_t+(oint_type_shift%hfintsize))
  #define var_bit1_hf  (var_bit1_t+(oint_type_shift%hfintsize))
#else
  /* Three possible layouts of type, flags, length:
     8 bits type, 24 bits length [Vrecord, Lrecord]
     8 bits type, 8 bits flags, 16 bits length [Srecord]
     8 bits type, 8 bits flags, 8 bits length, 8 bits xlength [Xrecord] */
  #define vrecord_tfl(type,length)  \
    ((uintL)(uintB)(type)+((uintL)(length)<<8))
  #define lrecord_tfl(type,length)  \
    ((uintL)(uintB)(type)+((uintL)(length)<<8))
  #define srecord_tfl(type,flags,length)  \
    ((uintL)(uintB)(type)+((uintL)(uintB)(flags)<<8)+((uintL)(length)<<16))
  #define xrecord_tfl(type,flags,length,xlength)  \
    ((uintL)(uintB)(type)+((uintL)(uintB)(flags)<<8)+((uintL)(uintB)(length)<<16)+((uintL)(uintB)(xlength)<<24))
  #define varobject_type(ptr) ((sintB)((ptr)->tfl & 0xFF))
  /* Bits for symbols in the flags: */
  #define header_flags  tfl
  #define var_bit0_hf  (var_bit0_f+8)
  #define var_bit1_hf  (var_bit1_f+8)
#endif
%% export_def(VAROBJECT_HEADER);
%% #ifndef TYPECODES
%%  export_def(GCself);
%%  export_def(varobject_type(ptr));
%% #endif

/* Records
 These are varobjects with a one-byte type field in memory.
 There are three types of records:
   Vector-Records can have up to 16777215 elements, but have no flags and
   if TYPECODES also no type (because the type info is in the pointer).
   Long-Records can have up to 16777215 elements, but have no flags.
   Simple-Records can have up to 65535 elements,
   Extended-Records have room for up to 255 elements and 255 extra (non-Lisp)
   elements.
 Vector-Records are recognized by their type field:
   rectype == Rectype_Sbvector, Rectype_Sb[2|4|8|16|32]vector,
              Rectype_S[8|16|32]string, Rectype_Imm_S[8|16|32]string,
              Rectype_Svector.
 Long-Records are recognized by rectype >= rectype_longlimit, or if TYPECODES
 equivalently by their typecode lrecord_type.
 The others are partitioned into:
   - Simple-Records, if rectype < rectype_limit.
   - Extended-Records, if rectype >= rectype_limit. */

#ifdef TYPECODES
  #define RECORD_HEADER                                               \
    VAROBJECT_HEADER   /* self-pointer GC */                          \
    sintB rectype;     /* for OtherRecord and LongRecord: sub-type */ \
    uintB recflags;    /* for OtherRecord: flags */                   \
    uintW reclength;   /* lengths and others */
#else
  #define RECORD_HEADER  \
    VAROBJECT_HEADER   /* self-pointer for GC, tfl */
#endif
typedef struct {
  RECORD_HEADER
  gcv_object_t recdata[unspecified] _attribute_aligned_object_; /* elements */
} record_;
typedef record_ *  Record;
/* access to type, flags: */
#ifdef TYPECODES
  #define record_type(ptr)  ((ptr)->rectype)
#else
  #define record_type(ptr)  varobject_type(ptr)
#endif
#define Record_type(obj)  record_type(TheRecord(obj))
#ifdef TYPECODES
  #define record_flags(ptr)  ((ptr)->recflags)
#else
  #define record_flags(ptr)  (((ptr)->tfl >> 8) & 0xFF)
#endif
#define Record_flags(obj)  record_flags(TheRecord(obj))
#ifdef TYPECODES
  #define record_flags_clr(ptr,bits)  ((ptr)->recflags &= ~(bits))
  #define record_flags_set(ptr,bits)  ((ptr)->recflags |= (bits))
  #define record_flags_replace(ptr,newflags)  ((ptr)->recflags = (newflags))
#else
  #define record_flags_clr(ptr,bits)  ((ptr)->tfl &= ~((uintL)(bits) << 8))
  #define record_flags_set(ptr,bits)  ((ptr)->tfl |= ((uintL)(bits) << 8))
  #define record_flags_replace(ptr,newflags)  \
    ((ptr)->tfl ^= (((ptr)->tfl ^ (uintL)(newflags)<<8) & 0xFF00))
#endif
%% export_def(RECORD_HEADER);
%% sprintf(buf,"struct { RECORD_HEADER gcv_object_t recdata[unspecified]%s; }",attribute_aligned_object);
%% emit_typedef(buf,"record_");
%% emit_typedef("record_ *","Record");
%% export_def(record_type(ptr));
%% export_def(Record_type(obj));
%% export_def(record_flags(ptr));
%% export_def(record_flags_set(ptr,bits));
%% export_def(Record_flags(obj));

#ifdef TYPECODES
  #define VRECORD_HEADER  \
                 VAROBJECT_HEADER /* self-pointer for GC  */\
                 uintL length;    /* length */
#else
  #define VRECORD_HEADER  \
                 VAROBJECT_HEADER /* self-pointer for GC, tfl */
#endif
typedef struct {
  VRECORD_HEADER
} vrecord_;
typedef vrecord_ *  Vrecord;
#ifdef TYPECODES
  #define vrecord_length(ptr)  ((ptr)->length)
#else
  #define vrecord_length(ptr)  ((ptr)->tfl >> 8)
#endif
%% export_def(VRECORD_HEADER);
%% emit_typedef("struct { VRECORD_HEADER }","vrecord_");
%% emit_typedef("vrecord_ *","Vrecord");
%% export_def(vrecord_length(ptr));

#ifdef TYPECODES
  #define LRECORD_HEADER  \
                 VAROBJECT_HEADER /* self-pointer for GC  */\
                 uintL tfl;       /* subtype (1 byte), then length (3 bytes) */
#else
  #define LRECORD_HEADER  \
                 VAROBJECT_HEADER /* self-pointer for GC, tfl */
#endif
typedef struct {
  LRECORD_HEADER
  gcv_object_t recdata[unspecified] _attribute_aligned_object_; /* reclength elements */
} lrecord_;
typedef lrecord_ *  Lrecord;
#ifdef TYPECODES
  #if BIG_ENDIAN_P
    #define lrecord_tfl(type,length)  \
      (((uintL)(uintB)(type)<<24)+(uintL)(length))
    #define lrecord_length(ptr)  ((ptr)->tfl & 0xFFFFFF)
  #else
    #define lrecord_tfl(type,length)  \
      ((uintL)(uintB)(type)+((uintL)(length)<<8))
    #define lrecord_length(ptr)  ((ptr)->tfl >> 8)
  #endif
#else
  #define lrecord_length(ptr)  ((ptr)->tfl >> 8)
#endif
#define Lrecord_length(obj)  lrecord_length(TheLrecord(obj))

#ifdef TYPECODES
  #define SRECORD_HEADER                                        \
                 VAROBJECT_HEADER /* self-pointer GC              */\
                 sintB rectype;   /* subtype, < rectype_limit     */\
                 uintB recflags;  /* flags                        */\
                 uintW reclength; /* lengths in objects */
#else
  #define SRECORD_HEADER  \
                 VAROBJECT_HEADER /* self-pointer for GC, tfl */
#endif
typedef struct {
  SRECORD_HEADER
  gcv_object_t recdata[unspecified] _attribute_aligned_object_; /* reclength elements */
} srecord_;
typedef srecord_ *  Srecord;
#ifdef TYPECODES
  #define srecord_length(ptr)  ((ptr)->reclength)
#else
  #define srecord_length(ptr)  ((ptr)->tfl >> 16)
#endif
#define Srecord_length(obj)  srecord_length(TheSrecord(obj))
%% export_def(SRECORD_HEADER);
%% sprintf(buf,"struct { SRECORD_HEADER gcv_object_t recdata[unspecified]%s; }",attribute_aligned_object);
%% emit_typedef(buf,"srecord_");
%% emit_typedef("srecord_ *","Srecord");
%% export_def(srecord_length(ptr));

#ifdef TYPECODES
  #define XRECORD_HEADER                                                \
                 VAROBJECT_HEADER  /* self-pointer for GC                 */\
                 sintB rectype;    /* subtype, >= rectype_limit           */\
                 uintB recflags;   /* flags                               */\
                 uintB reclength;  /* lengths in objects                  */\
                 uintB recxlength; /* lengths of the extra objects */
#else
  #define XRECORD_HEADER  \
                 VAROBJECT_HEADER  /* self-pointer for GC, tfl */
#endif
typedef struct {
  XRECORD_HEADER
  gcv_object_t recdata[unspecified] _attribute_aligned_object_; /* reclength elements */
  /* uintB      recxdata[unspecified]; # recxlength extra elements */
} xrecord_;
typedef xrecord_ *  Xrecord;
#ifdef TYPECODES
  #define xrecord_length(ptr)  ((ptr)->reclength)
  #define xrecord_xlength(ptr)  ((ptr)->recxlength)
#else
  #define xrecord_length(ptr)  (((ptr)->tfl >> 16) & 0xFF)
  #define xrecord_xlength(ptr)  ((ptr)->tfl >> 24)
#endif
#define Xrecord_length(obj)  xrecord_length(TheXrecord(obj))
#define Xrecord_xlength(obj)  xrecord_xlength(TheXrecord(obj))
%% export_def(XRECORD_HEADER);
%% #if notused
%% sprintf(buf,"struct { XRECORD_HEADER gcv_object_t recdata[unspecified]%s; }",attribute_aligned_object);
%% emit_typedef(buf,"xrecord_");
%% emit_typedef("xrecord_ *","Xrecord");
%% #endif

/* *** Possible rectype values for records. *** */
enum {
  enum_rectype_first = -4, /* Try to keep rectype_limit = 0. */
  Rectype_Closure,
%% printf("#define Rectype_Closure %d\n",Rectype_Closure);
  Rectype_Structure,     /* only used #ifndef case_structure */
%% printf("#define Rectype_Structure %d\n",Rectype_Structure);
  Rectype_Instance,
%% printf("#define Rectype_Instance %d\n",Rectype_Instance);
     rectype_limit, /* Here is the limit between Srecord and Xrecord. */
  Rectype_Hashtable = rectype_limit,
%% printf("#define Rectype_Hashtable %d\n",Rectype_Hashtable);
#ifndef TYPECODES
%% #ifndef TYPECODES
  /* Rectype_vector is the bottom ARRAY & VECTOR */
  Rectype_vector,       /* 1 -- Iarray, not Srecord/Xrecord */
%% printf("#define Rectype_vector %d\n",Rectype_vector);
  Rectype_bvector,      /* 2 -- Iarray, not Srecord/Xrecord */
%% printf("#define Rectype_bvector %d\n",Rectype_bvector);
  Rectype_b2vector,     /* 3 -- Iarray, not Srecord/Xrecord */
%% printf("#define Rectype_b2vector %d\n",Rectype_b2vector);
  Rectype_b4vector,     /* 4 -- Iarray, not Srecord/Xrecord */
%% printf("#define Rectype_b4vector %d\n",Rectype_b4vector);
  Rectype_b8vector,     /* 5 -- Iarray, not Srecord/Xrecord */
%% printf("#define Rectype_b8vector %d\n",Rectype_b8vector);
  Rectype_b16vector,    /* 6 -- Iarray, not Srecord/Xrecord */
%% printf("#define Rectype_b16vector %d\n",Rectype_b16vector);
  Rectype_b32vector,    /* 7 -- Iarray, not Srecord/Xrecord */
%% printf("#define Rectype_b32vector %d\n",Rectype_b32vector);
  Rectype_unused1,           /* 8 */
  /* Rectype_Svector is the bottom SIMPLE VECTOR */
  Rectype_Svector,     /* 9 -- Svector, not Srecord/Xrecord */
%% printf("#define Rectype_Svector %d\n",Rectype_Svector);
  Rectype_Sbvector,   /* 10 -- Sbvector, not Srecord/Xrecord */
%% printf("#define Rectype_Sbvector %d\n",Rectype_Sbvector);
  Rectype_Sb2vector,  /* 11 -- Sbvector, not Srecord/Xrecord */
%% printf("#define Rectype_Sb2vector %d\n",Rectype_Sb2vector);
  Rectype_Sb4vector,  /* 12 -- Sbvector, not Srecord/Xrecord */
%% printf("#define Rectype_Sb4vector %d\n",Rectype_Sb4vector);
  Rectype_Sb8vector,  /* 13 -- Sbvector, not Srecord/Xrecord */
%% printf("#define Rectype_Sb8vector %d\n",Rectype_Sb8vector);
  Rectype_Sb16vector, /* 14 -- Sbvector, not Srecord/Xrecord */
%% printf("#define Rectype_Sb16vector %d\n",Rectype_Sb16vector);
  Rectype_Sb32vector, /* 15 -- Sbvector, not Srecord/Xrecord */
%% printf("#define Rectype_Sb32vector %d\n",Rectype_Sb32vector);
  Rectype_unused2,          /* 16 */
  /* Rectype_S8string is the bottom STRING */
  Rectype_S8string,   /* 17 -- S8string, not Srecord/Xrecord */
%% printf("#define Rectype_S8string %d\n",Rectype_S8string);
  Rectype_Imm_S8string, /* 18 -- immutable S8string, not Srecord/Xrecord */
%% printf("#define Rectype_Imm_S8string %d\n",Rectype_Imm_S8string);
  Rectype_S16string, /* 19 -- S16string, not Srecord/Xrecord */
%% printf("#define Rectype_S16string %d\n",Rectype_S16string);
  Rectype_Imm_S16string, /* 20 -- immutable S16string, not Srecord/Xrecord */
%% printf("#define Rectype_Imm_S16string %d\n",Rectype_Imm_S16string);
  Rectype_S32string, /* 21 -- S32string, not Srecord/Xrecord */
%% printf("#define Rectype_S32string %d\n",Rectype_S32string);
  Rectype_Imm_S32string, /* 22 -- immutable S32string, not Srecord/Xrecord */
%% printf("#define Rectype_Imm_S32string %d\n",Rectype_Imm_S32string);
  Rectype_reallocstring, /* 23 -- reallocated simple string, an Sistring, only used #ifdef HAVE_SMALL_SSTRING */
%% printf("#define Rectype_reallocstring %d\n",Rectype_reallocstring);
  /* Rectype_reallocstring is the top SIMPLE-STRING & SIMPLE-VECTOR */
  Rectype_string,      /* 24 -- Iarray, not Srecord/Xrecord */
%% printf("#define Rectype_string %d\n",Rectype_string);
  /* Rectype_string is the top STRING */
  Rectype_mdarray,     /* 25 -- Iarray, not Srecord/Xrecord */
%% printf("#define Rectype_mdarray %d\n",Rectype_mdarray);
  /* Rectype_mdarray is the top ARRAY
   Rectype_Bignum is the bottom NUMBER */
  Rectype_Bignum,        /* Bignum, not Srecord/Xrecord */
%% printf("#define Rectype_Bignum %d\n",Rectype_Bignum);
  Rectype_Lfloat,        /* Lfloat, not Srecord/Xrecord */
%% printf("#define Rectype_Lfloat %d\n",Rectype_Lfloat);
  Rectype_Dfloat,
%% printf("#define Rectype_Dfloat %d\n",Rectype_Dfloat);
  Rectype_Ffloat,
%% printf("#define Rectype_Ffloat %d\n",Rectype_Ffloat);
%% #endif
#endif  /* TYPECODES */
#ifdef SPVW_MIXED
%% #ifdef SPVW_MIXED
  Rectype_Ratio,
%% printf("#define Rectype_Ratio %d\n",Rectype_Ratio);
  Rectype_Complex,
%% printf("#define Rectype_Complex %d\n",Rectype_Complex);
%% #endif
#endif /* SPVW_MIXED */
  /* *** Here the numbers end. *** */
#ifndef TYPECODES
%% #ifndef TYPECODES
  Rectype_Symbol,
%% printf("#define Rectype_Symbol %d\n",Rectype_Symbol);
%% #endif
#endif  /* TYPECODES */
  Rectype_Package,
%% printf("#define Rectype_Package %d\n",Rectype_Package);
  Rectype_Readtable,
%% printf("#define Rectype_Readtable %d\n",Rectype_Readtable);
  Rectype_Pathname,
%% printf("#define Rectype_Pathname %d\n",Rectype_Pathname);
  Rectype_Logpathname,
%% printf("#define Rectype_Logpathname %d\n",Rectype_Logpathname);
  Rectype_Random_State,
%% printf("#define Rectype_Random_State %d\n",Rectype_Random_State);
  Rectype_Stream,
%% printf("#define Rectype_Stream %d\n",Rectype_Stream);
  Rectype_Byte,
%% printf("#define Rectype_Byte %d\n",Rectype_Byte);
  Rectype_Subr,
%% printf("#define Rectype_Subr %d\n",Rectype_Subr);
  Rectype_Fsubr,
%% printf("#define Rectype_Fsubr %d\n",Rectype_Fsubr);
  Rectype_Loadtimeeval,
%% printf("#define Rectype_Loadtimeeval %d\n",Rectype_Loadtimeeval);
  Rectype_Symbolmacro,
%% printf("#define Rectype_Symbolmacro %d\n",Rectype_Symbolmacro);
  Rectype_GlobalSymbolmacro,
%% printf("#define Rectype_GlobalSymbolmacro %d\n",Rectype_GlobalSymbolmacro);
  Rectype_Macro,
%% printf("#define Rectype_Macro %d\n",Rectype_Macro);
  Rectype_FunctionMacro,
%% printf("#define Rectype_FunctionMacro %d\n",Rectype_FunctionMacro);
  Rectype_BigReadLabel,
%% printf("#define Rectype_BigReadLabel %d\n",Rectype_BigReadLabel);
  Rectype_Encoding,
%% printf("#define Rectype_Encoding %d\n",Rectype_Encoding);
  Rectype_Fpointer,      /* only used #ifdef FOREIGN */
%% printf("#define Rectype_Fpointer %d\n",Rectype_Fpointer);
#ifdef DYNAMIC_FFI
%% #ifdef DYNAMIC_FFI
  Rectype_Faddress,
%% printf("#define Rectype_Faddress %d\n",Rectype_Faddress);
  Rectype_Fvariable,
%% printf("#define Rectype_Fvariable %d\n",Rectype_Fvariable);
  Rectype_Ffunction,
%% printf("#define Rectype_Ffunction %d\n",Rectype_Ffunction);
%% #endif
#endif  /* DYNAMIC_FFI */
  Rectype_Weakpointer,
%% printf("#define Rectype_Weakpointer %d\n",Rectype_Weakpointer);
  Rectype_MutableWeakList,
%% printf("#define Rectype_MutableWeakList %d\n",Rectype_MutableWeakList);
  Rectype_MutableWeakAlist,
%% printf("#define Rectype_MutableWeakAlist %d\n",Rectype_MutableWeakAlist);
  Rectype_Weakmapping,
%% printf("#define Rectype_Weakmapping %d\n",Rectype_Weakmapping);
  Rectype_Finalizer,
%% printf("#define Rectype_Finalizer %d\n",Rectype_Finalizer);
#ifdef SOCKET_STREAMS
%% #ifdef SOCKET_STREAMS
  Rectype_Socket_Server,
%% printf("#define Rectype_Socket_Server %d\n",Rectype_Socket_Server);
%% #endif
#endif  /* SOCKET_STREAMS */
#ifdef MULTITHREAD
%% #ifdef MULTITHREAD
  Rectype_Thread,
%%  printf("#define Rectype_Thread %d\n",Rectype_Thread);
  Rectype_Mutex,
%%  printf("#define Rectype_Mutex %d\n",Rectype_Mutex);
  Rectype_Exemption,
%%  printf("#define Rectype_Exemption %d\n",Rectype_Exemption);
%% #endif
#endif
#ifdef YET_ANOTHER_RECORD
%% #ifdef YET_ANOTHER_RECORD
  Rectype_Yetanother,
%% printf("#define Rectype_Yetanother %d\n",Rectype_Yetanother);
%% #endif
#endif /* YET_ANOTHER_RECORD */
  rectype_longlimit, /* the boundary between Srecord/Xrecord and Lrecord. */
  Rectype_WeakList,
%% printf("#define Rectype_WeakList %d\n",Rectype_WeakList);
  Rectype_WeakAnd,
%% printf("#define Rectype_WeakAnd %d\n",Rectype_WeakAnd);
  Rectype_WeakOr,
%% printf("#define Rectype_WeakOr %d\n",Rectype_WeakOr);
  Rectype_WeakAndMapping,
%% printf("#define Rectype_WeakAndMapping %d\n",Rectype_WeakAndMapping);
  Rectype_WeakOrMapping,
%% printf("#define Rectype_WeakOrMapping %d\n",Rectype_WeakOrMapping);
  Rectype_WeakAlist_Key,
%% printf("#define Rectype_WeakAlist_Key %d\n",Rectype_WeakAlist_Key);
  Rectype_WeakAlist_Value,
%% printf("#define Rectype_WeakAlist_Value %d\n",Rectype_WeakAlist_Value);
  Rectype_WeakAlist_Either,
%% printf("#define Rectype_WeakAlist_Either %d\n",Rectype_WeakAlist_Either);
  Rectype_WeakAlist_Both,
%% printf("#define Rectype_WeakAlist_Both %d\n",Rectype_WeakAlist_Both);
  Rectype_WeakHashedAlist_Key,
%% printf("#define Rectype_WeakHashedAlist_Key %d\n",Rectype_WeakHashedAlist_Key);
  Rectype_WeakHashedAlist_Value,
%% printf("#define Rectype_WeakHashedAlist_Value %d\n",Rectype_WeakHashedAlist_Value);
  Rectype_WeakHashedAlist_Either,
%% printf("#define Rectype_WeakHashedAlist_Either %d\n",Rectype_WeakHashedAlist_Either);
  Rectype_WeakHashedAlist_Both,
%% printf("#define Rectype_WeakHashedAlist_Both %d\n",Rectype_WeakHashedAlist_Both);
  /* when adding a new built-in type, do not forget to ...
     add a def-atomic-type to type.lisp;
     and a case in describe.lisp:describe-object;
     add an enum_hg_<newtype> in predtype.d and update heap_statistics_mapper,
       TYPE-OF and CLASS-OF */
  rectype_for_broken_compilers_that_dont_like_trailing_commas
};

/* -------------------------- the various types -------------------------- */

/* Cons */
typedef struct {
  gcv_object_t cdr _attribute_aligned_object_; /* CDR */
  gcv_object_t car _attribute_aligned_object_; /* CAR */
} cons_;
typedef cons_ *  Cons;
%% sprintf(buf,"struct { gcv_object_t cdr%s; gcv_object_t car%s; }",attribute_aligned_object,attribute_aligned_object);
%% emit_typedef(buf,"cons_");
%% emit_typedef("cons_ *","Cons");

/* Ratio */
typedef struct {
  #ifdef SPVW_MIXED
  XRECORD_HEADER
  #endif
  gcv_object_t rt_num _attribute_aligned_object_; /* numerator, Integer */
  gcv_object_t rt_den _attribute_aligned_object_; /* denominator, Integer >0 */
} ratio_;
typedef ratio_ *  Ratio;
%% #if notused
%% #ifdef SPVW_MIXED
%%   sprintf(buf,"struct { XRECORD_HEADER gcv_object_t rt_num%s; gcv_object_t rt_den%s; }",attribute_aligned_object,attribute_aligned_object);
%% #else
%%   sprintf(buf,"struct { gcv_object_t rt_num%s; gcv_object_t rt_den%s; }",attribute_aligned_object,attribute_aligned_object);
%% #endif
%% emit_typedef(buf,"ratio_");
%% emit_typedef("ratio_ *","Ratio");
%% #endif

/* Complex */
typedef struct {
  #ifdef SPVW_MIXED
  XRECORD_HEADER
  #endif
  gcv_object_t c_real _attribute_aligned_object_; /* real part, real number */
  gcv_object_t c_imag _attribute_aligned_object_; /* imaginary part, real number */
} complex_;
typedef complex_ *  Complex;
%% #if notused
%% #ifdef SPVW_MIXED
%%   sprintf(buf,"struct { XRECORD_HEADER gcv_object_t c_real%s; gcv_object_t c_imag%s; }",attribute_aligned_object,attribute_aligned_object);
%% #else
%%   sprintf(buf,"struct { gcv_object_t c_real%s; gcv_object_t c_imag%s; }",attribute_aligned_object,attribute_aligned_object);
%% #endif
%% emit_typedef(buf,"complex_");
%% emit_typedef("complex_ *","Complex");
%% #endif

/* Symbol */
typedef struct {
  VAROBJECT_HEADER
  gcv_object_t symvalue    _attribute_aligned_object_; /* value cell */
  gcv_object_t symfunction _attribute_aligned_object_; /* function definition cell */
  gcv_object_t hashcode    _attribute_aligned_object_; /* hash code */
  gcv_object_t proplist    _attribute_aligned_object_; /* property list */
  gcv_object_t pname       _attribute_aligned_object_; /* Printname */
  gcv_object_t homepackage _attribute_aligned_object_; /* Home-Package or NIL */
#define symbol_length  6
  /* If necessary, add a filler here to ensure sizeof_symbol_is_multiple_of_varobject_alignment. */
  #if defined(sizeof_symbol_is_multiple_of_varobject_alignment)
   #if ((((defined(TYPECODES) ? 1 : 2) + symbol_length) * pointer_bitsize/8) % varobject_alignment) != 0
    gcv_object_t symfiller _attribute_aligned_object_; /* never accessed, not subject to GC */
    #define symbol_has_symfiller
   #endif
  #endif
  #if defined(MULTITHREAD)
    /* the first symvalue in thread is dummy - for faster Symbol_value*/
    #define SYMBOL_TLS_INDEX_NONE ((aint)0)
    #define SYMVALUE_EMPTY make_system(0xEEEEEFUL)
    aint tls_index _attribute_aligned_object_; /* TLS index */
  #endif
} symbol_;
typedef symbol_ *  Symbol;
#if defined(sizeof_symbol_is_multiple_of_varobject_alignment)
  /* Compile-time check: sizeof(symbol_) is a multiple of varobject_alignment. */
  typedef int symbol_size_check[1 - 2 * (int)(sizeof(symbol_) % varobject_alignment)];
#endif
#define symbol_objects_offset  offsetof(symbol_,symvalue)
#define symbol_xlength (sizeof(*(Symbol)0)-symbol_objects_offset-symbol_length*sizeof(gcv_object_t))
%% sprintf(buf,"struct { VAROBJECT_HEADER gcv_object_t symvalue%s; gcv_object_t symfunction%s; gcv_object_t hashcode%s; gcv_object_t proplist%s; gcv_object_t pname%s; gcv_object_t homepackage%s; ",attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object);
%% #if defined(symbol_has_symfiller)
%%   sprintf(buf+strlen(buf),"gcv_object_t symfiller%s; ",attribute_aligned_object);
%% #endif
%% #if defined(MULTITHREAD)
%%   sprintf(buf+strlen(buf)," aint tls_index%s;",attribute_aligned_object);
%% #endif
%% sprintf(buf+strlen(buf),"}");
%% emit_typedef(buf,"symbol_");
%% emit_typedef("symbol_ *","Symbol");

/* Every keyword is a constant. */

/* Tests whether a symbol is a keyword: */
  #define keywordp(sym)  \
    (eq(TheSymbol(sym)->homepackage,O(keyword_package)))

/* For constants, the special-bit is meaningless (since constants
 can't be bound lexically nor dynamically). */

/* Tests whether a symbol is a constant: */
  #define constant_var_p(sym)  \
    (((bit(var_bit0_hf)|bit(var_bit1_hf)) & ~((sym)->header_flags)) == 0)

/* Tests whether a symbol is a SPECIAL-proclaimed variable or a constant: */
  #define special_var_p(sym)  (((sym)->header_flags) & bit(var_bit0_hf))

/* Tests whether a symbol is a symbol-macro: */
  #define symmacro_var_p(sym)  \
    ((((sym)->header_flags) & bit(var_bit1_hf))            \
     && ((((sym)->header_flags) & bit(var_bit0_hf)) == 0))

/* Set the constant-flag of a non-symbol-macro symbol: */
  #define set_const_flag(sym)  \
    (((sym)->header_flags) |= bit(var_bit0_hf)|bit(var_bit1_hf))

/* Delete the constant-flag of a symbol that is a constant:
 (Symbol must not be a Keyword, comp. spvw.d:case_symbolwithflags) */
  #define clear_const_flag(sym)  \
    (((sym)->header_flags) &= ~(bit(var_bit0_hf)|bit(var_bit1_hf)))

/* Set the special-flag of a non-symbol-macro symbol: */
  #define set_special_flag(sym)  \
    (((sym)->header_flags) |= bit(var_bit0_hf))

/* Delete the special-flag of a symbol that is special and non-constant: */
  #define clear_special_flag(sym)  \
    (((sym)->header_flags) &= ~bit(var_bit0_hf))

/* Set the symbol-macro-flag of a non-special/constant symbol: */
  #define set_symmacro_flag(sym)  \
    (((sym)->header_flags) |= bit(var_bit1_hf))

/* Delete the symbol-macro-flag of a symbol that is a symbol-macro: */
  #define clear_symmacro_flag(sym)  \
    (((sym)->header_flags) &= ~bit(var_bit1_hf))

/* Define symbol as constant with given value val.
 val must not trigger the GC! */
  #define define_constant(sym,val)                              \
    do { var Symbol sym_from_define_constant = TheSymbol(sym);  \
         set_const_flag(sym_from_define_constant);              \
         sym_from_define_constant->symvalue = (val);            \
    } while(0)

/* Define symbol as variable and initialize it with a given value val.
 val must not trigger the GC! */
  #define define_variable(sym,val)                              \
    do { var Symbol sym_from_define_variable = TheSymbol(sym);  \
         set_special_flag(sym_from_define_variable);            \
         sym_from_define_variable->symvalue = (val);            \
    } while(0)

/* Remove flag-bits of a symbol: */
#if defined(NO_symbolflags)
  #define symbol_without_flags(symbol)  symbol
#elif (oint_symbolflags_shift==oint_type_shift)
  #define symbol_without_flags(symbol)  \
    as_object(as_oint(symbol) & (type_zero_oint(symbol_type) | oint_addr_mask))
#else
  #define symbol_without_flags(symbol)  \
    as_object(as_oint(symbol) & ~((wbit(active_bit)|wbit(dynam_bit)|wbit(svar_bit))<<oint_symbolflags_shift))
#endif
/* add a flag to the object */
#define SET_BIT(o,b)  as_object(as_oint(o) | wbit(b))
/* remove a flag from the object */
#define CLR_BIT(o,b)  as_object(as_oint(o) & ~wbit(b))

/* Characters */

/* Integer type holding the data of a character: */
#ifdef ENABLE_UNICODE
  #define char_int_len 24  /* anything between 21 and 32 will do */
  #define char_int_limit 0x110000UL
#else
  #define char_int_len 8
  #define char_int_limit 0x100UL
#endif
typedef unsigned_int_with_n_bits(char_int_len)  cint;
#define char_code_limit  char_int_limit
/* Converting an integral code to a character: */
#define int_char(int_from_int_char)  \
  type_data_object(char_type,(aint)(cint)(int_from_int_char))
/* Converting a character to an integral code: */
#if !((oint_data_shift==0) && (char_int_len<=oint_data_len) && (exact_uint_size_p(char_int_len)))
  #ifdef TYPECODES
    #define char_int(char_from_char_int)  \
      ((cint)(untype(char_from_char_int)))
  #else
    #if (char_type>>oint_data_shift)==0 || (char_int_len<=16)
      #define char_int(char_from_char_int)  \
        ((cint)(as_oint(char_from_char_int)>>oint_data_shift))
    #else
      #define char_int(char_from_char_int)  \
        ((cint)((as_oint(char_from_char_int)>>oint_data_shift)&(bitm(oint_data_len)-1)))
    #endif
  #endif
#else
  /* If oint_data_shift=0, untype does not need to shift. If also
   char_int_len<=oint_data_len, and if a cint has exactly char_int_len
   bits, untype does not need to AND. */
  #define char_int(char_from_char_int)  \
    ((cint)as_oint(char_from_char_int))
#endif
/* Characters can therefore be compared for equality using EQ, this is an
 oint comparison, among the characters a comparison of their integral code. */
%% sprintf(buf,"uint%d",char_int_len); emit_typedef(buf,"cint");
%% export_def(int_char(int_from_int_char));
%% export_def(char_int(char_from_char_int));

/* A standalone character. Prefer `chart' to `cint' wherever possible because
 it is typesafe. sizeof(chart) = sizeof(cint). */
#ifdef CHART_STRUCT
  #ifdef __cplusplus
    struct chart { chart() {} chart(int c) : one_c(c) {} cint one_c; };
  #else
    typedef struct { cint one_c; } chart;
  #endif
#else
  typedef cint chart;
#endif
/* Conversions between both:
 as_cint(ch)   chart --> cint
 as_chart(c)   cint --> chart */
#ifdef CHART_STRUCT
  #define as_cint(ch)  ((ch).one_c)
  #if 1
    #ifdef __cplusplus
      inline chart as_chart(int c) { return chart(c); }
    #else
      #define as_chart(c)  ((chart){designated_init(one_c,(c))})
    #endif
  #else
    extern __inline__ chart as_chart (register cint c)
      { register chart ch; ch.one_c = c; return ch; }
  #endif
#else
  #define as_cint(ch)  (ch)
  #define as_chart(c)  (c)
#endif
/* Conversion chart --> object. */
#define code_char(ch)  int_char(as_cint(ch))
/* Conversion object --> chart. */
#define char_code(obj)  as_chart(char_int(obj))
/* Comparison operations. */
#define chareq(ch1,ch2)  (as_cint(ch1) == as_cint(ch2))
#define charlt(ch1,ch2)  (as_cint(ch1) < as_cint(ch2))
#define chargt(ch1,ch2)  (as_cint(ch1) > as_cint(ch2))
%% #ifdef CHART_STRUCT
%%   emit_typedef("struct { cint one_c; }","chart");
%% #else
%%   emit_typedef("cint","chart");
%% #endif
%% export_def(as_cint(ch));
%% export_def(as_chart(c));
%% export_def(code_char(ch));
%% export_def(char_code(obj));

/* Conversion standard char (in ASCII encoding) --> chart. */
#define ascii(x)  as_chart((uintB)(x))
/* Conversion standard char (in ASCII encoding) --> object. */
#define ascii_char(x)  code_char(ascii(x))

/* Test for STANDARD-CHAR. */
#define standard_cint_p(x)  ((('~' >= (x)) && ((x) >= ' ')) || ((x) == NL))

/* Whether to use three different kinds of string representations. */
#if defined(ENABLE_UNICODE) && (defined(GNU) || (defined(UNIX) && !defined(NO_ALLOCA) && !defined(SPARC)) || defined(MICROSOFT)) && !defined(NO_SMALL_SSTRING)
#define HAVE_SMALL_SSTRING
#endif

#ifdef HAVE_SMALL_SSTRING
  #define if_HAVE_SMALL_SSTRING(statement)  statement
  /* We have three kinds of simple strings, with 8-bit codes (ISO-8859-1
   strings), with 16-bit codes (UCS-2 strings) and with 32-bit codes
   (UCS-4/UTF-32 strings). */
  typedef uint8 cint8;
  #define cint8_limit (1UL<<8)
  typedef uint16 cint16;
  #define cint16_limit (1UL<<16)
  typedef uint32 cint32;
  #define cint32_limit 0x110000UL
#else
  #define if_HAVE_SMALL_SSTRING(statement)  /*nop*/
  /* Only one kind of simple strings. */
  typedef cint cint8;
  #define cint8_limit char_int_limit
  typedef cint cint16;
  #define cint16_limit char_int_limit
  typedef cint cint32;
  #define cint32_limit char_int_limit
#endif
%% #ifdef HAVE_SMALL_SSTRING
%%   emit_typedef("uint8","cint8");
%%   emit_typedef("uint16","cint16");
%%   emit_typedef("uint32","cint32");
%% #endif

/* Base characters. */
#define base_char_int_len char_int_len
#define base_char_code_limit  char_code_limit
/* The BASE-CHAR type is defined as
     (upgraded-array-element-type 'standard-char),
 i.e. the element-type of arrays created with (make-array 'standard-char ...).
 Since it defeats the purpose of ENABLE_UNICODE to have different 8-bit, 16-bit
 and 24-bit character types, we define BASE-CHAR=CHARACTER. */

/* Fixnums */

/* fixnum(x) is a fixnum with value x>=0.
 x is an expression with 0 <= x < 2^oint_data_len.
 (Should really be called posfixnum(x).) */
#define fixnum(x)  type_data_object(fixnum_type,x)
%% export_def(fixnum(x));

/* Fixnum_0 is the number 0, Fixnum_1 is the number 1,
 Fixnum_minus1 is the number -1 */
#define Fixnum_0  fixnum(0)
#define Fixnum_1  fixnum(1)
#define Fixnum_minus1  type_data_object( fixnum_type | bit(sign_bit_t), vbitm(oint_data_len)-1 )
%% export_def(Fixnum_0);
%% export_def(Fixnum_1);
%% export_def(Fixnum_minus1);

/* Value of a non-negative fixnum:
 posfixnum_to_V(obj)
 result is >= 0, < 2^oint_data_len. */
#if !defined(SPARC)
  #define posfixnum_to_V(obj)  \
    ((uintV)((as_oint(obj)&((oint)wbitm(oint_data_len+oint_data_shift)-1))>>oint_data_shift))
#else
  /* Long constants are slower than shifts on a SPARC-processor: */
  #if (oint_data_len+oint_data_shift<=intVsize)
    #define posfixnum_to_V(obj)  \
      (((uintV)as_oint(obj) << (intVsize-oint_data_len-oint_data_shift)) >> (intVsize-oint_data_len))
  #else
    #define posfixnum_to_V(obj)  \
      ((uintV)(as_oint(obj) >> (oint_data_len+oint_data_shift-intVsize)) >> (intVsize-oint_data_len))
  #endif
#endif
%% export_def(posfixnum_to_V(obj));

/* Value of a negative fixnum:
 negfixnum_to_V(obj)
 Result is >= - 2^oint_data_len, < 0. */
#define negfixnum_to_V(obj)  (posfixnum_to_V(obj) | (-vbitm(oint_data_len)))
%% #if notused
%% export_def(negfixnum_to_V(obj));
%% #endif

/* Absolute value of a negative fixnum:
 negfixnum_abs_V(obj)
 Result is > 0, <= 2^oint_data_len.
 Beware: Possible wraparound at oint_data_len=intVsize! */
#define negfixnum_abs_V(obj)  \
  ((uintV)((as_oint(fixnum_inc(Fixnum_minus1,1))-as_oint(obj))>>oint_data_shift))

/* Value of a fixnum, obj should be a variable:
 fixnum_to_V(obj)
 Result is >= - 2^oint_data_len, < 2^oint_data_len and of Type sintV.
 This macro should only be used for oint_data_len+1 <= intLsize! */
#if (oint_data_len>=intVsize)
  /* No space left for the sign-bit, thus fixnum_to_V = posfixnum_to_V = negfixnum_to_V ! */
  #define fixnum_to_V(obj)  (sintV)posfixnum_to_V(obj)
#elif (sign_bit_o == oint_data_len+oint_data_shift)
  #define fixnum_to_V(obj)  \
    (((sintV)as_oint(obj) << (intVsize-1-sign_bit_o)) >> (intVsize-1-sign_bit_o+oint_data_shift))
#else
  #if !defined(SPARC)
    #define fixnum_to_V(obj)  \
      (sintV)( ((((sintV)as_oint(obj) >> sign_bit_o) << (intVsize-1)) >> (intVsize-1-oint_data_len)) \
              |((uintV)((as_oint(obj) & ((oint)wbitm(oint_data_len+oint_data_shift)-1)) >> oint_data_shift)) \
             )
  #else
    /* Long constants are slower than shifts on a SPARC-processor: */
    #if (oint_data_len+oint_data_shift<=intVsize)
      #define fixnum_to_V(obj)  \
        (sintV)( ((((sintV)(as_oint(obj) >> sign_bit_o)) << (intVsize-1)) >> (intVsize-1-oint_data_len)) \
                |(((uintV)as_oint(obj) << (intVsize-oint_data_len-oint_data_shift)) >> (intVsize-oint_data_len)) \
               )
    #else
      #define fixnum_to_V(obj)  \
        (sintV)( ((((sintV)(as_oint(obj) >> sign_bit_o)) << (intVsize-1)) >> (intVsize-1-oint_data_len)) \
                |((uintV)(as_oint(obj) >> (oint_data_len+oint_data_shift-intVsize)) >> (intVsize-oint_data_len)) \
               )
    #endif
  #endif
#endif
%% export_def(fixnum_to_V(obj));

#ifdef intQsize
  /* Value of a fixnum, obj should be a variable:
   fixnum_to_Q(obj)
   Result is >= - 2^oint_data_len, < 2^oint_data_len. */
  #if (sign_bit_o == oint_data_len+oint_data_shift)
    #define fixnum_to_Q(obj)  \
      (((sintQ)as_oint(obj) << (intQsize-1-sign_bit_o)) >> (intQsize-1-sign_bit_o+oint_data_shift))
  #else
    #define fixnum_to_Q(obj)  \
      (sintQ)( ((((sintQ)as_oint(obj) >> sign_bit_o) << (intQsize-1)) >> (intQsize-1-oint_data_len)) \
              |((uintQ)((as_oint(obj) & (wbitm(oint_data_len+oint_data_shift)-1)) >> oint_data_shift)) \
             )
  #endif
#endif

/* Add a constant to a non-negative fixnum, given that
 the result is a non-negative fixnum as well:
 fixnum_inc(obj,delta)
 > obj: a fixnum
 > delta: a constant
 < result: incremented fixnum */
#define fixnum_inc(obj,delta)                                           \
    objectplus(obj, (soint)(delta) << oint_data_shift)
%% export_def(fixnum_inc(obj,delta));

/* posfixnum(x) is a fixnum with value x>=0. */
#define posfixnum(x)  fixnum_inc(Fixnum_0,x)
%% export_def(posfixnum(x));

/* negfixnum(x) is a fixnum with value x<0.
 (Beware if x is unsigned!) */
#define negfixnum(x)  fixnum_inc(fixnum_inc(Fixnum_minus1,1),x)
%% export_def(negfixnum(x));

/* sfixnum(x) is a fixnum with value x,
 x is a constant-expression with -2^oint_data_len <= x < 2^oint_data_len. */
#define sfixnum(x) ((x)>=0 ? posfixnum(x) : negfixnum(x))
%% export_def(sfixnum(x));

/* Convert a character into a fixnum >=0 (the same as for char-int): */
#ifdef WIDE_STRUCT
  #define char_to_fixnum(obj)  \
    type_data_object(fixnum_type,untype(obj))
#else
  #define char_to_fixnum(obj)  \
    objectplus(obj,type_zero_oint(fixnum_type)-type_zero_oint(char_type))
#endif

/* Make a character from a fitting fixnum >=0 (the same as for int-char): */
#ifdef WIDE_STRUCT
  #define fixnum_to_char(obj)  \
    type_data_object(char_type,untype(obj))
#else
  #define fixnum_to_char(obj)  \
    objectplus(obj,type_zero_oint(char_type)-type_zero_oint(fixnum_type))
#endif

/* Bignums */
typedef struct {
  VAROBJECT_HEADER  /* self-pointer for GC */
  #ifdef TYPECODES
  uintC length;     /* length in digits */
  #endif
  uintD data[unspecified]; /* number as its two's complement representation */
} bignum_;
typedef bignum_ *  Bignum;
/* The length is actually an uintWC. */
#ifdef TYPECODES
  #define bignum_length(ptr)  ((ptr)->length)
#else
  #define bignum_length(ptr)  srecord_length(ptr)
#endif
#define Bignum_length(obj)  bignum_length(TheBignum(obj))
%% #ifdef TYPECODES
%%   emit_typedef("struct { VAROBJECT_HEADER uintC length; uintD data[unspecified]; }","bignum_");
%% #else
%%   emit_typedef("struct { VAROBJECT_HEADER uintD data[unspecified]; }","bignum_");
%% #endif
%% emit_typedef("bignum_ *","Bignum");
%% export_def(bignum_length(ptr));
%% export_def(Bignum_length(obj));

/* Single-Floats */
typedef uint32 ffloat; /* 32-Bit-Float in IEEE-format */
typedef union {
  ffloat eksplicit;    /* Value, explicit */
  #ifdef FAST_FLOAT
  float machine_float; /* Value, as C-'float' */
  #endif
} ffloatjanus;
#if !defined(IMMEDIATE_FFLOAT)
typedef struct {
  VAROBJECT_HEADER            /* self-pointer for GC */
  ffloatjanus representation; /* Value */
} ffloat_;
typedef ffloat_ *  Ffloat;
#define ffloat_value(obj)  (TheFfloat(obj)->float_value)
#else
/* The float-value is stored in the pointer itself, like short-floats. */
#define ffloat_value(obj)  ((ffloat)untype(obj))
#endif
%% emit_typedef("uint32","ffloat");
%% emit_typedef("union { ffloat eksplicit; }","ffloatjanus");

/* Double-Floats */
typedef /* 64-Bit-Float in IEEE-format: */
        #ifdef intQsize
          /* Sign/Exponent/Mantissa */
          uint64
        #else
          /* Sign/Exponent/MantissaHigh and MantissaLow */
          #if BIG_ENDIAN_P || (defined(ARM) && !defined(__ARM_EABI__))
            struct {uint32 semhi,mlo;}
          #else
            struct {uint32 mlo,semhi;}
          #endif
        #endif
  dfloat;
typedef union {
  dfloat eksplicit _attribute_in_misaligned_varobjects_; /* Value, explicit */
  #ifdef FAST_DOUBLE
  double machine_double _attribute_in_misaligned_varobjects_; /* Value, as C-'double' */
  #endif
} dfloatjanus;
typedef struct {
  VAROBJECT_HEADER            /* self-pointer for GC */
  dfloatjanus representation _attribute_in_misaligned_varobjects_; /* value */
} dfloat_;
typedef dfloat_ *  Dfloat;
%% #ifdef intQsize
%%   emit_typedef("uint64","dfloat");
%% #else
%%   #if BIG_ENDIAN_P || (defined(ARM) && !defined(__ARM_EABI__))
%%     emit_typedef("struct {uint32 semhi,mlo;}","dfloat");
%%   #else
%%     emit_typedef("struct {uint32 mlo,semhi;}","dfloat");
%%   #endif
%% #endif
%% #ifdef FAST_DOUBLE
%%   sprintf(buf,"union { dfloat eksplicit%s; double machine_double%s; }",attribute_in_misaligned_varobjects,attribute_in_misaligned_varobjects);
%%   emit_typedef(buf,"dfloatjanus");
%% #else
%%   sprintf(buf,"union { dfloat eksplicit%s; }",attribute_in_misaligned_varobjects);
%%   emit_typedef(buf,"dfloatjanus");
%% #endif

/* Single- and Double-Floats */
  #define float_value  representation.eksplicit

/* Long-Floats */
typedef struct {
  VAROBJECT_HEADER   /* Self-pointer for GC */
  #ifdef TYPECODES
  uintC  len;        /* length of the mantissa in digits */
  #endif
  uint32 expo;       /* exponent */
  uintD  data[unspecified]; /* mantissa */
} lfloat_;
typedef lfloat_ *  Lfloat;
/* The length is actually an uintWC. */
#ifdef TYPECODES
  #define lfloat_length(ptr)  ((ptr)->len)
#else
  #define lfloat_length(ptr)  srecord_length(ptr)
#endif
#define Lfloat_length(obj)  lfloat_length(TheLfloat(obj))

/* simple array (cover simple linear arrays: simple bit vector, simple vector) */
typedef struct {
  VRECORD_HEADER /* Self-pointer for GC, length in elements */
} sarray_;
typedef sarray_ *  Sarray;
#define sarray_length(ptr)  vrecord_length(ptr)
#define Sarray_length(obj)  sarray_length(TheSarray(obj))
%% #if notused
%% emit_typedef("struct { VRECORD_HEADER }","sarray_");
%% emit_typedef("sarray_ *","Sarray");
%% #endif
%% export_def(sarray_length(ptr));
%% export_def(Sarray_length(obj));

/* simple bit vector */
typedef struct {
  VRECORD_HEADER /* self-pointer for GC, length in bits */
  uint8  data[unspecified]; /* Bits, divided into bytes */
} sbvector_;
typedef sbvector_ *  Sbvector;
#define sbvector_length(ptr)  sarray_length(ptr)
#define Sbvector_length(obj)  sbvector_length(TheSbvector(obj))
%% emit_typedef("struct { VRECORD_HEADER uint8  data[unspecified]; }","sbvector_");
%% emit_typedef("sbvector_ *","Sbvector");
%% export_def(sbvector_length(ptr));
%% export_def(Sbvector_length(obj));

/* simple string template */
#ifdef TYPECODES
  #define SSTRING_HEADER  \
                 VAROBJECT_HEADER /* self-pointer for GC  */\
                 uintL tfl;       /* type, flags, length */
#else
  #define SSTRING_HEADER  \
                 VAROBJECT_HEADER /* self-pointer for GC, tfl */
#endif
typedef struct {
  SSTRING_HEADER
} sstring_;
typedef sstring_ *  Sstring;
#define STRUCT_SSTRING(cint_type) \
  struct {                                                                     \
    SSTRING_HEADER /* self-pointer for GC, type+flags, length in characters */ \
    cint_type  data[unspecified];  /* characters */                            \
  }
#ifdef HAVE_SMALL_SSTRING
  typedef STRUCT_SSTRING(cint8)  s8string_;
  typedef s8string_ *  S8string;
  typedef STRUCT_SSTRING(cint16)  s16string_;
  typedef s16string_ *  S16string;
  typedef STRUCT_SSTRING(cint32)  s32string_;
  typedef s32string_ *  S32string;
#else
  /* Only one kind of simple strings. */
  #ifdef ENABLE_UNICODE
    typedef STRUCT_SSTRING(cint32)  s32string_;
    typedef s32string_ *  S32string;
    /* Aliases. */
    typedef s32string_  s16string_;
    typedef S32string  S16string;
    typedef s32string_  s8string_;
    typedef S32string  S8string;
  #else
    typedef STRUCT_SSTRING(cint8)  s8string_;
    typedef s8string_ *  S8string;
    /* Aliases. */
    typedef s8string_  s16string_;
    typedef S8string  S16string;
    typedef s8string_  s32string_;
    typedef S8string  S32string;
  #endif
#endif
/* A "normal simple string" is one of maximum-width element type.
 It cannot be reallocated. Only strings with smaller element type
 (called "small simple strings") can be reallocated. */
  typedef STRUCT_SSTRING(chart)  snstring_;
  typedef snstring_ *  Snstring;
/* These accessors work on any simple string, except reallocated simple-strings. */
#ifdef TYPECODES
  #define sstring_length(ptr)  ((ptr)->tfl >> 6)
#else
  #define sstring_length(ptr)  ((ptr)->tfl >> 10)
#endif
#define Sstring_length(obj)  sstring_length(TheSstring(obj))
/* Maximum allowed simple-string length: */
#ifdef TYPECODES
  #define stringsize_limit_1  ((uintL)(bit(intLsize-6)-1))
#else
  #define stringsize_limit_1  ((uintL)(bit(intLsize-10)-1))
#endif
/* Constructing the tfl word: */
#ifdef TYPECODES
  #define sstring_tfl(eltype,imm,flags,length)  \
    (((length) << 6) + ((eltype) << 4) + ((imm) << 3) + (flags))
#else
  /* This must be consistent with vrecord_tfl. */
  #define sstringrecord_tfl(rectype,flags,length)  \
    (((length) << 10) + ((flags) << 8) + (rectype))
  #define sstring_tfl(eltype,imm,flags,length)  \
    sstringrecord_tfl(Rectype_S8string + ((eltype) << 1) + (imm),flags,length)
#endif
/* Test whether a simple string is reallocated: */
#ifdef HAVE_SMALL_SSTRING
  #ifdef TYPECODES
    #define sstringflags_forwarded_B  bit(2)
    #define sstring_reallocatedp(ptr)  ((ptr)->tfl & sstringflags_forwarded_B)
  #else
    #define sstring_reallocatedp(ptr)  (record_type(ptr) == Rectype_reallocstring)
  #endif
#else
  #define sstring_reallocatedp(ptr)  0
#endif
/* Extract the element type of a not-reallocated simple string: */
#ifdef TYPECODES
  #define sstring_eltype(ptr)  (((ptr)->tfl >> 4) & 3)
#else
  #define sstring_eltype(ptr)  ((record_type(ptr) - Rectype_S8string) >> 1)
#endif
/* Possible values of sstring_eltype: */
  #define Sstringtype_8Bit   0
  #define Sstringtype_16Bit  1
  #define Sstringtype_32Bit  2
/* Extract the immutable bit of a simple string (reallocated or not): */
#ifdef TYPECODES
  #define sstring_immutable(ptr)  (((ptr)->tfl >> 3) & 1)
#else
  #define sstring_immutable(ptr)  ((record_type(ptr) - Rectype_S8string) & 1)
#endif
/* Extract the flags of a simple string (reallocated or not): */
#ifdef TYPECODES
  /* Three bits, containing also sstringflags_forwarded_B. */
  #define sstring_flags(ptr)  ((ptr)->tfl & 7)
  #define sstring_flags_clr(ptr,bits)  ((ptr)->tfl &= ~(uintL)(bits))
  #define sstring_flags_set(ptr,bits)  ((ptr)->tfl |= (uintL)(bits))
#else
  #define sstring_flags(ptr)  (((ptr)->tfl >> 8) & 3)
  #define sstring_flags_clr(ptr,bits)  ((ptr)->tfl &= ~((uintL)(bits) << 8))
  #define sstring_flags_set(ptr,bits)  ((ptr)->tfl |= ((uintL)(bits) << 8))
#endif
/* Bit masks in the flags. Only used during garbage collection. */
  #define sstringflags_backpointer_B  bit(0)
  #define sstringflags_relocated_B    bit(1)
  #define mark_sstring_clean(ptr)  \
    sstring_flags_clr(ptr,sstringflags_backpointer_B|sstringflags_relocated_B)
%% export_def(SSTRING_HEADER);
%% emit_typedef("struct { SSTRING_HEADER }","sstring_");
%% emit_typedef("sstring_ *","Sstring");
%% #ifdef HAVE_SMALL_SSTRING
%%   export_def(STRUCT_SSTRING(cint_type));
%%   emit_typedef("STRUCT_SSTRING(cint8)","s8string_");
%%   emit_typedef("s8string_ *","S8string");
%%   emit_typedef("STRUCT_SSTRING(cint16)","s16string_");
%%   emit_typedef("s16string_ *","S16string");
%%   emit_typedef("STRUCT_SSTRING(cint32)","s32string_");
%%   emit_typedef("s32string_ *","S32string");
%% #endif
%% emit_typedef("struct { SSTRING_HEADER chart data[unspecified]; }","snstring_");
%% emit_typedef("snstring_*","Snstring");
%% export_def(sstring_length(ptr));
%% export_def(Sstring_length(obj));
%% export_def(sstring_eltype(ptr));

/* simple vector */
typedef struct {
  VRECORD_HEADER /* self-pointer for GC, length in objects */
  gcv_object_t data[unspecified] _attribute_aligned_object_; /* elements */
} svector_;
typedef svector_ *  Svector;
#define svector_length(ptr)  sarray_length(ptr)
#define Svector_length(obj)  svector_length(TheSvector(obj))
%% sprintf(buf,"struct { VRECORD_HEADER gcv_object_t data[unspecified]%s; }",attribute_aligned_object);
%% emit_typedef(buf,"svector_");
%% emit_typedef("svector_ *","Svector");

/* simple indirect string */
typedef struct {
  SSTRING_HEADER   /* self-pointer for GC, tfl */
  gcv_object_t data _attribute_aligned_object_; /* data vector */
} sistring_;
typedef sistring_ *  Sistring;
#define sistring_data_offset  offsetof(sistring_,data)

/* non-simple indirect Array */
typedef struct {
  VAROBJECT_HEADER   /* self-pointer for GC */
  #ifdef TYPECODES
  uintB flags;       /* flags */
  uintC rank;        /* rank n */
  #endif
  gcv_object_t data _attribute_aligned_object_; /* data vector */
  uintL totalsize;   /* totalsize = product of the n dimensions */
  uintL dims[unspecified]; /* poss. displaced-offset, n dimensions, poss. fill-pointer */
} iarray_;
typedef iarray_ *  Iarray;
#define iarray_data_offset  offsetof(iarray_,data)
/* The rank is actually an uintWC.
 access Rang, Flags: */
#ifdef TYPECODES
  #define iarray_rank(ptr)  ((ptr)->rank)
#else
  #define iarray_rank(ptr)  srecord_length(ptr)
#endif
#define Iarray_rank(obj)  iarray_rank(TheIarray(obj))
#ifdef TYPECODES
  #define iarray_flags(ptr)  ((ptr)->flags)
#else
  #define iarray_flags(ptr)  record_flags(ptr)
#endif
#define Iarray_flags(obj)  iarray_flags(TheIarray(obj))
#ifdef TYPECODES
  #define iarray_flags_clr(ptr,bits)  ((ptr)->flags &= ~(bits))
  #define iarray_flags_set(ptr,bits)  ((ptr)->flags |= (bits))
  #define iarray_flags_replace(ptr,newflags)  ((ptr)->flags = (newflags))
#else
  #define iarray_flags_clr(ptr,bits)  record_flags_clr(ptr,bits)
  #define iarray_flags_set(ptr,bits)  record_flags_set(ptr,bits)
  #define iarray_flags_replace(ptr,newflags) record_flags_replace(ptr,newflags)
#endif
/* Bits in the Flags: */
#define arrayflags_adjustable_bit  7 /* set, if array is adjustable */
#define arrayflags_fillp_bit       6 /* set, if a fill-pointer exists (only possible for n=1) */
#define arrayflags_displaced_bit   5 /* set, if array is displaced */
#define arrayflags_dispoffset_bit  4 /* set, if there is space for the
                                        displaced-offset
                                        (<==> array adjustable or displaced) */
#define arrayflags_atype_mask  0x0F  /* mask for the element-type */
/* Element-types of arrays in Bits 3..0 of its flags:
 The first ones are chosen, so that 2^Atype_nBit = n. */
#define Atype_Bit    0  /* storage vector is of type sbvector_type */
#define Atype_2Bit   1  /* storage vector is of type sb2vector_type */
#define Atype_4Bit   2  /* storage vector is of type sb4vector_type */
#define Atype_8Bit   3  /* storage vector is of type sb8vector_type */
#define Atype_16Bit  4  /* storage vector is of type sb16vector_type */
#define Atype_32Bit  5  /* storage vector is of type sb32vector_type */
#define Atype_T      6  /* storage vector is of type svector_type */
#define Atype_Char   7  /* storage vector is of type sstring_type */
#define Atype_NIL    8  /* storage vector is NIL */
%% export_def(Atype_Bit);
%% export_def(Atype_8Bit);
%% export_def(Atype_32Bit);
%% export_def(Atype_T);

/* array-types */
#ifdef TYPECODES
  #define Array_type(obj)  typecode(obj)
  #define Array_type_bvector     bvector_type      /* Iarray */
  #define Array_type_b2vector    b2vector_type     /* Iarray */
  #define Array_type_b4vector    b4vector_type     /* Iarray */
  #define Array_type_b8vector    b8vector_type     /* Iarray */
  #define Array_type_b16vector   b16vector_type    /* Iarray */
  #define Array_type_b32vector   b32vector_type    /* Iarray */
  #define Array_type_string      string_type       /* Iarray */
  #define Array_type_vector      vector_type       /* Iarray */
  #define Array_type_mdarray     mdarray_type      /* Iarray */
  #define Array_type_sbvector    sbvector_type     /* Sbvector */
  #define Array_type_sb2vector   sb2vector_type    /* Sbvector */
  #define Array_type_sb4vector   sb4vector_type    /* Sbvector */
  #define Array_type_sb8vector   sb8vector_type    /* Sbvector */
  #define Array_type_sb16vector  sb16vector_type   /* Sbvector */
  #define Array_type_sb32vector  sb32vector_type   /* Sbvector */
  #define Array_type_sstring     sstring_type      /* Sstring */
  #define Array_type_svector     svector_type      /* Svector */
  #define Array_type_snilvector  symbol_type       /* Symbol NIL */
  /* Array_type_simple_bit_vector(atype)
   maps Atype_[n]Bit to Array_type_sb[n]vector. Depends on TB0, TB1, TB2.
   The formula works because there are only 4 possible cases:
    (TB0,TB1,TB2)   formula
      (0, 1, 2)      atype
      (0, 1, 3)      atype + (atype & -4)
      (0, 2, 3)      atype + (atype & -2)
      (1, 2, 3)      atype + (atype & -1) = atype << 1 */
  #define Array_type_simple_bit_vector(atype)  \
    (Array_type_sbvector + ((atype)<<TB0) + ((atype)&(bit(TB0+1)-bit(TB1))) + ((atype)&(bit(TB1+1)-bit(TB2))))
#else
  #define Array_type(obj)  Record_type(obj)
  #define Array_type_bvector     Rectype_bvector     /* Iarray */
  #define Array_type_b2vector    Rectype_b2vector    /* Iarray */
  #define Array_type_b4vector    Rectype_b4vector    /* Iarray */
  #define Array_type_b8vector    Rectype_b8vector    /* Iarray */
  #define Array_type_b16vector   Rectype_b16vector   /* Iarray */
  #define Array_type_b32vector   Rectype_b32vector   /* Iarray */
  #define Array_type_string      Rectype_string      /* Iarray */
  #define Array_type_vector      Rectype_vector      /* Iarray */
  #define Array_type_mdarray     Rectype_mdarray     /* Iarray */
  #define Array_type_sbvector    Rectype_Sbvector    /* Sbvector */
  #define Array_type_sb2vector   Rectype_Sb2vector   /* Sbvector */
  #define Array_type_sb4vector   Rectype_Sb4vector   /* Sbvector */
  #define Array_type_sb8vector   Rectype_Sb8vector   /* Sbvector */
  #define Array_type_sb16vector  Rectype_Sb16vector  /* Sbvector */
  #define Array_type_sb32vector  Rectype_Sb32vector  /* Sbvector */
  #define Array_type_sstring     Rectype_S8string: case Rectype_Imm_S8string: case Rectype_S16string: case Rectype_Imm_S16string: case Rectype_S32string: case Rectype_Imm_S32string: case Rectype_reallocstring   /* S[8|16|32]string, reallocated string */
  #define Array_type_svector     Rectype_Svector     /* Svector */
  #define Array_type_snilvector  Rectype_Symbol      /* Symbol NIL */
#endif
/* Determining the atype of a [simple-]bit-array: */
#define sbNvector_atype(obj)  \
  type_bits_to_atype(Array_type(obj) - Array_type_sbvector)
#define bNvector_atype(obj)  \
  type_bits_to_atype(Array_type(obj) - Array_type_bvector)
#ifdef TYPECODES
  /* There are only 4 cases:
    (TB0,TB1,TB2)   formula
      (0, 1, 2)      type
      (0, 1, 3)      (type + (type & 3)) >> 1 = type - ((type & -8) >> 1)
      (0, 2, 3)      (type + (type & 1)) >> 1 = type - ((type & -4) >> 1)
      (1, 2, 3)      type >> 1                = type - ((type & -2) >> 1) */
  #if TB2 > 2
    #define type_bits_to_atype(type)  \
      (((type) + ((type)&(bit(6-TB0-TB1-TB2)-1))) >> 1)
  #else
    #define type_bits_to_atype(type)  (type)
  #endif
#else
  #define type_bits_to_atype(type)  (type)
#endif
%% #ifdef TYPECODES
%%  export_def(Array_type_simple_bit_vector(atype));
%% #endif

/* Packages */
typedef struct {
  XRECORD_HEADER
  gcv_object_t pack_external_symbols  _attribute_aligned_object_;
  gcv_object_t pack_internal_symbols  _attribute_aligned_object_;
  gcv_object_t pack_shadowing_symbols _attribute_aligned_object_;
  gcv_object_t pack_use_list          _attribute_aligned_object_;
  gcv_object_t pack_used_by_list      _attribute_aligned_object_;
  gcv_object_t pack_name              _attribute_aligned_object_;
  gcv_object_t pack_nicknames         _attribute_aligned_object_;
  gcv_object_t pack_docstring         _attribute_aligned_object_;
  gcv_object_t pack_shortest_name     _attribute_aligned_object_;
#ifdef MULTITHREAD
  gcv_object_t pack_mutex             _attribute_aligned_object_;
#endif
} *  Package;
#define package_length  ((sizeof(*(Package)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))
/* Some packages are case-sensitive. */
#define mark_pack_casesensitive(obj)  record_flags_set(ThePackage(obj),bit(0))
#define mark_pack_caseinsensitive(obj) record_flags_clr(ThePackage(obj),bit(0))
#define pack_casesensitivep(obj)      (record_flags(ThePackage(obj)) & bit(0))
/* Some packages are case-inverted. */
#define mark_pack_caseinverted(obj)  record_flags_set(ThePackage(obj),bit(1))
#define mark_pack_casepreserved(obj) record_flags_clr(ThePackage(obj),bit(1))
#define pack_caseinvertedp(obj)      (record_flags(ThePackage(obj)) & bit(1))
/* Some packages, such as COMMON-LISP, are locked. */
#define mark_pack_locked(obj)    record_flags_set(ThePackage(obj),bit(2))
#define mark_pack_unlocked(obj)  record_flags_clr(ThePackage(obj),bit(2))
#define pack_locked_p(obj)       (record_flags(ThePackage(obj)) & bit(2))
/* Do not do anything with deleted packages. */
#define mark_pack_deleted(obj)  record_flags_set(ThePackage(obj),bit(7))
#define pack_deletedp(obj)      (record_flags(ThePackage(obj)) & bit(7))
%% #if notused
%% sprintf(buf,"struct { XRECORD_HEADER gcv_object_t pack_external_symbols%s; gcv_object_t pack_internal_symbols%s; gcv_object_t pack_shadowing_symbols%s; gcv_object_t pack_use_list%s; gcv_object_t pack_used_by_list%s; gcv_object_t pack_name%s; gcv_object_t pack_nicknames%s; gcv_object_t pack_docstring%s; gcv_object_t pack_shortest_name%s; %s %s %s %s } *",attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,
%% #ifdef MULTITHREAD
%% "gcv_object_t", "pack_mutex", attribute_aligned_object, ";");
%% #else
%% "", "", "", "");
%% #endif
%% emit_typedef(buf,"Package");
%% #endif

/* Hash-Tables */
typedef struct {
  XRECORD_HEADER
  #ifdef GENERATIONAL_GC
  gcv_object_t ht_lastrehash         _attribute_aligned_object_;
  #endif
  gcv_object_t ht_maxcount           _attribute_aligned_object_;
  gcv_object_t ht_kvtable            _attribute_aligned_object_;
  gcv_object_t ht_lookupfn           _attribute_aligned_object_;
  gcv_object_t ht_hashcodefn         _attribute_aligned_object_;
  gcv_object_t ht_testfn             _attribute_aligned_object_;
  gcv_object_t ht_gcinvariantfn      _attribute_aligned_object_;
  gcv_object_t ht_rehash_size        _attribute_aligned_object_;
  gcv_object_t ht_mincount_threshold _attribute_aligned_object_;
  gcv_object_t ht_mincount           _attribute_aligned_object_;
  gcv_object_t ht_test               _attribute_aligned_object_; /* hash-table-test - for define-hash-table-test */
  gcv_object_t ht_hash               _attribute_aligned_object_; /* hash function */
  uintL ht_size;
} *  Hashtable;
#ifdef GENERATIONAL_GC
  #define hashtable_length  12
#else
  #define hashtable_length  11
#endif
#define hashtable_xlength  (sizeof(*(Hashtable)0)-offsetofa(record_,recdata)-hashtable_length*sizeof(gcv_object_t))
/* Mark a Hash Table as now to reorganize
 set_ht_invalid(TheHashtable(ht));
 mark_ht_invalid(TheHashtable(ht));
 A bit that is set when the list structure is invalid and a rehash is needed. */
#define htflags_invalid_B  bit(7)
/* A bit that is set if the table has a key whose hash code is not GC-invariant. */
#define htflags_gc_rehash_B  bit(6)
#ifdef GENERATIONAL_GC
  #define mark_ht_invalid(ptr)  \
    (record_flags_set(ptr,htflags_invalid_B), \
     (ptr)->ht_lastrehash = unbound)
  #define mark_ht_valid(ptr)  \
    (record_flags_clr(ptr,htflags_invalid_B), \
     (ptr)->ht_lastrehash = O(gc_count))
  #define ht_validp(ptr)  \
    ((record_flags(ptr) & htflags_invalid_B) == 0       \
     && ((record_flags(ptr) & htflags_gc_rehash_B) == 0 \
         || eq((ptr)->ht_lastrehash,O(gc_count))))
#else
  #define mark_ht_invalid(ptr)  record_flags_set(ptr,htflags_invalid_B)
  #define mark_ht_valid(ptr)  record_flags_clr(ptr,htflags_invalid_B)
  #define ht_validp(ptr)  ((record_flags(ptr) & htflags_invalid_B) == 0)
#endif
#ifdef GENERATIONAL_GC
  #define set_ht_invalid(ptr)  mark_ht_invalid(ptr)
  #define set_ht_valid(ptr)  mark_ht_valid(ptr)
#else
  extern bool hash_lookup_builtin (object ht, object obj, bool allowgc, gcv_object_t** KVptr_, gcv_object_t** Iptr_);
  extern bool hash_lookup_builtin_with_rehash (object ht, object obj, bool allowgc, gcv_object_t** KVptr_, gcv_object_t** Iptr_);
  #define set_ht_invalid(ptr)  \
    (mark_ht_invalid(ptr),                                               \
     eq((ptr)->ht_lookupfn,P(hash_lookup_builtin))                       \
     ? ((ptr)->ht_lookupfn = P(hash_lookup_builtin_with_rehash), 0) : 0)
  #define set_ht_valid(ptr)  \
    (mark_ht_valid(ptr),                                       \
     eq((ptr)->ht_lookupfn,P(hash_lookup_builtin_with_rehash)) \
     ? ((ptr)->ht_lookupfn = P(hash_lookup_builtin), 0) : 0)
#endif
#define set_ht_invalid_if_needed(ptr)  \
  if (record_flags(ptr) & htflags_gc_rehash_B) \
    set_ht_invalid(ptr)/*;*/
/* A bit that indicates whether to warn about this situation. */
#define htflags_warn_gc_rehash_B  bit(5)
/* Extract the part of the flags that encodes the test. */
#define ht_test_code(flags)  \
  (flags & (bit(0) | bit(1) | bit(2) | bit(3)))
/* Tests whether a test code indicates a user-defined test function. */
#define ht_test_code_user_p(test_code)  \
  (((test_code) & bit(2)) != 0)
/* Test whether a hash table is weak. */
#define ht_weak_p(ht)  \
  !simple_vector_p(TheHashtable(ht)->ht_kvtable)
/* The kvtable array is either a HashedAlist or a WeakHashedAlist.
 Both share the same layout, i.e.
   &((HashedAlist)0)->hal_data == &((WeakHashedAlist)0)->whal_data. */
typedef struct {
  VRECORD_HEADER /* self-pointer for GC, length in objects */
  gcv_object_t hal_filler            _attribute_aligned_object_; /* for consistency with WeakHashedAlist */
  gcv_object_t hal_itable            _attribute_aligned_object_; /* index-vector */
  gcv_object_t hal_count             _attribute_aligned_object_; /* remaining pairs */
  gcv_object_t hal_freelist          _attribute_aligned_object_; /* start index of freelist */
  gcv_object_t hal_data[unspecified] _attribute_aligned_object_; /* (key, value, next) triples */
} * HashedAlist;
/* TheHashedAlist is used to access both HashedAlist and WeakHashedAlist. */
#define TheHashedAlist(obj)  ((HashedAlist)TheVarobject(obj))

/* Readtables */
typedef struct {
  XRECORD_HEADER
  gcv_object_t readtable_syntax_table _attribute_aligned_object_;
  gcv_object_t readtable_macro_table  _attribute_aligned_object_;
  gcv_object_t readtable_case         _attribute_aligned_object_;
} *  Readtable;
#define readtable_length  ((sizeof(*(Readtable)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* Pathnames */
typedef struct {
  XRECORD_HEADER
  #if HAS_HOST
    gcv_object_t pathname_host      _attribute_aligned_object_;
  #endif
  #if HAS_DEVICE
    gcv_object_t pathname_device    _attribute_aligned_object_;
  #endif
  #if 1
    gcv_object_t pathname_directory _attribute_aligned_object_;
    gcv_object_t pathname_name      _attribute_aligned_object_;
    gcv_object_t pathname_type      _attribute_aligned_object_;
    gcv_object_t pathname_version   _attribute_aligned_object_;
  #endif
} *  Pathname;
#define pathname_length  ((sizeof(*(Pathname)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* Logical Pathnames */
typedef struct {
  XRECORD_HEADER
  gcv_object_t pathname_host      _attribute_aligned_object_;
  gcv_object_t pathname_directory _attribute_aligned_object_;
  gcv_object_t pathname_name      _attribute_aligned_object_;
  gcv_object_t pathname_type      _attribute_aligned_object_;
  gcv_object_t pathname_version   _attribute_aligned_object_;
} *  Logpathname;
#define logpathname_length  ((sizeof(*(Logpathname)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* Random-States */
typedef struct {
  XRECORD_HEADER
  gcv_object_t random_state_seed _attribute_aligned_object_;
} *  Random_state;
#define random_state_length  ((sizeof(*(Random_state)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* Bytes */
typedef struct {
  XRECORD_HEADER
  gcv_object_t byte_size     _attribute_aligned_object_;
  gcv_object_t byte_position _attribute_aligned_object_;
} *  Byte;
#define byte_length  ((sizeof(*(Byte)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* Fsubrs */
typedef struct {
  XRECORD_HEADER
  gcv_object_t name    _attribute_aligned_object_;
  gcv_object_t argtype _attribute_aligned_object_;
  void* function; /* actually a fsubr_function_t* */
} *  Fsubr;
#define fsubr_length  2
#define fsubr_xlength  (sizeof(*(Fsubr)0)-offsetofa(record_,recdata)-fsubr_length*sizeof(gcv_object_t))

/* Load-time-evals */
typedef struct {
  XRECORD_HEADER
  gcv_object_t loadtimeeval_form _attribute_aligned_object_;
} *  Loadtimeeval;
#define loadtimeeval_length  ((sizeof(*(Loadtimeeval)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* Symbol-macros */
typedef struct {
  XRECORD_HEADER
  gcv_object_t symbolmacro_expansion _attribute_aligned_object_;
} *  Symbolmacro;
#define symbolmacro_length  ((sizeof(*(Symbolmacro)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* Global-Symbol-macros */
typedef struct {
  XRECORD_HEADER
  gcv_object_t globalsymbolmacro_definition _attribute_aligned_object_;
} *  GlobalSymbolmacro;
#define globalsymbolmacro_length  ((sizeof(*(GlobalSymbolmacro)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* Macros */
typedef struct {
  XRECORD_HEADER
  gcv_object_t macro_expander _attribute_aligned_object_;
  gcv_object_t macro_lambda_list _attribute_aligned_object_;
} *  Macro;
#define macro_length  ((sizeof(*(Macro)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* FunctionMacros */
typedef struct {
  XRECORD_HEADER
  gcv_object_t functionmacro_macro_expander _attribute_aligned_object_;
  gcv_object_t functionmacro_function       _attribute_aligned_object_;
} *  FunctionMacro;
#define functionmacro_length  ((sizeof(*(FunctionMacro)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* BigReadLabel */
typedef struct {
  XRECORD_HEADER
  gcv_object_t brl_value _attribute_aligned_object_;
} *  BigReadLabel;
#define bigreadlabel_length  ((sizeof(*(BigReadLabel)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* Encoding */
typedef struct {
  XRECORD_HEADER
  gcv_object_t enc_eol         _attribute_aligned_object_; /* line termination, a keyword (:UNIX, :MAC, :DOS) */
  gcv_object_t enc_towcs_error _attribute_aligned_object_; /* input error action, :ERROR or :IGNORE or a character */
  gcv_object_t enc_tombs_error _attribute_aligned_object_; /* output error action, :ERROR or :IGNORE or a character or an uint8 */
  #ifdef ENABLE_UNICODE
  gcv_object_t enc_charset     _attribute_aligned_object_; /* character set, a symbol in the CHARSET package or a simple-string */
  /* Functions to convert bytes to characters. */
    gcv_object_t enc_mblen     _attribute_aligned_object_; /* uintL (*) (object encoding, const uintB* src, const uintB* srcend); */
    gcv_object_t enc_mbstowcs  _attribute_aligned_object_; /* void (*) (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend); */
  /* Functions to convert characters to bytes. */
    gcv_object_t enc_wcslen    _attribute_aligned_object_; /* uintL (*) (object encoding, const chart* src, const chart* srcend); */
    gcv_object_t enc_wcstombs  _attribute_aligned_object_; /* void (*) (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend); */
  /* Function to return the set of defined characters in the range [start,end],
   as a simple-string of intervals #(start1 end1 ... startm endm). */
    gcv_object_t enc_range     _attribute_aligned_object_; /* object (*) (object encoding, uintL start, uintL end, uintL maxintervals); */
  /* An auxiliary pointer. */
  gcv_object_t enc_table       _attribute_aligned_object_;
  /* Minimum number of bytes needed to represent a character
   caveat: correct only for some encodings, defaults to 1 */
  uintL min_bytes_per_char;
  /* Maximum number of bytes needed to represent a character
   caveat: correct only for some encodings, defaults to 8 */
  uintL max_bytes_per_char;
  #endif
} *  Encoding;
#ifdef ENABLE_UNICODE
  #define encoding_length  10
#else
  #define encoding_length  3
#endif
#define encoding_xlength  (sizeof(*(Encoding)0)-offsetofa(record_,recdata)-encoding_length*sizeof(gcv_object_t))
#ifdef ENABLE_UNICODE
  #define Encoding_mblen(encoding)  ((uintL (*) (object, const uintB*, const uintB*)) ThePseudofun(TheEncoding(encoding)->enc_mblen))
  #define Encoding_mbstowcs(encoding)  ((void (*) (object, object, const uintB**, const uintB*, chart**, chart*)) ThePseudofun(TheEncoding(encoding)->enc_mbstowcs))
  #define Encoding_wcslen(encoding)  ((uintL (*) (object, const chart*, const chart*)) ThePseudofun(TheEncoding(encoding)->enc_wcslen))
  #define Encoding_wcstombs(encoding)  ((void (*) (object, object, const chart**, const chart*, uintB**, uintB*)) ThePseudofun(TheEncoding(encoding)->enc_wcstombs))
  #define Encoding_range(encoding)  ((object (*) (object, uintL, uintL, uintL)) ThePseudofun(TheEncoding(encoding)->enc_range))
#endif
#ifdef ENABLE_UNICODE
  #define cslen(encoding,src,srclen)  \
    Encoding_wcslen(encoding)(encoding,src,(src)+(srclen))
  #define cstombs_help_(encoding,src,srclen,dest,destlen,asserter)  \
    do { var const chart* _srcptr = (src);                          \
      var const chart* _srcendptr = _srcptr+(srclen);               \
      var uintB* _destptr = (dest);                                 \
      var uintB* _destendptr = _destptr+(destlen);                  \
      Encoding_wcstombs(encoding)(encoding,nullobj,&_srcptr,_srcendptr,&_destptr,_destendptr); \
      asserter((_srcptr == _srcendptr) && (_destptr == _destendptr)); \
    } while(0)
#else
  #define cslen(encoding,src,srclen)  (srclen)
  #define cstombs_help_(encoding,src,srclen,dest,destlen,asserter)  \
    do { asserter((srclen) == (destlen));                           \
         begin_system_call(); memcpy(dest,src,srclen); end_system_call(); \
    } while(0)
#endif
#define cstombs(encoding,src,srclen,dest,destlen)  cstombs_help_(encoding,src,srclen,dest,destlen,ASSERT)
%% sprintf(buf,"struct { XRECORD_HEADER gcv_object_t enc_eol%s; gcv_object_t enc_towcs_error%s; gcv_object_t enc_tombs_error%s;",attribute_aligned_object,attribute_aligned_object,attribute_aligned_object);
%% #ifdef ENABLE_UNICODE
%%   sprintf(buf+strlen(buf)," gcv_object_t enc_charset%s; gcv_object_t enc_mblen%s; gcv_object_t enc_mbstowcs%s; gcv_object_t enc_wcslen%s; gcv_object_t enc_wcstombs%s; gcv_object_t enc_range%s; gcv_object_t enc_table%s; uintL min_bytes_per_char; uintL max_bytes_per_char;",attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object);
%% #endif
%% strcat(buf," } *");
%% emit_typedef(buf,"Encoding");
%% #ifdef ENABLE_UNICODE
%%  export_def(Encoding_mblen(encoding));
%%  export_def(Encoding_wcslen(encoding));
%%  export_def(Encoding_wcstombs(encoding));
%% #endif
%% export_def(cslen(encoding,src,srclen));
%% export_def(cstombs_help_(encoding,src,srclen,dest,destlen,asserter));
%% puts("#define cstombs(encoding,src,srclen,dest,destlen)  cstombs_help_(encoding,src,srclen,dest,destlen,ASSERT)");

#ifdef FOREIGN
/* foreign pointer wrap */
typedef struct {
  XRECORD_HEADER
  void* fp_pointer;
} *  Fpointer;
#define fpointer_length  0
#define fpointer_xlength  (sizeof(*(Fpointer)0)-offsetofa(record_,recdata)-fpointer_length*sizeof(gcv_object_t))
#define mark_fp_invalid(ptr)  record_flags_set(ptr,bit(7))
#define mark_fp_valid(ptr)  record_flags_clr(ptr,bit(7))
#define fp_validp(ptr)  ((record_flags(ptr) & bit(7)) == 0)
#else
#define mark_fp_invalid(ptr)
#endif
%% #ifdef FOREIGN
%%   emit_typedef("struct { XRECORD_HEADER void* fp_pointer;} *","Fpointer");
%%   export_def(fp_validp(ptr));
%%   export_def(mark_fp_invalid(ptr));
%%   export_def(mark_fp_valid(ptr));
%% #endif

#ifdef DYNAMIC_FFI

/* foreign adresses */
typedef struct {
  XRECORD_HEADER
  gcv_object_t fa_base _attribute_aligned_object_;
  sintP fa_offset;
} * Faddress;
#define faddress_length  1
#define faddress_xlength  (sizeof(*(Faddress)0)-offsetofa(record_,recdata)-faddress_length*sizeof(gcv_object_t))

/* foreign variables */
typedef struct {
  XRECORD_HEADER
  gcv_object_t fv_name    _attribute_aligned_object_;
  gcv_object_t fv_version _attribute_aligned_object_;
  gcv_object_t fv_address _attribute_aligned_object_;
  gcv_object_t fv_size    _attribute_aligned_object_;
  gcv_object_t fv_type    _attribute_aligned_object_;
} * Fvariable;
#define fvariable_length  ((sizeof(*(Fvariable)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* foreign functions */
typedef struct {
  XRECORD_HEADER
  gcv_object_t ff_name       _attribute_aligned_object_;
  gcv_object_t ff_version    _attribute_aligned_object_;
  gcv_object_t ff_address    _attribute_aligned_object_;
  gcv_object_t ff_resulttype _attribute_aligned_object_;
  gcv_object_t ff_argtypes   _attribute_aligned_object_;
  gcv_object_t ff_flags      _attribute_aligned_object_;
  gcv_object_t ff_properties _attribute_aligned_object_;
} * Ffunction;
#define ffunction_length  ((sizeof(*(Ffunction)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

#endif

/* weak pointer */
typedef struct {
  XRECORD_HEADER
  gcv_object_t wp_cdr   _attribute_aligned_object_; /* active weak-pointers form a chained list */
  gcv_object_t wp_value _attribute_aligned_object_; /* the referenced object */
} * Weakpointer;
/* Both wp_cdr and wp_value are invisible to gc_mark routines.
 When the weak-pointer becomes inactive, both fields are turned to unbound.
 When wp_value is GC-invariant, WP does not have to be on the
 O(all_weakpointers) list!  WP is on the list <==> ( wp_cdr != unbound ) */
#define weakpointer_length  ((sizeof(*(Weakpointer)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))
#define weakpointer_broken_p(wp) (!boundp(TheWeakpointer(wp)->wp_value))

/* weak list */
typedef struct {
  LRECORD_HEADER
  gcv_object_t wp_cdr                   _attribute_aligned_object_; /* active weak-pointers form a chained list */
  gcv_object_t wl_count                 _attribute_aligned_object_; /* remaining objects */
  gcv_object_t wl_elements[unspecified] _attribute_aligned_object_; /* the referenced objects */
} * WeakList;

/* mutable weak list */
typedef struct {
  XRECORD_HEADER
  gcv_object_t mwl_list _attribute_aligned_object_;
} * MutableWeakList;
#define mutableweaklist_length  ((sizeof(*(MutableWeakList)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* weak "and" relation */
typedef struct {
  LRECORD_HEADER
  gcv_object_t wp_cdr                _attribute_aligned_object_; /* active weak-pointers form a chained list */
  gcv_object_t war_keys_list         _attribute_aligned_object_; /* list to copy the keys into */
  gcv_object_t war_keys[unspecified] _attribute_aligned_object_; /* the referenced objects */
} * WeakAnd;

/* weak "or" relation */
typedef struct {
  LRECORD_HEADER
  gcv_object_t wp_cdr                _attribute_aligned_object_; /* active weak-pointers form a chained list */
  gcv_object_t wor_keys_list         _attribute_aligned_object_; /* list to copy the keys into */
  gcv_object_t wor_keys[unspecified] _attribute_aligned_object_; /* the referenced objects */
} * WeakOr;

/* weak mapping */
typedef struct {
  XRECORD_HEADER
  gcv_object_t wp_cdr   _attribute_aligned_object_; /* active weak-pointers form a chained list */
  gcv_object_t wm_value _attribute_aligned_object_; /* the dependent referenced object */
  gcv_object_t wm_key   _attribute_aligned_object_; /* the weak referenced object */
} * Weakmapping;
#define weakmapping_length  ((sizeof(*(Weakmapping)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* weak "and" mapping */
typedef struct {
  LRECORD_HEADER
  gcv_object_t wp_cdr                _attribute_aligned_object_; /* active weak-pointers form a chained list */
  gcv_object_t wam_value             _attribute_aligned_object_; /* the dependent referenced object */
  gcv_object_t wam_keys_list         _attribute_aligned_object_; /* list to copy the keys into */
  gcv_object_t wam_keys[unspecified] _attribute_aligned_object_; /* the referenced objects */
} * WeakAndMapping;

/* weak "or" mapping */
typedef struct {
  LRECORD_HEADER
  gcv_object_t wp_cdr                _attribute_aligned_object_; /* active weak-pointers form a chained list */
  gcv_object_t wom_value             _attribute_aligned_object_; /* the dependent referenced object */
  gcv_object_t wom_keys_list         _attribute_aligned_object_; /* list to copy the keys into */
  gcv_object_t wom_keys[unspecified] _attribute_aligned_object_; /* the referenced objects */
} * WeakOrMapping;

/* weak alist (rectype = Rectype_WeakAlist_{Key,Value,Either,Both}) */
typedef struct {
  LRECORD_HEADER
  gcv_object_t wp_cdr                _attribute_aligned_object_; /* active weak-pointers form a chained list */
  gcv_object_t wal_count             _attribute_aligned_object_; /* remaining pairs */
  gcv_object_t wal_data[unspecified] _attribute_aligned_object_; /* key, value alternating */
} * WeakAlist;

/* mutable weak alist */
typedef struct {
  XRECORD_HEADER
  gcv_object_t mwal_list _attribute_aligned_object_;
} * MutableWeakAlist;
#define mutableweakalist_length  ((sizeof(*(MutableWeakAlist)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* weak hashed alist (rectype = Rectype_WeakHashedAlist_{Key,Value,Either,Both}) */
typedef struct {
  LRECORD_HEADER
  gcv_object_t wp_cdr                 _attribute_aligned_object_; /* active weak-pointers form a chained list */
  gcv_object_t whal_itable            _attribute_aligned_object_; /* index-vector */
  gcv_object_t whal_count             _attribute_aligned_object_; /* remaining pairs */
  gcv_object_t whal_freelist          _attribute_aligned_object_; /* start index of freelist */
  gcv_object_t whal_data[unspecified] _attribute_aligned_object_; /* (key, value, next) triples */
} * WeakHashedAlist;

/* Finalizer */
typedef struct {
  XRECORD_HEADER
  gcv_object_t fin_alive    _attribute_aligned_object_; /* only if this object is alive */
  gcv_object_t fin_trigger  _attribute_aligned_object_; /* wait for the death of this object */
  gcv_object_t fin_function _attribute_aligned_object_; /* then this function is called */
  gcv_object_t fin_cdr      _attribute_aligned_object_;
} * Finalizer;
#define finalizer_length  ((sizeof(*(Finalizer)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

#ifdef SOCKET_STREAMS
/* Socket-Server */
typedef struct {
  XRECORD_HEADER
  gcv_object_t socket_handle _attribute_aligned_object_; /* socket handle */
  gcv_object_t host          _attribute_aligned_object_; /* host string */
  gcv_object_t port          _attribute_aligned_object_; /* port number */
} * Socket_server;
#define socket_server_length  ((sizeof(*(Socket_server)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

/* Information about any of the two ends of a socket connection. */
#ifndef MAXHOSTNAMELEN
  #define MAXHOSTNAMELEN 64
#endif
typedef struct host_data_t {
  char hostname[45+1];   /* IP address in presentable, printable format
                        (IPv4 max. 15 characters, IPv6 max. 45 characters) */
  char truename[MAXHOSTNAMELEN+1]; /* hostname, with or without domain name */
  unsigned int port;
} host_data_t;
#endif

#ifdef YET_ANOTHER_RECORD

/* Yet another record */
typedef struct {
  XRECORD_HEADER
  gcv_object_t yetanother_x _attribute_aligned_object_;
  gcv_object_t yetanother_y _attribute_aligned_object_;
  gcv_object_t yetanother_z _attribute_aligned_object_;
} * Yetanother;
#define yetanother_length  ((sizeof(*(Yetanother)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))

#endif

/* Streams with metaclass BUILT-IN-CLASS */
typedef struct {
  XRECORD_HEADER
  /* Because of space requirements, I have to put strmflags and strmtype
   into a fixnum in recdata[0]. */
  union {
    /* Use a union, so that we have fast access to strmflags and strmtype.
       With DEBUG_GCSAFETY and C++ prior to -std=gnu++11, we cannot put a
       gcv_object_t in a union. */
    #if defined(OBJECT_STRUCT) || defined(WIDE_STRUCT)
    struct { INNARDS_OF_GCV_OBJECT } recdata0_o _attribute_aligned_object_;
    #else
    gcv_object_t                     recdata0_o _attribute_aligned_object_;
    #endif
    struct {
      #if !((oint_addr_len+oint_addr_shift>=24) && (8>=oint_addr_shift))
        #error No room for stream flags -- re-accommodate Stream-Flags!!
      #endif
      #if defined(WIDE) && BIG_ENDIAN_P
        uintL r0_filler0;
      #endif
      uintB r0_filler1;
      uintB r0_flags; /* Flags */
      uintB r0_type;  /* Subtype */
      uintB r0_filler2;
      #if defined(WIDE) && !BIG_ENDIAN_P
        uintL r0_filler0;
      #endif
    } recdata0_decomposed;
  } strm_recdata0;
  #define strmflags strm_recdata0.recdata0_decomposed.r0_flags
  #define strmtype  strm_recdata0.recdata0_decomposed.r0_type
  gcv_object_t strm_rd_by            _attribute_aligned_object_;
  gcv_object_t strm_rd_by_array      _attribute_aligned_object_;
  gcv_object_t strm_wr_by            _attribute_aligned_object_;
  gcv_object_t strm_wr_by_array      _attribute_aligned_object_;
  gcv_object_t strm_rd_ch            _attribute_aligned_object_;
  gcv_object_t strm_pk_ch            _attribute_aligned_object_;
  gcv_object_t strm_rd_ch_array      _attribute_aligned_object_;
  gcv_object_t strm_rd_ch_last       _attribute_aligned_object_;
  gcv_object_t strm_wr_ch            _attribute_aligned_object_;
  gcv_object_t strm_wr_ch_array      _attribute_aligned_object_;
  gcv_object_t strm_wr_ch_npnl       _attribute_aligned_object_;
  gcv_object_t strm_wr_ch_array_npnl _attribute_aligned_object_;
  gcv_object_t strm_wr_ch_lpos       _attribute_aligned_object_;
  gcv_object_t strm_other[unspecified] _attribute_aligned_object_; /* type-specific components */
} *  Stream;
/* The macro TheStream actually means TheBuiltinStream. */
#define strm_len  ((sizeof(*(Stream)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t)-unspecified)
#define stream_length(ptr)  xrecord_length(ptr)
#define stream_xlength(ptr)  xrecord_xlength(ptr)
#define Stream_length(obj)  stream_length(TheStream(obj))
#define Stream_xlength(obj)  stream_xlength(TheStream(obj))
/* Bit-masks in the Flags: */
  #define strmflags_open_bit_B   0  /* set, if the Stream is open */
  #define strmflags_immut_bit_B  1  /* set if read literals are immutable */
  #define strmflags_fasl_bit_B    2  /* Read-Eval is permitted; \r=#\Return */
  #define strmflags_rd_by_bit_B  4  /* set, if READ-BYTE is possible */
  #define strmflags_wr_by_bit_B  5  /* set, if WRITE-BYTE is possible */
  #define strmflags_rd_ch_bit_B  6  /* set, if READ-CHAR is possible */
  #define strmflags_wr_ch_bit_B  7  /* set, if WRITE-CHAR is possible */
  #define strmflags_open_B   bit(strmflags_open_bit_B)
  #define strmflags_rd_by_B  bit(strmflags_rd_by_bit_B)
  #define strmflags_wr_by_B  bit(strmflags_wr_by_bit_B)
  #define strmflags_rd_ch_B  bit(strmflags_rd_ch_bit_B)
  #define strmflags_wr_ch_B  bit(strmflags_wr_ch_bit_B)
  #define strmflags_rd_B  (strmflags_rd_by_B | strmflags_rd_ch_B)
  #define strmflags_wr_B  (strmflags_wr_by_B | strmflags_wr_ch_B)
/* approach Typinfo: */
  enum { /* The ordered values of this enumeration are 0,1,2,... */
  /* First the OS independent streams. */
                              enum_strmtype_synonym,
  #define strmtype_synonym    (uintB)enum_strmtype_synonym
                              enum_strmtype_broad,
  #define strmtype_broad      (uintB)enum_strmtype_broad
                              enum_strmtype_concat,
  #define strmtype_concat     (uintB)enum_strmtype_concat
                              enum_strmtype_twoway,
  #define strmtype_twoway     (uintB)enum_strmtype_twoway
                              enum_strmtype_echo,
  #define strmtype_echo       (uintB)enum_strmtype_echo
                              enum_strmtype_str_in,
  #define strmtype_str_in     (uintB)enum_strmtype_str_in
                              enum_strmtype_str_out,
  #define strmtype_str_out    (uintB)enum_strmtype_str_out
                              enum_strmtype_str_push,
  #define strmtype_str_push   (uintB)enum_strmtype_str_push
                              enum_strmtype_pphelp,
  #define strmtype_pphelp     (uintB)enum_strmtype_pphelp
                              enum_strmtype_buff_in,
  #define strmtype_buff_in    (uintB)enum_strmtype_buff_in
                              enum_strmtype_buff_out,
  #define strmtype_buff_out   (uintB)enum_strmtype_buff_out
  #ifdef GENERIC_STREAMS
                              enum_strmtype_generic,
  #define strmtype_generic    (uintB)enum_strmtype_generic
  #endif
  /* Then the OS dependent streams. */
                              enum_strmtype_file,
  #define strmtype_file       (uintB)enum_strmtype_file
  #ifdef KEYBOARD
                              enum_strmtype_keyboard,
  #define strmtype_keyboard   (uintB)enum_strmtype_keyboard
  #endif
                              enum_strmtype_terminal,
  #define strmtype_terminal   (uintB)enum_strmtype_terminal
  #ifdef SCREEN
                              enum_strmtype_window,
  #define strmtype_window     (uintB)enum_strmtype_window
  #endif
  #ifdef PRINTER
                              enum_strmtype_printer,
  #define strmtype_printer    (uintB)enum_strmtype_printer
  #endif
  #ifdef PIPES
                              enum_strmtype_pipe_in,
  #define strmtype_pipe_in    (uintB)enum_strmtype_pipe_in
                              enum_strmtype_pipe_out,
  #define strmtype_pipe_out   (uintB)enum_strmtype_pipe_out
  #endif
  #ifdef X11SOCKETS
                              enum_strmtype_x11socket,
  #define strmtype_x11socket  (uintB)enum_strmtype_x11socket
  #endif
  #ifdef SOCKET_STREAMS
                              enum_strmtype_socket,
  #define strmtype_socket     (uintB)enum_strmtype_socket
                              enum_strmtype_twoway_socket,
  #define strmtype_twoway_socket (uintB)enum_strmtype_twoway_socket
  #endif
                              enum_strmtype_dummy
  };
  /* When this table is changed, also adapt
   - the 12 jumptables for STREAM-ELEMENT-TYPE, SET-STREAM-ELEMENT-TYPE,
     STREAM-EXTERNAL-FORMAT, SET-STREAM-EXTERNAL-FORMAT, INTERACTIVE-STREAM-P,
     CLOSE, LISTEN-CHAR, CLEAR_INPUT, LISTEN-BYTE, FINISH_OUTPUT,
     FORCE_OUTPUT, CLEAR_OUTPUT in STREAM.D and
   - the name-table in CONSTOBJ.D and
   - the jumptable for PR_STREAM in IO.D and
   - the pseudo-function-table in PSEUDOFUN.D */

/* more type-specific components: */
  #define strm_eltype          strm_other[0] /* CHARACTER or ([UN]SIGNED-BYTE n) */
  #define strm_encoding        strm_other[1] /* an encoding */
  #define strm_file_name       strm_other[6] /* filename, a pathname or NIL */
  #define strm_file_truename   strm_other[7] /* truename, a non-logical pathname or NIL */
  #define strm_buffered_channel  strm_other[5] /* packed Handle */
  #define strm_synonym_symbol  strm_other[0]
  #define strm_broad_list      strm_other[0] /* list of Streams */
  #define strm_concat_list     strm_other[0] /* list of Streams */
  #define strm_pphelp_lpos     strm_wr_ch_lpos /* Line Position (Fixnum>=0) */
  #define strm_pphelp_strings  strm_other[0]   /* Semi-Simple-Strings for Output */
  #define strm_pphelp_modus    strm_other[1]   /* Mode (NIL=Single line, T=multiple lines) */
  #define strm_pphelp_miserp   strm_other[2] /* miser mode indicator */
  #define strm_pphelp_offset   strm_other[3] /* initial line offset (indent) */
  #define strm_buff_in_fun     strm_other[0] /* read function */
  #define strm_buff_out_fun    strm_other[0] /* output function */
  #define strm_twoway_input    strm_other[0] /* stream for input */
  #define strm_twoway_output   strm_other[1] /* stream for output */
  #ifdef PIPES
  #define strm_pipe_pid        strm_other[6] /* process-Id, a Fixnum >=0 */
  #endif
  #ifdef X11SOCKETS
  #define strm_x11socket_connect  strm_other[6] /* List (host display) */
  #endif
  #ifdef SOCKET_STREAMS
  #define strm_socket_port     strm_other[6] /* port, a fixnum >=0 */
  #define strm_socket_host     strm_other[7] /* host, NIL or a string */
  #define strm_twoway_socket_input  strm_other[0] /* input side, a socket stream */
  #endif
  #ifdef GENERIC_STREAMS
  #define strm_controller_object strm_other[0] /* Controller (usually a CLOS-instance) */
  #endif
  #define strm_buffered_bufflen 4096   /* buffer length, a power of 2, <2^16 */
/* is used by stream.d, pathname.d, io.d */
%% export_def(strm_buffered_bufflen);

/* Structures */
typedef Srecord  Structure;
  #define structure_types   recdata[0]
#define structure_length(ptr)  srecord_length(ptr)
#define Structure_length(obj)  structure_length(TheStructure(obj))
%% emit_typedef("Srecord","Structure");
%% export_def(structure_types);

/* CLOS class-versions, see clos.lisp */
typedef struct {
  VRECORD_HEADER
  gcv_object_t cv_newest_class             _attribute_aligned_object_; /* the CLASS object describing the newest available version */
  gcv_object_t cv_class                    _attribute_aligned_object_; /* the CLASS object describing the slots */
  gcv_object_t cv_shared_slots             _attribute_aligned_object_; /* simple-vector with the values of all shared slots, or nil */
  gcv_object_t cv_serial                   _attribute_aligned_object_; /* serial number of this class version */
  gcv_object_t cv_next                     _attribute_aligned_object_; /* next class-version, or nil */
  gcv_object_t cv_slotlists_valid_p        _attribute_aligned_object_; /* true if the following fields are already computed */
  gcv_object_t cv_kept_slot_locations      _attribute_aligned_object_; /* plist of old and new slot locations of those slots that remain local or were shared and become local */
  gcv_object_t cv_added_slots              _attribute_aligned_object_; /* list of local slots that are added in the next version */
  gcv_object_t cv_discarded_slots          _attribute_aligned_object_; /* list of local slots that are removed or become shared in the next version */
  gcv_object_t cv_discarded_slot_locations _attribute_aligned_object_; /* plist of local slots and their old slot locations that are removed or become shared in the next version */
} *  ClassVersion;
#define classversion_length  ((sizeof(*(ClassVersion)0)-offsetofa(svector_,data))/sizeof(gcv_object_t))

/* CLOS-instances */
typedef struct {
  SRECORD_HEADER
  gcv_object_t inst_class_version _attribute_aligned_object_; /* indirect pointer to a CLOS-class */
  gcv_object_t other[unspecified] _attribute_aligned_object_;
} *  Instance;
/* Bit masks in the flags: */
  #define instflags_forwarded_B    bit(0)
  #define instflags_beingupdated_B bit(3)
  /* The following are only used during garbage collection. */
  #define instflags_backpointer_B  bit(1)
  #define instflags_relocated_B    bit(2)
  #define mark_inst_clean(ptr)  \
    record_flags_clr(ptr,instflags_backpointer_B|instflags_relocated_B)
%% sprintf(buf,"struct { SRECORD_HEADER gcv_object_t inst_class_version%s; gcv_object_t other[unspecified]%s; } *",attribute_aligned_object,attribute_aligned_object);
%% emit_typedef(buf,"Instance");

/* Structures that inherit from <structure-stablehash> */
typedef struct {
  SRECORD_HEADER
  gcv_object_t _structure_types   _attribute_aligned_object_;
  gcv_object_t stablehashcode     _attribute_aligned_object_;
  gcv_object_t other[unspecified] _attribute_aligned_object_;
} *  StablehashStructure;

/* CLOS instances that inherit from <standard-stablehash> */
typedef struct {
  SRECORD_HEADER
  gcv_object_t inst_class_version _attribute_aligned_object_; /* indirect pointer to a CLOS-class */
  gcv_object_t stablehashcode     _attribute_aligned_object_;
  gcv_object_t other[unspecified] _attribute_aligned_object_;
} *  StablehashInstance;

/* Slot definitions (= instances of <slot-definition>, see clos-slotdef1.lisp */
typedef struct {
  SRECORD_HEADER
  gcv_object_t inst_class_version         _attribute_aligned_object_;
  gcv_object_t slotdef_name               _attribute_aligned_object_;
  gcv_object_t slotdef_initargs           _attribute_aligned_object_;
  gcv_object_t slotdef_type               _attribute_aligned_object_;
  gcv_object_t slotdef_allocation         _attribute_aligned_object_;
  gcv_object_t slotdef_inheritable_initer _attribute_aligned_object_;
  gcv_object_t slotdef_inheritable_doc    _attribute_aligned_object_;
  /* from here on only for subclasses of <effective-slot-definition> */
  gcv_object_t slotdef_location           _attribute_aligned_object_;
  gcv_object_t slotdef_efm_svuc           _attribute_aligned_object_;
  gcv_object_t slotdef_efm_ssvuc          _attribute_aligned_object_;
  gcv_object_t slotdef_efm_sbuc           _attribute_aligned_object_;
  gcv_object_t slotdef_efm_smuc           _attribute_aligned_object_;
} *  SlotDefinition;

/* CLOS-Classes (= instances of <class>), see clos-class1.lisp */
typedef struct {
  SRECORD_HEADER
  gcv_object_t inst_class_version       _attribute_aligned_object_; /* indirect pointer to a CLOS-class */
  gcv_object_t hashcode                 _attribute_aligned_object_; /* GC invariant hash code */
  gcv_object_t direct_methods           _attribute_aligned_object_; /* set of methods that use this specializer */
  gcv_object_t classname                _attribute_aligned_object_; /* a symbol */
  gcv_object_t direct_subclasses        _attribute_aligned_object_; /* weak-list or weak-hash-table of all direct subclasses */
  /* from here on only for subclasses of <defined-class> */
  gcv_object_t direct_superclasses      _attribute_aligned_object_; /* direct superclasses */
  gcv_object_t all_superclasses         _attribute_aligned_object_; /* all superclasses, including itself */
  gcv_object_t precedence_list          _attribute_aligned_object_; /* ordered list of all superclasses */
  gcv_object_t direct_slots             _attribute_aligned_object_;
  gcv_object_t slots                    _attribute_aligned_object_;
  gcv_object_t slot_location_table      _attribute_aligned_object_; /* hashtable slotname -> where the slot is located */
  gcv_object_t direct_default_initargs  _attribute_aligned_object_;
  gcv_object_t default_initargs         _attribute_aligned_object_;
  gcv_object_t documentation            _attribute_aligned_object_; /* string or NIL */
  gcv_object_t listeners                _attribute_aligned_object_; /* list of objects to be notified upon a change */
  gcv_object_t initialized              _attribute_aligned_object_; /* describes which parts of the class are initialized */
  /* from here on only for subclasses of <standard-class> or <funcallable-standard-class> or <structure-class> */
  gcv_object_t subclass_of_stablehash_p _attribute_aligned_object_; /* true if <standard-stablehash> or <structure-stablehash> is among the superclasses */
  gcv_object_t generic_accessors        _attribute_aligned_object_;
  gcv_object_t direct_accessors         _attribute_aligned_object_;
  gcv_object_t valid_initargs_from_slots _attribute_aligned_object_;
  gcv_object_t instance_size            _attribute_aligned_object_;
  /* from here on only for subclasses of <standard-class> or <funcallable-standard-class> */
  gcv_object_t current_version          _attribute_aligned_object_; /* most recent class-version, points back to this class */
  gcv_object_t funcallablep             _attribute_aligned_object_;
  gcv_object_t fixed_slot_locations     _attribute_aligned_object_;
  gcv_object_t instantiated             _attribute_aligned_object_;
  gcv_object_t direct_instance_specializers _attribute_aligned_object_;
  gcv_object_t finalized_direct_subclasses _attribute_aligned_object_; /* weak-list or weak-hash-table of all finalized direct subclasses */
  gcv_object_t prototype                _attribute_aligned_object_; /* class prototype - an instance or NIL */
  /* from here on only for subclasses of <standard-class> */
  gcv_object_t other[unspecified]       _attribute_aligned_object_;
} *  Class;

/* Length of a <defined-class>. */
#define defined_class_length ((((aint)&((Class)0)->initialized-offsetofa(record_,recdata))/sizeof(gcv_object_t))+1)
/* Length of a <built-in-class>. */
#define built_in_class_length  (defined_class_length+1) /* = clos::*<built-in-class>-instance-size* */

/* Closures */
typedef struct {
  SRECORD_HEADER
  gcv_object_t clos_name_or_class_version _attribute_aligned_object_;
  gcv_object_t clos_codevec               _attribute_aligned_object_;
  gcv_object_t other[unspecified]         _attribute_aligned_object_;
} *  Closure;
/* interpreted Closure: */
typedef struct {
  SRECORD_HEADER
  gcv_object_t clos_name       _attribute_aligned_object_;
  gcv_object_t clos_form       _attribute_aligned_object_;
  gcv_object_t clos_docstring  _attribute_aligned_object_;
  gcv_object_t clos_body       _attribute_aligned_object_;
  gcv_object_t clos_var_env    _attribute_aligned_object_;
  gcv_object_t clos_fun_env    _attribute_aligned_object_;
  gcv_object_t clos_block_env  _attribute_aligned_object_;
  gcv_object_t clos_go_env     _attribute_aligned_object_;
  gcv_object_t clos_decl_env   _attribute_aligned_object_;
  gcv_object_t clos_vars       _attribute_aligned_object_;
  gcv_object_t clos_varflags   _attribute_aligned_object_;
  gcv_object_t clos_spec_count _attribute_aligned_object_;
  gcv_object_t clos_req_count  _attribute_aligned_object_;
  gcv_object_t clos_opt_count  _attribute_aligned_object_;
  gcv_object_t clos_opt_inits  _attribute_aligned_object_;
  gcv_object_t clos_key_count  _attribute_aligned_object_;
  gcv_object_t clos_keywords   _attribute_aligned_object_;
  gcv_object_t clos_key_inits  _attribute_aligned_object_;
  gcv_object_t clos_allow_flag _attribute_aligned_object_;
  gcv_object_t clos_rest_flag  _attribute_aligned_object_;
  gcv_object_t clos_aux_count  _attribute_aligned_object_;
  gcv_object_t clos_aux_inits  _attribute_aligned_object_;
} *  Iclosure;
#define iclos_length  ((sizeof(*(Iclosure)0)-offsetofa(record_,recdata))/sizeof(gcv_object_t))
/* compiled Closure: */
typedef struct {
  SRECORD_HEADER
  gcv_object_t clos_name_or_class_version _attribute_aligned_object_;
  gcv_object_t clos_codevec               _attribute_aligned_object_;
  gcv_object_t clos_consts[unspecified]   _attribute_aligned_object_; /* Closure-constants */
} cclosure_;
typedef cclosure_ *  Cclosure;
#define cclosure_length(ptr)  srecord_length(ptr)
#define Cclosure_length(obj)  cclosure_length(TheCclosure(obj))
/* Flags in a closure. They must be disjoint from the instflags_* bits. */
#ifdef TYPECODES
  #define closure_flags(ptr)  ((ptr)->recflags)
#else
  #define closure_flags(ptr)  record_flags(ptr)
#endif
#define Closure_flags(obj)  closure_flags(TheClosure(obj))
#define Cclosure_seclass(obj)  ((Closure_flags(obj) >> 4) & 0x07)
#define Cclosure_set_seclass(obj,se)  \
  (record_flags_clr(TheCclosure(obj),0x07<<4), \
   record_flags_set(TheCclosure(obj),(se)<<4))
#define closflags_instance_B  bit(7)
#define closure_instancep(ptr)  (closure_flags(ptr) & closflags_instance_B)
#define Closure_instancep(obj)  closure_instancep(TheClosure(obj))
/* Closed-over environment, as a set of nested simple-vectors. */
#define clos_venv  clos_consts[0]
/* The function's name. Depends on whether instancep or not. */
#define Closure_name(obj)  \
  (Closure_instancep(obj)             \
   ? TheCclosure(obj)->clos_consts[1] \
   : TheClosure(obj)->clos_name_or_class_version)
typedef struct {
  VRECORD_HEADER               /* self-pointer for GC, length in bits */
  /* Here: Content of the Bitvector. */
  uintW  ccv_spdepth_1;          /* maximal SP-depth, 1-part */
  uintW  ccv_spdepth_jmpbufsize; /* maximal SP-depth, jmpbufsize-part */
  uintW  ccv_numreq;             /* number of required parameters */
  uintW  ccv_numopt;             /* number of optional parameters */
  uintB  ccv_flags; /* Flags: Bit 0: &REST - parameter given?
                              Bit 1: full lambda list at the end of const vec
                              Bit 2: docstring at the end of const vec
                              Bit 3: generic function with call-inhibition?
                              Bit 4: generic function?
                              Bit 5: JITC code at the end of const vec
                              Bit 6: &ALLOW-OTHER-KEYS-Flag
                              Bit 7: keyword-parameter given? */
  uintB  ccv_signature; /* abbreviated argument type, for faster FUNCALL */
  /* If keyword-parameters are given: */
  uintW  ccv_numkey;    /* Number of keyword-parameters */
  uintW  ccv_keyconsts; /* Offset in FUNC of the keywords */
} *  Codevec;
#define CCV_SPDEPTH_1           0
#define CCV_SPDEPTH_JMPBUFSIZE  2
#define CCV_NUMREQ              4
#define CCV_NUMOPT              6
#define CCV_FLAGS               8
#define CCV_SIGNATURE           9
#define CCV_NUMKEY             10
#define CCV_KEYCONSTS          12
#define CCV_START_NONKEY       10
#define CCV_START_KEY          14
/* Compiled closures, where Bit 4 has been set in the flags of clos_codevec
   are generic functions. */
%% export_def(closure_flags(ptr));
%% export_def(closure_instancep(ptr));
%% export_def(Closure_instancep(obj));

/* the position of the last const (or doc or lalist!) */
#define Cclosure_last_const(obj)  (Cclosure_length(obj) - 1 -           \
   (offsetofa(cclosure_,clos_consts)-offsetofa(srecord_,recdata))/sizeof(gcv_object_t))
#define ccv_flags_lambda_list_p(ccv_flags)    (((ccv_flags) & bit(1)) != 0)
#define ccv_flags_documentation_p(ccv_flags)  (((ccv_flags) & bit(2)) != 0)
#define ccv_flags_jitc_p(ccv_flags)           (((ccv_flags) & bit(5)) != 0)
#define cclosure_jitc(closure)  TheCclosure(closure)->clos_consts[Cclosure_last_const(closure)]
#define cclosure_jitc_p(closure)  ccv_flags_jitc_p(TheCodevec(TheCclosure(closure)->clos_codevec)->ccv_flags)

/* A compiled LISP-function gets its arguments on the STACK
 and returns its values in MULTIPLE_VALUE_SPACE.
 It does not return a value as a C-function. */
  /* Return of multiple values is completely done through
   MULTIPLE_VALUE_SPACE. As C-function: result-type Values. */
  #ifndef Values
    typedef void Values;
  #endif
  /* To pass a type of the value Values: return_Values(...); */
  #define return_Values  return_void
  /* A Lisp-function is a pointer to a C-function without returned value. */
  typedef Values (*lisp_function_t)();
/* If this is changed, every call of a C-function with the result type
 'Values' (especially 'funcall', 'apply', 'eval') is to be checked. */
%% puts("typedef void Values;"); /* emit_typedef useless: no sizeof(void) */
%% emit_typedef_f("Values (*%s)()","lisp_function_t");

/* FSUBRs
 As C-functions: of type fsubr_function_t (no arguments, no value): */
typedef Values fsubr_function_t (void);
/* The addesses of these C-functions are jumped to directly
 For SAVEMEM/LOADMEM there is a table containing all FSUBRs. */
typedef fsubr_function_t * fsubr_t;
/* Signature of FSUBRs in the Lisp-way:
         argtype          short for the argument type     fsubr_argtype_t
         req_count        number of required parameters   uintW
         opt_count        number of optional parameters   uintW
         body_flag        Body-Flag                       fsubr_body_t
 The component body_flag contains one uintW, but we mean: */
typedef enum {
  fsubr_nobody,
  fsubr_body
} fsubr_body_t;
/* The component argtype contains a Fixnum, but it's supposed to be: */
typedef enum {
  fsubr_argtype_1_0_nobody,
  fsubr_argtype_2_0_nobody,
  fsubr_argtype_1_1_nobody,
  fsubr_argtype_2_1_nobody,
  fsubr_argtype_0_body,
  fsubr_argtype_1_body,
  fsubr_argtype_2_body
} fsubr_argtype_t;
/* conversion: see SPVW:
 extern fsubr_argtype_t fsubr_argtype (uintW req_count, uintW opt_count, fsubr_body_t body_flag); */

/* SUBRs
 SUBR table entry: */
typedef struct {
  XRECORD_HEADER
  gcv_object_t name     _attribute_aligned_object_; /* name */
  gcv_object_t keywords _attribute_aligned_object_; /* NIL or vector with the keywords */
  lisp_function_t function;     /* function */
  uintW argtype;                /* short for the argument-type */
  uintW req_count;              /* number of required parameters */
  uintW opt_count;              /* number of optional parameters */
  uintB rest_flag;              /* flag for arbitrary number of arguments */
  uintB key_flag;               /* flag for keywords */
  uintW key_count;              /* number of keyword parameter */
  uintB seclass;                /* side-effect class */
  uintB fastcmp;                /* fast comparison method */
  /* If necessary, add fillers here to ensure sizeof(subr_t)
     is a multiple of varobject_alignment. */
} subr_t
#if defined(GENERIC64_HEAPCODES) && (alignment_long < 8) && defined(GNU)
/* Force all Subrs to be allocated with a 8-byte alignment. GC needs this. */
  __attribute__ ((aligned (8)))
#elif defined(HEAPCODES) && (alignment_long < 4) && defined(GNU)
/* Force all Subrs to be allocated with a 4-byte alignment. GC needs this. */
  __attribute__ ((aligned (4)))
#endif
;
typedef subr_t *  Subr;
/* Compile-time check: sizeof(subr_t) is a multiple of varobject_alignment. */
typedef int subr_size_check[1 - 2 * (int)(sizeof(subr_t) % varobject_alignment)];
/* GC needs information where objects are in here: */
#define subr_length  2
#define subr_xlength  (sizeof(*(Subr)0)-offsetofa(record_,recdata)-subr_length*sizeof(gcv_object_t))
/* the rest_flag component is a uintB, while we really mean: */
typedef enum {
  subr_norest,
  subr_rest
} subr_rest_t;
/* the key_flag component is a uintB, while we really mean: */
typedef enum {
  subr_nokey,
  subr_key,
  subr_key_allow
} subr_key_t;
/* the argtype component is a uintW, while we really mean: */
typedef enum {
  subr_argtype_0_0,
  subr_argtype_1_0,
  subr_argtype_2_0,
  subr_argtype_3_0,
  subr_argtype_4_0,
  subr_argtype_5_0,
  subr_argtype_6_0,
  subr_argtype_0_1,
  subr_argtype_1_1,
  subr_argtype_2_1,
  subr_argtype_3_1,
  subr_argtype_4_1,
  subr_argtype_0_2,
  subr_argtype_1_2,
  subr_argtype_2_2,
  subr_argtype_3_2,
  subr_argtype_0_3,
  subr_argtype_1_3,
  subr_argtype_2_3,
  subr_argtype_0_4,
  subr_argtype_0_5,
  subr_argtype_0_0_rest,
  subr_argtype_1_0_rest,
  subr_argtype_2_0_rest,
  subr_argtype_3_0_rest,
  subr_argtype_0_0_key,
  subr_argtype_1_0_key,
  subr_argtype_2_0_key,
  subr_argtype_3_0_key,
  subr_argtype_4_0_key,
  subr_argtype_0_1_key,
  subr_argtype_1_1_key,
  subr_argtype_1_2_key
} subr_argtype_t;
/* Conversion: see SPVW:
 extern subr_argtype_t subr_argtype (uintW req_count, uintW opt_count, subr_rest_t rest_flag, subr_key_t key_flag); */
%% sprintf(buf,"struct { XRECORD_HEADER gcv_object_t name%s; gcv_object_t keywords%s; lisp_function_t function; uintW argtype; uintW req_count; uintW opt_count; uintB rest_flag; uintB key_flag; uintW key_count; uintB seclass; uintB fastcmp; } %%s",attribute_aligned_object,attribute_aligned_object);
%% #if defined(HEAPCODES) && (alignment_long < 4) && defined(GNU)
%%   strcat(buf," __attribute__ ((aligned (4)))");
%% #endif
%% emit_typedef_f(buf,"subr_t");
%% emit_typedef("subr_t *","Subr");
%% emit_typedef("enum { subr_norest, subr_rest }","subr_rest_t");
%% emit_typedef("enum { subr_nokey, subr_key, subr_key_allow }","subr_key_t");

/* side-effect class is really seclass_t: */
typedef enum {
  seclass_foldable, /* the function allows Constant-Folding:
     two calls with identical arguments give the same result,
     and calls with constant arguments can be evaluated at compile time.
     In particular, no side effects, do not depend on global variables or such,
     do not even look "inside" their arguments */
  seclass_no_se, /* no side effects, do not depend on global variables or such,
     do not even look "inside" their arguments, but not "foldable". */
  seclass_read, /* no side effects, but depend on global variables
     or look "inside" their arguments. */
  seclass_rd_sig, /* same as read,
     but is also advertised to have "Exceptional situations" in unsafe code */
  seclass_write, /* only side effects: does not read anything,
     just sets some global variables. */
  seclass_default /* may do side effects */
} seclass_t;
%% puts("enum { seclass_foldable, seclass_no_se, seclass_read, seclass_rd_sig, seclass_write, seclass_default};");

/* fast comparison method is really fastcmp_t:
 when you want to make another comparison function bypass FUNCALL in
 :TEST/:TEST-NOT sequence functions, you need to
 -- add fastcmp_FOO here and
 -- augment funarg.d:check_test_args(), and
 -- add call_test_FOO and call_test_not_FOO in funarg.d */
typedef enum {
  fastcmp_none=0,   /* no special tricks */
  fastcmp_eq,       /* EQ */
  fastcmp_eql,      /* EQL */
  fastcmp_equal,    /* EQUAL */
  fastcmp_equalp,   /* EQUALP */
  fastcmp_for_broken_compilers_that_dont_like_trailing_commas
} fastcmp_t;

/* Small-Read-Label */
#ifdef TYPECODES
  #define make_small_read_label(n)  \
    type_data_object(system_type, ((uintV)(n)<<1) + bit(0))
  #define small_read_label_integer_p(obj)  \
    (posfixnump(obj) && (posfixnum_to_V(obj) < vbit(oint_data_len-2)))
  #define small_read_label_value(obj)  \
    fixnum((as_oint(obj) >> (oint_data_shift+1)) & (vbit(oint_data_len-2)-1))
#else
  #define make_small_read_label(n)  \
    type_data_object(small_read_label_type, (uintV)(n))
  #define small_read_label_integer_p(obj)  posfixnump(obj)
  #define small_read_label_value(obj)  \
    fixnum((as_oint(obj) >> oint_data_shift) & (vbit(oint_data_len)-1))
#endif

/* Machine pointers:
 make_machine(ptr)
 ptr must be a multiple of PSEUDODATA_ALIGNMENT. */
#ifdef TYPECODES
  #define make_machine(ptr)  type_pointer_object(machine_type,ptr)
#else
  #define make_machine(ptr)  as_object((oint)(ptr)+machine_bias)
#endif
%% export_def(make_machine(ptr));

#ifdef MULTITHREAD
/* load the multithread stuff and export parts required by modules */
#include "xthread.c"
%% #ifdef MULTITHREAD
%%  #if defined(POSIX_THREADS)
%%   puts("#include <pthread.h>");
%%   puts("#include <sched.h>");
%%  #endif
%%  export_def(xthread_t);
%%  export_def(xthread_key_t);
%%  export_def(xmutex_raw_t);
%%  export_def(spinlock_t);
%% #endif

/* forward declaration */
struct clisp_thread_t;

typedef struct {
  XRECORD_HEADER
  gcv_object_t xth_name _attribute_aligned_object_; /* name */
  gcv_object_t xth_join_lock _attribute_aligned_object_; /* lock for thread-join waiting */
  gcv_object_t xth_join_exemption _attribute_aligned_object_; /* exemption for thread-join waiting */
  gcv_object_t xth_values _attribute_aligned_object_; /* return values */
  struct clisp_thread_t *xth_globals; /* all thread specific things */
  xthread_t xth_system;               /* OS object */
  uintL xth_flags; /* flags for various thread features */
} * Thread;
#define thread_length  4
#define thread_xlength (sizeof(*(Thread)0)-offsetofa(record_,recdata)-thread_length*sizeof(gcv_object_t))

/* has the thread exited normally? */
#define thread_flag_normal_exit  0x0001
#define thread_killedp(obj) (!(TheThread(obj)->xth_flags & thread_flag_normal_exit))

typedef struct {
  XRECORD_HEADER
  gcv_object_t xmu_name _attribute_aligned_object_; /* name */
  gcv_object_t xmu_owner _attribute_aligned_object_; /* owner (thread) */
  uintL xmu_flags; /* mutex flags - recursive? (by default - no)*/
  uintL xmu_recurse_count; /* how many times we have obtained the mutex */
  /* following is pointer to malloc()-ed memory. it's location should not
     change across GC since we may wait on it while GC is working.
     another option is to pin the mutex record but this leads to heap
     fragmentation and there should not be so many many mutex objects
     anyway */
  xmutex_t *xmu_system;                              /* OS object */
} * Mutex;
#define mutex_length  2
#define mutex_xlength (sizeof(*(Mutex)0)-offsetofa(record_,recdata)-mutex_length*sizeof(gcv_object_t))

#define mutex_flag_recursive  0x0001
#define mutex_recursivep(obj) (TheMutex(obj)->xmu_flags & mutex_flag_recursive)

typedef struct {
  XRECORD_HEADER
  gcv_object_t xco_name _attribute_aligned_object_; /* name */
  /* following is malloc()-ed. see Mutex comment */
  xcondition_t *xco_system;                         /* OS object */
} * Exemption;
#define exemption_length  1
#define exemption_xlength (sizeof(*(Exemption)0)-offsetofa(record_,recdata)-exemption_length*sizeof(gcv_object_t))

#endif

/* Pointer to machine code
 make_machine_code(ptr)
 ptr must be a multiple of PSEUDOCODE_ALIGNMENT.
 make_machine_code_unchecked(ptr) does the same thing, without alignment check.
*/
#if PSEUDODATA_ALIGNMENT <= C_CODE_ALIGNMENT
  #define PSEUDOCODE_ALIGNMENT  C_CODE_ALIGNMENT
  #define log2_PSEUDOCODE_ALIGNMENT  log2_C_CODE_ALIGNMENT
  /* The C_CODE_ALIGNMENT implies the PSEUDODATA_ALIGNMENT. */
  #define make_machine_code_unchecked(ptr)  make_machine(ptr)
#elif defined(HPPA) && !defined(HPPA64) && PSEUDODATA_ALIGNMENT == 4
  /* Assume that all function pointers are == 2 mod 4. */
  #define PSEUDOCODE_ALIGNMENT  4
  #define log2_PSEUDOCODE_ALIGNMENT  2
  #define make_machine_code_unchecked(ptr)  make_machine((uintP)(ptr)&~(uintP)3)
#elif !defined(NO_ADDRESS_SPACE_ASSUMPTIONS) \
      && (CODE_ADDRESS_RANGE < (oint_data_mask >> oint_data_shift >> (log2_PSEUDODATA_ALIGNMENT-log2_C_CODE_ALIGNMENT))) \
      && (MALLOC_ADDRESS_RANGE < (oint_data_mask >> oint_data_shift >> (log2_PSEUDODATA_ALIGNMENT-log2_C_CODE_ALIGNMENT))) \
      && (SHLIB_ADDRESS_RANGE < (oint_data_mask >> oint_data_shift >> (log2_PSEUDODATA_ALIGNMENT-log2_C_CODE_ALIGNMENT))) \
      && (STACK_ADDRESS_RANGE < (oint_data_mask >> oint_data_shift >> (log2_PSEUDODATA_ALIGNMENT-log2_C_CODE_ALIGNMENT)))
  /* We can shift addresses left by log2_PSEUDODATA_ALIGNMENT-log2_C_CODE_ALIGNMENT
     bits, and they will still fit into the data part of an oint. */
  #define PSEUDOCODE_ALIGNMENT  C_CODE_ALIGNMENT
  #define log2_PSEUDOCODE_ALIGNMENT  log2_C_CODE_ALIGNMENT
  #define make_machine_code_unchecked(ptr)  make_machine((uintP)(ptr)<<(log2_PSEUDODATA_ALIGNMENT-log2_C_CODE_ALIGNMENT))
#else
  /* An alignment of PSEUDODATA_ALIGNMENT is also necessary for the C functions.
     When using gcc, this may require adding -falign-functions=4 or
     -falign-functions=8, respectively, to the FALIGNFLAGS in the Makefile. */
  #define PSEUDOCODE_ALIGNMENT  PSEUDODATA_ALIGNMENT
  #define log2_PSEUDOCODE_ALIGNMENT  log2_PSEUDODATA_ALIGNMENT
  #define make_machine_code_unchecked(ptr)  make_machine(ptr)
#endif
#if (SAFETY < 2) || (PSEUDOCODE_ALIGNMENT == 1)
  #define make_machine_code(ptr)  make_machine_code_unchecked(ptr)
#else
  extern _Noreturn void error_pseudocode_alignment (uintP address, const char* prefix, const char* name);
  #define make_machine_code(ptr)  \
    (((((uintP)(void*)(ptr)-C_FUNCTION_POINTER_BIAS) & (PSEUDOCODE_ALIGNMENT-1)) \
      ? (error_pseudocode_alignment((uintP)(void*)(ptr),"",#ptr), 0) \
      : 0),                                                          \
     make_machine_code_unchecked(ptr))
#endif

/* System-Pointer */
#define make_system(data)  \
  type_data_object(system_type, vbit(oint_data_len-1) | bit(0) | ((vbitm(oint_data_len)-1) & (data)))
/* all such go into the special print routine io.d:pr_system() */
%% export_def(make_system(data));

/* missing value */
#define unbound  make_system(0xFFFFFFUL)
%% export_def(unbound);

/* missing object (internal use only): */
#define nullobj  make_machine(0)  /* = as_object((oint)0) */
#ifdef DEBUG_GCSAFETY
  #define gcv_nullobj  (gcv_object_t)nullobj
#else
  #define gcv_nullobj  nullobj
#endif
%% export_def(nullobj);
%% export_def(gcv_nullobj);


/* cgci_pointable(obj)  converts a certainly GC-invariant object to an aint.
 pgci_pointable(obj)  converts a possibly GC-invariant object to an aint.
 ngci_pointable(obj)  converts a not GC-invariant object to an aint. */
#if defined(DEBUG_GCSAFETY)
  static inline aint cgci_pointable (object obj) {
    return obj.one_o;
  }
  static inline aint cgci_pointable (gcv_object_t obj) {
    return obj.one_o;
  }
  static inline aint pgci_pointable (object obj) {
    if (!(gcinvariant_object_p(obj) || gcinvariant_symbol_p(obj)
          || obj.allocstamp == alloccount || nonimmsubrp(obj)))
      abort();
    nonimmprobe(pointable_address_unchecked(obj.one_o));
    return obj.one_o;
  }
  static inline aint pgci_pointable (gcv_object_t obj) {
    nonimmprobe(pointable_address_unchecked(obj.one_o));
    return obj.one_o;
  }
  static inline aint ngci_pointable (object obj) {
    if (!(gcinvariant_symbol_p(obj)
          || obj.allocstamp == alloccount || nonimmsubrp(obj)))
      abort();
    nonimmprobe(pointable_address_unchecked(obj.one_o));
    return obj.one_o;
  }
  static inline aint ngci_pointable (gcv_object_t obj) {
    nonimmprobe(pointable_address_unchecked(obj.one_o));
    return obj.one_o;
  }
#else
  #define cgci_pointable(obj)  as_oint(obj)
  #define pgci_pointable(obj)  as_oint(obj)
  #define ngci_pointable(obj)  as_oint(obj)
#endif
%% #if defined(DEBUG_GCSAFETY)
%%   puts("static inline aint cgci_pointable (object obj) { return obj.one_o; }");
%%   puts("static inline aint cgci_pointable (gcv_object_t obj) { return obj.one_o; }");
%%   puts("static inline aint pgci_pointable (object obj) { if (!(gcinvariant_object_p(obj) || gcinvariant_symbol_p(obj) || obj.allocstamp == alloccount || nonimmsubrp(obj))) abort(); nonimmprobe(pointable_address_unchecked(obj.one_o)); return obj.one_o; }");
%%   puts("static inline aint pgci_pointable (gcv_object_t obj) { nonimmprobe(pointable_address_unchecked(obj.one_o)); return obj.one_o; }");
%%   puts("static inline aint ngci_pointable (object obj) { if (!(gcinvariant_symbol_p(obj) || obj.allocstamp == alloccount || nonimmsubrp(obj))) abort(); nonimmprobe(pointable_address_unchecked(obj.one_o)); return obj.one_o; }");
%%   puts("static inline aint ngci_pointable (gcv_object_t obj) { nonimmprobe(pointable_address_unchecked(obj.one_o)); return obj.one_o; }");
%% #else
%%   export_def(cgci_pointable(obj));
%%   export_def(pgci_pointable(obj));
%%   export_def(ngci_pointable(obj));
%% #endif

/* TheCons(object) yields the Cons that's equivalent to object.
 The information that it is a Cons has to be put into it.
 The other type conversions are similar. */
#ifdef TYPECODES
  #ifdef DEBUG_GCSAFETY
    #define cgci_types_pointable(ORed_types,obj)  pointable_address_unchecked(cgci_pointable(obj))
    #define pgci_types_pointable(ORed_types,obj)  pointable_address_unchecked(pgci_pointable(obj))
    #define ngci_types_pointable(ORed_types,obj)  pointable_address_unchecked(ngci_pointable(obj))
  #else
    #define cgci_types_pointable(ORed_types,obj)  types_pointable(ORed_types,obj)
    #define pgci_types_pointable(ORed_types,obj)  types_pointable(ORed_types,obj)
    #define ngci_types_pointable(ORed_types,obj)  types_pointable(ORed_types,obj)
  #endif
  #define TheCons(obj)  ((Cons)(ngci_types_pointable(cons_type,obj)))
  #define TheRatio(obj)  ((Ratio)(ngci_types_pointable(ratio_type|bit(sign_bit_t),obj)))
  #define TheComplex(obj)  ((Complex)(ngci_types_pointable(complex_type,obj)))
  #define TheSymbol(obj)  ((Symbol)(ngci_types_pointable(symbol_type,obj)))
  #if (oint_symbolflags_shift==oint_type_shift)
  #define TheSymbolflagged(obj)  ((Symbol)(ngci_types_pointable(symbol_type|bit(active_bit)|bit(dynam_bit)|bit(svar_bit),obj)))
  #else
  #define TheSymbolflagged(obj)  TheSymbol(symbol_without_flags(obj))
  #endif
  #define TheBignum(obj)  ((Bignum)(ngci_types_pointable(bignum_type|bit(sign_bit_t),obj)))
  #ifndef IMMEDIATE_FFLOAT
  #define TheFfloat(obj)  ((Ffloat)(ngci_types_pointable(ffloat_type|bit(sign_bit_t),obj)))
  #endif
  #define TheDfloat(obj)  ((Dfloat)(ngci_types_pointable(dfloat_type|bit(sign_bit_t),obj)))
  #define TheLfloat(obj)  ((Lfloat)(ngci_types_pointable(lfloat_type|bit(sign_bit_t),obj)))
  #define TheSarray(obj)  ((Sarray)(ngci_types_pointable(sbvector_type|sb2vector_type|sb4vector_type|sb8vector_type|sb16vector_type|sb32vector_type|sstring_type|svector_type,obj)))
  #define TheSbvector(obj)  ((Sbvector)(ngci_types_pointable(sbvector_type|sb2vector_type|sb4vector_type|sb8vector_type|sb16vector_type|sb32vector_type,obj)))
  #define TheCodevec(obj)  ((Codevec)(ngci_types_pointable(sb8vector_type,obj)))
  #define TheS8string(obj)  ((S8string)(ngci_types_pointable(sstring_type,obj)))
  #define TheS16string(obj)  ((S16string)(ngci_types_pointable(sstring_type,obj)))
  #define TheS32string(obj)  ((S32string)(ngci_types_pointable(sstring_type,obj)))
  #define TheSnstring(obj)  ((Snstring)(ngci_types_pointable(sstring_type,obj)))
  #define TheSistring(obj)  ((Sistring)(ngci_types_pointable(sstring_type,obj)))
  #define TheSstring(obj)  ((Sstring)(ngci_types_pointable(sstring_type,obj)))
  #define TheSvector(obj)  ((Svector)(ngci_types_pointable(svector_type,obj)))
  #define TheIarray(obj)  ((Iarray)(ngci_types_pointable(mdarray_type|bvector_type|b2vector_type|b4vector_type|b8vector_type|b16vector_type|b32vector_type|string_type|vector_type,obj)))
  #define TheRecord(obj)  ((Record)(ngci_types_pointable(closure_type|structure_type|stream_type|orecord_type|instance_type|lrecord_type,obj)))
  #define TheLrecord(obj)  ((Lrecord)(ngci_types_pointable(lrecord_type,obj)))
  #define TheSrecord(obj)  ((Srecord)(ngci_types_pointable(closure_type|structure_type|orecord_type|instance_type,obj)))
  #define TheXrecord(obj)  ((Xrecord)(ngci_types_pointable(stream_type|orecord_type,obj)))
  #define ThePackage(obj)  ((Package)(ngci_types_pointable(orecord_type,obj)))
  #define TheHashtable(obj)  ((Hashtable)(ngci_types_pointable(orecord_type,obj)))
  #define TheReadtable(obj)  ((Readtable)(ngci_types_pointable(orecord_type,obj)))
  #define ThePathname(obj)  ((Pathname)(ngci_types_pointable(orecord_type,obj)))
  #define TheLogpathname(obj)  ((Logpathname)(ngci_types_pointable(orecord_type,obj)))
  #define The_Random_state(obj)  ((Random_state)(ngci_types_pointable(orecord_type,obj)))
  #define TheByte(obj)  ((Byte)(ngci_types_pointable(orecord_type,obj)))
  #define TheFsubr(obj)  ((Fsubr)(ngci_types_pointable(orecord_type,obj)))
  #define TheLoadtimeeval(obj)  ((Loadtimeeval)(ngci_types_pointable(orecord_type,obj)))
  #define TheSymbolmacro(obj)  ((Symbolmacro)(ngci_types_pointable(orecord_type,obj)))
  #define TheGlobalSymbolmacro(obj)  ((GlobalSymbolmacro)(ngci_types_pointable(orecord_type,obj)))
  #define TheMacro(obj)  ((Macro)(ngci_types_pointable(orecord_type,obj)))
  #define TheFunctionMacro(obj)  ((FunctionMacro)(ngci_types_pointable(orecord_type,obj)))
  #define TheBigReadLabel(obj)  ((BigReadLabel)(ngci_types_pointable(orecord_type,obj)))
  #define TheEncoding(obj)  ((Encoding)(ngci_types_pointable(orecord_type,obj)))
  #ifdef FOREIGN
  #define TheFpointer(obj)  ((Fpointer)(ngci_types_pointable(orecord_type,obj)))
  #endif
  #ifdef DYNAMIC_FFI
  #define TheFaddress(obj)  ((Faddress)(ngci_types_pointable(orecord_type,obj)))
  #define TheFvariable(obj)  ((Fvariable)(ngci_types_pointable(orecord_type,obj)))
  #define TheFfunction(obj)  ((Ffunction)(ngci_types_pointable(orecord_type,obj)))
  #endif
  #define TheWeakpointer(obj)  ((Weakpointer)(ngci_types_pointable(orecord_type,obj)))
  #define TheMutableWeakList(obj)  ((MutableWeakList)(ngci_types_pointable(orecord_type,obj)))
  #define TheWeakList(obj)  ((WeakList)(ngci_types_pointable(lrecord_type,obj)))
  #define TheWeakAnd(obj)  ((WeakAnd)(ngci_types_pointable(lrecord_type,obj)))
  #define TheWeakOr(obj)  ((WeakOr)(ngci_types_pointable(lrecord_type,obj)))
  #define TheWeakmapping(obj)  ((Weakmapping)(ngci_types_pointable(orecord_type,obj)))
  #define TheWeakAndMapping(obj)  ((WeakAndMapping)(ngci_types_pointable(lrecord_type,obj)))
  #define TheWeakOrMapping(obj)  ((WeakOrMapping)(ngci_types_pointable(lrecord_type,obj)))
  #define TheMutableWeakAlist(obj)  ((MutableWeakAlist)(ngci_types_pointable(orecord_type,obj)))
  #define TheWeakAlist(obj)  ((WeakAlist)(ngci_types_pointable(lrecord_type,obj)))
  #define TheWeakHashedAlist(obj)  ((WeakHashedAlist)(ngci_types_pointable(lrecord_type,obj)))
  #define TheFinalizer(obj)  ((Finalizer)(ngci_types_pointable(orecord_type,obj)))
  #ifdef SOCKET_STREAMS
  #define TheSocketServer(obj) ((Socket_server)(ngci_types_pointable(orecord_type,obj)))
  #endif
  #ifdef YET_ANOTHER_RECORD
  #define TheYetanother(obj)  ((Yetanother)(ngci_types_pointable(orecord_type,obj)))
  #endif
  #define TheStream(obj)  ((Stream)(ngci_types_pointable(stream_type,obj)))
  #define TheStructure(obj)  ((Structure)(ngci_types_pointable(structure_type,obj)))
  #define TheClosure(obj)  ((Closure)(ngci_types_pointable(closure_type,obj)))
  #define TheIclosure(obj)  ((Iclosure)(ngci_types_pointable(closure_type,obj)))
  #define TheCclosure(obj)  ((Cclosure)(ngci_types_pointable(closure_type,obj)))
  #define TheInstance(obj)  ((Instance)(ngci_types_pointable(instance_type|closure_type,obj)))
  #define TheSubr(obj)  ((Subr)(cgci_types_pointable(subr_type,obj)))
  #define TheFramepointer(obj)  ((gcv_object_t*)(cgci_types_pointable(system_type,obj)))
  #define TheMachine(obj)  ((void*)(cgci_types_pointable(machine_type,obj)))
  #ifdef FOREIGN_HANDLE
  /* pack Handle in Sbvector */
  #define TheHandle(obj)  (*(Handle*)(&TheSbvector(obj)->data[0]))
  #else
  /* pack Handle in Fixnum>=0 */
  #define TheHandle(obj)  ((Handle)posfixnum_to_V(obj))
  #endif
  /* variable length object: */
  #define TheVarobject(obj)                                              \
    ((Varobject)                                                         \
     (ngci_types_pointable                                               \
      (sbvector_type|sb2vector_type|sb4vector_type|sb8vector_type        \
         |sb16vector_type|sb32vector_type                                \
       |sstring_type|svector_type                                        \
       |mdarray_type                                                     \
       |bvector_type|b2vector_type|b4vector_type|b8vector_type           \
         |b16vector_type|b32vector_type                                  \
       |string_type|vector_type                                          \
       |closure_type|structure_type|stream_type|orecord_type             \
       |instance_type|lrecord_type|symbol_type                           \
       |bignum_type|ffloat_type|dfloat_type|lfloat_type|bit(sign_bit_t), \
       obj                                                               \
    )))
  /* Object that represents a pointer into the memory: */
  #define ThePointer(obj)                                               \
    (pgci_types_pointable                                               \
     (sbvector_type|sb2vector_type|sb4vector_type|sb8vector_type        \
        |sb16vector_type|sb32vector_type                                \
      |sstring_type|svector_type                                        \
      |mdarray_type                                                     \
      |bvector_type|b2vector_type|b4vector_type|b8vector_type           \
        |b16vector_type|b32vector_type                                  \
      |string_type|vector_type                                          \
      |closure_type|structure_type|stream_type|orecord_type             \
      |instance_type|lrecord_type|symbol_type                           \
      |cons_type                                                        \
      |bignum_type|ffloat_type|dfloat_type|lfloat_type                  \
      |ratio_type|complex_type|bit(sign_bit_t),                         \
      obj                                                               \
    ))
 #ifdef MULTITHREAD
  #define TheThread(obj)  ((Thread)(ngci_types_pointable(orecord_type,obj)))
  #define TheMutex(obj)   ((Mutex)(ngci_types_pointable(orecord_type,obj)))
  #define TheExemption(obj) ((Exemption)(ngci_types_pointable(orecord_type,obj)))
 #endif
#else /* no TYPECODES */
  #define TheCons(obj)  ((Cons)(ngci_pointable(obj)-cons_bias))
  #define TheRatio(obj)  ((Ratio)(ngci_pointable(obj)-varobject_bias))
  #define TheComplex(obj)  ((Complex)(ngci_pointable(obj)-varobject_bias))
  #define TheSymbol(obj)  ((Symbol)(ngci_pointable(obj)-varobject_bias))
  #define TheSymbolflagged(obj)  TheSymbol(symbol_without_flags(obj))
  #define TheBignum(obj)  ((Bignum)(ngci_pointable(obj)-varobject_bias))
  #define TheFfloat(obj)  ((Ffloat)(ngci_pointable(obj)-varobject_bias))
  #define TheDfloat(obj)  ((Dfloat)(ngci_pointable(obj)-varobject_bias))
  #define TheLfloat(obj)  ((Lfloat)(ngci_pointable(obj)-varobject_bias))
  #define TheSarray(obj)  ((Sarray)(ngci_pointable(obj)-varobject_bias))
  #define TheSbvector(obj)  ((Sbvector)(ngci_pointable(obj)-varobject_bias))
  #define TheCodevec(obj)  ((Codevec)TheSbvector(obj))
  #define TheS8string(obj)  ((S8string)(ngci_pointable(obj)-varobject_bias))
  #define TheS16string(obj)  ((S16string)(ngci_pointable(obj)-varobject_bias))
  #define TheS32string(obj)  ((S32string)(ngci_pointable(obj)-varobject_bias))
  #define TheSnstring(obj)  ((Snstring)(ngci_pointable(obj)-varobject_bias))
  #define TheSistring(obj)  ((Sistring)(ngci_pointable(obj)-varobject_bias))
  #define TheSstring(obj)  ((Sstring)(ngci_pointable(obj)-varobject_bias))
  #define TheSvector(obj)  ((Svector)(ngci_pointable(obj)-varobject_bias))
  #define TheIarray(obj)  ((Iarray)(ngci_pointable(obj)-varobject_bias))
  #define TheRecord(obj)  ((Record)(ngci_pointable(obj)-varobject_bias))
  #define TheLrecord(obj)  ((Lrecord)(ngci_pointable(obj)-varobject_bias))
  #define TheSrecord(obj)  ((Srecord)(ngci_pointable(obj)-varobject_bias))
  #define TheXrecord(obj)  ((Xrecord)(ngci_pointable(obj)-varobject_bias))
  #define ThePackage(obj)  ((Package)(ngci_pointable(obj)-varobject_bias))
  #define TheHashtable(obj)  ((Hashtable)(ngci_pointable(obj)-varobject_bias))
  #define TheReadtable(obj)  ((Readtable)(ngci_pointable(obj)-varobject_bias))
  #define ThePathname(obj)  ((Pathname)(ngci_pointable(obj)-varobject_bias))
  #define TheLogpathname(obj)  ((Logpathname)(ngci_pointable(obj)-varobject_bias))
  #define The_Random_state(obj)  ((Random_state)(ngci_pointable(obj)-varobject_bias))
  #define TheByte(obj)  ((Byte)(ngci_pointable(obj)-varobject_bias))
  #define TheFsubr(obj)  ((Fsubr)(ngci_pointable(obj)-varobject_bias))
  #define TheLoadtimeeval(obj)  ((Loadtimeeval)(ngci_pointable(obj)-varobject_bias))
  #define TheSymbolmacro(obj)  ((Symbolmacro)(ngci_pointable(obj)-varobject_bias))
  #define TheGlobalSymbolmacro(obj)  ((GlobalSymbolmacro)(ngci_pointable(obj)-varobject_bias))
  #define TheMacro(obj)  ((Macro)(ngci_pointable(obj)-varobject_bias))
  #define TheFunctionMacro(obj)  ((FunctionMacro)(ngci_pointable(obj)-varobject_bias))
  #define TheBigReadLabel(obj)  ((BigReadLabel)(ngci_pointable(obj)-varobject_bias))
  #define TheEncoding(obj)  ((Encoding)(ngci_pointable(obj)-varobject_bias))
  #ifdef FOREIGN
  #define TheFpointer(obj)  ((Fpointer)(ngci_pointable(obj)-varobject_bias))
  #endif
  #ifdef DYNAMIC_FFI
  #define TheFaddress(obj)  ((Faddress)(ngci_pointable(obj)-varobject_bias))
  #define TheFvariable(obj)  ((Fvariable)(ngci_pointable(obj)-varobject_bias))
  #define TheFfunction(obj)  ((Ffunction)(ngci_pointable(obj)-varobject_bias))
  #endif
  #define TheWeakpointer(obj)  ((Weakpointer)(ngci_pointable(obj)-varobject_bias))
  #define TheMutableWeakList(obj)  ((MutableWeakList)(ngci_pointable(obj)-varobject_bias))
  #define TheWeakList(obj)  ((WeakList)(ngci_pointable(obj)-varobject_bias))
  #define TheWeakAnd(obj)  ((WeakAnd)(ngci_pointable(obj)-varobject_bias))
  #define TheWeakOr(obj)  ((WeakOr)(ngci_pointable(obj)-varobject_bias))
  #define TheWeakmapping(obj)  ((Weakmapping)(ngci_pointable(obj)-varobject_bias))
  #define TheWeakAndMapping(obj)  ((WeakAndMapping)(ngci_pointable(obj)-varobject_bias))
  #define TheWeakOrMapping(obj)  ((WeakOrMapping)(ngci_pointable(obj)-varobject_bias))
  #define TheMutableWeakAlist(obj)  ((MutableWeakAlist)(ngci_pointable(obj)-varobject_bias))
  #define TheWeakAlist(obj)  ((WeakAlist)(ngci_pointable(obj)-varobject_bias))
  #define TheWeakHashedAlist(obj)  ((WeakHashedAlist)(ngci_pointable(obj)-varobject_bias))
  #define TheFinalizer(obj)  ((Finalizer)(ngci_pointable(obj)-varobject_bias))
  #ifdef SOCKET_STREAMS
  #define TheSocketServer(obj) ((Socket_server)(ngci_pointable(obj)-varobject_bias))
  #endif
  #ifdef YET_ANOTHER_RECORD
  #define TheYetanother(obj)  ((Yetanother)(ngci_pointable(obj)-varobject_bias))
  #endif
  #define TheStream(obj)  ((Stream)(ngci_pointable(obj)-varobject_bias))
  #define TheStructure(obj)  ((Structure)(ngci_pointable(obj)-varobject_bias))
  #define TheClosure(obj)  ((Closure)(ngci_pointable(obj)-varobject_bias))
  #define TheIclosure(obj)  ((Iclosure)(ngci_pointable(obj)-varobject_bias))
  #define TheCclosure(obj)  ((Cclosure)(ngci_pointable(obj)-varobject_bias))
  #define TheInstance(obj)  ((Instance)(ngci_pointable(obj)-varobject_bias))
  #define TheSubr(obj)  ((Subr)(cgci_pointable(obj)-subr_bias))
  #define TheFramepointer(obj)  ((gcv_object_t*)(cgci_pointable(obj)-machine_bias))
  #define TheMachine(obj)  ((void*)(cgci_pointable(obj)-machine_bias))
  #ifdef FOREIGN_HANDLE
  /* pack Handle in Sbvector */
  #define TheHandle(obj)  (*(Handle*)(&TheSbvector(obj)->data[0]))
  #else
  /* pack Handle in Fixnum>=0 */
  #define TheHandle(obj)  ((Handle)posfixnum_to_V(obj))
  #endif
  /* Object of variable length: */
  #define TheVarobject(obj)  ((Varobject)(ngci_pointable(obj)-varobject_bias))
  /* Object, represents a pointer into the memory: */
  #define ThePointer(obj)  ((void*)(pgci_pointable(obj) & ~(aint)nonimmediate_bias_mask))
 #ifdef MULTITHREAD
  #define TheThread(obj)    ((Thread)(ngci_pointable(obj)-varobject_bias))
  #define TheMutex(obj)     ((Mutex)(ngci_pointable(obj)-varobject_bias))
  #define TheExemption(obj) ((Exemption)(ngci_pointable(obj)-varobject_bias))
 #endif
#endif
/* TheMachineCode is the opposite of make_machine_code. */
#if PSEUDODATA_ALIGNMENT <= C_CODE_ALIGNMENT
  #define TheMachineCode(obj)  TheMachine(obj)
#elif defined(HPPA) && !defined(HPPA64) && PSEUDODATA_ALIGNMENT == 4
  #define TheMachineCode(obj)  ((void*)((uintP)TheMachine(obj)+C_FUNCTION_POINTER_BIAS))
#elif PSEUDOCODE_ALIGNMENT == C_CODE_ALIGNMENT
  #define TheMachineCode(obj)  ((void*)(((uintP)TheMachine(obj)>>(log2_PSEUDODATA_ALIGNMENT-log2_C_CODE_ALIGNMENT))|(CODE_ADDRESS_RANGE&~((~(uintP)0)>>(log2_PSEUDODATA_ALIGNMENT-log2_C_CODE_ALIGNMENT)))))
#elif PSEUDOCODE_ALIGNMENT == PSEUDODATA_ALIGNMENT
  #define TheMachineCode(obj)  TheMachine(obj)
#endif
#define ThePseudofun(obj)  ((Pseudofun)TheMachineCode(obj))
#define TheClassVersion(obj)  ((ClassVersion)TheSvector(obj))
#define TheSlotDefinition(obj)  ((SlotDefinition)TheInstance(obj))
#define TheClass(obj)  ((Class)TheInstance(obj))
%% export_def(TheCons(obj));
%% #if notused
%%   export_def(TheRatio(obj));
%%   export_def(TheComplex(obj));
%% #endif
%% export_def(TheSymbol(obj));
%% export_def(TheBignum(obj));
%% #if notused
%%   export_def(TheSarray(obj));
%% #endif
%% export_def(TheSbvector(obj));
%% #ifdef HAVE_SMALL_SSTRING
%%   export_def(TheS8string(obj));
%%   export_def(TheS16string(obj));
%%   export_def(TheS32string(obj));
%% #endif
%% export_def(TheSstring(obj));
%% export_def(TheSvector(obj));
%% export_def(TheRecord(obj));
%% export_def(TheSrecord(obj));
%% #if notused
%%   export_def(TheXrecord(obj));
%%   export_def(ThePackage(obj));
%% #endif
%% export_def(TheEncoding(obj));
%% #ifdef FOREIGN
%%   export_def(TheFpointer(obj));
%% #endif
%% export_def(TheStructure(obj));
%% export_def(TheClosure(obj));
%% export_def(TheInstance(obj));
%% export_def(TheSubr(obj));
%% export_def(TheMachine(obj));
%% export_def(TheMachineCode(obj));
%% export_def(ThePseudofun(obj));

/* Some acronyms
 Access to objects that are conses: */
#define Car(obj)  (TheCons(obj)->car)
#define Cdr(obj)  (TheCons(obj)->cdr)
#define Symbol_function(obj)  (TheSymbol(obj)->symfunction)
#define Symbol_plist(obj)  (TheSymbol(obj)->proplist)
#define Symbol_name(obj)  (TheSymbol(obj)->pname)
#define Symbol_package(obj)  (TheSymbol(obj)->homepackage)
/* Length (number of objects) of a record, obj has to be a Srecord/Xrecord: */
#define SXrecord_length(obj)  \
  (Record_type(obj) < rectype_limit ? Srecord_length(obj) : Xrecord_length(obj))
/* Likewise, but ignoring weak pointers: */
#define SXrecord_nonweak_length(obj)  \
  (Record_type(obj) < rectype_limit              \
   ? Srecord_length(obj)                         \
   : ((Record_type(obj)==Rectype_Weakpointer     \
       || Record_type(obj)==Rectype_Weakmapping) \
      ? 0                                        \
      : Xrecord_length(obj)))
/* Length of an Lrecord, ignoring weak pointers: */
#define Lrecord_nonweak_length(obj)  \
  ((Record_type(obj) >= Rectype_WeakList                 \
    && Record_type(obj) <= Rectype_WeakHashedAlist_Both) \
   ? 0                                                   \
   : Lrecord_length(obj))
/* Length (number of objects) of a record, obj has to be a Record: */
#define Record_length(obj)  \
  (Record_type(obj) >= rectype_longlimit \
   ? Lrecord_length(obj)                 \
   : SXrecord_length(obj))
/* Likewise, but ignoring weak pointers: */
#define Record_nonweak_length(obj)  \
  (Record_type(obj) >= rectype_longlimit \
   ? Lrecord_nonweak_length(obj)         \
   : SXrecord_nonweak_length(obj))
%% export_def(Car(obj));
%% export_def(Cdr(obj));
%% export_def(Symbol_function(obj));
%% export_def(Symbol_plist(obj));
%% export_def(Symbol_name(obj));
%% export_def(Symbol_package(obj));


/* ####################### type test predicates ########################### #
 There are two kinds of predicates:
 1.  ???p, query with 'if':  if ???p(object)
 2.  if_???p, called as
         if_???p(object, statement1, statement2)
       instead of
         if ???p(object) statement1 else statement2

 UP: tests for equality of pointers EQ
 eq(obj1,obj2)
 > obj1,obj2: Lisp-objects
 < result: true, if objects are equal */
#if defined(DEBUG_GCSAFETY)
  #define eq(obj1,obj2)  (pgci_pointable(obj1) == pgci_pointable(obj2))
#elif defined(WIDE_STRUCT) || defined(OBJECT_STRUCT)
  #define eq(obj1,obj2)  (as_oint(obj1) == as_oint(obj2))
#else
  #define eq(obj1,obj2)  ((obj1) == (obj2))
#endif
%% export_def(eq(obj1,obj2));

/* Symbol_value() definition moved here - since in MT we need eq() to be defined. */
/* Access to objects that are symbols: */
#if defined(MULTITHREAD)
  /* helper inline functions to keep ANSI compliance and prevent multiple
     time arguments evaluation. Should we __forceinline them ? */
  static inline gcv_object_t *symbol_value_i(Symbol s, gcv_object_t *thrsyms) {
    return (s->tls_index && !eq(SYMVALUE_EMPTY,thrsyms[s->tls_index]) ?
	    thrsyms+s->tls_index : &s->symvalue);
  }
  static inline gcv_object_t *symbol_value_h(Symbol s, gcv_object_t *thrsyms) {
    return (s->tls_index ? thrsyms+s->tls_index : &s->symvalue);
  }
  static inline object symbol_value_b(Symbol s, gcv_object_t *thrsyms) {
    return (s->tls_index ? (object)thrsyms[s->tls_index] : SYMVALUE_EMPTY);
  }
  #define Symbol_value(sym) \
    *(symbol_value_i(TheSymbol(sym),current_thread()->_ptr_symvalues))
  #define Symbol_thread_value(sym) \
    *(symbol_value_h(TheSymbol(sym),current_thread()->_ptr_symvalues))
  #define Symbol_thread_binding(sym) \
    symbol_value_b(TheSymbol(sym),current_thread()->_ptr_symvalues)
  #define Symbolflagged_value(sym) \
    *(symbol_value_h(TheSymbolflagged(sym),current_thread()->_ptr_symvalues))
#else
  #define Symbol_value(sym)  (TheSymbol(sym)->symvalue)
  #define Symbol_thread_value(sym) Symbol_value(sym)
  #define Symbol_thread_binding(sym) Symbol_value(sym)
  #define Symbolflagged_value(sym) (TheSymbolflagged(sym)->symvalue)
#endif
%% #if defined(MULTITHREAD)
%%   export_def(SYMVALUE_EMPTY);
%%   export_def(SYMBOL_TLS_INDEX_NONE);
%%   puts("static inline gcv_object_t *symbol_value_i(Symbol s, gcv_object_t *thrsyms) {");
%%   puts("return (s->tls_index && !eq(SYMVALUE_EMPTY,thrsyms[s->tls_index]) ?");
%%   puts("thrsyms+s->tls_index : &s->symvalue);}");
%%   puts("static inline gcv_object_t *symbol_value_h(Symbol s, gcv_object_t *thrsyms) {");
%%   puts(" return (s->tls_index ? thrsyms+s->tls_index : &s->symvalue);}");
%% #endif
%% export_def(Symbol_value(obj));

/* Test for NIL */
#define nullp(obj)  (eq(obj,NIL))
%% export_def(nullp(obj));

/* Shorthand: Test a fixed symbol's value for NIL */
#define nullpSv(sym) ( nullp(Symbol_value(S(sym))))

/* Test for an argument's value, whether the argument was provided. */
#define boundp(obj)  (!eq(obj,unbound))
%% export_def(boundp(obj));

/* Test for an argument's value, whether the argument was not provided or NIL. */
#define missingp(obj)  (!boundp(obj) || nullp(obj))
%% export_def(missingp(obj));

/* Test for Cons */
#ifdef TYPECODES
  #if defined(cons_bit_o)
    /* define consp(obj)  (as_oint(obj) & wbit(cons_bit_o)) */
    #define consp(obj)  (wbit_test(as_oint(obj),cons_bit_o))
    #ifdef fast_mtypecode
      #ifdef WIDE_STRUCT
        #undef consp
        #define consp(obj)  (typecode(obj) & bit(cons_bit_t))
      #endif
      #define mconsp(obj)  (mtypecode(obj) & bit(cons_bit_t))
    #else
      #define mconsp(obj)  consp(obj)
    #endif
  #else
    #define consp(obj)  (typecode(obj) == cons_type)
    #define mconsp(obj)  (mtypecode(obj) == cons_type)
  #endif
#else
  #define consp(obj)  ((as_oint(obj) & 7) == (cons_bias+conses_misaligned))
  #define mconsp(obj)  consp(obj)
#endif
%% export_def(consp(obj));
%% export_def(mconsp(obj));

/* Test for Atom */
#ifdef TYPECODES
  #if defined(cons_bit_o)
    /* define atomp(obj)  ((as_oint(obj) & wbit(cons_bit_o))==0) */
    #define atomp(obj)  (!wbit_test(as_oint(obj),cons_bit_o))
    #ifdef fast_mtypecode
      #ifdef WIDE_STRUCT
        #undef atomp
        #define atomp(obj)  ((typecode(obj) & bit(cons_bit_t))==0)
      #endif
      #define matomp(obj)  ((mtypecode(obj) & bit(cons_bit_t))==0)
    #else
      #define matomp(obj)  atomp(obj)
    #endif
  #else
    #define atomp(obj)  (!(typecode(obj) == cons_type))
    #define matomp(obj)  (!(mtypecode(obj) == cons_type))
  #endif
#else
  #define atomp(obj)  (!consp(obj))
  #define matomp(obj)  atomp(obj)
#endif
%% export_def(atomp(obj));
%% export_def(matomp(obj));

/* For all type tests below this line, the argument must be side-effect-free.
 Ideally a variable, but a STACK_(n) reference works as well. */

/* Test for List */
#define listp(obj)  (nullp(obj) || consp(obj))
%% export_def(listp(obj));

#ifndef TYPECODES
  /* Test for Object with variable length */
  #define varobjectp(obj)  ((as_oint(obj) & nonimmediate_heapcode_mask) == (varobject_bias+varobjects_misaligned))
#endif
%% #ifndef TYPECODES
%%   export_def(varobjectp(obj));
%% #endif

/* Test for Symbol */
#ifdef TYPECODES
  #define symbolp(obj)  (typecode(obj) == symbol_type)
#else
  #define symbolp(obj)  \
    (varobjectp(obj) && (Record_type(obj) == Rectype_Symbol))
#endif
%% export_def(symbolp(obj));

/* Test for number */
#ifdef TYPECODES
  /* define numberp(obj)  (as_oint(obj) & wbit(number_bit_o)) */
  #define numberp(obj)  (wbit_test(as_oint(obj),number_bit_o))
  #ifdef WIDE_STRUCT
    #undef numberp
    #define numberp(obj)  (typecode(obj) & bit(number_bit_t))
  #endif
#else
  #define immediate_number_p(obj)  \
    ((as_oint(obj) & ((4 << imm_type_shift) | immediate_bias_mask)) == (fixnum_type&sfloat_type))
  #define numberp(obj)  \
    (immediate_number_p(obj) \
     || (varobjectp(obj)     \
         && ((uintB)(Record_type(obj)-Rectype_Bignum) <= Rectype_Complex-Rectype_Bignum)))
#endif
%% #if notused
%% #ifdef TYPECODES
%%  export_def(numberp(obj));
%% #else
%%  export_def(immediate_number_p(obj));
%% #endif
%% #endif

/* Test for Vector (typebytes %001,%010,%011,%101,%110,%111) */
#ifdef TYPECODES
  #define vectorp(obj)  \
    ((tint)(typecode(obj) - sbvector_type) <= (tint)(vector_type - sbvector_type))
#else
  /* cases: Rectype_Sbvector, Rectype_Sb[2|4|8|16|32]vector, Rectype_Svector, Rectype_[Imm_]S[8|16|32]string,
          Rectype_bvector, Rectype_b[2|4|8|16|32]vector, Rectype_vector, Rectype_reallocstring, Rectype_string */
  #define vectorp(obj)  \
    (varobjectp(obj) && ((uintB)(Record_type(obj) - Rectype_vector) \
                         <= Rectype_string - Rectype_vector))
#endif
%% export_def(vectorp(obj));

/* Test for simple-vector or simple-bit-vector or simple-string */
#ifdef TYPECODES
  #define simplep(obj)  \
    ((tint)(typecode(obj) - sbvector_type) <= (tint)(svector_type - sbvector_type))
#else
  /* cases: Rectype_Sbvector, Rectype_Sb[2|4|8|16|32]vector, Rectype_Svector, Rectype_[Imm_]S[8|16|32]string,
          Rectype_reallocstring */
  #define simplep(obj)  \
    (varobjectp(obj) && ((uintB)(Record_type(obj) - Rectype_Svector) \
                         <= Rectype_reallocstring - Rectype_Svector))
#endif

/* Tests an Array for simple-vector or simple-bit-vector or simple-string */
#ifdef TYPECODES
  #define array_simplep(obj)  \
    ((typecode(obj) & bit(notsimple_bit_t)) == 0)
#else
  /* cases: Rectype_Sbvector, Rectype_Sb[2|4|8|16|32]vector, Rectype_Svector, Rectype_[Imm_]S[8|16|32]string,
          Rectype_reallocstring */
  #define array_simplep(obj)  \
    ((uintB)(Record_type(obj) - Rectype_Svector) \
     <= Rectype_reallocstring - Rectype_Svector)
#endif

/* Test for simple-vector */
#ifdef TYPECODES
  #define simple_vector_p(obj)  \
    (typecode(obj) == svector_type)
#else
  /* cases: Rectype_Svector */
  #define simple_vector_p(obj)  \
    (varobjectp(obj) && (Record_type(obj) == Rectype_Svector))
#endif
%% export_def(simple_vector_p(obj));

/* Test for general-vector=(vector t) */
#ifdef TYPECODES
  #define general_vector_p(obj)  \
    ((typecode(obj) & ~bit(notsimple_bit_t)) == svector_type)
#else
  /* cases: Rectype_Svector, Rectype_vector */
  #define general_vector_p(obj)  \
    (varobjectp(obj) \
     && ((Record_type(obj) & ~(Rectype_Svector ^ Rectype_vector)) == (Rectype_Svector & Rectype_vector)) \
    )
#endif
%% export_def(general_vector_p(obj));

/* Test for simple-string */
#ifdef TYPECODES
  #define simple_string_p(obj)  \
    (typecode(obj) == sstring_type)
#else
  /* cases: Rectype_[Imm_]S[8|16|32]string, Rectype_reallocstring */
  #define simple_string_p(obj)  \
    (varobjectp(obj) && ((uintB)(Record_type(obj) - Rectype_S8string) \
                         <= Rectype_reallocstring - Rectype_S8string))
#endif
%% export_def(simple_string_p(obj));

/* Test for string */
#ifdef TYPECODES
  #define stringp(obj)  \
    ((typecode(obj) & ~bit(notsimple_bit_t)) == sstring_type)
#else
  /* cases: Rectype_[Imm_]S[8|16|32]string, Rectype_reallocstring, Rectype_string */
  #define stringp(obj)  \
    (varobjectp(obj) && ((uintB)(Record_type(obj) - Rectype_S8string) \
                         <= Rectype_string - Rectype_S8string))
#endif
%% export_def(stringp(obj));

/* test for (VECTOR NIL) */
#ifdef TYPECODES
  #define nil_vector_p(obj)  \
    (typecode(obj) == vector_type \
     && (Iarray_flags(obj) & arrayflags_atype_mask) == Atype_NIL \
    )
#else
  /* cases: Rectype_Svector, Rectype_vector */
  #define nil_vector_p(obj)  \
    (varobjectp(obj) \
     && (Record_type(obj) == Rectype_vector \
         && (Iarray_flags(obj) & arrayflags_atype_mask) == Atype_NIL \
    )   )
#endif

/* Test for simple-bit[n]-vector */
#ifdef TYPECODES
  #define simple_bit_vector_p(atype,obj)  \
    (typecode(obj) == Array_type_simple_bit_vector(atype))
#else
  /* cases: Rectype_Sb[2^n]vector */
  #define simple_bit_vector_p(atype,obj)  \
    (varobjectp(obj) && (Record_type(obj) == Rectype_Sbvector+(atype)))
#endif
%% export_def(simple_bit_vector_p(atype,obj));

/* Test for bit[n]-vector */
#ifdef TYPECODES
  #define bit_vector_p(atype,obj)  \
    ((typecode(obj) & ~bit(notsimple_bit_t)) == Array_type_simple_bit_vector(atype))
#else
  /* cases: Rectype_Sb[2^n]vector, Rectype_b[2^n]vector */
  #define bit_vector_p(atype,obj)  \
    (varobjectp(obj) \
     && ((Record_type(obj) & ~(Rectype_Sbvector ^ Rectype_bvector)) == (Rectype_Sbvector & Rectype_bvector) + (atype)) \
    )
#endif
%% export_def(bit_vector_p(atype,obj));

/* Test for Array (general) */
#ifdef TYPECODES
  #define arrayp(obj)  \
    ((tint)(typecode(obj) - mdarray_type) <= (tint)(vector_type - mdarray_type))
#else
  /* cases: Rectype_Sbvector, Rectype_Sb[2|4|8|16|32]vector, Rectype_Svector, Rectype_[Imm_]S[8|16|32]string,
          Rectype_bvector, Rectype_b[2|4|8|16|32]vector, Rectype_vector, Rectype_reallocstring, Rectype_string,
          Rectype_mdarray */
  #define arrayp(obj)  \
    (varobjectp(obj) && ((uintB)(Record_type(obj) - Rectype_vector) \
                         <= Rectype_mdarray - Rectype_vector))
#endif
%% export_def(arrayp(obj));

/* Test for Array, that isn't a Vector (type byte %100) */
#ifdef TYPECODES
  #define mdarrayp(obj)  \
    (typecode(obj) == mdarray_type)
#else
  /* cases: Rectype_mdarray */
  #define mdarrayp(obj)  \
    (varobjectp(obj) && (Record_type(obj) == Rectype_mdarray))
#endif

#ifdef TYPECODES
  /* Test for Closure/Structure/Stream/Instance/OtherRecord/LongRecord */
    #define if_recordp(obj,statement1,statement2)  \
      switch (typecode(obj)) {          \
        case_record: statement1; break; \
        default: statement2; break;     \
      }
#else
  /* Test for Lrecord/Srecord/Xrecord */
    #define if_recordp(obj,statement1,statement2)  \
      if (orecordp(obj))                                                     \
        switch (Record_type(obj)) {                                          \
          case Rectype_Sbvector:                                             \
          case Rectype_S8string: case Rectype_Imm_S8string:                  \
          case Rectype_S16string: case Rectype_Imm_S16string:                \
          case Rectype_S32string: case Rectype_Imm_S32string:                \
          case Rectype_Svector:                                              \
          case Rectype_mdarray:                                              \
          case Rectype_bvector: case Rectype_string: case Rectype_vector:    \
          case Rectype_reallocstring:                                        \
          case Rectype_Bignum: case Rectype_Lfloat:                          \
            goto not_record;                                                 \
          default: { statement1 } break;                                     \
        }                                                                    \
      else                                                                   \
        not_record: { statement2 }
#endif

/* Test for Closure */
#ifdef TYPECODES
  #define closurep(obj)  (typecode(obj)==closure_type)
#else
  #define closurep(obj)  \
    (varobjectp(obj) && (Record_type(obj) == Rectype_Closure))
#endif

/* Test for compiled Closure
 The second component of a closure is either a list
 (the Lambdabody for interpreted Closures)
 or a Simple-Bit-Vector (the code vector for compiled Closures). */
#define cclosurep(obj)  \
  (closurep(obj)        \
   && simple_bit_vector_p(Atype_8Bit,TheClosure(obj)->clos_codevec))

/* Test for a function with a code vector produced by %GENERIC-FUNCTION-LAMBDA. */
#define genericlambda_function_p(obj)  \
  (cclosurep(obj) \
   && (TheCodevec(TheClosure(obj)->clos_codevec)->ccv_flags & bit(4)))

/* Test for CLOS-Instance */
#ifdef TYPECODES
  #define instancep(obj)  \
    (typecode(obj)==instance_type                                \
     || (typecode(obj)==closure_type && Closure_instancep(obj)))
#else
  #define instancep(obj)  \
    (varobjectp(obj)                                                  \
     && (Record_type(obj) == Rectype_Instance                         \
         || (Record_type(obj) == Rectype_Closure && Closure_instancep(obj))))
#endif
/* Test for non-funcallable CLOS-Instance */
#ifdef TYPECODES
  #define regular_instance_p(obj)  (typecode(obj)==instance_type)
#else
  #define regular_instance_p(obj)  \
    (varobjectp(obj) && (Record_type(obj) == Rectype_Instance))
#endif
/* Test for funcallable CLOS-Instance */
#define funcallable_instance_p(obj)  \
  (closurep(obj) && Closure_instancep(obj))
%% export_def(instancep(obj));

/* Test for CLOS-class or forward-reference.
 Our CLOS implements all classes as instances of a
 (not necessarily direct) subclass of <class>. */
#define if_potential_class_p(obj,statement1,statement2)  \
  if (instancep(obj)) {                                               \
    {                                                                 \
      var object obj_forwarded = obj;                                 \
      instance_un_realloc(obj_forwarded);                             \
      /*instance_update(obj,obj_forwarded); - not needed since we don't access a slot */ \
     {var object cv = TheInstance(obj_forwarded)->inst_class_version; \
      /* Treat the most frequent cases first, for speed. */           \
      if (eq(cv,O(class_version_standard_class))) /* direct instance of STANDARD-CLASS?  */\
        goto obj##_classp_yes;                                        \
      if (eq(cv,O(class_version_structure_class))) /* direct instance of STRUCTURE-CLASS?  */\
        goto obj##_classp_yes;                                        \
      if (eq(cv,O(class_version_built_in_class))) /* direct instance of BUILT-IN-CLASS?  */\
        goto obj##_classp_yes;                                        \
      /* Now a slow, but general instanceof test. */                  \
      {var object objclas = TheClassVersion(cv)->cv_newest_class;     \
       if (eq(gethash(O(class_potential_class),TheClass(objclas)->all_superclasses,false),nullobj)) \
         goto obj##_classp_no;                                        \
    }}}                                                               \
   obj##_classp_yes: statement1;                                      \
  } else {                                                            \
   obj##_classp_no: statement2;                                       \
  }

/* Test for CLOS-class.
 Our CLOS implements all classes as instances of a
 (not necessarily direct) subclass of <defined-class>. */
#define if_defined_class_p(obj,statement1,statement2)  \
  if (instancep(obj)) {                                               \
    {                                                                 \
      var object obj_forwarded = obj;                                 \
      instance_un_realloc(obj_forwarded);                             \
      /*instance_update(obj,obj_forwarded); - not needed since we don't access a slot */ \
     {var object cv = TheInstance(obj_forwarded)->inst_class_version; \
      /* Treat the most frequent cases first, for speed. */           \
      if (eq(cv,O(class_version_standard_class))) /* direct instance of STANDARD-CLASS?  */\
        goto obj##_classp_yes;                                        \
      if (eq(cv,O(class_version_structure_class))) /* direct instance of STRUCTURE-CLASS?  */\
        goto obj##_classp_yes;                                        \
      if (eq(cv,O(class_version_built_in_class))) /* direct instance of BUILT-IN-CLASS?  */\
        goto obj##_classp_yes;                                        \
      /* Now a slow, but general instanceof test. */                  \
      {var object objclas = TheClassVersion(cv)->cv_newest_class;     \
       if (eq(gethash(O(class_defined_class),TheClass(objclas)->all_superclasses,false),nullobj)) \
         goto obj##_classp_no;                                        \
    }}}                                                               \
   obj##_classp_yes: statement1;                                      \
  } else {                                                            \
   obj##_classp_no: statement2;                                       \
  }

/* Test for Other-Record
 This is not really a type test (because there is no well-defined type
 Other-Record). It's just a precondition for calling Record_type(obj). */
#ifdef TYPECODES
  #define orecordp(obj)  (typecode(obj)==orecord_type)
#else
  #define orecordp(obj)  varobjectp(obj)
#endif
%% export_def(orecordp(obj));

/* Test for Long-Record
 This is not really a type test (because there is no well-defined type
 Long-Record). It's just a precondition for calling Record_type(obj). */
#ifdef TYPECODES
  #define lrecordp(obj)  (typecode(obj)==lrecord_type)
#else
  #define lrecordp(obj)  varobjectp(obj)
#endif

/* Test for Structure */
#ifdef case_structure
  #define structurep(obj)  (typecode(obj)==structure_type)
#else
  #define structurep(obj)  \
    (orecordp(obj) && (Record_type(obj) == Rectype_Structure))
#endif
%% export_def(structurep(obj));

/* Test for Builtin-Stream */
#ifdef case_stream
  #define builtin_stream_p(obj)  (typecode(obj)==stream_type)
#else
  #define builtin_stream_p(obj)  \
    (orecordp(obj) && (Record_type(obj) == Rectype_Stream))
#endif
/* %% export_def(builtin_stream_p(obj)); */

/* Test for Stream */
#define streamp(obj)  \
  (builtin_stream_p(obj) || instanceof(obj,O(class_fundamental_stream)))

/* Test for Package */
#define packagep(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_Package))
%% #if notused
%% export_def(packagep(obj));
%% #endif

/* Test for Hash-Table */
#define hash_table_p(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_Hashtable))

/* Test for Readtable */
#define readtablep(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_Readtable))

/* Test for Pathname */
#define pathnamep(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_Pathname))

/* Test for Logical Pathname */
#define logpathnamep(obj) \
  (orecordp(obj) && (Record_type(obj) == Rectype_Logpathname))

/* Test for Extended Pathname (i.e., Pathname or Logical Pathname)
 define xpathnamep(obj)  (pathnamep(obj) || logpathnamep(obj)) */
#define xpathnamep(obj)                                 \
  (orecordp(obj)                                        \
   && ((Record_type(obj) == Rectype_Pathname)           \
       || (Record_type(obj) == Rectype_Logpathname)))

/* Test for Random-State */
#define random_state_p(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_Random_State))

/* Test for Byte */
#define bytep(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_Byte))

/* Test for Fsubr */
#define fsubrp(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_Fsubr))

/* Test for Loadtimeeval */
#define loadtimeevalp(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_Loadtimeeval))

/* Test for Symbolmacro */
#define symbolmacrop(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_Symbolmacro))

/* Test for GlobalSymbolmacro */
#define globalsymbolmacrop(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_GlobalSymbolmacro))

/* Test for Macro */
#define macrop(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_Macro))

/* Test for FunctionMacro */
#define functionmacrop(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_FunctionMacro))

/* Test for BigReadLabel */
#define big_read_label_p(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_BigReadLabel))

/* Test for Encoding */
#define encodingp(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_Encoding))

/* Test for Fpointer */
#define fpointerp(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_Fpointer))
%% #ifdef FOREIGN
%%   export_def(fpointerp(obj));
%% #endif

/* Test for Faddress */
#define faddressp(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_Faddress))

/* Test for Fvariable */
#define fvariablep(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_Fvariable))

/* Test for Ffunction */
#ifdef DYNAMIC_FFI
  #define ffunctionp(obj)  \
    (orecordp(obj) && (Record_type(obj) == Rectype_Ffunction))
#else
  #define ffunctionp(obj)  ((void)(obj), 0)
#endif

/* Test for Function */
#define functionp(obj) (subrp(obj) || closurep(obj) || ffunctionp(obj))

/* Test for Weakpointer */
#define weakpointerp(obj)  \
  (orecordp(obj) && (Record_type(obj) == Rectype_Weakpointer))

/* test for socket-server and for socket-stream */
#ifdef SOCKET_STREAMS
  #define socket_server_p(obj)  \
    (orecordp(obj) && (Record_type(obj) == Rectype_Socket_Server))
  #define socket_stream_p(obj)  \
    (builtin_stream_p(obj) && (TheStream(obj)->strmtype==strmtype_socket))
#endif

#if defined(MULTITHREAD)
  #define threadp(obj) \
    (orecordp(obj) && (Record_type(obj) == Rectype_Thread))
  #define mutexp(obj) \
    (orecordp(obj) && (Record_type(obj) == Rectype_Mutex))
  #define exemptionp(obj) \
    (orecordp(obj) && (Record_type(obj) == Rectype_Exemption))

#endif

#ifdef YET_ANOTHER_RECORD
  /* Test for Yetanother */
  #define yetanotherp(obj)  \
    (orecordp(obj) && (Record_type(obj) == Rectype_Yetanother))
#endif

/* Test for Character */
#ifdef TYPECODES
  #define charp(obj)  (typecode(obj)==char_type)
#else
  #define charp(obj)  ((as_oint(obj) & ((7 << imm_type_shift) | immediate_bias_mask)) == char_type)
#endif
%% export_def(charp(obj));

#if (base_char_code_limit < char_code_limit)
/* Test for base character */
  #define base_char_p(obj)  \
    ((as_oint(obj) & ~((oint)(bit(base_char_int_len)-1)<<oint_data_shift)) == type_zero_oint(char_type))
#endif

/* Test for SUBR (compiled functional object) */
#ifdef TYPECODES
  #define subrp(obj)  (typecode(obj)==subr_type)
#else
  #ifdef ONE_FREE_BIT_HEAPCODES
    #define subrp(obj)  ((as_oint(obj) & 3) == subr_bias)
    #define immsubrp(obj)  subrp(obj)
  #endif
  #if defined(KERNELVOID32_HEAPCODES) || defined(GENERIC64_HEAPCODES)
    #define subrp(obj)  (orecordp(obj) && (Record_type(obj) == Rectype_Subr))
    #define immsubrp(obj)  false
    #ifdef DEBUG_GCSAFETY
      /* This is used by pgci_pointable, so it cannot use pgci_pointable itself. */
      static inline bool nonimmsubrp (object obj) {
        return (varobjectp(obj)
                && (inside_gc /* Avoid doing memory accesses during GC. */
                    || (varobject_type((Record)(cgci_pointable(obj)-varobject_bias)) == Rectype_Subr)));
      }
    #endif
  #endif
#endif
%% #ifndef TYPECODES
%%   #if defined(KERNELVOID32_HEAPCODES) || defined(GENERIC64_HEAPCODES)
%%     #ifdef DEBUG_GCSAFETY
%%       printf2("static inline bool nonimmsubrp (object obj) { return (varobjectp(obj) && (varobject_type((Record)(cgci_pointable(obj)-%d)) == %d)); }\n",varobject_bias,Rectype_Subr);
%%     #endif
%%   #endif
%% #endif

/* Test for pointer into the STACK (usually at a frame) */
#ifdef TYPECODES
  #define framepointerp(obj)  (typecode(obj)==system_type) /* other cases?? */
#else /* HEAPCODES */
  #if defined(GENERIC64C_HEAPCODES)
    #define framepointerp(obj)  ((as_oint(obj) & 7) == machine_bias) /* other cases?? */
  #else
    #define framepointerp(obj)  ((as_oint(obj) & 3) == machine_bias) /* other cases?? */
  #endif
#endif

#ifndef TYPECODES

  /* Test for Machine-Pointer */
  #ifdef ONE_FREE_BIT_HEAPCODES
    #define machinep(obj)  ((as_oint(obj) & 3) == machine_bias)
  #endif
  #ifdef KERNELVOID32A_HEAPCODES
    #define machinep(obj)  \
      ((as_oint(obj) & 3) == machine_bias            \
       && (as_oint(obj) & 0xE0000000) != 0xC0000000)
  #endif
  #ifdef KERNELVOID32B_HEAPCODES
    #define machinep(obj)  \
      ((as_oint(obj) & 3) == machine_bias    \
       && (0xE0000000 & ~as_oint(obj)) != 0)
  #endif
  #if defined(GENERIC64A_HEAPCODES) || defined(GENERIC64B_HEAPCODES)
    #define machinep(obj)  \
      ((as_oint(obj) & 3) == machine_bias                                \
       && (as_oint(obj) & 0xE000000000000000UL) != 0xC000000000000000UL)
  #endif
  #ifdef GENERIC64C_HEAPCODES
    #define machinep(obj)  ((as_oint(obj) & 7) == machine_bias)
  #endif

  /* Test for Small-Read-Label */
  #define small_read_label_p(obj)  ((as_oint(obj) & ((7 << imm_type_shift) | immediate_bias_mask)) == small_read_label_type)

  /* Test for System-Pointer */
  #define systemp(obj)  ((as_oint(obj) & ((7 << imm_type_shift) | immediate_bias_mask)) == system_type)

#endif

/* Test for real number */
#ifdef TYPECODES
  #define if_realp(obj,statement1,statement2)                           \
    do {                                                                \
      var object obj_from_if_realp = (obj);                             \
      var tint type_from_if_realp = typecode(obj_from_if_realp);        \
      if ( (type_from_if_realp & bit(number_bit_t))                     \
           && !(type_from_if_realp==complex_type) )                     \
        { statement1 } else { statement2 }                              \
    } while(0)
#else
  #define if_realp(obj,statement1,statement2)                           \
    do { if (((as_oint(obj) & ((4 << imm_type_shift) | immediate_bias_mask)) \
              == fixnum_type)                                           \
             || (varobjectp(obj)                                        \
                 && ((uintB)(Record_type(obj)-Rectype_Bignum) <=        \
                     Rectype_Ratio-Rectype_Bignum)))                    \
           { statement1 } else { statement2 }                           \
    } while(0)
#endif

/* Test for rational number */
#ifdef TYPECODES
  #define if_rationalp(obj,statement1,statement2)                        \
    do {                                                                 \
      var object obj_from_if_rationalp = (obj);                          \
      var tint type_from_if_rationalp = typecode(obj_from_if_rationalp); \
      if ((type_from_if_rationalp != complex_type)                       \
           && ((type_from_if_rationalp &                                 \
                ~((fixnum_type|bignum_type|ratio_type|bit(sign_bit_t))   \
                  & ~(fixnum_type&bignum_type&ratio_type)))              \
               == (fixnum_type&bignum_type&ratio_type)))                 \
        { statement1 } else { statement2 }                               \
    } while(0)
#else
  #define if_rationalp(obj,statement1,statement2)                        \
    do { if (((as_oint(obj) & ((6 << imm_type_shift) | immediate_bias_mask))  \
              == fixnum_type)                                            \
             || (varobjectp(obj)                                         \
                 && ((Record_type(obj) == Rectype_Bignum)                \
                     || (Record_type(obj) == Rectype_Ratio))))           \
           { statement1 } else { statement2 }                            \
    } while(0)

#endif

/* Test for Integer */
#ifdef TYPECODES
  #define integerp(obj)  \
    ((typecode(obj) &                                                        \
      ~((fixnum_type|bignum_type|bit(sign_bit_t)) & ~(fixnum_type&bignum_type)) \
     ) == (fixnum_type&bignum_type))
#else
  #define integerp(obj)  \
   (((as_oint(obj) & ((6 << imm_type_shift) | immediate_bias_mask)) == fixnum_type) \
    || (varobjectp(obj) && (Record_type(obj) == Rectype_Bignum)))
#endif
%% export_def(integerp(obj));

/* Test for Fixnum */
#ifdef TYPECODES
  #define fixnump(obj)  ((typecode(obj) & ~bit(sign_bit_t)) == fixnum_type)
#else
  #define fixnump(obj)  ((as_oint(obj) & ((6 << imm_type_shift) | immediate_bias_mask)) == fixnum_type)
#endif
%% export_def(fixnump(obj));

/* Test for Fixnum >=0 */
#ifdef TYPECODES
  #define posfixnump(obj)  (typecode(obj) == fixnum_type)
#else
  #define posfixnump(obj)  ((as_oint(obj) & ((7 << imm_type_shift) | immediate_bias_mask)) == fixnum_type)
#endif
%% export_def(posfixnump(obj));

/* Test for Bignum */
#ifdef TYPECODES
  #define bignump(obj)  ((typecode(obj) & ~bit(sign_bit_t)) == bignum_type)
#else
  #define bignump(obj)  \
    (varobjectp(obj) && (Record_type(obj) == Rectype_Bignum))
#endif
%% export_def(bignump(obj));

/* Test for Bignum >=0 */
#ifdef TYPECODES
  #define posbignump(obj)  (typecode(obj) == bignum_type)
#else
  #define posbignump(obj)  \
    (varobjectp(obj)                         \
     && (Record_type(obj) == Rectype_Bignum) \
     && ((Record_flags(obj) & bit(7)) == 0))
#endif
%% export_def(posbignump(obj));

/* Test for Ratio */
#ifdef TYPECODES
  #define ratiop(obj)  ((typecode(obj) & ~bit(sign_bit_t)) == ratio_type)
#else
  #define ratiop(obj)  (varobjectp(obj) && (Record_type(obj) == Rectype_Ratio))
#endif
%% #if notused
%%   export_def(ratiop(obj));
%% #endif

/* Test for Float */
#ifdef TYPECODES
  #define floatp(obj)  \
    ((typecode(obj) &  \
     ~((sfloat_type|ffloat_type|dfloat_type|lfloat_type|bit(sign_bit_t)) & ~(sfloat_type&ffloat_type&dfloat_type&lfloat_type)) \
     ) == (sfloat_type&ffloat_type&dfloat_type&lfloat_type))
#else
  #define floatp(obj)  \
    (((as_oint(obj) & ((6 << imm_type_shift) | immediate_bias_mask)) == sfloat_type) \
     || (varobjectp(obj)                    \
         && ((uintB)(Record_type(obj)-Rectype_Lfloat) <= Rectype_Ffloat-Rectype_Lfloat)))
#endif
%% #if notused
%%   export_def(floatp(obj));
%% #endif

/* Test for Short-Float */
#ifdef TYPECODES
  #define short_float_p(obj)  ((typecode(obj) & ~bit(sign_bit_t)) == sfloat_type)
#else
  #define short_float_p(obj)  ((as_oint(obj) & ((6 << imm_type_shift) | immediate_bias_mask)) == sfloat_type)
#endif
%% #if notused
%%   export_def(short_float_p(obj));
%% #endif

/* Test for Single-Float */
#ifdef TYPECODES
  #define single_float_p(obj)  ((typecode(obj) & ~bit(sign_bit_t)) == ffloat_type)
#else
  #define single_float_p(obj)  (varobjectp(obj) && (Record_type(obj) == Rectype_Ffloat))
#endif
%% export_def(single_float_p(obj));

/* Test for Double-Float */
#ifdef TYPECODES
  #define double_float_p(obj)  ((typecode(obj) & ~bit(sign_bit_t)) == dfloat_type)
#else
  #define double_float_p(obj)  (varobjectp(obj) && (Record_type(obj) == Rectype_Dfloat))
#endif
%% export_def(double_float_p(obj));

/* Test for Long-Float */
#ifdef TYPECODES
  #define long_float_p(obj)  ((typecode(obj) & ~bit(sign_bit_t)) == lfloat_type)
#else
  #define long_float_p(obj)  (varobjectp(obj) && (Record_type(obj) == Rectype_Lfloat))
#endif
%% #if notused
%%   export_def(long_float_p(obj));
%% #endif

/* Test for Complex */
#ifdef TYPECODES
  #define complexp(obj)  (typecode(obj) == complex_type)
#else
  #define complexp(obj)  (varobjectp(obj) && (Record_type(obj) == Rectype_Complex))
#endif
%% #if notused
%%   export_def(complexp(obj));
%% #endif

/* Test if a real number is >=0: */
#ifdef TYPECODES
  /* define positivep(obj)  ((as_oint(obj) & wbit(sign_bit_o)) == 0) */
  #define positivep(obj)  (!wbit_test(as_oint(obj),sign_bit_o))
  #ifdef WIDE_STRUCT
    #undef positivep
    #define positivep(obj)  ((typecode(obj) & bit(sign_bit_t)) == 0)
  #endif
#else
  #define positivep(obj)  \
    (number_immediatep(obj)                                        \
     ? /* fixnum, sfloat */ (as_oint(obj) & wbit(sign_bit_o)) == 0 \
     : /* bignum, [fdl]float */ (Record_flags(obj) & bit(7)) == 0)
#endif
%% export_def(positivep(obj));


/* switch with typcodes:
 example:
   switch (typecode(obj)) {
     case_symbol: ....
     case_orecord:
       switch (Record_type(obj)) {
         case_Rectype_Symbol_above;
         ...
       }
   } */

#ifdef case_structure
  #define case_Rectype_Structure_above
#else
  #define case_Rectype_Structure_above  \
    case Rectype_Structure: goto case_structure;
#endif

#ifdef case_stream
  #define case_Rectype_Stream_above
#else
  #define case_Rectype_Stream_above  \
    case Rectype_Stream: goto case_stream;
#endif

#ifdef TYPECODES
  #define case_Rectype_Closure_above
  #define case_Rectype_Instance_above
  #define case_Rectype_Sbvector_above
  #define case_Rectype_Sb2vector_above
  #define case_Rectype_Sb4vector_above
  #define case_Rectype_Sb8vector_above
  #define case_Rectype_Sb16vector_above
  #define case_Rectype_Sb32vector_above
  #define case_Rectype_Sstring_above
  #define case_Rectype_Svector_above
  #define case_Rectype_mdarray_above
  #define case_Rectype_obvector_above
  #define case_Rectype_ob2vector_above
  #define case_Rectype_ob4vector_above
  #define case_Rectype_ob8vector_above
  #define case_Rectype_ob16vector_above
  #define case_Rectype_ob32vector_above
  #define case_Rectype_ostring_above
  #define case_Rectype_ovector_above
  #define case_Rectype_Bignum_above
  #define case_Rectype_Lfloat_above
  #define case_Rectype_Dfloat_above
  #define case_Rectype_Ffloat_above
  #define case_Rectype_Ratio_above
  #define case_Rectype_Complex_above
  #define case_Rectype_Symbol_above
  /* Composite cases: */
  #define case_Rectype_string_above
  #define case_Rectype_bvector_above
  #define case_Rectype_b2vector_above
  #define case_Rectype_b4vector_above
  #define case_Rectype_b8vector_above
  #define case_Rectype_b16vector_above
  #define case_Rectype_b32vector_above
  #define case_Rectype_vector_above
  #define case_Rectype_array_above
  #define case_Rectype_number_above
  #define case_Rectype_float_above
  #define case_Rectype_integer_above
#else
  #define case_Rectype_Closure_above  \
    case Rectype_Closure: goto case_closure;
  #define case_Rectype_Instance_above  \
    case Rectype_Instance: goto case_instance;
  #define case_Rectype_Sbvector_above  \
    case Rectype_Sbvector: goto case_sbvector;
  #define case_Rectype_Sb2vector_above  \
    case Rectype_Sb2vector: goto case_sb2vector;
  #define case_Rectype_Sb4vector_above  \
    case Rectype_Sb4vector: goto case_sb4vector;
  #define case_Rectype_Sb8vector_above  \
    case Rectype_Sb8vector: goto case_sb8vector;
  #define case_Rectype_Sb16vector_above  \
    case Rectype_Sb16vector: goto case_sb16vector;
  #define case_Rectype_Sb32vector_above  \
    case Rectype_Sb32vector: goto case_sb32vector;
  #define case_Rectype_Sstring_above  \
    case Rectype_S8string: case Rectype_Imm_S8string: case Rectype_S16string: case Rectype_Imm_S16string: case Rectype_S32string: case Rectype_Imm_S32string: case Rectype_reallocstring: goto case_sstring;
  #define case_Rectype_Svector_above  \
    case Rectype_Svector: goto case_svector;
  #define case_Rectype_mdarray_above  \
    case Rectype_mdarray: goto case_mdarray;
  #define case_Rectype_obvector_above  \
    case Rectype_bvector: goto case_obvector;
  #define case_Rectype_ob2vector_above  \
    case Rectype_b2vector: goto case_ob2vector;
  #define case_Rectype_ob4vector_above  \
    case Rectype_b4vector: goto case_ob4vector;
  #define case_Rectype_ob8vector_above  \
    case Rectype_b8vector: goto case_ob8vector;
  #define case_Rectype_ob16vector_above  \
    case Rectype_b16vector: goto case_ob16vector;
  #define case_Rectype_ob32vector_above  \
    case Rectype_b32vector: goto case_ob32vector;
  #define case_Rectype_ostring_above  \
    case Rectype_string: goto case_ostring;
  #define case_Rectype_ovector_above  \
    case Rectype_vector: goto case_ovector;
  #define case_Rectype_Bignum_above  \
    case Rectype_Bignum: goto case_bignum;
  #define case_Rectype_Lfloat_above  \
    case Rectype_Lfloat: goto case_lfloat;
  #define case_Rectype_Dfloat_above  \
    case Rectype_Dfloat: goto case_dfloat;
  #define case_Rectype_Ffloat_above  \
    case Rectype_Ffloat: goto case_ffloat;
  #define case_Rectype_Ratio_above  \
    case Rectype_Ratio: goto case_ratio;
  #define case_Rectype_Complex_above  \
    case Rectype_Complex: goto case_complex;
  #define case_Rectype_Symbol_above  \
    case Rectype_Symbol: goto case_symbol;
  /* Composite cases: */
  #define case_Rectype_string_above  \
    case Rectype_S8string: case Rectype_Imm_S8string: case Rectype_S16string: case Rectype_Imm_S16string: case Rectype_S32string: case Rectype_Imm_S32string: case Rectype_reallocstring: case Rectype_string: goto case_string;
  #define case_Rectype_bvector_above  \
    case Rectype_Sbvector: case Rectype_bvector: goto case_bvector;
  #define case_Rectype_b2vector_above  \
    case Rectype_Sb2vector: case Rectype_b2vector: goto case_b2vector;
  #define case_Rectype_b4vector_above  \
    case Rectype_Sb4vector: case Rectype_b4vector: goto case_b4vector;
  #define case_Rectype_b8vector_above  \
    case Rectype_Sb8vector: case Rectype_b8vector: goto case_b8vector;
  #define case_Rectype_b16vector_above  \
    case Rectype_Sb16vector: case Rectype_b16vector: goto case_b16vector;
  #define case_Rectype_b32vector_above  \
    case Rectype_Sb32vector: case Rectype_b32vector: goto case_b32vector;
  #define case_Rectype_vector_above  \
    case Rectype_Svector: case Rectype_vector: goto case_vector;
  #define case_Rectype_array_above                            \
    case Rectype_S8string: case Rectype_Imm_S8string:         \
    case Rectype_S16string: case Rectype_Imm_S16string:       \
    case Rectype_S32string: case Rectype_Imm_S32string:       \
    case Rectype_reallocstring: case Rectype_string:          \
    case Rectype_Sbvector: case Rectype_bvector:              \
    case Rectype_Sb2vector: case Rectype_b2vector:            \
    case Rectype_Sb4vector: case Rectype_b4vector:            \
    case Rectype_Sb8vector: case Rectype_b8vector:            \
    case Rectype_Sb16vector: case Rectype_b16vector:          \
    case Rectype_Sb32vector: case Rectype_b32vector:          \
    case Rectype_Svector: case Rectype_vector:                \
    case Rectype_mdarray:                                     \
      goto case_array;
  #define case_Rectype_number_above  /* don't forget immediate_number_p */ \
    case Rectype_Complex: case Rectype_Ratio:                      \
    case Rectype_Ffloat: case Rectype_Dfloat: case Rectype_Lfloat: \
    case Rectype_Bignum:                                           \
      goto case_number;
  #define case_Rectype_float_above  /* don't forget short_float_p */ \
    case Rectype_Ffloat: case Rectype_Dfloat: case Rectype_Lfloat: \
      goto case_float;
  #define case_Rectype_integer_above  /* don't forget fixnump */ \
    case Rectype_Bignum: goto case_integer;
#endif

#if defined(TYPECODES) || defined(ONE_FREE_BIT_HEAPCODES)
  #define case_Rectype_Subr_above
#else /* KERNELVOID32_HEAPCODES || GENERIC64_HEAPCODES */
  #define case_Rectype_Subr_above  \
    case Rectype_Subr: goto case_subr;
#endif


/* ################# Declarations for the arithmetics #######################

 Type hierachy :
 Number (N) =
    Real (R) =
       Float (F) =
          Short float (SF)
          Single float (FF)
          Double float (DF)
          Long float (LF)
       Rational (RA) =
          Integer (I) =
             Fixnum (FN)
             Bignum (BN)
          Ratio (RT)
    Complex (C)

 Type field:
 Bytes for testing whether it's that type (Bit set, is yes).
 _bit_t to test in the type byte (tint)
 _bit_o to test in the object (oint) */

#ifndef NUMBER_BITS_INVERTED
  #define number_wbit_test  wbit_test
#else
  #define number_wbit_test  !wbit_test
#endif

#ifdef TYPECODES

/* see above:
 #define number_bit_t  4  -- set only for numbers
 #define number_bit_o  (number_bit_t+oint_type_shift)  -- set only for numbers

 float_bit:
 in a number : Bit set, if it's a Float.
               Bit unset, if it's a rational or complex number.
 (For NUMBER_BITS_INVERTED it's exactly the other way around.)
 #define float_bit_t      1
 #define float_bit_o      (float_bit_t+oint_type_shift)

 float1_bit:
 In a floating-point: discriminates further: */
#ifndef NUMBER_BITS_INVERTED
/* Float-Bit   1 2
             0 0    Short Float (SF)
             0 1    Single Float (FF)
             1 0    Double Float (DF)
             1 1    Long Float (LF) */
#else
/* Float-Bit   1 2
             0 0    Long Float (LF)
             0 1    Double Float (DF)
             1 0    Single Float (FF)
             1 1    Short Float (SF) */
#endif
/* #define float1_bit_t     3
 #define float1_bit_o     (float1_bit_t+oint_type_shift)
 #define float2_bit_t     2
 #define float2_bit_o     (float2_bit_t+oint_type_shift) */

/* ratio_bit:
 For rational numbers: Bit set , if it's a real fraction.
                       Bit unset, if it's an Integer.
 (For NUMBER_BITS_INVERTED it's exactly the other way around..)
 #define ratio_bit_t      3
 #define ratio_bit_o      (ratio_bit_t+oint_type_shift) */

/* bignum_bit:
 For Integers:     Bit set, if it's a Bignum.
                   Bit unset, if it's a Fixnum.
 (For NUMBER_BITS_INVERTED it's exactly the other way around..)
 #define bignum_bit_t     2
 #define bignum_bit_o     (bignum_bit_t+oint_type_shift) */

/* vorz_bit: (sign bit)
 For Reals:
 returns the sign of the number.
 Bit set, if number < 0,
 Bit unset, if number >=0. */
  #define vorz_bit_t       sign_bit_t
                           /* should be = 0, so the sign-extend
                            is easier for Fixnums. */
  #define vorz_bit_o       (vorz_bit_t+oint_type_shift)

#endif

/* return the sign of a real number (0 if >=0, -1 if <0) */
#ifdef TYPECODES
  #if (vorz_bit_o<32) && !defined(WIDE_STRUCT)
    #define R_sign(obj)  ((signean)sign_of_sint32( (sint32)((uint32)as_oint(obj) << (31-vorz_bit_o)) ))
  #else
    /* define R_sign(obj)  ((signean)sign_of_sint32( (sint32)(uint32)(as_oint(obj) >> (vorz_bit_o-31)) )) */
    #define R_sign(obj)  ((signean)sign_of_sint32( (sint32)((uint32)typecode(obj) << (31-vorz_bit_t)) ))
  #endif
#else
  #define R_sign(obj)  ((signean)sign_of_sint32(_R_sign(obj)))
  #define _R_sign(obj)  \
    (number_immediatep(obj)                                         \
     ? /* fixnum, sfloat */ (sint32)as_oint(obj) << (31-sign_bit_o) \
     : /* [fdl]float */ (sint32)(sintB)Record_flags(obj))
#endif

/* Gives the sign of a Fixnum/Bignum/Ratio/
 Short-/Single-/Double-/Long-Float. */
#ifdef TYPECODES
  #define FN_sign(obj)  R_sign(obj)
  #define BN_sign(obj)  R_sign(obj)
  #define RT_sign(obj)  R_sign(obj)
  #define SF_sign(obj)  R_sign(obj)
  #define FF_sign(obj)  R_sign(obj)
  #define DF_sign(obj)  R_sign(obj)
  #define LF_sign(obj)  R_sign(obj)
#else
  #define FN_sign(obj)  \
    ((signean)sign_of_sint32((sint32)as_oint(obj) << (31-sign_bit_o)))
  #define BN_sign(obj)  \
    ((signean)sign_of_sint32((sint32)(sintB)Record_flags(obj)))
  #define RT_sign(obj)  \
    ((signean)sign_of_sint32((sint32)(sintB)Record_flags(obj)))
  #define SF_sign(obj)  \
    ((signean)sign_of_sint32((sint32)as_oint(obj) << (31-sign_bit_o)))
  #define FF_sign(obj)  \
    ((signean)sign_of_sint32((sint32)(sintB)Record_flags(obj)))
  #define DF_sign(obj)  \
    ((signean)sign_of_sint32((sint32)(sintB)Record_flags(obj)))
  #define LF_sign(obj)  \
    ((signean)sign_of_sint32((sint32)(sintB)Record_flags(obj)))
#endif

/* Checks whether two real numbers have the same sign: */
#ifdef TYPECODES
  #define same_sign_p(obj1,obj2)  \
    (wbit_test(as_oint(obj1)^as_oint(obj2),vorz_bit_o)==0)
#else
  #define same_sign_p(obj1,obj2)  \
    ((sint32)(_R_sign(obj1) ^ _R_sign(obj2)) >= 0)
#endif


/* Type test macros:
 (Return /=0, if satisfied. Prefix 'm', if argument is in memory) */

/* Tests an objects whether it's a number: (see above)
 define numberp(obj)  ... */

/* Tests a number whether it's a Float. */
#ifdef TYPECODES
  #ifndef NUMBER_BITS_INVERTED
    /* define N_floatp(obj)  ( as_oint(obj) & wbit(float_bit_o) ) */
    #define N_floatp(obj)  (wbit_test(as_oint(obj),float_bit_o))
  #else
    #define N_floatp(obj)  (!wbit_test(as_oint(obj),float_bit_o))
  #endif
#else
  #define N_floatp(obj)  floatp(obj)
#endif

/* Tests a number whether it's an Integer. */
#ifdef TYPECODES
  #ifndef NUMBER_BITS_INVERTED
    #define N_integerp(obj)  (!( as_oint(obj) & (wbit(float_bit_o)|wbit(ratio_bit_o)) ))
  #else
    #define N_integerp(obj)  (!( (wbit(float_bit_o)|wbit(ratio_bit_o)) & ~as_oint(obj) ))
  #endif
#else
  #define N_integerp(obj)  integerp(obj)
#endif

/* Tests a real number whether it's rational. */
#ifdef TYPECODES
  #ifndef NUMBER_BITS_INVERTED
    /* define R_rationalp(obj)  (!( as_oint(obj) & wbit(float_bit_o) )) */
    #define R_rationalp(obj)  (!wbit_test(as_oint(obj),float_bit_o))
  #else
    #define R_rationalp(obj)  (wbit_test(as_oint(obj),float_bit_o))
  #endif
#else
  #define R_rationalp(obj)  (!floatp(obj))
#endif

/* Tests a real number whether it's a Float. */
#ifdef TYPECODES
  #ifndef NUMBER_BITS_INVERTED
    /* define R_floatp(obj)  ( as_oint(obj) & wbit(float_bit_o) ) */
    #define R_floatp(obj)  (wbit_test(as_oint(obj),float_bit_o))
  #else
    #define R_floatp(obj)  (!wbit_test(as_oint(obj),float_bit_o))
  #endif
#else
  #define R_floatp(obj)  floatp(obj)
#endif

/* Tests a real number whether it's <0. */
#ifdef TYPECODES
  /* define R_minusp(obj)  ( as_oint(obj) & wbit(vorz_bit_o) ) */
  #define R_minusp(obj)  (wbit_test(as_oint(obj),vorz_bit_o))
#else
  #define R_minusp(obj)  (!positivep(obj))
#endif
%% export_def(R_minusp(obj));

/* Tests a rational number whether it's an Integer. */
#ifdef TYPECODES
  #ifndef NUMBER_BITS_INVERTED
    /* define RA_integerp(obj)  (!( as_oint(obj) & wbit(ratio_bit_o) )) */
    #define RA_integerp(obj)  (!wbit_test(as_oint(obj),ratio_bit_o))
  #else
    #define RA_integerp(obj)  (wbit_test(as_oint(obj),ratio_bit_o))
  #endif
#else
  #define RA_integerp(obj)  (!ratiop(obj))
#endif

/* Tests a rational number whether it's a fraction. */
#ifdef TYPECODES
  #ifndef NUMBER_BITS_INVERTED
    /* define RA_ratiop(obj)  ( as_oint(obj) & wbit(ratio_bit_o) ) */
    #define RA_ratiop(obj)  (wbit_test(as_oint(obj),ratio_bit_o))
  #else
    #define RA_ratiop(obj)  (!wbit_test(as_oint(obj),ratio_bit_o))
  #endif
#else
  #define RA_ratiop(obj)  ratiop(obj)
#endif

/* Tests an Integer whether it's a Bignum. */
#ifndef NUMBER_BITS_INVERTED
  /* define I_bignump(obj)  ( as_oint(obj) & wbit(bignum_bit_o) ) */
  #define I_bignump(obj)  (wbit_test(as_oint(obj),bignum_bit_o))
#else
  #define I_bignump(obj)  (!wbit_test(as_oint(obj),bignum_bit_o))
#endif

/* Tests an Integer whether it's a Fixnum. */
#ifndef NUMBER_BITS_INVERTED
  /* define I_fixnump(obj)  (!( as_oint(obj) & wbit(bignum_bit_o) )) */
  #define I_fixnump(obj)  (!wbit_test(as_oint(obj),bignum_bit_o))
#else
  #define I_fixnump(obj)  (wbit_test(as_oint(obj),bignum_bit_o))
#endif

/* Tests a Fixnum whether it is >=0. */
#ifdef TYPECODES
  #define FN_positivep(obj)  positivep(obj)
#else
  #define FN_positivep(obj)  ((as_oint(obj) & wbit(sign_bit_o)) == 0)
#endif
%% export_def(FN_positivep(obj));

/* Tests a Bignum whether it is >=0. */
#ifdef TYPECODES
  #define BN_positivep(obj)  positivep(obj)
#else
  #define BN_positivep(obj)  ((Record_flags(obj) & bit(7)) == 0)
#endif
%% export_def(BN_positivep(obj));

/* Tests a number whether it's a real number */
#define N_realp(obj)  (!complexp(obj))

/* Tests a number whether it's a complex number */
#define N_complexp(obj)  complexp(obj)

/* Tests two Integers whether both are Bignum. */
#ifndef NUMBER_BITS_INVERTED
  #define I_I_bignums_p(obj1,obj2)  \
    (wbit_test(as_oint(obj1)&as_oint(obj2),bignum_bit_o))
#else
  #define I_I_bignums_p(obj1,obj2)  \
    (!wbit_test(as_oint(obj1)|as_oint(obj2),bignum_bit_o))
#endif

/* Tests for an Integer from a given range.
 obj should be a variable */
#define uint1_p(obj)  \
  ((as_oint(obj) & ~((oint)0x01 << oint_data_shift)) == as_oint(Fixnum_0))
#define uint2_p(obj)  \
  ((as_oint(obj) & ~((oint)0x03 << oint_data_shift)) == as_oint(Fixnum_0))
#define uint4_p(obj)  \
  ((as_oint(obj) & ~((oint)0x0F << oint_data_shift)) == as_oint(Fixnum_0))
#define uint8_p(obj)  \
  ((as_oint(obj) & ~((oint)0xFF << oint_data_shift)) == as_oint(Fixnum_0))
#define sint8_p(obj)  \
  (((as_oint(obj) ^ (FN_positivep(obj) ? 0 : as_oint(Fixnum_minus1)^as_oint(Fixnum_0))) & ~((oint)0x7F << oint_data_shift)) == as_oint(Fixnum_0))
#define uint16_p(obj)  \
  ((as_oint(obj) & ~((oint)0xFFFF << oint_data_shift)) == as_oint(Fixnum_0))
#define sint16_p(obj)  \
  (((as_oint(obj) ^ (FN_positivep(obj) ? 0 : as_oint(Fixnum_minus1)^as_oint(Fixnum_0))) & ~((oint)0x7FFF << oint_data_shift)) == as_oint(Fixnum_0))
#if (oint_data_len>=32)
  #define uint32_p(obj)  \
    ((as_oint(obj) & ~((oint)0xFFFFFFFFUL << oint_data_shift)) == as_oint(Fixnum_0))
#else
  #define uint32_p(obj)  \
    (posfixnump(obj) \
     || (posbignump(obj) \
         && (Bignum_length(obj) <= ceiling(33,intDsize)) \
         && ((Bignum_length(obj) < ceiling(33,intDsize)) \
             || (TheBignum(obj)->data[0] < (uintD)bit(32%intDsize)))))
#endif
#if (oint_data_len>=31)
  #define sint32_p(obj)  \
    (((as_oint(obj) ^ (FN_positivep(obj) ? 0 : as_oint(Fixnum_minus1)^as_oint(Fixnum_0))) & ~((oint)0x7FFFFFFFUL << oint_data_shift)) == as_oint(Fixnum_0))
#else
  #define sint32_p(obj)  \
    (fixnump(obj) \
     || (bignump(obj) \
         && (Bignum_length(obj) <= ceiling(32,intDsize)) \
         && ((Bignum_length(obj) < ceiling(32,intDsize)) \
             || ((TheBignum(obj)->data[0] ^ (BN_positivep(obj) ? (uintD)0 : ~(uintD)0)) < (uintD)bit(31%intDsize)))))
#endif
#define uint64_p(obj)  \
  (posfixnump(obj) \
   || (posbignump(obj) \
       && (Bignum_length(obj) <= ceiling(65,intDsize)) \
       && ((Bignum_length(obj) < ceiling(65,intDsize)) \
           || (TheBignum(obj)->data[0] < (uintD)bit(64%intDsize)))))
#define sint64_p(obj)  \
  (fixnump(obj) \
   || (bignump(obj) \
       && (Bignum_length(obj) <= ceiling(64,intDsize)) \
       && ((Bignum_length(obj) < ceiling(64,intDsize)) \
           || ((TheBignum(obj)->data[0] ^ (BN_positivep(obj) ? (uintD)0 : ~(uintD)0)) < (uintD)bit(63%intDsize)))))
#if (int_bitsize==16)
  #define uint_p  uint16_p
  #define sint_p  sint16_p
#else /* (int_bitsize==32) */
  #define uint_p  uint32_p
  #define sint_p  sint32_p
#endif
#if (long_bitsize==32)
  #define ulong_p  uint32_p
  #define slong_p  sint32_p
#else /* (long_bitsize==64) */
  #define ulong_p  uint64_p
  #define slong_p  sint64_p
#endif
%% export_def(uint8_p(obj));
%% export_def(sint8_p(obj));
%% export_def(uint16_p(obj));
%% export_def(sint16_p(obj));
%% export_def(uint32_p(obj));
%% export_def(sint32_p(obj));
%% export_def(uint64_p(obj));
%% export_def(sint64_p(obj));
%% export_def(uint_p);
%% export_def(sint_p);
%% export_def(ulong_p);
%% export_def(slong_p);


/* ####################### TIMEBIBL in TIME.D #############################

 (* 25567 24 60 60) => 2208988800
 the number of seconds from 1900-01-01 to 1970-01-01 */
#define UNIX_LISP_TIME_DIFF 2208988800UL
%% export_def(UNIX_LISP_TIME_DIFF);

/* Type which is used for 'Internal Time': */
#ifdef TIME_UNIX
typedef struct {
  uintL tv_sec;    /* number of seconds since 1.1.1970 00:00 GMT,
                      'uintL' for tv_sec is good for 136 years. */
  uintL tv_usec;   /* additional microseconds */
} internal_time_t;
  #define ticks_per_second  1000000UL /* 1 Tick = 1 mu-sec */
  #define sub_internal_time(x,y, z)   /* z:=x-y */ \
    do { (z).tv_sec = (x).tv_sec - (y).tv_sec;                \
      if ((x).tv_usec < (y).tv_usec)                          \
        { (x).tv_usec += ticks_per_second; (z).tv_sec -= 1; } \
      (z).tv_usec = (x).tv_usec - (y).tv_usec;                \
    } while(0)
  #define add_internal_time(x,y, z)   /* z:=x+y */ \
    do { (z).tv_sec = (x).tv_sec + (y).tv_sec;                \
      (z).tv_usec = (x).tv_usec + (y).tv_usec;                \
      if ((z).tv_usec >= ticks_per_second)                    \
        { (z).tv_usec -= ticks_per_second; (z).tv_sec += 1; } \
    } while(0)
#endif
#ifdef TIME_WIN32
typedef /* struct _FILETIME { DWORD dwLowDateTime; DWORD dwHighDateTime; } */
  FILETIME /* number of 0.1 mu-sec since 1.1.1601 00:00 GMT. */
  internal_time_t;
  #define ticks_per_second  10000000UL /* 1 Tick = 0.1 mu-sec */
  #define sub_internal_time(x,y, z)    /* z:=x-y */ \
    do { (z).dwHighDateTime = (x).dwHighDateTime - (y).dwHighDateTime;      \
      if ((x).dwLowDateTime < (y).dwLowDateTime) { (z).dwHighDateTime -= 1;}\
      (z).dwLowDateTime = (x).dwLowDateTime - (y).dwLowDateTime;            \
    } while(0)
  #define add_internal_time(x,y, z)    /* z:=x+y */ \
    do { (z).dwHighDateTime = (x).dwHighDateTime + (y).dwHighDateTime;      \
      (z).dwLowDateTime = (x).dwLowDateTime + (y).dwLowDateTime;            \
      if ((z).dwLowDateTime < (x).dwLowDateTime) { (z).dwHighDateTime += 1;}\
    } while(0)
#endif

/* UP: yields the real-time
 get_real_time()
 < internal_time_t* result: absolute time */
  extern void get_real_time (internal_time_t*);
/* is used by LISPARIT */

/* UP: Yields the run-time
 get_running_times(&timescore);
 < timescore.runtime:  Run-time since LISP-system-start (in Ticks)
 < timescore.realtime: Real-time since LISP-system-start (in Ticks)
 < timescore.gctime:   GC-Time since LISP-system-start (in Ticks)
 < timescore.gccount:  Number of GC's since LISP-system-start
 < timescore.gcfreed:  Size of the space reclaimed by the GC's so far*/
typedef struct {
  internal_time_t runtime;
  internal_time_t realtime;
  internal_time_t gctime;
  uintL gccount;
  uintL2 gcfreed;
} timescore_t;
extern void get_running_times (timescore_t*);
/* is used by TIME */

/* Converts an internal_time_t to a Lisp integer.
 internal_time_to_I(&it) */
extern object internal_time_to_I (const internal_time_t* tp);
/* used by TIME, DEBUG */

/* UP: yields the run-time
 get_thread_run_time(&runtime, thread);
 > thread: thread for which to obtain info (nullobj for process wide)
 < runtime: Run-time (in Ticks)
 < returns true if successful (may fail in MT) */
extern bool get_thread_run_time (internal_time_t* runtime, object thread);
#define get_run_time(runtime) get_thread_run_time(runtime, nullobj)
/* is used by SPVW */

/* Time in decoded-time: */
typedef struct {
  object seconds;
  object minutes;
  object hours;
  object day;
  object month;
  object year;
} decoded_time_t;

#ifdef UNIX
/* UP: Converts the system-time-format into Decoded-Time.
 convert_time(&time,&timepoint);
 > time_t time: time in the system-time-format
 < timepoint.seconds, timepoint.minutes, timepoint.hours,
   timepoint.day, timepoint.month, timepoint.year, each a Fixnum */
  extern void convert_time (const time_t* time, decoded_time_t* timepoint);
/* is used by PATHNAME */
#endif
#ifdef WIN32_NATIVE
/* UP: Converts the system-time-format into Decoded-Time.
 convert_time(&time,&timepoint);
 > FILETIME time: time in the system-time-format
 < timepoint.seconds, timepoint.minutes, timepoint.hours,
   timepoint.day, timepoint.month, timepoint.year, each a Fixnum */
  extern void convert_time (const FILETIME* time, decoded_time_t* timepoint);
/* is used by PATHNAME */
#endif

#ifdef UNIX
/* UP: Converts the system time-format into Universal-Time.
 convert_time_to_universal(&time)
 > time_t time: time in the system time-format
 < result: integer denoting the seconds since 1900-01-01 00:00 GMT
 can trigger GC */
  extern maygc object convert_time_to_universal (const time_t* time);
/* is used by PATHNAME */
#endif
#ifdef WIN32_NATIVE
/* UP: converts the system time-format into Universal-Time.
 convert_time_to_universal(&time)
 > FILETIME time: Time in the system-time-format
 < result: integer denoting the seconds since 1900-01-01 00:00 GMT
 can trigger GC */
  extern maygc object convert_time_to_universal (const FILETIME* time);
/* is used by PATHNAME */
#endif
%% #ifdef UNIX
%%   exportF(object,convert_time_to_universal,(const time_t* time));
%% #endif
%% #ifdef WIN32_NATIVE
%%   exportF(object,convert_time_to_universal,(const FILETIME* time));
%% #endif

#ifdef UNIX
/* the inverse of convert_time_to_universal() */
extern void convert_time_from_universal (object universal, time_t* time);
#endif
#ifdef WIN32_NATIVE
/* the inverse of convert_time_to_universal() */
extern void convert_time_from_universal (object universal, FILETIME* time);
#endif
%% #ifdef UNIX
%%   exportF(void,convert_time_from_universal,(object universal, time_t* time));
%% #endif
%% #ifdef WIN32_NATIVE
%%   exportF(void,convert_time_from_universal,(object universal, FILETIME* time));
%% #endif

/* UP: Initializes the thread or global time variables. */
extern void init_time ();
/* is used by SPVW */


/* ####################### SPVWBIBL for SPVW.D #############################

                          The Stacks
                          ==========

Two Stacks are being used :
  - the C-program stack (Stackpointer SP = Register A7),
  - the LISP-Stack (Stackpointer STACK).
All calls of sub-programs are done through BSR/JSR via the program stack;
it's also used to temporarily store data, that is not a LISP-object.
The LISP-Stack is used to store frames and for the temporary storage
of LISP-objects.
For both stacks the limits of growth are controlled by the memory management
and the following macros:
  check_SP();             tests the program stack for overflow
  check_STACK();          tests the LISP-Stack for overflow
  get_space_on_STACK(n);  tests, whether there are still D0.L
                          Bytes free on the LISP-Stack
Basically only long words may be stored on the LISP-Stack.
If FRAME_BIT is set, it's the lower end of a frame;
this long word is a pointer above the Frame, together with a
Frame-type-Byte; if SKIP2_BIT is unset in it, the longword above
it is not a LISP-object.
All other long words on the LISP-Stack are LISP-objects.

 machine stack: SP
 SP() returns the current value of the  SP.
 setSP(adresse); sets the SP to a given value. Extremely dangerous!
 FAST_SP defined, if SP-accesses are fast. */
#if defined(GNU) && !(__APPLE_CC__ > 1) && !defined(__clang__)
  /* definition of the register, in which the SP resides. */
  #ifdef M68K
    #define SP_register "sp"  /* %sp = %a7 */
  #endif
  #ifdef SPARC
    #define SP_register "%sp"  /* %sp = %o6 */
  #endif
  #ifdef HPPA
    #define SP_register "%r30"  /* %sp = %r30 */
  #endif
  #ifdef MIPS
    #define SP_register "$sp"  /* $sp = $29 */
  #endif
  #ifdef POWERPC
    #define SP_register "r1"
  #endif
  #ifdef ARM
    #define SP_register "%sp"  /* %sp = %r13 */
  #endif
  #ifdef DECALPHA
    #define SP_register "$30"  /* $sp = $30 */
  #endif
  #ifdef I80386
    #define SP_register "%esp"
  #endif
  #ifdef IA64
    #define SP_register "r12"
  #endif
  #ifdef AMD64
    #define SP_register "%rsp"
  #endif
  #ifdef S390
    #define SP_register "15"
  #endif
  #ifdef RISCV64
    #define SP_register "sp"
  #endif
#endif
#if (defined(GNU) || defined(INTEL)) && !defined(NO_ASM)
  /* Assembler-instruction that copies the SP-register into a variable. */
  #ifdef M68K
    #ifdef __REGISTER_PREFIX__ /* GNU C Version >= 2.4 has %/ and __REGISTER_PREFIX__ */
      /* But the value of __REGISTER_PREFIX__ is useless, because we might be
       cross-compiling. */
      #define REGISTER_PREFIX  "%/"
    #else
      #define REGISTER_PREFIX  "" /* or "%%", depends on the assembler that's being used */
    #endif
    #define ASM_get_SP_register(resultvar)  ("movel "REGISTER_PREFIX"sp,%0" : "=g" (resultvar) : )
  #endif
  #ifdef SPARC
    #ifdef SPARC64
      #define ASM_get_SP_register(resultvar)  ("add %%sp,2048,%0" : "=r" (resultvar) : )
    #else
      #define ASM_get_SP_register(resultvar)  ("mov %%sp,%0" : "=r" (resultvar) : )
    #endif
  #endif
  #ifdef HPPA
    #define ASM_get_SP_register(resultvar)  ("copy %%r30,%0" : "=r" (resultvar) : )
  #endif
  #ifdef MIPS
    #define ASM_get_SP_register(resultvar)  ("move\t%0,$sp" : "=r" (resultvar) : )
  #endif
  #ifdef POWERPC
    #define ASM_get_SP_register(resultvar)  ("mr %0,r1" : "=r" (resultvar) : )
  #endif
  #ifdef ARM
    #define ASM_get_SP_register(resultvar)  ("mov\t%0, sp" : "=r" (resultvar) : )
  #endif
  #ifdef DECALPHA
    #define ASM_get_SP_register(resultvar)  ("bis $30,$30,%0" : "=r" (resultvar) : )
  #endif
  #ifdef I80386
    #define ASM_get_SP_register(resultvar)  ("movl %%esp,%0" : "=g" (resultvar) : )
  #endif
  #ifdef IA64
    #define ASM_get_SP_register(resultvar)  ("mov %0 = r12" : "=r" (resultvar) : )
  #endif
  #ifdef AMD64
    #define ASM_get_SP_register(resultvar)  ("movq %%rsp,%0" : "=g" (resultvar) : )
  #endif
  #ifdef S390
    #define ASM_get_SP_register(resultvar)  ("lr %0,%%r15" : "=r" (resultvar) : )
  #endif
  #ifdef RISCV64
    #define ASM_get_SP_register(resultvar)  ("mv %0,sp" : "=r" (resultvar) : )
  #endif
#endif
#if defined(GNU) && defined(M68K) && !defined(NO_ASM)
  /* Access to a global register-"variable" SP */
  #define SP()  \
    ({var aint __SP;                                                          \
      __asm__ __volatile__ ("movel "REGISTER_PREFIX"sp,%0" : "=g" (__SP) : ); \
      __SP;                                                                   \
     })
  #define setSP(adresse)  \
    ({ __asm__ __volatile__ ("movel %0,"REGISTER_PREFIX"sp" : : "g" ((aint)(adresse)) : "sp" ); })
  #define FAST_SP
#elif (defined(GNU) || defined(INTEL)) && defined(I80386) && !defined(NO_ASM)
  /* Access to a register-"variable" %esp */
  #define SP()  \
    ({var aint __SP;                                           \
      __asm__ __volatile__ ("movl %%esp,%0" : "=g" (__SP) : ); \
      __SP;                                                    \
     })
  /* Doesn't work with gcc 3.1 any more. */
  #if (__GNUC__ < 3) || (__GNUC__ == 3 && __GNUC_MINOR__ < 1)
    #define setSP(adresse)  \
      ({ __asm__ __volatile__ ("movl %0,%%esp" : : "g" ((aint)(adresse)) : "sp" ); })
    #define FAST_SP
  #endif
#elif defined(GNU) && defined(SP_register)
  register __volatile__ aint __SP __asm__(SP_register);
  #ifdef SPARC64
    #define SP()  (__SP+2048)
  #else
    #define SP()  __SP
  #endif
  #if defined(SPARC)
    /* We must not do a setSP() here without taking care that
     1. %sp has to pay attention to an alignment of 8 Bytes,
     2. above %sp 92 Bytes have to be kept free (that's where the
        register contents are saved, if a 'register window overflow trap'
        is triggered by a 'save' in a sub-program). */
  #endif
#elif defined(MICROSOFT) && defined(I80386) && !defined(NO_ASM)
  /* access the register %esp */
  #define SP  getSP
  static __inline aint getSP () { __asm mov eax,esp }
  static __inline aint setSP (aint address) { __asm mov esp,address }
#elif (defined(M68K) || defined(SPARC) || defined(MIPS) || (defined(I80386) && !defined(UNIX_MACOSX))) && !defined(NO_SP_ASM)
  /* access functions extern, in assembler */
  #define SP  getSP
  #define getSP asm_getSP
  extern_C void* getSP (void);
  #define setSP asm_setSP
  extern_C void setSP (void* adresse);
#else
  /* access function portable in C */
  #define SP()  getSP()
  extern void* getSP (void);
  #define NEED_OWN_GETSP
#endif
#if defined(stack_grows_down) /* defined(M68K) || defined(I80386) || defined(SPARC) || defined(MIPS) || defined(DECALPHA) || defined(IA64) || defined(AMD64) || defined(S390) || defined(RISCV64) || ... */
  #define SP_DOWN /* SP grows downward */
  #define SPoffset 0 /* top-of-SP ist *(SP+SPoffset) */
#endif
#if defined(stack_grows_up) /* defined(HPPA) || ... */
  #define SP_UP /* SP grows upward */
  #define SPoffset -1 /* top-of-SP ist *(SP+SPoffset) */
#endif
#if (defined(SP_DOWN) && defined(SP_UP)) || (!defined(SP_DOWN) && !defined(SP_UP))
  #error Unknown SP direction -- readjust SP_DOWN/SP_UP!
#endif
/* Derived from that:
 SPint  is the type of the elements on the SP, an Integer type at least as
        wide as uintL and at least as wide as aint resp. void*.
 SP_(n) = (n+1)th longword on the SP.
 _SP_(n) = &SP_(n).
 pushSP(item)  puts a longword on the SP. Synonym: -(SP).
 popSP(item=)  returns item=SP_(0) and takes it off the SP.
 skipSP(n);  takes n long words of the SP. */
typedef uintP  SPint;
%% #ifdef export_unwind_protect_macros
%%  emit_typedef("uintP","SPint");
%% #endif
#ifdef SP_DOWN
  #define skipSPop  +=
  #define SPop      +
#endif
#ifdef SP_UP
  #define skipSPop  -=
  #define SPop      -
#endif
#define _SP_(n)  (((SPint*)SP()) + SPoffset SPop (uintP)(n))
#if !(defined(GNU) && defined(M68K) && !defined(NO_ASM)) /* generally */
  #define SP_(n)  (((SPint*)SP())[SPoffset SPop (uintP)(n)])
  #define skipSP(n)                             \
    do { var register SPint* sp = (SPint*)SP(); \
         sp skipSPop (uintP)(n);                \
         setSP(sp);                             \
    } while(0)
  #define pushSP(item)                                                     \
    do { var register SPint* sp = (SPint*)SP();                            \
         sp skipSPop -1;                                                   \
         setSP(sp); /* First decrease SP (because of a possible interrupt!)  */\
         sp[SPoffset] = (item); /* then insert item as top-of-SP */        \
    } while(0)
  #define popSP(item_assignment)                                        \
    do { var register SPint* sp = (SPint*)SP();                         \
         item_assignment sp[SPoffset]; /* First fetch top-of-SP           */\
         sp skipSPop 1;                                                 \
         setSP(sp); /* then (danger of interrupt!) increase SP            */\
    } while(0)
#endif
#if defined(GNU) && defined(M68K) && !defined(NO_ASM)
  /* With GNU on as 680X0 SP is in a register. Thus access and
   modification of SP are a unit that cannot be interrupted.
   And SP_DOWN as well as SPoffset=0 hold. */
  #define SP_(n)  \
    ({var register uintL __n = sizeof(SPint) * (n); \
      var register SPint __item;                    \
      __asm__ __volatile__ ("movel "REGISTER_PREFIX"sp@(%1:l),%0" : "=g" (__item) : "r" (__n) ); \
      __item;                                       \
     })
  #define skipSP(n)  \
    do { var register uintL __n = sizeof(SPint) * (n);  \
     __asm__ __volatile__ ("addl %0,"REGISTER_PREFIX"sp" : : "g" (__n) : "sp" ); \
    } while(0)
  #define pushSP(item)  \
    do { var register SPint __item = (item); \
     __asm__ __volatile__ ("movel %0,"REGISTER_PREFIX"sp@-" : : "g" (__item) : "sp" ); \
    } while(0)
  #define popSP(item_assignment)  \
    do {  var register SPint __item; \
     __asm__ __volatile__ ("movel "REGISTER_PREFIX"sp@+,%0" : "=r" (__item) : : "sp" ); \
     item_assignment __item;                                                 \
    } while(0)
#endif
/* An sp_jmp_buf is exactly the same as a jmp_buf,
 except that on Irix 6.5 in 32-bit mode, a jmp_buf has alignment 8,
 whereas an SPint only has alignment 4.
 Need to add some padding.
 Then jmpbufsize = sizeof(sp_jmp_buf)/sizeof(SPint). */
#define sp_jmp_buf_incr  (alignof(jmp_buf)>alignof(SPint)?alignof(jmp_buf)-alignof(SPint):0)
#define sp_jmp_buf_to_jmp_buf(x)  (*(jmp_buf*)(((long)&(x)+(long)sp_jmp_buf_incr)&-(long)(alignof(jmp_buf)>alignof(SPint)?alignof(jmp_buf):1)))
#define setjmpspl(x)  setjmpl(sp_jmp_buf_to_jmp_buf(x))
#define longjmpspl(x,y)  longjmpl(sp_jmp_buf_to_jmp_buf(x),y)
#define jmpbufsize  ceiling(sizeof(jmp_buf)+sp_jmp_buf_incr,sizeof(SPint))
typedef SPint sp_jmp_buf[jmpbufsize];
%% #ifdef export_unwind_protect_macros
%%   printf("#define jmpbufsize %d\n",jmpbufsize);
%%   puts("typedef SPint sp_jmp_buf[jmpbufsize];");
%% #endif
/* The initial value of SP() during main(). */
extern void* SP_anchor;
%% #if (defined(GNU) || defined(INTEL)) && defined(I80386) && !defined(NO_ASM)
%%   printf("%s\n","#define SP()  ({aint __SP; __asm__ __volatile__ (\"movl %%esp,%0\" : \"=g\" (__SP) : ); __SP; })");
%% #endif

/* LISP-Stack: STACK */
#if !defined(STACK_register)
  /* a global variable */
  extern  gcv_object_t* STACK;
#else
  /* a global register variable */
  register gcv_object_t* STACK __asm__(STACK_register);
#endif
#if defined(SPARC) && !defined(GNU) && !defined(__SUNPRO_C) && !defined(MULTITHREAD) && (SAFETY < 2)
  /* a global register variable, but access functions externally in assembler */
  #define STACK  _getSTACK()
  extern_C gcv_object_t* _getSTACK (void);
  #define setSTACK(allocation)  /* hem, yuck! */ \
    do { var gcv_object_t* tempSTACK; _setSTACK(temp##allocation); } while(0)
  extern_C void _setSTACK (void* new_STACK);
#else
  #define setSTACK(allocation)  allocation
#endif
#if defined(UNIX) || defined(WIN32) || defined(HYPERSTONE)
  #define STACK_UP /* STACK grows upward */
#endif
#if (defined(STACK_DOWN) && defined(STACK_UP)) || (!defined(STACK_DOWN) && !defined(STACK_UP))
  #error Unknown STACK direction -- readjust STACK_DOWN/STACK_UP!
#endif
%% #if defined(STACK_register)
%%   puts("#ifndef IN_MODULE_CC");
%%   printf("register gcv_object_t* STACK __asm__(\"%s\");\n",STACK_register);
%%   puts("#endif");
%% #elif !defined(STACK)
%%   exportV(gcv_object_t*,STACK);
%% #endif

/* A singly-linked list of all currently active function calls.
   Resides in the C stack. */
struct backtrace_t {
  const struct backtrace_t* bt_next; /* Link to the caller */
  gcv_object_t bt_function;          /* Function or FSUBR being called */
  gcv_object_t *bt_stack;            /* STACK value where the frame area begins */
  int bt_num_arg;                    /* Number of arguments, if known, or -1 */
};
extern void back_trace_check (const struct backtrace_t *bt,
                              const char* label, const char* file, int line);
#ifdef DEBUG_BACKTRACE
#define BT_CHECK(b,l) back_trace_check(b,l,__FILE__,__LINE__)
#else
#define BT_CHECK(b,l)
#endif
#define BT_CHECK1(l)  BT_CHECK(back_trace,l)
%% puts("struct backtrace_t {\n  struct backtrace_t* bt_next;\n  gcv_object_t bt_function;\n  gcv_object_t *bt_stack;\n  int bt_num_arg;\n};");

#if defined(DEBUG_BACKTRACE) && defined(__cplusplus)
struct p_backtrace_t {
  const struct backtrace_t * ba_tr_p;
  p_backtrace_t (void* bt) { ba_tr_p = (struct backtrace_t*)bt; }
  /* assignment should check for circularities */
  p_backtrace_t& operator= (const struct backtrace_t *bt) {
    if (this->ba_tr_p != bt) {
      BT_CHECK(bt,"=: new value");
      BT_CHECK(ba_tr_p,"=: current value");
      this->ba_tr_p = bt;
    }
    return *this;
  };
  /* back_trace->foo means back_trace.ba_tr_p->foo */
  const struct backtrace_t* operator-> () {
    BT_CHECK(ba_tr_p,"->");
    return this->ba_tr_p;
  };
  /* cast p_backtrace_t to struct backtrace_t* */
  operator const struct backtrace_t* () const {
    BT_CHECK(ba_tr_p,"(struct backtrace_t*)");
    return ba_tr_p;
  }
};
#else
typedef const struct backtrace_t* p_backtrace_t;
#endif
%% emit_typedef("struct backtrace_t *","p_backtrace_t");

/* Returns the top-of-frame of a back_trace element. */
extern gcv_object_t* top_of_back_trace_frame (const struct backtrace_t *bt);

#define bt_beyond_stack_p(bt,st) \
  ((bt) != NULL && !((aint)(st) cmpSTACKop (aint)top_of_back_trace_frame(bt)))
/* unwind backtrace to the stack location */
#define unwind_back_trace(bt,st)                                        \
  do { BT_CHECK(bt,"unwind_back_trace");                                \
    while (bt_beyond_stack_p(bt,st))                                    \
      bt = bt->bt_next;                                                 \
  } while(0)

/* Evaluate statement, augmenting back_trace with an activation record for
   the given function.
   stack permits to locate the top-of-frame, namely
     - for FSUBRs:
         stack = top-of-frame - (req + opt + (body-flag ? 1 : 0))
     - for SUBRs:
         stack = top-of-frame - (req + opt + length(keyword-list))
     - for compiled closures:
         stack = top-of-frame - (req + opt + (rest-flag ? 1 : 0) + length(keyword-list))
     - for interpreted closures:
         stack = top-of-frame
 */
#if STACKCHECKS || STACKCHECKC
#define with_saved_back_trace(fun,stack,num_arg,statement)              \
  do {                                                                  \
    p_backtrace_t bt_save = back_trace;                                 \
    struct backtrace_t bt_here;                                         \
    bt_here.bt_next = back_trace;                                       \
    bt_here.bt_function = (fun);                                        \
    bt_here.bt_stack = (stack);                                         \
    bt_here.bt_num_arg = (num_arg);                                     \
    BT_CHECK1("w/s/b/t: before");                                       \
    back_trace = &bt_here;                                              \
    statement;                                                          \
    if (back_trace != &bt_here) abort();                                \
    if (back_trace->bt_next != bt_save) abort();                        \
    BT_CHECK1("w/s/b/t: after");                                        \
    back_trace = back_trace->bt_next;                                   \
  } while(0)
#else
#define with_saved_back_trace(fun,stack,num_arg,statement)              \
  do {                                                                  \
    struct backtrace_t bt_here;                                         \
    bt_here.bt_next = back_trace;                                       \
    bt_here.bt_function = (fun);                                        \
    bt_here.bt_stack = (stack);                                         \
    bt_here.bt_num_arg = (num_arg);                                     \
    back_trace = &bt_here;                                              \
    statement;                                                          \
    back_trace = back_trace->bt_next;                                   \
  } while(0)
#endif
#define with_saved_back_trace_fsubr(fun,statement)  \
  with_saved_back_trace(fun,STACK,-1,statement)
#define with_saved_back_trace_subr(fun,stack,num_arg,statement)  \
  with_saved_back_trace(fun,stack,num_arg,statement)
#define with_saved_back_trace_cclosure(fun,statement)  \
  with_saved_back_trace(fun,STACK,-1,statement)
#define with_saved_back_trace_iclosure(fun,stack,num_arg,statement)  \
  with_saved_back_trace(fun,stack,num_arg,statement)

/* Every call of an external function (or a sequence of those) has to be framed
 with
   begin_call();
 and
   end_call();
 Purpose: The stack, if it resides in a register,
 should be brought to a halfway recent value
 in case of an interrupt during the corresponding timespan.

 If you want to access the STACK while an external function run,
 you have to frame the corresponding code with
   begin_callback();
 and
   end_callback(); */
#ifdef HAVE_SAVED_mv_count
  extern  uintC saved_mv_count;
  #define SAVE_mv_count()     saved_mv_count = mv_count
  #define RESTORE_mv_count()  mv_count = saved_mv_count
#else
  #define SAVE_mv_count()
  #define RESTORE_mv_count()
#endif
#ifdef HAVE_SAVED_value1
  extern  object saved_value1;
  #define SAVE_value1()     saved_value1 = value1
  #define RESTORE_value1()  value1 = saved_value1
#else
  #define SAVE_value1()
  #define RESTORE_value1()
#endif
#ifdef HAVE_SAVED_back_trace
  extern  p_backtrace_t saved_back_trace;
  #define SAVE_back_trace()     saved_back_trace = back_trace
  #define RESTORE_back_trace()  back_trace = saved_back_trace
#else
  #define SAVE_back_trace()
  #define RESTORE_back_trace()
#endif
#define SAVE_GLOBALS()     SAVE_mv_count(); SAVE_value1(); SAVE_back_trace();
#define RESTORE_GLOBALS()  RESTORE_mv_count(); RESTORE_value1(); RESTORE_back_trace();
#if defined(HAVE_SAVED_STACK)
  extern  gcv_object_t* saved_STACK;
  #define begin_call()  SAVE_GLOBALS(); saved_STACK = STACK
  #define end_call()  RESTORE_GLOBALS(); saved_STACK = (gcv_object_t*)NULL
  #define begin_callback()  SAVE_REGISTERS( STACK = saved_STACK; ); end_call()
  #define end_callback()  SAVE_GLOBALS(); RESTORE_REGISTERS( saved_STACK = STACK; )
#else
  #define begin_call()  SAVE_GLOBALS()
  #define end_call()  RESTORE_GLOBALS()
  #define begin_callback()  SAVE_REGISTERS(;); end_call()
  #define end_callback()  SAVE_GLOBALS(); RESTORE_REGISTERS(;)
#endif
%% #ifdef HAVE_SAVED_mv_count
%%   exportV(uintC,saved_mv_count);
%% #endif
%% #ifdef HAVE_SAVED_value1
%%   exportV(object,saved_value1);
%% #endif
%% #ifdef HAVE_SAVED_back_trace
%%   exportV(p_backtrace_t,saved_back_trace);
%% #endif
%% #if defined(HAVE_SAVED_STACK)
%%   exportV(gcv_object_t*,saved_STACK);
%% #endif
%% export_def(begin_call());
%% export_def(end_call());
%% export_def(begin_callback());
%% export_def(end_callback());

/* Every OS-call (or a sequence thereof) has to be framed with
   begin_system_call();
 and
   end_system_call();
 Purpose: The STACK - if it resides in a register -
 should be brought to a halfway recent value,
 if an interrupt happens during the corresponding timespan.
 While a break-semaphore has been set, you don't have to use the macros
 because of that. */
#ifdef NO_ASYNC_INTERRUPTS
  /* NO_ASYNC_INTERRUPTS: if we don't react to asynchronous Interrupts,
     the program can't be interrupted. */
  #define begin_system_call()
  #define end_system_call()
#else
  #define begin_system_call()  begin_call()
  #define end_system_call()  end_call()
#endif
/* The same holds for setjmp()/longjmp(). Here we avoid an unneeded overhead
 if at all possible.
 You don't have to use these macros when a break-semaphore has been
 set. */
#if 0
  /* Disassembly of setjmp() and longjmp() shows, that the STACK-register
   isn't used arbitrarily. */
  #define begin_setjmp_call()
  #define end_setjmp_call()
  #define begin_longjmp_call()
  #define end_longjmp_call()
#elif defined(I80386) && (defined(UNIX_LINUX) || defined(UNIX_GNU))
  /* Disassembly of setjmp() shows, that the STACK-register %ebx
   isn't used arbitrarily. */
  #define begin_setjmp_call()
  #define end_setjmp_call()
  #define begin_longjmp_call()  begin_system_call()
  #define end_longjmp_call()  end_system_call()
#else
  #define begin_setjmp_call()  begin_system_call()
  #define end_setjmp_call()  end_system_call()
  #define begin_longjmp_call()  begin_system_call()
  #define end_longjmp_call()  end_system_call()
#endif
/* The same holds for arithmetics-functions that use the STACK_registers.
 On I80386 (%ebx) these are SHIFT_LOOPS, MUL_LOOPS, DIV_LOOPS. */
#if defined(I80386) && !defined(NO_ARI_ASM) && defined(HAVE_SAVED_STACK)
  #define begin_arith_call()  begin_system_call()
  #define end_arith_call()  end_system_call()
#else
  #define begin_arith_call()
  #define end_arith_call()
#endif
%% export_def(begin_system_call());
%% export_def(end_system_call());

#if defined(MULTITHREAD)
  #define HANDLE_PENDING_INTERRUPTS(thr)       \
    do {                                       \
      if (thr->_pending_interrupts)            \
        handle_pending_interrupts();           \
    } while (0)

  /* acknowledge suspend request and wait for resume */
  #define GC_SAFE_ACK_SUSPEND_REQUEST_()                \
    do {                                                \
      var clisp_thread_t *thr=current_thread();         \
      SET_SP_BEFORE_SUSPEND(thr); /* debug only */      \
      spinlock_release(&thr->_gc_suspend_ack);          \
      thr->_raw_wait_mutex = &thr->_gc_suspend_lock;    \
      xmutex_raw_lock(&thr->_gc_suspend_lock);          \
      spinlock_acquire(&thr->_gc_suspend_ack);          \
      xmutex_raw_unlock(&thr->_gc_suspend_lock);        \
      thr->_raw_wait_mutex = NULL;                      \
      HANDLE_PENDING_INTERRUPTS(thr);                   \
    } while (0)
  /* gc statement is executed in case we have to suspend ourselves
     otherwise no_gc statement is executed. */
  #define GC_SAFE_POINT_IF(gc,no_gc)                    \
    do{                                                 \
      if (spinlock_tryacquire(&(current_thread()->_gc_suspend_request))) \
        {GCTRIGGER();gc;} else {no_gc;}                                 \
    }while(0)
  #define GC_SAFE_POINT() GC_SAFE_POINT_IF(GC_SAFE_ACK_SUSPEND_REQUEST_(), ;)
/* Giving up suspend ack while we are in system call.
   So we can be considered suspended for GC. */
  #define GC_SAFE_REGION_BEGIN() \
    do {                                                    \
      GCTRIGGER();                                          \
      var clisp_thread_t *thr=current_thread();             \
      SET_SP_BEFORE_SUSPEND(thr);  /* debug only */         \
      spinlock_release(&thr->_gc_suspend_ack);              \
    }while(0)
/* following two macroses are workarround for differences between WIN32 and
   POSIX "signal" handling. With POSIX signals we really interrupt thread,
   while with WIN32 threads we should deffer the handling after system call
   returns */
  #ifdef HAVE_SIGNALS /* POSIX_THREADS */
    #define _thr_ptb_(s) s
    #define _thr_pta_(s)
  #else /* WIN32_THREADS */
    #define _thr_ptb_(s)
    #define _thr_pta_(s) s
  #endif
/* If we cannot get the suspend ack lock again - it means there is/was GC -
   so try to wait for it's end if it is not already finished. */
  #define GC_SAFE_REGION_END_i(statement)                 \
    do {                                                  \
      GCTRIGGER();                                        \
      var clisp_thread_t *thr=current_thread();           \
      if (!spinlock_tryacquire(&thr->_gc_suspend_ack)) {  \
        thr->_raw_wait_mutex = &thr->_gc_suspend_lock;    \
        xmutex_raw_lock(&thr->_gc_suspend_lock);          \
        spinlock_acquire(&thr->_gc_suspend_ack);          \
        spinlock_acquire(&thr->_gc_suspend_request);      \
        xmutex_raw_unlock(&thr->_gc_suspend_lock);        \
        thr->_raw_wait_mutex = NULL;                      \
        _thr_ptb_(statement);                             \
      }                                                   \
      _thr_pta_(statement);                               \
    }while(0)
  #define GC_SAFE_REGION_END()  \
    GC_SAFE_REGION_END_i(HANDLE_PENDING_INTERRUPTS(thr))
  #define GC_SAFE_REGION_END_WITHOUT_INTERRUPTS() \
    GC_SAFE_REGION_END_i(;)

#else /* ! MULTITHREAD */
  #define GC_SAFE_POINT_IF(gc,no_gc)
  #define GC_SAFE_POINT()
  #define GC_SAFE_REGION_BEGIN()
  #define GC_SAFE_REGION_END()
#endif

#define begin_blocking_system_call() begin_system_call();GC_SAFE_REGION_BEGIN()
#define end_blocking_system_call() end_system_call();GC_SAFE_REGION_END()

/* when we are in big region that is already marked as system call -
   we would like just to enable GC on some blocking calls */
#define begin_blocking_call() GC_SAFE_REGION_BEGIN()
#define end_blocking_call() GC_SAFE_REGION_END()

#define GC_SAFE_CALL(statement) \
  do {                          \
    begin_blocking_call();      \
    statement;                  \
    end_blocking_call();        \
  } while(0)

#define GC_SAFE_SYSTEM_CALL(statement) \
  do {                                 \
    begin_blocking_system_call();      \
    statement;                         \
    end_blocking_system_call();        \
  } while(0)

%% export_def(begin_blocking_system_call());
%% export_def(end_blocking_system_call());
%% export_def(begin_blocking_call());
%% export_def(end_blocking_call());
%% export_def(GC_SAFE_CALL(statement));
%% export_def(GC_SAFE_SYSTEM_CALL(statement));

#if defined(HAVE_STACK_OVERFLOW_RECOVERY)
  /* Detection of SP-overflow through a Guard-Page or other mechanisms. */
  #define NOCOST_SP_CHECK
#else
  /* The OS is responsible for the SP.
   From where should we get a reasonable value for SP_bound? */
  #define NO_SP_CHECK
#endif

/* Tests for SP-overflow.
 check_SP();            tests for overflow
 check_SP_notUNIX();    dito, except when a temporary overflow doesn't matter */
#define check_SP()  if (SP_overflow()) SP_ueber()
#if !(defined(NO_SP_CHECK) || defined(NOCOST_SP_CHECK))
  #ifdef SP_DOWN
    #define SP_overflow()  ( (aint)SP() < (aint)SP_bound )
  #endif
  #ifdef SP_UP
    #define SP_overflow()  ( (aint)SP() > (aint)SP_bound )
  #endif
#else /* NO_SP_CHECK || NOCOST_SP_CHECK */
  #define SP_overflow()  false
  #ifdef NOCOST_SP_CHECK
    #ifdef WIN32_NATIVE
      #ifdef SP_DOWN
        #define near_SP_overflow()  ( (aint)SP() < (aint)SP_bound+0x1000 )
      #endif
      #ifdef SP_UP
        #define near_SP_overflow()  ( (aint)SP() > (aint)SP_bound-0x1000 )
      #endif
    #else
      extern bool near_SP_overflow (void);
    #endif
  #endif
#endif
extern  void* SP_bound;
extern _Noreturn void SP_ueber (void);
#ifdef UNIX
  #define check_SP_notUNIX()
#else
  #define check_SP_notUNIX()  check_SP()
#endif

/* Tests for STACK-overflow.
 check_STACK(); */
#define check_STACK()  if (STACK_overflow()) STACK_ueber()
#ifdef STACK_DOWN
  #define STACK_overflow()  ( (aint)STACK < (aint)STACK_bound )
#endif
#ifdef STACK_UP
  #define STACK_overflow()  ( (aint)STACK > (aint)STACK_bound )
#endif
extern  void* STACK_bound;
extern  void* STACK_start;
extern _Noreturn void STACK_ueber (void);
%% #if notused
%% export_def(check_STACK());
%% export_def(STACK_overflow());
%% export_def(get_space_on_STACK(n));
%% exportV(void*,STACK_bound);
%% exportE(STACK_ueber,(void));
%% #endif

/* Tests, if there are still n Bytes free on the STACK.
 get_space_on_STACK(n); */
#ifdef STACK_DOWN
  #define get_space_on_STACK(n)  \
    if ( (aint)STACK < (aint)STACK_bound + (aint)(n) ) STACK_ueber()
#else
  #define get_space_on_STACK(n)  \
    if ( (aint)STACK + (aint)(n) > (aint)STACK_bound ) STACK_ueber()
#endif

/* Exit the LISP-Interpreter
 quit();
 > final_exitcode: 0 for a normal end, >0 for failure, -signum for a signal */
extern _GL_NORETURN_FUNC void quit (void);
extern int final_exitcode;
/* is used by CONTROL */

/* Error message if an unreachable program part has been reached.
 Does not return.
 error_notreached(file,line);
 > file: Filename (with quotation marks) as constant ASCIZ-String
 > line: line number */
extern _Noreturn void error_notreached (const char * file, uintL line);
/* used by all modules */
%% exportE(error_notreached,(const char * file, uintL line));

/* Language that's used to communicate with the user: */
#if defined(GNU_GETTEXT)    /* many languages, determined at runtime. */
    #ifndef COMPILE_STANDALONE
      #include <libintl.h>
    #endif
    /* Fetch the message translations from a message catalog. */
    #ifndef gettext  /* Sometimes `gettext' is a macro... */
      extern char* gettext (const char * msgid)
        #ifdef __GNUC__
        __attribute__ ((__format_arg__ (1)))
        #endif
        ;
    #endif
    extern const char * clgettext (const char * msgid)
      #ifdef __GNUC__
      __attribute__ ((__format_arg__ (1)))
      #endif
      ;
    extern const char * clgettextl (const char * msgid)
      #ifdef __GNUC__
      __attribute__ ((__format_arg__ (1)))
      #endif
      ;
    /* GETTEXT(english_message) fetches the translation of english_message
     and returns it in UTF-8 (if ENABLE_UNICODE is defined).
     GETTEXTL(english_message) fetches the translation of english_message
     and returns it in the locale encoding.
     GETTEXT and GETTEXTL are special tags recognized by clisp-xgettext. We
     choose English because it's the only language understood by all CLISP
     developers. */
    #define GETTEXT  clgettext
    #define GETTEXTL clgettextl
  /* the value of *current-language* */
  extern object current_language_o (void);
  /* init the language and the locale */
  extern void init_language (const char*, const char*, bool lisp_error_p);
#else  /* static language */
  #define GETTEXT(english)   english
  #define GETTEXTL(english)  english
  #define current_language_o()  S(english)
#endif
%% #if defined(GNU_GETTEXT)
%%   puts("#define GNU_GETTEXT");
%%   puts("#ifndef COMPILE_STANDALONE");
%%   puts("#include <libintl.h>");
%%   puts("#endif");
%%   exportF(const char *,clgettext,(const char * msgid));
%%   export_def(GETTEXT);
%%   exportF(object,CLSTEXT,(const char* asciz));
%% #else
%%   export_def(GETTEXT(english));
%%   emit_define("CLSTEXT","ascii_to_string");
%% #endif

/* Fetch the message translations of a string: "CL String getTEXT"
 CLSTEXT(string)
 > obj: C string
 < result: String
 can trigger GC */
extern maygc object CLSTEXT (const char*);

/* Fetch the "translation" of a Lisp object: "CL Object getTEXT"
 CLOTEXT(string)
 > obj: String
 can trigger GC */
extern maygc object CLOTEXT (const char*);

/* Print a Lisp object in Lisp notation relatively directly
 through the operating system:
 object_out(obj);
 can trigger GC */
extern maygc object object_out (object obj);
/* can trigger GC
 print the object with label, file name and line number
 this can trigger GC, but will save and restore OBJ */
#define OBJECT_OUT(obj,label)                                           \
  (printf("[%s:%d] %s: %s:\n",__FILE__,__LINE__,STRING(obj),label),     \
   fflush(stdout), obj=object_out(obj))
/* print the object to a C stream - not all objects can be handled yet!
 non-consing, STACK non-modifying */
extern maygc object nobject_out (FILE* out, object obj);
#define NOBJECT_OUT(obj,label)                                         \
  (printf("[%s:%d] %s: %s: ",__FILE__,__LINE__,STRING(obj),label),     \
   nobject_out(stdout,obj), print("\n"), fflush(stdout))
/* used for debugging purposes */
%% exportF(object,object_out,(object obj));
%% puts("#define OBJECT_OUT(obj,label)  (printf(\"[%s:%d] %s: %s:\\n\",__FILE__,__LINE__,STRING(obj),label),obj=object_out(obj))");

/* After allocating memory for an object, add the type infos. */
#ifdef TYPECODES
  #define bias_type_pointer_object(bias,type,ptr) type_pointer_object(type,ptr)
#else
  #define bias_type_pointer_object(bias,type,ptr) as_object((oint)(ptr)+(bias))
#endif
/* used by SPVW, macros SP_allocate_bit_vector, SP_allocate_string */

/* UP: executes a Garbage Collection
 gar_col(level);
 > level: if 1, also drop all jitc code
 can trigger GC */
extern maygc void gar_col (int level);
/* is used by DEBUG */

/* GC-statistics */
extern uintL gc_count;
extern uintL2 gc_space;
extern internal_time_t gc_time;
/* is used by TIME */

/* UP:  allocates a Cons
 allocate_cons()
 < result: pointer to a new CONS, with CAR and CDR =NIL
 can trigger GC */
extern maygc object allocate_cons (void);
/* is used by LIST, SEQUENCE, PACKAGE, EVAL, CONTROL, RECORD,
            PREDTYPE, IO, STREAM, PATHNAME, SYMBOL, ARRAY, LISPARIT */
%% exportF(object,allocate_cons,(void));

/* UP: Returns a newly created uninterned symbol with a given Printname.
 make_symbol(string)
 > string: immutable Simple-String
 < result: new symbol with this name, with Home-Package=NIL.
 can trigger GC */
extern maygc object make_symbol (object string);
/* is used by PACKAGE, IO, SYMBOL */
%% #if notused
%% exportF(object,make_symbol,(object string));
%% #endif

/* UP: allocates a general vector
 allocate_vector(len)
 > len: length of the vector
 < result: fresh simple general vector (elements are initialized with NIL)
 can trigger GC */
extern maygc object allocate_vector (uintL len);
/* is used by ARRAY, IO, EVAL, PACKAGE, CONTROL, HASHTABL */
%% exportF(object,allocate_vector,(uintL len));

/* Function: Allocates a bit/byte vector.
 allocate_bit_vector(atype,len)
 > uintB atype: Atype_nBit
 > uintL len: length (number of n-bit blocks)
 < result: fresh simple bit/byte-vector of the given length
 can trigger GC */
extern maygc object allocate_bit_vector (uintB atype, uintL len);
/* is used by ARRAY, IO, RECORD, LISPARIT, STREAM, CLX */
%% exportF(object,allocate_bit_vector,(uintB atype, uintL len));

/* Macro: Allocates a 8bit-vector on the stack, with dynamic extent.
   { var DYNAMIC_8BIT_VECTOR(obj,len);
     ...
     FREE_DYNAMIC_8BIT_VECTOR(obj);
   }
 > uintL len: length (number of bytes)
 < object obj: simple-8bit-vector with dynamic extent
   (may or may not be heap-allocated, therefore not GC-invariant)
 can trigger GC */
#if defined(SPVW_PURE) || defined(NO_ADDRESS_SPACE_ASSUMPTIONS) || ((((STACK_ADDRESS_RANGE << addr_shift) >> garcol_bit_o) & 1) != 0)
  /* No way to allocate a Lisp object on the stack. */
  #define DYNAMIC_8BIT_VECTOR(objvar,len)  \
    var uintL objvar##_len = (len);               \
    var object objvar = TLO(dynamic_8bit_vector); \
    TLO(dynamic_8bit_vector) = NIL;               \
    if (!(simple_bit_vector_p(Atype_8Bit,objvar) && (Sbvector_length(objvar) >= objvar##_len))) \
      objvar = allocate_bit_vector(Atype_8Bit,objvar##_len); \
    GCTRIGGER1(objvar)
  #define FREE_DYNAMIC_8BIT_VECTOR(objvar)  \
    TLO(dynamic_8bit_vector) = objvar
#else
  #define CAN_ALLOCATE_8BIT_VECTORS_ON_C_STACK
  /* Careful: Fill GCself with pointers to itself, so that GC will leave
   pointers to this object untouched. */
  #ifdef TYPECODES
    #define DYNAMIC_8BIT_VECTOR(objvar,len)  \
      DYNAMIC_ARRAY(objvar##_storage,object,ceiling((uintL)(len)+offsetofa(sbvector_,data),sizeof(gcv_object_t))); \
      var object objvar = ((Sbvector)objvar##_storage)->GCself = bias_type_pointer_object(varobject_bias,sb8vector_type,(Sbvector)objvar##_storage); \
      ((Sbvector)objvar##_storage)->length = (len); \
      GCTRIGGER1(objvar)
  #else
    #define DYNAMIC_8BIT_VECTOR(objvar,len)  \
      DYNAMIC_ARRAY(objvar##_storage,object,ceiling((uintL)(len)+offsetofa(sbvector_,data)+varobjects_misaligned,sizeof(gcv_object_t))); \
      var object* objvar##_address = (object*)((uintP)objvar##_storage | varobjects_misaligned); \
      var object objvar = ((Sbvector)objvar##_address)->GCself = bias_type_pointer_object(varobject_bias,sb8vector_type,(Sbvector)objvar##_address); \
      ((Sbvector)objvar##_address)->tfl = vrecord_tfl(Rectype_Sb8vector,len); \
      GCTRIGGER1(objvar)
  #endif
  #define FREE_DYNAMIC_8BIT_VECTOR(objvar)  \
    FREE_DYNAMIC_ARRAY(objvar##_storage)
#endif
/* used by STREAM, PATHNAME */

/* Macro: Wraps a GC-invariant uintB* pointer in a fake simple-8bit-vector.
 FAKE_8BIT_VECTOR(ptr)
 > uintB* ptr: pointer to GC-invariant data
 < gcv_object_t obj: a fake simple-8bit-vector,
                     with TheSbvector(obj)->data == ptr,
                     that must *not* be stored in GC-visible locations */
#ifdef TYPECODES
  #define FAKE_8BIT_VECTOR(ptr)  \
    type_pointer_object(0, (const char*)(ptr) - offsetofa(sbvector_,data))
#else
  #define FAKE_8BIT_VECTOR(ptr)  \
    fake_gcv_object((aint)((const char*)(ptr) - offsetofa(sbvector_,data)) + varobject_bias)
#endif

#if !defined(ENABLE_UNICODE) || defined(HAVE_SMALL_SSTRING)
/* UP, provides 8-bit character string
 allocate_s8string(len)
 > len: length of the string (in characters), must be <= stringsize_limit_1
 < result: new 8-bit character simple-string (LISP-object)
 can trigger GC */
extern maygc object allocate_s8string (uintL len);
/* used by */
#endif
#if defined(ENABLE_UNICODE) && !defined(HAVE_SMALL_SSTRING)
#define allocate_s8string(len)  allocate_s32string(len)
#endif
%% #if !defined(ENABLE_UNICODE)
%%   exportF(object,allocate_s8string,(uintL len));
%% #endif

#if !defined(ENABLE_UNICODE) || defined(HAVE_SMALL_SSTRING)
/* UP, provides immutable 8-bit character string
 allocate_imm_s8string(len)
 > len: length of the string (in characters), must be <= stringsize_limit_1
 < result: new immutable 8-bit character simple-string (LISP-object)
 can trigger GC */
extern maygc object allocate_imm_s8string (uintL len);
/* used by */
#endif

#ifdef HAVE_SMALL_SSTRING
/* UP, provides 16-bit character string
 allocate_s16string(len)
 > len: length of the string (in characters), must be <= stringsize_limit_1
 < result: new 16-bit character simple-string (LISP-object)
 can trigger GC */
extern maygc object allocate_s16string (uintL len);
/* used by */
#endif
#if defined(ENABLE_UNICODE) && !defined(HAVE_SMALL_SSTRING)
#define allocate_s16string(len)  allocate_s32string(len)
#endif

#ifdef HAVE_SMALL_SSTRING
/* UP, provides immutable 16-bit character string
 allocate_imm_s16string(len)
 > len: length of the string (in characters), must be <= stringsize_limit_1
 < result: new immutable 16-bit character simple-string (LISP-object)
 can trigger GC */
extern maygc object allocate_imm_s16string (uintL len);
/* used by */
#endif

#ifdef ENABLE_UNICODE
/* UP, provides 32-bit character string
 allocate_s32string(len)
 > len: length of the string (in characters), must be <= stringsize_limit_1
 < result: new 32-bit character simple-string (LISP-object)
 can trigger GC */
extern maygc object allocate_s32string (uintL len);
#endif
%% #ifdef ENABLE_UNICODE
%%   exportF(object,allocate_s32string,(uintL len));
%% #endif

#ifdef ENABLE_UNICODE
/* UP, provides immutable 32-bit character string
 allocate_imm_s32string(len)
 > len: length of the string (in characters), must be <= stringsize_limit_1
 < result: new immutable 32-bit character simple-string (LISP-object)
 can trigger GC */
extern maygc object allocate_imm_s32string (uintL len);
#endif

/* UP: allocates String
 allocate_string(len)
 > len: length of the Strings (in Characters), must be <= stringsize_limit_1
 < result: new Normal-Simple-String (LISP-object)
 can trigger GC */
#ifdef ENABLE_UNICODE
  #define allocate_string(len)  allocate_s32string(len)
#else
  #define allocate_string(len)  allocate_s8string(len)
#endif
/* is used by ARRAY, CHARSTRG, STREAM, PATHNAME */
%% export_def(allocate_string(len));

/* Macro: Allocates a normal string on the stack, with dynamic extent.
   { var DYNAMIC_STRING(obj,len);
     ...
     FREE_DYNAMIC_STRING(obj);
   }
 > uintL len: length (number of characters)
 < object obj: normal-simple-string with dynamic extent
   (may or may not be heap-allocated, therefore not GC-invariant)
 can trigger GC */
#if defined(SPVW_PURE) || defined(NO_ADDRESS_SPACE_ASSUMPTIONS) || ((((STACK_ADDRESS_RANGE << addr_shift) >> garcol_bit_o) & 1) != 0)
  /* No way to allocate a Lisp object on the stack. */
  #define DYNAMIC_STRING(objvar,len)  \
    var uintL objvar##_len = (len);           \
    var object objvar = TLO(dynamic_string);  \
    TLO(dynamic_string) = NIL;                \
    if (!(simple_string_p(objvar) && (Sstring_length(objvar) >= objvar##_len))) { \
      if (objvar##_len > stringsize_limit_1)  \
        error_stringsize(objvar##_len);      \
      objvar = allocate_string(objvar##_len); \
    }                                         \
    GCTRIGGER1(objvar)
  #define FREE_DYNAMIC_STRING(objvar)  \
    TLO(dynamic_string) = objvar;
#else
  #define CAN_ALLOCATE_STRINGS_ON_C_STACK
  /* Careful: Fill GCself with pointers to itself, so that GC will leave
   pointers to this object untouched. */
  #ifdef ENABLE_UNICODE
    #define DYNAMIC_STRING(objvar,len)  \
      DYNAMIC_ARRAY(objvar##_storage,object,ceiling((uintL)(len)*sizeof(chart)+offsetofa(s32string_,data)+varobjects_misaligned,sizeof(gcv_object_t))); \
      var object* objvar##_address = (object*)((uintP)objvar##_storage | varobjects_misaligned); \
      var object objvar = ((Sstring)objvar##_address)->GCself = bias_type_pointer_object(varobject_bias,sstring_type,(Sstring)objvar##_address); \
      ((Sstring)objvar##_address)->tfl = sstring_tfl(Sstringtype_32Bit,0,0,len); \
      GCTRIGGER1(objvar)
  #else
    #define DYNAMIC_STRING(objvar,len)  \
      DYNAMIC_ARRAY(objvar##_storage,object,ceiling((uintL)(len)*sizeof(chart)+offsetofa(s8string_,data)+varobjects_misaligned,sizeof(gcv_object_t))); \
      var object* objvar##_address = (object*)((uintP)objvar##_storage | varobjects_misaligned); \
      var object objvar = ((Sstring)objvar##_address)->GCself = bias_type_pointer_object(varobject_bias,sstring_type,(Sstring)objvar##_address); \
      ((Sstring)objvar##_address)->tfl = sstring_tfl(Sstringtype_8Bit,0,0,len); \
      GCTRIGGER1(objvar)
  #endif
  #define FREE_DYNAMIC_STRING(objvar)  \
    FREE_DYNAMIC_ARRAY(objvar##_storage)
#endif
/* used by LISPARIT */

/* UP: allocates an immutable String
 allocate_imm_string(len)
 > len: length of the String (in Characters)
 < result: new immutable Normal-Simple-String (LISP-object)
 can trigger GC */
#ifdef ENABLE_UNICODE
  #define allocate_imm_string(len)  allocate_imm_s32string(len)
#else
  #define allocate_imm_string(len)  allocate_imm_s8string(len)
#endif
/* is used by CHARSTRG */

#ifdef HAVE_SMALL_SSTRING
/* UP: Changes the allocation of a Small-String to an Sistring, while
 copying the contents to a fresh normal string.
 reallocate_small_string(string)
 > string: a nonempty Small-String
 > newtype: new wider string type, Sstringtype_16Bit or Sstringtype_32Bit
 < result: an Sistring pointing to a wider String
 can trigger GC */
  extern maygc object reallocate_small_string (object string, uintB newtype);
/* is used by ARRAY */
#endif

/* Attempts to reallocate a simple-string, for debugging purposes.
 DBGREALLOC(string); */
#if defined(DEBUG_SMALL_SSTRING) && defined(HAVE_SMALL_SSTRING)
  #define DBGREALLOC(string)  \
    if (simple_string_p(string) && !sstring_reallocatedp(TheSstring(string)) \
        && !sstring_immutable(TheSstring(string))                            \
        && sstring_eltype(TheSstring(string)) != Sstringtype_32Bit           \
        && sstring_length(TheSstring(string)) > 0)                           \
      string = reallocate_small_string(string,sstring_eltype(TheSstring(string))+1)/*;*/
#else
  #define DBGREALLOC(string)  (void)0 /*nop*/
#endif

/* UP: allocates indirect array
 allocate_iarray(flags,rank,type)
 > uintB flags: Flags
 > uintC (actually uintWC) rank: rank
 > tint type: Typinfo
 < result: LISP-object Array
 can trigger GC */
extern maygc object allocate_iarray (uintB flags, uintC rank, tint type);
/* is used by ARRAY, IO */

/* UP: allocates Long-Record
 allocate_lrecord(rectype,reclen,type)
 > sintB rectype: further type-info
 > uintL reclen: length
 > tint type: type-info
 < result: LISP-object Record (elements are initialized with NIL)
 can trigger GC */
#ifdef TYPECODES
  extern maygc object allocate_lrecord (uintB rectype, uintL reclen, tint type);
#else
  #define allocate_lrecord(rectype,reclen,type)  /* ignore type */ \
    allocate_lrecord_(rectype,reclen)
  extern object allocate_lrecord_ (uintB rectype, uintL reclen);
#endif
/* is used by WEAK */

/* UP: allocates Simple-Record
 allocate_srecord(flags,rectype,reclen,type)
 > uintB flags: Flags
 > sintB rectype: further type-info
 > uintC (actually uintW) reclen: length
 > tint type: type-info
 < result: LISP-object Record (elements are initialized with NIL)
 can trigger GC */
#ifdef TYPECODES
  #define allocate_srecord(flags,rectype,reclen,type)  \
    allocate_srecord_(                                                     \
       (BIG_ENDIAN_P ? (uintW)(flags)+((uintW)(uintB)(rectype)<<intBsize)  \
                     : ((uintW)(flags)<<intBsize)+(uintW)(uintB)(rectype)),\
       reclen,                                                             \
       type)
  extern maygc object allocate_srecord_ (uintW flags_rectype, uintC reclen, tint type);
#else
  #define allocate_srecord(flags,rectype,reclen,type)  /* ignore type */ \
    allocate_srecord_(((uintW)(flags)<<8)+(uintW)(uintB)(rectype),reclen)
  extern maygc object allocate_srecord_ (uintW flags_rectype, uintC reclen);
#endif
/* is used by RECORD, EVAL */

/* UP: allocates Extended-Record
 allocate_xrecord(flags,rectype,reclen,recxlen,type)
 > uintB flags: Flags
 > sintB rectype: further type-info
 > uintC (actually uintB) reclen: length
 > uintC (actually uintB) recxlen: extra-length
 > tint type: Typinfo
 < result: LISP-object Record (elements are initialized with NIL resp. 0)
 can trigger GC */
#ifdef TYPECODES
  #define allocate_xrecord(flags,rectype,reclen,recxlen,type)  \
    allocate_xrecord_(                                                     \
       (BIG_ENDIAN_P ? (uintW)(flags)+((uintW)(uintB)(rectype)<<intBsize)  \
                     : ((uintW)(flags)<<intBsize)+(uintW)(uintB)(rectype)),\
       reclen,                                                             \
       recxlen,                                                            \
       type)
  extern maygc object allocate_xrecord_ (uintW flags_rectype, uintC reclen, uintC recxlen, tint type);
#else
  #define allocate_xrecord(flags,rectype,reclen,recxlen,type)  \
    allocate_xrecord_((((uintW)(flags)<<8)+(uintW)(uintB)(rectype)),reclen,recxlen)
  extern maygc object allocate_xrecord_ (uintW flags_rectype, uintC reclen, uintC recxlen);
#endif
/* is used by */

/* UP: allocates Closure
 allocate_closure(reclen)
 > uintC reclen: length
 < result: LISP-object Closure (elements are initialized with NIL) */
#define allocate_closure(reclen,flags)                               \
  allocate_srecord(flags,Rectype_Closure,reclen,closure_type)
/* is used by EVAL, RECORD */

/* copy a section of memory */
#define copy_mem_b(dest,orig,len) /* bytes */                   \
  do { var char* newptr = (char*)(dest);                        \
       var const char* oldptr = (const char*)(orig);            \
       var uintL count;                                         \
       var uintL leng = (len);                                  \
       dotimespL(count,leng,{ *newptr++ = *oldptr++; });        \
  } while(0)
#define copy_mem_o(dest,orig,len) /* objects */                  \
  do { var gcv_object_t* newptr = (dest);                \
       var const gcv_object_t* oldptr = (orig);          \
       var uintC count;                                  \
       var uintC leng = (len);                           \
       dotimespC(count,leng,{ *newptr++ = *oldptr++; }); \
  } while(0)
#if 0 /* the libc alternative turns out to be ~3-5% slower */
#define copy_mem_b(dest,orig,len)                       \
    do { begin_system_call(); memcpy(dest,orig,len);    \
         end_system_call(); } while(0)
#define copy_mem_o(dest,orig,len)                                           \
    do { begin_system_call(); memcpy(dest,orig,(len)*sizeof(gcv_object_t)); \
         end_system_call(); } while(0)
#endif

/* Copying a compiled closure:
 newclos = allocate_cclosure_copy(oldclos);
 can trigger GC */
#define allocate_cclosure_copy(oldclos)  \
  allocate_closure(Cclosure_length(oldclos),Closure_flags(oldclos))
/* do_cclosure_copy(newclos,oldclos); */
#define do_cclosure_copy(newclos,oldclos)               \
  copy_mem_o(((Srecord)TheCclosure(newclos))->recdata,  \
             ((Srecord)TheCclosure(oldclos))->recdata,  \
             Cclosure_length(oldclos))
/* is used by EVAL, IO, RECORD */

/* UP: allocates Structure
 allocate_structure(reclen)
 > uintC reclen: length
 < result: LISP-Object Structure (Elements are initialized with NIL)
 can trigger GC */
#ifdef case_structure
  #define allocate_structure(reclen)  \
    allocate_srecord(0,Rectype_Structure,reclen,structure_type)
#else
  #define allocate_structure(reclen)  \
    allocate_srecord(0,Rectype_Structure,reclen,orecord_type)
#endif
/* is used by RECORD */

/* UP: allocates Stream
 allocate_stream(streamflags,streamtype,reclen,recxlen)
 > uintB streamflags: Flags
 > uintB streamtype: further type-info
 > uintC reclen: length in objects
 > uintC recxlen: extra-length in bytes
 < result: LISP-object Stream (elements are initialized with NIL)
 can trigger GC */
extern maygc object allocate_stream (uintB streamflags, uintB streamtype, uintC reclen, uintC recxlen);
/* is used by STREAM */

/* UP: allocates Package
 allocate_package()
 < result: LISP-object Package
 can trigger GC */
#define allocate_package()  \
  allocate_xrecord(0,Rectype_Package,package_length,0,orecord_type)
/* is used by PACKAGE */

/* UP: allocates Hash-Table
 allocate_hash_table()
 < result: LISP-object Hash-Table
 can trigger GC */
#define allocate_hash_table()  \
  allocate_xrecord(0,Rectype_Hashtable,hashtable_length,hashtable_xlength, \
                   orecord_type)
/* is used by */

/* UP: allocates  Readtable
 allocate_readtable()
 < result: LISP-object Readtable
 can trigger GC */
#define allocate_readtable()  \
  allocate_xrecord(0,Rectype_Readtable,readtable_length,0,orecord_type)
/* is used by IO */

/* UP: allocates Pathname
 allocate_pathname()
 < result: LISP-object Pathname
 can trigger GC */
#define allocate_pathname()  \
  allocate_xrecord(0,Rectype_Pathname,pathname_length,0,orecord_type)
/* is used by PATHNAME */

/* UP: allocates Logical Pathname
 allocate_logpathname()
 < result: LISP-object Logical Pathname
 can trigger GC */
#define allocate_logpathname()  \
  allocate_xrecord(0,Rectype_Logpathname,logpathname_length,0,orecord_type)
/* is used by PATHNAME */

/* UP: allocates Random-State
 allocate_random_state()
 < result: LISP-object Random-State
 can trigger GC */
#define allocate_random_state()  \
  allocate_xrecord(0,Rectype_Random_State,random_state_length,0,orecord_type)
/* is used by IO, LISPARIT */

/* UP: allocates Byte
 allocate_byte()
 < result: LISP-object Byte
 can trigger GC */
#define allocate_byte()  \
  allocate_xrecord(0,Rectype_Byte,byte_length,0,orecord_type)
/* is used by LISPARIT */

/* UP: allocates Fsubr
 allocate_fsubr()
 < result: LISP-object Fsubr
 can trigger GC */
#define allocate_fsubr()  \
  allocate_xrecord(0,Rectype_Fsubr,fsubr_length,fsubr_xlength,orecord_type)
/* is used by SPVW */

/* UP: allocates Load-time-Eval
 allocate_loadtimeeval()
 < result: LISP-object Load-time-Eval
 can trigger GC */
#define allocate_loadtimeeval()  \
  allocate_xrecord(0,Rectype_Loadtimeeval,loadtimeeval_length,0,orecord_type)
/* is used by IO, RECORD */

/* UP: allocates Symbol-Macro
 allocate_symbolmacro()
 < result: LISP-object Symbol-Macro
 can trigger GC */
#define allocate_symbolmacro()  \
  allocate_xrecord(0,Rectype_Symbolmacro,symbolmacro_length,0,orecord_type)
/* is used by CONTROL, RECORD */

/* UP: allocates Global-Symbol-Macro
 allocate_globalsymbolmacro()
 < result: LISP-object Global-Symbol-Macro
 can trigger GC */
#define allocate_globalsymbolmacro()  \
  allocate_xrecord(0,Rectype_GlobalSymbolmacro,globalsymbolmacro_length,0,orecord_type)
/* is used by RECORD */

/* UP: allocates a Macro
 allocate_macro()
 < result: a fresh Macro
 can trigger GC */
#define allocate_macro()  \
  allocate_xrecord(0,Rectype_Macro,macro_length,0,orecord_type)
/* is used by RECORD */

/* UP: allocates a FunctionMacro
 allocate_functionmacro()
 < result: a fresh FunctionMacro
 can trigger GC */
#define allocate_functionmacro()  \
  allocate_xrecord(0,Rectype_FunctionMacro,functionmacro_length,0,orecord_type)
/* is used by RECORD */

/* UP: allocates a BigReadLabel
 allocate_big_read_label()
 < result: a fresh BigReadLabel
 can trigger GC */
#define allocate_big_read_label()  \
  allocate_xrecord(0,Rectype_BigReadLabel,bigreadlabel_length,0,orecord_type)
/* is used by IO */

/* UP: allocates an Encoding
 allocate_encoding()
 < result: a fresh Encoding
 can trigger GC */
#define allocate_encoding()  \
  allocate_xrecord(0,Rectype_Encoding,encoding_length,encoding_xlength,orecord_type)
/* is used by ENCODING */

#ifdef FOREIGN
/* UP: allocates a foreign-pointer packing
 allocate_fpointer(foreign)
 > foreign: of Type FOREIGN
 < result: LISP-object, contains the foreign pointer
 can trigger GC */
  extern maygc object allocate_fpointer (FOREIGN foreign);
/* used by FFI & modules */
#endif
%% #ifdef FOREIGN
%%   exportF(object,allocate_fpointer,(FOREIGN foreign));
%% #endif

/* UP: allocates foreign address
 allocate_faddress()
 < result: LISP-object foreign address
 can trigger GC */
#define allocate_faddress()  \
  allocate_xrecord(0,Rectype_Faddress,faddress_length,faddress_xlength,orecord_type)
/* is used by FOREIGN */

/* UP: allocates foreign variable
 allocate_fvariable()
 < result: LISP-object foreign variable
 can trigger GC */
#define allocate_fvariable()  \
  allocate_xrecord(0,Rectype_Fvariable,fvariable_length,0,orecord_type)
/* is used by FOREIGN */

/* UP: allocates foreign function
 allocate_ffunction()
 < result: LISP-object foreign function
 can trigger GC */
#define allocate_ffunction()  \
  allocate_xrecord(0,Rectype_Ffunction,ffunction_length,0,orecord_type)
/* is used by FOREIGN */

/* UP: allocates finalizer
 allocate_finalizer()
 < result: LISP-object finalizer
 can trigger GC */
#define allocate_finalizer()  \
  allocate_xrecord(0,Rectype_Finalizer,finalizer_length,0,orecord_type)
/* is used by RECORD */

/* UP: allocates Socket-Server
 allocate_socket_server()
 < result: LISP-object Socket-Server */
#ifdef SOCKET_STREAMS
  #define allocate_socket_server() \
    allocate_xrecord(0,Rectype_Socket_Server,socket_server_length,0,orecord_type)
#endif

#ifdef YET_ANOTHER_RECORD
/* UP: allocates Yetanother
 allocate_yetanother()
 < result: LISP-object Yetanother
 can trigger GC */
  #define allocate_yetanother()  \
    allocate_xrecord(0,Rectype_Yetanother,yetanother_length,0,orecord_type)
/* is used by */
#endif

/* UP: allocates handle
 allocate_handle(handle)
 < result: LISP-object, that contains handle
 can trigger GC */
#ifdef FOREIGN_HANDLE
  /* can trigger GC */
  extern maygc object allocate_handle (Handle handle);
#else
  #define allocate_handle(handle)  fixnum((uintL)(handle))
#endif
%% #if defined(FOREIGN_HANDLE)
%%   exportF(object,allocate_handle,(Handle handle));
%% #else
%%   export_def(allocate_handle(handle));
%% #endif

/* UP: allocates Bignum
 allocate_bignum(len,sign)
 > uintC (actually uintWC) len: length of the number (in Digits)
 > sintB sign: flag for sign (0 = +, -1 = -)
 < result: new Bignum (LISP-object)
 can trigger GC */
extern maygc object allocate_bignum (uintC len, sintB sign);
/* is used by LISPARIT, STREAM */

/* UP: allocates Single-Float
 allocate_ffloat(value)
 > ffloat value: value (Bit 31 = sign)
 < result: new Single-Float (LISP-object)
 can trigger GC */
extern maygc object allocate_ffloat (ffloat value);
/* is used by LISPARIT */

/* UP: allocates Double-Float */
#ifdef intQsize
/* allocate_dfloat(value)
 > dfloat value: value (Bit 63 = sign)
 < result: new Double-Float (LISP-object)
 can trigger GC */
  extern maygc object allocate_dfloat (dfloat value);
#else
/* allocate_dfloat(semhi,mlo)
 > semhi,mlo: value (Bit 31 of semhi = sign )
 < result: new Double-Float (LISP-object)
 can trigger GC */
  extern maygc object allocate_dfloat (uint32 semhi, uint32 mlo);
#endif
/* is used by LISPARIT */

/* UP: allocates Long-Float
 allocate_lfloat(len,expo,sign)
 > uintC (actually uintWC) len: length of the mantissa (in Digits)
 > uintL expo: exponent
 > signean sign: sign (0 = +, -1 = -)
 < result: new Long-Float, without mantissa
 It will only be a LISP-object when the mantissa has been entered!
 can trigger GC */
extern maygc object allocate_lfloat (uintC len, uintL expo, signean sign);
/* is used by LISPARIT */

/* UP: makes a rational number
 make_ratio(num,den)
 > object num: numerator (has to be an integer /= 0, relatively prime to den)
 > object den: denominator (has to be an Integer > 1)
 < result: rational number
 can trigger GC */
extern maygc object make_ratio (object num, object den);
/* is used by LISPARIT */

/* UP: makes a complex number
 make_complex(real,imag)
 > real: real part (has to be a real number)
 > imag: imaginary part (has to be a real number /= Fixnum 0)
 < result: complex number
 can trigger GC */
extern maygc object make_complex (object real, object imag);
/* is used by LISPARIT */

#ifdef MULTITHREAD
/* allocate a thread object
 allocate_thread()
 > *name_ : thread name (usually a symbol)
 < result : new thread object (not started)
 can trigger GC */
global maygc object allocate_thread (gcv_object_t *name_);
/* used by ZTHREAD */

/* allocate a mutex object and inserts it in O(all_mutexes)
 allocate_mutex()
 > *name_ : mutex name (usually a symbol)
 < result : new mutex object (initialized)
 can trigger GC */
global maygc object allocate_mutex (gcv_object_t *name_);
/* used by ZTHREAD */

/* allocate an exemption object and inserts it in O(all_exemptions)
 allocate_exemption()
 > *name_ : exemption name (usually a symbol)
 < result : new exemption object (initialized)
 can trigger GC */
global maygc object allocate_exemption (gcv_object_t *name_);
/* used by ZTHREAD */
#endif

/* Adds a freshly allocated object to the list of weak pointers.
 activate_weak(obj);
 > obj: A fresh but filled object of type Rectype_Weak* */
extern void activate_weak (object obj);
/* is used by WEAK */

/* True if we are in the process of quitting */
extern bool quit_on_signal_in_progress;
/* used by ERROR */

/* UP: return the length of the ASCIZ-String
 asciz_length(asciz)
 > char* asciz: ASCIZ-String
       (added with a NULL byte determines the end of string)
 < result: Length of the character sequence (without the NULL byte) */
extern uintL asciz_length (const char * asciz);
#if defined(GNU) && (SAFETY < 2)
  #ifdef HAVE_BUILTIN_STRLEN
    #define asciz_length(a)  ((uintL)__builtin_strlen(a))
  #endif
#endif
#ifndef asciz_length
  #ifdef HAVE_SAVED_STACK
    /* can not use strlen() instead of asciz_length() , because this would
     require a begin_system_call()/end_system_call() . */
  #else
    /* let us presume, that strlen() is implemented efficiently. */
    #define asciz_length(a)  ((uintL)strlen(a))
  #endif
#endif
/* is used by SPVW */
%% #ifdef asciz_length
%%   export_def(asciz_length(a));
%% #else
%%   exportF(uintL,asciz_length,(const char * asciz));
%% #endif

/* UP: Compares two ASCIZ-Strings.
 asciz_equal(asciz1,asciz2)
 > char* asciz1: first ASCIZ-String
 > char* asciz2: second ASCIZ-String
 < result: true if the number-sequences are equal */
extern bool asciz_equal (const char * asciz1, const char * asciz2);
/* is used by STREAM, ENCODING, SPVW, CHARSTRG */
%% #if notused
%%   exportF(bool,asciz_equal,(const char * asciz1, const char * asciz2));
%% #endif

/* UP: check that the first ASCIZ-string starts with the second one.
 asciz_startswith(asciz,prefix) === (strncmp(asciz,prefix,strlen(prefix))==0)
 > char* asciz: first ASCIZ-string
 > char* prefix: second ASCIZ-string
 < result: true if both sequences are equal up to the length of the second */
extern bool asciz_startswith (const char *asciz, const char *prefix);
/* used by PATHNAME */
%% #if notused
%%   exportF(bool,asciz_startswith,(const char *asciz, const char *prefix));
%% #endif

/* allocate memory and check for success */
extern void* clisp_malloc (size_t size);
/* used by FOREIGN and modules */
%% exportF(void*,clisp_malloc,(size_t size));

/* reallocate memory and check for success */
extern void* clisp_realloc (void* ptr, size_t size);
/* used by modules */
%% exportF(void*,clisp_realloc,(void *ptr, size_t size));

/* UP: Returns a Table of all circularities within an Object.
 (A circularity is a Sub-Object contained within this Object,
 which has more than one access-path to it.)
 get_circularities(obj,pr_array,pr_closure)
 > object obj: Object
 > bool pr_array: Flag, if Array-Elements recursively count as Sub-Objects
 > bool pr_closure: Flag, if Closure-Components recursively count as Sub-Objects
 < result: T if Stack-Overflow occurred,
             NIL if no circularities available,
             #(0 ...) an (n+1)-element Vector, that contains the number 0 and the n
                      circularities as Elements, n>0.
 can trigger GC */
extern maygc object get_circularities (object obj, bool pr_array, bool pr_closure);
/* is used by IO */

/* UP: unentangles #n# - References in Object *ptr with help from Aliste alist.
 > *ptr : Object
 > alist : Alist (Read-Label --> Object, to be substituted)
 < *ptr : Object with unentangled References
 < result : erroneous Reference or nullobj if everything is OK */
extern object subst_circ (gcv_object_t* ptr, object alist);
/* is used by IO */

/* UP: Runs through the whole memory, and calls for each
 Object obj: fun(arg,obj,bytelen) .
 map_heap_objects(fun,arg);
 > fun: C-Function
 > arg: arbitrary given Argument */
typedef void map_heap_function_t (void* arg, object obj, uintM bytelen);
extern void map_heap_objects (map_heap_function_t* fun, void* arg);
/* is used by PREDTYPE */

/* UP: returns the size (in Bytes) of an object.
 varobject_bytelength(obj)
 > obj: Heap-object with variable length
 < result; the number of bytes occupied by it (header included) */
extern uintM varobject_bytelength (object obj);
/* is used by PREDTYPE */

/* Break-Semaphores
 As long as a Break-Semaphore is set, the Lisp-Program can not
 be interrupted. Purpose:
 - backup of Consistencies,
 - Non-reentrant Data-Structures (like e.g. DTA_buffer) can not
   be used recursively. */
typedef union {uintB einzeln[8]; uintL gesamt[2]; } break_sems_;

#define break_sem_0  break_sems.einzeln[0]
#define break_sem_1  break_sems.einzeln[1]
#define break_sem_2  break_sems.einzeln[2]
#define break_sem_3  break_sems.einzeln[3]
#define break_sem_4  break_sems.einzeln[4]
#define break_sem_5  break_sems.einzeln[5]
#define break_sem_6  break_sems.einzeln[6]
#define break_sem_7  break_sems.einzeln[7]
/* is used by SPVW, Macros set/clr_break_sem_0/1/2/3/4/5/6/7 */

/* MULTITHREAD:
   Semaphores are not used. Async signals may come only on two
   points - immediately after the world is resumed and when
   inside blocking system calls. So there is no way to
   interrupt critical initialization of lisp objects.
   Of course it is possible a thread to access others threads
   objects - but this is responsibility of the threads themsleves
   not of the runtime (and break_sems cannot prevent it as well).
 */
#if defined(MULTITHREAD)
   #define SEMA_(statement)
   #define SEGV_SEMA_(statement)
   #define break_sems_cleared()
   #define clear_break_sems()
#else /* !MULTITHREAD*/
   extern break_sems_ break_sems;
   #define SEMA_(statement) (statement)
   #define SEGV_SEMA_(statement) SEMA_(statement)
   /* Tests whether all break-semaphores have been cleared. */
   #define break_sems_cleared()                                 \
     (break_sems.gesamt[0] == 0 && break_sems.gesamt[1] == 0)
   /* is used by SPVW, WIN32AUX */
   /* clears all break-semaphores. Very dangerous! */
   #define clear_break_sems()  \
     (break_sems.gesamt[0] = 0, break_sems.gesamt[1] = 0)
   /* is used by SPVW */
#endif

/* sets break-semaphore 0 and thus protects against interrupts
 set_break_sem_0(); */
#define set_break_sem_0()  SEGV_SEMA_(break_sem_0 = 1)
/* is used by SPVW */

/* clears the break-semaphore 0 and thus releases the interrupts
 clr_break_sem_0(); */
#define clr_break_sem_0()  SEGV_SEMA_(break_sem_0 = 0)
/* is used by SPVW */

/* sets break-semaphore 1 and thus protects against interrupts
 set_break_sem_1(); */
#define set_break_sem_1()  SEMA_(break_sem_1 = 1)
/* is used by SPVW, ARRAY */

/* clears the break-semaphore 1 and thus releases the interrupts
 clr_break_sem_1(); */
#define clr_break_sem_1()  SEMA_(break_sem_1 = 0)
/* is used by SPVW, ARRAY */

/* sets break-semaphore 2 and thus protects against interrupts
 set_break_sem_2(); */
#define set_break_sem_2()  SEMA_(break_sem_2 = 1)
/* is used by PACKAGE, HASHTABL */

/* clears the break-semaphore 2 and thus releases the interrupts
 clr_break_sem_2(); */
#define clr_break_sem_2()  SEMA_(break_sem_2 = 0)
/* is used by PACKAGE, HASHTABL */

/* sets break-semaphore 3 and thus protects against interrupts
 set_break_sem_3(); */
#define set_break_sem_3()  SEMA_(break_sem_3 = 1)
/* is used by PACKAGE */

/* clears the break-semaphore 3 and thus releases the interrupts
 clr_break_sem_3(); */
#define clr_break_sem_3()  SEMA_(break_sem_3 = 0)
/* is used by PACKAGE */

/* sets break-semaphore 4 and thus protects against interrupts
 set_break_sem_4(); */
#define set_break_sem_4()  SEMA_(break_sem_4 = 1)
/* is used by STREAM, PATHNAME */

/* clears the break-semaphore 4 and thus releases the interrupts
 clr_break_sem_4(); */
#define clr_break_sem_4()  SEMA_(break_sem_4 = 0)
/* is used by STREAM, PATHNAME */

/* increments break-semaphore 5 and thus protects against interrupts
 inc_break_sem_5(); */
#define inc_break_sem_5()  SEMA_(break_sem_5++)
/* is used by SPVW */

/* decrements break-semaphore 5 and thus releases interrupts
 dec_break_sem_5(); */
#define dec_break_sem_5()  SEMA_(break_sem_5--)
/* is used by SPVW */

/* clears the break-semaphore 5 and thus releases the interrupts
 clr_break_sem_5(); */
#define clr_break_sem_5()  SEMA_(break_sem_5 = 0)
/* is used by SPVW */

/* Flag, whether SYS::READ-FORM should behave compatibly to ILISP */
extern bool ilisp_mode;

/* Flag, whether libreadline should be avoided */
extern bool disable_readline;

/* returns the amount of space occupied by static LISP-objects */
extern uintM static_space (void);
/* is used by DEBUG */

/* returns the amount of space occupied by LISP-objects */
extern uintM used_space (void);
/* is used by TIME, DEBUG */

/* returns the amount of space still available for LISP-objects */
extern uintM free_space (void);
/* is used by DEBUG */

/* UP: saves memory image to disc
 savemem(stream,executable);
 > object stream: open File-Output-Stream, will be closed
 > uintL executable: 0: no runtime; 1: runtime; 2: also delegate command line
 < file length
 can trigger GC */
extern maygc off_t savemem (object stream, uintL executable);
/* used by PATHNAME */

#ifdef HAVE_SIGNALS
/* Temporarily do not ignore the status of subprocesses. */
extern void begin_want_sigcld (void);
extern void end_want_sigcld (void);
/* is used by PATHNAME and module syscalls */
#endif
%% #ifdef HAVE_SIGNALS
%%  exportF(void,begin_want_sigcld,(void));
%%  exportF(void,end_want_sigcld,(void));
%% #endif


#if defined(HAVE_SIGNALS) && defined(SIGPIPE) && !defined(MULTITHREAD)
/* Set ONLY during write() calls to pipes directed to subprocesses. */
extern bool writing_to_subprocess;
#endif
%% #if defined(HAVE_SIGNALS) && defined(SIGPIPE) && !defined(MULTITHREAD)
%%  exportV(bool,writing_to_subprocess);
%% #endif


/* Declaration of the FSUBRs.
 As C-functions: C_name, of the type fsubr_function_t (no arguments, no value) */

/* make C-functions visible: */
#define LISPSPECFORM  LISPSPECFORM_A
#include "fsubr.c"
#undef LISPSPECFORM
/* is used by */

/* make Fsubr-table visible: */
#define LISPSPECFORM  LISPSPECFORM_C
struct fsubr_tab_ {
  #include "fsubr.c"
};
#undef LISPSPECFORM
extern const struct fsubr_tab_ fsubr_tab;
/* is used by CONTROL, SPVW */


/* Declaration of the SUBR-table:
 As C-functions: C_name
 of the type subr_norest_function_t (no arguments, no value)
 resp. subr_rest_function_t (two arguments, no value): */
typedef Values subr_norest_function_t (void);
typedef Values subr_rest_function_t (uintC argcount, gcv_object_t* rest_args_pointer);
%% #if notused
%% emit_typedef_f("Values %s(void)","subr_norest_function_t");
%% emit_typedef_f("Values %s(uintC argcount, object* rest_args_pointer)","subr_rest_function_t");
%% #endif

/* As LISP-Subr:    L(name) */

/* Make C-functions visible: */
#define LISPFUN  LISPFUN_A
#include "subr.c"
#undef LISPFUN
/* is used by */

/* Make Subr-tables visible: */
#define LISPFUN  LISPFUN_C
extern struct subr_tab_ {
  VAROBJECTS_ALIGNMENT_DUMMY_DECL
  #include "subr.c"
} subr_tab_data;
#undef LISPFUN
/* is used by Macro L */
%% puts(STRINGIFY(modimp) " struct subr_tab_ {");
%% puts("  VAROBJECTS_ALIGNMENT_DUMMY_DECL");
%% #undef LISPFUN
%% #define LISPFUN(name,sec,req_count,opt_count,rest_flag,key_flag,key_count,keywords) \
%%   printf("  subr_t %s;\n",STRING(D_##name));
%% #include "subr.c"
%% #undef LISPFUN
%% puts("} subr_tab_data;");
%% emit_dll_def("subr_tab_data");

/* Abbreviation for LISP-Subr with a given name: L(name) */
#if !defined(MAP_MEMORY_TABLES)
  #define subr_tab  subr_tab_data
  #ifdef TYPECODES
    #define subr_tab_ptr_as_object(subr_addr)  (type_constpointer_object(subr_type,subr_addr))
  #else
    #if defined(OBJECT_STRUCT)
      #define subr_tab_ptr_as_object(subr_addr)  as_object((oint)(subr_addr)+subr_bias)
    #else
      #define subr_tab_ptr_as_object(subr_addr)  objectplus(subr_addr,subr_bias)
    #endif
  #endif
  #define L_help_(name)  subr_tab_ptr_as_object(&subr_tab.name)
#else
  #ifdef SINGLEMAP_MEMORY
    #define subr_tab_addr  ((struct subr_tab_ *)(type_zero_oint(subr_type)+SINGLEMAP_ADDRESS_BASE))
  #else
    #define subr_tab_addr  ((struct subr_tab_ *)type_zero_oint(subr_type))
  #endif
  #define subr_tab  (*subr_tab_addr)
  #define subr_tab_ptr_as_object(subr_addr)  (as_object((oint)(subr_addr)))
  #define L_help_(name)  subr_tab_ptr_as_object(&subr_tab_addr->name)
#endif
#define L(name)  L_help_(D_##name)
/* is used by all modules */
%% #if defined(MAP_MEMORY_TABLES)
%%   export_def(subr_tab_addr);
%% #endif
%% export_def(subr_tab);
%% export_def(subr_tab_ptr_as_object(subr_addr));
%% export_def(L_help_(name));
%% emit_define("L(name)","L_help_(D_##name)");


/* Pseudofunctions are addresses of C functions (to be called directly, not via
 FUNCALL) or constant C data.
 For SAVEMEM/LOADMEM we have a table of all such pseudofunctions. */
typedef const void *  Pseudofun; /* assume function pointers fit in a void* */
%% emit_typedef("const void *","Pseudofun");

/* Declaration of the tables of relocatable pointers: */
#define PSEUDO  PSEUDO_A
extern struct pseudocode_tab_ {
  #include "pseudofun.c"
} pseudocode_tab;
#undef PSEUDO
#define PSEUDO  PSEUDO_B
extern struct pseudodata_tab_ {
  #include "pseudofun.c"
  #if defined(MICROSOFT) && !defined(ENABLE_UNICODE)
  Pseudofun dummy_pseudofun;
  #endif
} pseudodata_tab;
#undef PSEUDO
/* is used by STREAM, SPVW */

/* Declaration of the functions that can be stored in Lisp objects. */
#define PSEUDO  PSEUDO_C
#include "pseudofun.c"
#undef PSEUDO
/* is used by STREAM, and to avoid gcc -Wmissing-declarations warnings */

/* Return an ADDRESS object encapsulating a pseudofunction. */
#ifdef TYPECODES
  #define P(fun)  type_constpointer_object(machine_type,(Pseudofun)&(fun))
#else
  #define P(fun)  make_machine_code((Pseudofun)&(fun))
#endif
/* is used by STREAM, ENCODING */


/* Declaration if the Symbol-table: */
#define LISPSYM  LISPSYM_A
extern struct symbol_tab_ {
  VAROBJECTS_ALIGNMENT_DUMMY_DECL
  #include "constsym.c"
} symbol_tab_data;
#undef LISPSYM
/* is used by Macro S, gcinvariant_symbol_p */
%% puts(STRINGIFY(modimp) " struct symbol_tab_ {");
%% puts("  VAROBJECTS_ALIGNMENT_DUMMY_DECL");
%% #define LISPSYM(name,printname,package)  \
%%   printf("  symbol_ %s;\n",STRING(S_##name));
%% #include "constsym.c"
%% #undef LISPSYM
%% puts("} symbol_tab_data;");
%% emit_dll_def("symbol_tab_data");

/* Abbreviation for LISP-Symbol with a given name: S(name) */
#define S(name)  S_help_(S_##name)
#if !defined(MAP_MEMORY_TABLES)
  #define symbol_tab  symbol_tab_data
  #ifdef TYPECODES
    #define S_help_(name)  (type_constpointer_object(symbol_type,&symbol_tab.name))
  #else
    #if defined(OBJECT_STRUCT)
      #define S_help_(name)  as_object((oint)&symbol_tab.name+varobject_bias)
    #else
      #define S_help_(name)  objectplus(&symbol_tab.name,varobject_bias)
    #endif
  #endif
#else
  #ifdef SINGLEMAP_MEMORY
    #define symbol_tab_addr ((struct symbol_tab_ *)(type_zero_oint(symbol_type)+SINGLEMAP_ADDRESS_BASE))
  #else
    #define symbol_tab_addr ((struct symbol_tab_ *)type_zero_oint(symbol_type))
  #endif
  #define symbol_tab  (*symbol_tab_addr)
  #define S_help_(name)  (as_object((oint)(&symbol_tab_addr->name)))
  #if 0 /* Some compilers do not allow the above expression */
        /* - even though it's a 'constant expression' -
         as initializer of static variables.
         We have to assist: */
    #undef S_help_
    #define S_help_(name)  (as_object( (char*)(&((struct symbol_tab_ *)0)->name) + (uintP)symbol_tab_addr ))
  #endif
#endif
/* is used by all modules */
%% emit_define("S(name)","S_help_(S_##name)");
%% #if defined(MAP_MEMORY_TABLES)
%%   export_def(symbol_tab_addr);
%% #endif
%% export_def(symbol_tab);
%% export_def(S_help_(name));

#define NIL  S(nil)
#define T    S(t)
%% export_def(NIL);
%% export_def(T);

#if defined(DEBUG_GCSAFETY)
/* gcinvariant_symbol_p(obj)
 > obj: an object
 < result: true if obj is a symbol in symbol_tab */
static inline bool gcinvariant_symbol_p (object obj) {
  if (
      #ifdef TYPECODES
        symbolp(obj)
      #else
        varobjectp(obj)
      #endif
      &&
      (
       #if !defined(MAP_MEMORY_TABLES)
         #ifdef TYPECODES
           (as_oint(obj) >> (oint_addr_shift-addr_shift)) - ((aint)(tint)symbol_type<<oint_type_shift)
         #else
           as_oint(obj) - varobject_bias
         #endif
       #else
         as_oint(obj)
       #endif
       - (aint)&symbol_tab < sizeof(symbol_tab))
     )
    return true;
  else
    return false;
}
#endif
%% #if defined(DEBUG_GCSAFETY)
%%   print("static inline bool gcinvariant_symbol_p (object obj) { if (");
%%   #ifdef TYPECODES
%%     print("symbolp(obj)");
%%   #else
%%     print("varobjectp(obj)");
%%   #endif
%%   print(" && (");
%%   #if !defined(MAP_MEMORY_TABLES)
%%     #ifdef TYPECODES
%%       printf2("(as_oint(obj) >> %d) - %d", oint_addr_shift-addr_shift, (aint)(tint)symbol_type<<oint_type_shift);
%%     #else
%%       printf1("as_oint(obj) - %d", varobject_bias);
%%     #endif
%%   #else
%%     print("as_oint(obj)");
%%   #endif
%%   puts(" - (aint)&symbol_tab < sizeof(symbol_tab))) return true; else return false; }");
%% #endif

/* The macro NIL_IS_CONSTANT tells , whether NIL is recognized
 as 'constant expression' by the C-Compiler. If so, tables can
 already be initialized largely by the C-Compiler. */
#if (oint_addr_shift==0)
  #define NIL_IS_CONSTANT  true
#else
  #define NIL_IS_CONSTANT  false
#endif

/* Declaration of the table with the remaining constant objects: */
#define LISPOBJ  LISPOBJ_A
extern struct object_tab_ {
  #include "constobj.c"
} object_tab;
#undef LISPOBJ
/* is used by Macro O */
%% puts(STRINGIFY(modimp) " struct object_tab_ {");
%% #define LISPOBJ(name,init)  printf("  gcv_object_t %s;\n",STRING(name));
%% #include "constobj.c"
%% #undef LISPOBJ
%% puts("} object_tab;");
%% emit_dll_def("object_tab");

/* Abbreviation for other LISP-object with a given Name: */
#define O(name)  (object_tab.name)
#if !defined(MULTITHREAD)
#define TLO O
#endif
%% /* FIXME: Difference between lispbibl.d and clisp.h */
%% puts("#define GLO(name)  (object_tab.name)");

#if defined(GENERATIONAL_GC) && defined(SPVW_MIXED)
/* handle_fault_range(PROT_READ,start,end) makes an address range readable.
 handle_fault_range(PROT_READ_WRITE,start,end) makes an address range writable. */
  extern bool handle_fault_range (int prot, aint start_address, aint end_address);
#endif
%% export_def(PROT_READ);
%% export_def(PROT_READ_WRITE);
%% #if defined(GENERATIONAL_GC) && defined(SPVW_MIXED)
%% exportF(bool,handle_fault_range,(int prot, aint start_address, aint end_address));
%% #else
%% puts("#define handle_fault_range(p,s,e) ((void)(p),(void)(s),(void)(e))");
%% #endif


/* ###################### MODBIBL for MODULES.D ########################### */

#if defined(DYNAMIC_MODULES) && !defined(HAVE_DYNLOAD) && !defined(WIN32_NATIVE)
  /* if you want DYNAMIC_MODULES to work on a non-WIN32_NATIVE platform
     which does not HAVE_DYNLOAD (e.g., via ltdl), you will need to
     implement libopen() and find_name() in spvw.d for your platform */
  #error Dynamic modules require dynamic loading!
#endif

/* Number of external modules: */
extern uintC module_count;
%% exportV(uintC,module_count);

/* Data for initialization of a module's subr_tab: */
typedef struct {
  const char* packname; /* Name of the Home-Package of the Symbol or NULL */
  const char* symname; /* Name of the Symbol */
} subr_initdata_t;
%% emit_typedef("struct { const char* packname; const char* symname; }","subr_initdata_t");

/* Data for initialization of a module's object_tab: */
typedef struct {
  const char* initstring; /* Initialization-String */
} object_initdata_t;
%% emit_typedef("struct { const char* initstring; }","object_initdata_t");

/* Table resp. List of Modules: */
typedef struct module_t {
  const char* name; /* Name */
  subr_t* stab; const uintC* stab_size; /* a separate subr_tab */
  gcv_object_t* otab; const uintC* otab_size; /* a separate object_tab */
  bool initialized;
  /* Data for Initialization: */
  const subr_initdata_t* stab_initdata;
  const object_initdata_t* otab_initdata;
  /* Functions for Initialization */
  void (*initfunction1) (struct module_t *); /* only once */
  void (*initfunction2) (struct module_t *); /* always at start up */
  void (*finifunction) (struct module_t *);  /* before termination */
  #ifdef DYNAMIC_MODULES
    struct module_t * next; /* linked List */
  #endif
} module_t;
#ifdef DYNAMIC_MODULES
  extern module_t modules[]; /* List-Start */
  BEGIN_DECLS
  extern void add_module (module_t * new_module);
  END_DECLS
#else
  extern module_t modules[]; /* 1+module_count entries, then an empty entry */
#endif
%% strcpy(buf,"struct module_t { const char* name; subr_t* stab; const uintC* stab_size; gcv_object_t* otab; const uintC* otab_size; bool initialized; const subr_initdata_t* stab_initdata; const object_initdata_t* otab_initdata; void (*initfunction1) (struct module_t *); void (*initfunction2) (struct module_t *); void (*finifunction) (struct module_t *);");
%% #ifdef DYNAMIC_MODULES
%%   strcat(buf," struct module_t * next;");
%% #endif
%% strcat(buf," }"); emit_typedef(buf,"module_t");
%% #ifdef DYNAMIC_MODULES
%%   puts("BEGIN_DECLS");
%%   exportF(void,add_module,(module_t * new_module));
%%   puts("END_DECLS");
%% #else
%%   exportF(module_t,modules,[]);
%% #endif

#if defined(HAVE_DYNLOAD) || defined(WIN32_NATIVE)
/* open the dynamic library
 libname is the name of the library
 returns a handle suitable for find_name()
 calls dlopen() or LoadLibrary() */
extern void * libopen (const char* libname);
/* used by FOREIGN and spvw.d:dynload_modules() */

/* find the name in the dynamic library handle
 calls dlsym() or GetProcAddress()
 handle is an object returned by libopen()
        or NULL, which means emulate RTLD_DEFAULT on UNIX_FREEBSD
        and WIN32_NATIVE by searching through all libraries
 name is the name of the function (or variable) in the library */
extern void* find_name (void *handle, const char *name);
/* used by FOREIGN and spvw.d:dynload_modules() */
#endif

#if defined(DYNAMIC_MODULES)
/* Attaches a shared library to this process' memory, and attempts to load
   a number of clisp modules from it. */
extern maygc void dynload_modules (const char * library, uintC modcount,
                                   const char * const * modnames);
#endif

/* find the module with the given name */
extern module_t* find_module (const char *name);
/* push all module names to STACK and return the number of modules pushed
 can trigger GC */
extern maygc uintC modules_names_to_stack (void);

/* ####################### EVALBIBL for EVAL.D #############################

Specifications for the Evaluator
################################

SUBRs and FSUBRs
================

They're constructed through
  LISPFUN             for general LISP-functions,
  LISPFUNN            for normal  LISP-functions (only required parameters),
  LISPSPECFORM        for special forms (FSUBRs).
Note that SUBRs with KEY_COUNT=0 will be seen as SUBRs without keyword-
parameters by the evaluator (which in consequence means that in this case the
ALLOW_FLAG is meaningless and no keyword, not even :ALLOW-OTHER-KEYS,
will be accepted)!

Values
======

The following format is used for the passing of multiple values:
value1 contains the first value (NIL if there aren't values).
mv_count contains the number of values..
If there is at least one value       : value1 = first value.
If there are at least two values     : value2 = second value.
If there are at least three values   : value3 = third value .
All values are in mv_space .
Recommended commands for returning of values to the caller:
  0 values:   VALUES0;
  1 value :     VALUES1(...);
  2 values:   value1=...; value2=...; mv_count=2;
  3 values:   value1=...; value2=...; value3=...; mv_count=3;
  more than 3 values:
              if (number of values >= mv_limit) goto error_too_many_values;
              Put the values one after another onto the STACK
              STACK_to_mv(number of values);

Passing of parameters to SUBRs
==============================

The arguments are passed on the LISP-stack, with the first one being on the
top. The required arguments come first, then the optional ones
(each #UNBOUND, if not specified), then come the
keyword-arguments (again, each #UNBOUND, if not specified).
The SUBR-object can be found in back_trace.
This is all if no &REST-argument is planned. But if a &REST-argument
is planned, all further arguments follow (the optional ones) on the stack
one by one, and this will be passed: the number of these arguments and a pointer
above the first of these arguments. (This means that the number of LISP-objects on
the stack is not always the same!)
All arguments have to be removed from the LISP-stack at the return jump.
(for example. for SUBRs with &REST: the stackpointer STACK has to have the value
args_pointer = rest_args_pointer STACKop (fixed number of arguments)
= pointer above the very first argument), and mv_count/mv_space
has to hold the values.

Passing of parameters to FSUBRs
===============================

The parameters are passed on the LISP-stack with the first one being on top.
At first there are the required parametes, followed by the optional ones
(#UNBOUND, if not specifired), then - if body-flag true -
the whole rest of the body (most of the time a list).
So the number of objects on the LISP-stack is always the same, namely
numReqParameter + numOptParameter + (0 or 1 if body-flag).
At the call, back_trace holds the FSUBR-object, and the whole form is
in the EVAL-frame, directly above the parameters.
All parameters have to be removed from the LISP-stack at the return jump
(ie. the stackpointer STACK has to be incemented by the number of objects),
and mv_count/mv_space has to hold the values.

Environments
============

General
-------
The lexical environment is separated into 5 components:
  - the variables-environment (VAR_ENV),
  - the functions- and macro-environment (FUN_ENV),
  - the block-environment (BLOCK_ENV),
  - the tagbody-environment (GO_ENV),
  - the declarations-environment (DECL_ENV).
The environment is kept in 5 "global variables". They are dynamically bound
with special frames on change.
A single functions- and macro environment is passed to SYM_FUNCTION,
MACROEXP, MACROEXP0, PARSE_DD.
GET_CLOSURE expects a pointer to all environments en bloc: A3 with
VAR_(A3)=VAR_ENV, FUN_(A3)=FUN_ENV, BLOCK_(A3)=BLOCK_ENV, GO_(A3)=GO_ENV,
DECL_(A3)=DECL_ENV.

The variables-environment
-------------------------
It contains the local variable-bindings.
A variables-enviroment is given through a pointer to a
variable-binding frame, or NIL  (which means an empty lexical
environment) or a vector that is built as follows:
The vector contains n bindings and has the length 2n+1. The elements are
n-times each variable (a symbol) and  the value that belongs to it ("value" can
be #<SPECDECL> as well, and then the variable has to be referenced dynamically,
or it can be #<IMPLEMENTATION-DEPENDENT>, then a warning is emitted during
lookup) and as last element the predecessor environment.

The functions- and macro-environment
------------------------------------
It contains the local function- and macro-definitions.
A functions- and macro-environment is given through a pointer to
a functions- or macrobindings-frame or NIL (which means an empty
lexical environment) or through a vector that is built as follows:
The vector contains n bindings and has length 2n+1. The elements are
n-time each function-name (a symbol) and the definiton that belongs to it (a
closure or NIL or a SYS::MACRO object) and as last element
the predecessor environment.

The block-environment
---------------------
It contains the lexically visible block-exitpoints.
A block-environment is given through a pointer to a block-frame
or through an association-list, whose elements each have the block-name (a symbol)
as CAR and as CDR either the pointer to the appropriate
frame or #DISABLED, if the block has already been left.

The tagbody-environment
-----------------------
It contains the lexically visible Go-labels of the tagbodies.
A tagbody-environment is given through a pointer to a
tagbody-frame or an associations-list, whose elements have a vector (with the
Go-tags as elements) as CAR and as CDR either the pointer to the
related frame or #DISABLED, if the tagbody has already
been left.

The declarations-environment
----------------------------
It contains the lexically visible declarations.
A declarations-environment is given through a list of declaration-
specifiers, whose CAR is each either OPTIMIZE or DECLARATION or
a user-specified declaration-type.

Passing of environtments to LISP-functions
------------------------------------------
There are two data structures for this:
When it is passed as second argument to macro-expander-functions (CLTL p.
145-146) and when it is receipted by MACROEXPAND and MACROEXPAND-1 (CLTL p. 151)
it is simply a Simple-Vector with 2 elements, consisting of a nested
variable-environment and a nested functions- and macro-environment.
The same for passing to  SYSTEM::%EXPAND-LAMBDABODY-MAIN and the like.
If it is passed as second argument to the value of *EVALHOOK* or as third one
to the value of *APPLYHOOK* (CLTL p. 322) and on reception by
EVALHOOK and APPLYHOOK (CLTL p. 323) it is a Simple-Vector with
five elements with all five components nested.

Frames
======
Frames are not used to call SUBRs, FSUBRs and compiled closures.

There are the following 14 kinds of frames:
  - Environmentbinding-Frame (ENV_FRAME),
  - APPLY-frame (APPLY_FRAME),
  - EVAL-frame (EVAL_FRAME),
  - dynamic variable-bindings-frame (DYNBIND_FRAME),
  - Variable-bindings-frame (VAR_FRAME),
  - Function- or Macrobindings-Frame (FUN_FRAME),
  - interpreted block-frame (IBLOCK_FRAME),
  - compiled block-frame (CBLOCK_FRAME),
  - interpreted tagbody-frame (ITAGBODY_FRAME),
  - compiled tagbody-frame (CTAGBODY_FRAME),
  - Catch-Frame (CATCH_FRAME),
  - Unwind-Protect-frame (UNWIND_PROTECT_FRAME),
  - Handler-frame (HANDLER_FRAME),
  - C-Handler-frame (C_HANDLER_FRAME),
  - Driver-frame (DRIVER_FRAME).
Right at the bottom of a frame there is a long-word, that contains the
frame-type information and a pointer above the frame (= the value of the
STACK before and after the frame has been built).
In the frame-info there are the bits
  SKIP2_BIT      deleted, if another long-word comes above it,
                   that is not a LISP-object and thus has to be skipped
                   by the GC,
  EXITPOINT_BIT  set for all but VAR and FUN,
  NESTED_BIT     set for IBLOCK and ITAGBODY, if the exitpoint or
                   the Go-label has already been put into an Alist.
The default-values for  the frame-type info-bytes are ENVxx_FRAME_INFO,
APPLY_FRAME_INFO, EVAL_FRAME_INFO, VAR_FRAME_INFO, FUN_FRAME_INFO,
IBLOCK_FRAME_INFO, CBLOCK_FRAME_INFO, ITAGBODY_FRAME_INFO, CTAGBODY_FRAME_INFO,
CATCH_FRAME_INFO, UNWIND_PROTECT_FRAME_INFO, DRIVER_FRAME_INFO.
The routine that is in (SP).L with SP=SP_(STACK) (for IBLOCK-, CBLOCK-,
ITAGBODY-, CTAGBODY-, CATCH-, UNWIND-PROTECT-frames), is being
jumped to by MOVE.L SP_(STACK),SP ! RTS  .
For DRIVER-frames by MOVE.L SP_(STACK),SP ! MOVE.L (SP),-(SP) ! RTS  .
In the portable C-version in SP_(STACK) there is a pointer to a
setjmp/longjmp-buffer.

Environmentbindings-frames
--------------------------
They contain dynamic bindings of a maximum of 5 environments.
ENVxx_FRAME_INFO  is frame-info (xx depending on the environment that is
bound here). Structure:
    Offset        Stack-Contents
  20/16/12/8/4  [old value ofDECL_ENV]
  16/12/8/4     [old value ofGO_ENV]
  12/8/4        [old value ofBLOCK_ENV]
  8/4           [old value ofFUN_ENV]
  4             [old value ofVAR_ENV]
  0             Frame-Info; pointer above frame

ENV1V_frame    for 1 VAR_ENV
ENV1F_frame    for 1 FUN_ENV
ENV1B_frame    for 1 BLOCK_ENV
ENV1G_frame    for 1 GO_ENV
ENV1D_frame    for 1 DECL_ENV
ENV2VD_frame   for 1 VAR_ENV and 1 DECL_ENV
ENV5_frame     for all 5 environments

APPLY-frames
------------
They are created at every call (APPLY or FUNCALL) of an interpreted
closure.
Structure:
  Offset     Stack-contents
  4n+12
  4n+8      Argument 1
  ...
  12        Argument n
  8         Function that is being called
  4         SP
  0         Frame-info; pointer above frame
SP is a pointer into the program-stack. Jumping back to (SP).L after dissolving
the APPLY-fame returns the contents of A0/... as values of the form.
The frame-info has the value APPLY_FRAME_INFO or TRAPPED_APPLY_FRAME_INFO.

EVAL-frames
-----------
They are created for every call of the EVAL-procedure.
Layout:
  Offset     Stack-content
  8         Form that is being evaluated
  4         SP
  0         Frame-info; pointer above frame
SP is a pointer into the program stack. Jumping back to (SP).L after dissolving
the EVAL-frame returns the contents of A0/... as values of the form.
The frame-info has the value EVAL_FRAME_INFO or TRAPPED_EVAL_FRAME_INFO.

Dynamic variable-bindings frames
-----------------------------------
They bind symbols to values dynamically.
The structure of such a frame with n bindings is as follows::
  Offset  stack contents
  8n+4
  8n      value 1
  8n-4    symbol 1
  ...     ...
  8       value n
  4       symbol n
  0       frame-info; pointer above frame
The content of the frameinfo-byte is DYNBIND_FRAME_INFO.

Variable-bindings-frames
------------------------
They are created when interpreted closures are being used (for the variable
bindings specified in the Lambda-list and in the dynamic references that might
be specified in the declarations) and by LET and LET*, as well as by all
constructs, that use LET or LET* implicitly (such as DO, DO*, PROG, PROG*,
DOLIST, DOTIMES, ...).
The structure of a variable-bindings-frame with n bindings is as follows:
#ifndef NO_symbolflags
  Offset  stack contents
  12+8n
  8+8n    value 1
  4+8n    symbol 1
  ...     ...
  16      value n
  12      symbol n
  8       NEXT_ENV
  4       m
  0       frame-info; pointer above frame
#else
  Offset  stack contents
  12+12n
  8+12n   value 1
  4+12n   symbol 1
  12n     marker bits 1
  ...     ...
  20      value n
  16      symbol n
  12      marker bits n
  8       NEXT_ENV
  4       m
  0       frame-info; pointer above frame
#endif
The symbol/value-pairs are numbered and stored in the order in which the
bindings become active (i.e. for interpreted closures: at first the dynamic
references (SPECIAL-declarations), then the required-parameters, then the
optional parameters, then the remaining parameters, then the keyword
parameters, then the AUX-variables).
The symbols contain the following marker bits on the stack: ACTIVE_BIT, is
set, if the binding is active, DYNAM_BIT is set, if the binding is
dynamic. (Dynamic references are marked as lexical with
the special value #<SPECDECL>!).
NEXT_ENV is next upper variables-environment.
m is a long-word, 0 <= m <= n, and stands for the number of bindings that
have not yet been put into a vector by NEST-operations. Thus
the symbol/value-pairs 1,...,n-m have been active but been nested meanwhile
and thus inactive again on the stack (if the bindings were static).
Only some of the pairs n-m+1,...,n can be static and active.
The frameinfo-byte contains VAR_FRAME_INFO.

Function- and Macrobindings-Frames
-----------------------------------
They are created by FLET and MACROLET.
The structure of a variable-bindings-frame with n bindings is as follows:
  Offset  stack contents
  12+8n
  8+8n    value 1
  4+8n    symbol 1
  ...     ...
  16      value n
  12      symbol n
  8       NEXT_ENV
  4       m
  0       Frame-Info; pointer above frame
NEXT_ENV is the next higher function-environment.
m is a long word, 0 <= m <= n, and stands for the number of bindings, that
have not yet been put into a vector by NEST-operations. So the
symbol/value pais 1,...,n-m have been active, but nested meanwhile and thus
inactive on the stack again. Only the pairs n-m+1,...,n are active.
Marker bits are not needed here, as opposed to the variable-bindings frames

All values are closures or SYS::MACRO objects.
The content of the Frameinfo-bytes is FUN_FRAME_INFO.

Interpreted Block-Frames
------------------------
They are created by BLOCK and all constructs that contain an implicit
BLOCK (e.g. DO, DO*, LOOP, PROG, PROG*, ...). The structure is as follows:
  Offset  stack contents
  16
  12       NAME
  8        NEXT_ENV
  4        SP
  0        Frame-Info; pointer above frame
NAME is the name of the block. NEXT_ENV is the next higher Block-Environment.
SP is a pointer into the program stack, (SP).L is a routine, that unwinds the
Block-Frame and leaves the block with the values A0-A2/...
Frame-Info is IBLOCK_FRAME_INFO, possibly with set NESTED_BIT (then NEXT_ENV
points to an Alist, whose first element is the pair (NAME . <Framepointer>),
because the block is not DISABLED yet).

Compiled Block-Frames
---------------------
Structure:
  Offset  stack contents
   12
   8        Cons (NAME . <Framepointer>)
   4        SP
   0        Frame-Info; pointer above frame
NAME is the name of the block.
SP is a pointer into the program stack, (SP).L is a routine, that
unwinds the Block-Frame and leaves the block with the values A0-A2/...
Frame-Info is CBLOCK_FRAME_INFO.

Interpreted Tagbody-Frames
--------------------------
They are created by TAGBODY and all constructs that contain an implicit
TAGBODY (e.g. DO, DO*, PROG, PROG*, ...).
The structure of a Tagbody-Frames with n tags is as follows:
  Offset  stack contents
  12+8n
  8+8n     BODY 1
  4+8n     TAG 1
  ...      ...
  16       BODY n
  12       TAG n
  8        NEXT_ENV
  4        SP
  0        Frame-Info; pointer above frame
The tags are the jump destinations ; they are symbols and Integers, that are in
the Body. The corresponding "value" BODY i contains the part of the body
that follows TAG i. NEXT_ENV is the next higher Tagbody-Environment.
SP is a pointer into the program stack, (SP).L is a routine, that executes
the action (GO TAGi), if it is jumped to with BODYi in A0.
Frame-Info is ITAGBODY_FRAME_INFO, poss. with set NESTED_BIT (then
NEXT_ENV points to an Alist, whose first element has the form
(#(TAG1 ... TAGn) . <Framepointer>), because the Tagbody is not
DISABLED yet).

Compiled Tagbody-Frames
-----------------------
Structure:
  Offset  stack contents
   12
   8        Cons (#(TAG1 ... TAGn) . <Framepointer>)
   4        SP
   0        Frame-Info; above frame
TAG1, ..., TAGn are the names of the tags (actually only contained in
the compiled code to create error messages).
SP is a pointer into the program stack, (SP).L is a routine, that executes
the action (GO TAGi), if it has been jumped at with value1 = i (1 <= i <= n)

Frame-Info is CTAGBODY_FRAME_INFO.

Catch-Frames
------------
They are created by the  Special-Form CATCH. Its structure is as follows:
  Offset  stack contents
   12
   8        TAG
   4        SP
   0        Frame-Info; pointer above frame
TAG is the tag of the catcher.
SP is a pointer into the program stack, (SP).L is a routine, that unwinds
the Frame and returns the values A0-A2/...
Frame-Info is CATCH_FRAME_INFO.

Unwind-Protect-Frames
---------------------
They are created by the Special-Form UNWIND-PROTECT and all constructs
that contain an implicit UNWIND-PROTECT (like WITH-OPEN-STREAM or
WITH-OPEN-FILE). Their structure is as follows:
  Offset  Stack-contents
   8
   4        SP
   0        Frame-Info; pointer above frame
SP is a pointer into the program stack. (SP).L a routine, that unwinds the
Frame,saves the current values A0-A2/...  executes the cleanup,
writes the saved values back and finally jumps to the address
(with RTS), that has been entered into the program stack in place of their own
and leaves D6 unchanged.

Handler-Frames
--------------
They are created by the macro HANDLER-BIND. Their structure is as follows:
  Offset  Stack-contens
   16
   12       Cons (#(type1 label1 ... typem labelm) . SPdepth)
   8        Closure
   4        SP
   0        Frame-Info; pointer above frame
SP is a pointer into the program stack.
If there is a condition of the type typei
the closure starting at Byte labeli is interpreted as Handler, where at first
a piece of the program stack with the length SPdepth  is duplicated.

C-Handler-Frames
----------------
This is a variant of Handler-Frames, which calls a C handler:
  Offset  Stack-contents
   16
   12       Cons (#(type1 label1 ... typem labelm))
   8        Handler-function
   4        SP
   0        Frame-Info; pointer above frame
SP is a pointer into the program stack.
If there is a condition of the type typei
the handler-function is called with the arguments SP
(arbitrary pointer into the C-Stack, or NULL), frame (pointer above the frame),
labeli (arbitrary Lisp-object), condition.
If the Handler wants to yield control via unwind_upto(FRAME) by itself,
the Frame has to be created with finish_entry_frame.

Driver-Frames
-------------
They are created upon entry into a top-level loop (most of the time
a READ-EVAL-PRINT-loop) and are used to continue the previous top-level
loop after an error message. The structure is simple
  Offset  Stack-contens
   8
   4        SP
   0        Frame-Info; pointer above Frame
SP is a pointer into the program stack. (SP).L is a routine, that
re-enters the corresponding top-level loop.

 STACK:
 ------
 STACK is the LISP-Stack.
 STACK_0 is the first object on the STACK.
 STACK_1 is the second object on the STACK.
 etc., generally STACK_(n) = (n+1)th object on the STACK.
 pushSTACK(object)  puts an object onto the Stack. Synonym: -(STACK).
 popSTACK()  returns STACK_0 and removes it from the stack.
 skipSTACK(n);  removes n objects from the STACK.
 If you want to save the value of the stack, you do this:
   var gcv_object_t* temp = STACK; ... (no access through temp !) ... setSTACK(STACK = temp);
   but: access through STACKpointable(temp)  is possible.
 If you want a pointer that can traverse through the Stack, you do this:
   var gcv_object_t* ptr = &STACK_0;  or = STACKpointable(STACK);
   assert( *(ptr STACKop 0) == STACK_0 );
   assert( *(ptr STACKop 1) == STACK_1 );
   ...
   ptr skipSTACKop n;
   assert( *(ptr STACKop 0) == STACK_(n) );
   ...
   This pointer must not be assigned to the STACK again!
 If you store blocks of objects on the STACK and want to get the (n+1)-th block,
   you do this:  STACKblock_(type,n). type should be a
   struct-type with sizeof(type) a multiple of sizeof(gcv_object_t). */

#ifdef STACK_DOWN
  #define STACK_(n)  (STACK[(sintP)(n)])
  #define STACKpointable(STACKvar)  ((gcv_object_t*)(STACKvar))
  #define skipSTACKop  +=
  #define STACKop      +
  #define cmpSTACKop   <
  #define STACKblock_(type,n)  (((type*)STACK)[(sintP)(n)])
#endif
#ifdef STACK_UP
  #define STACK_(n)  (STACK[-1-(sintP)(n)])
  #define STACKpointable(STACKvar)  ((gcv_object_t*)(STACKvar)-1)
  #define skipSTACKop  -=
  #define STACKop      -
  #define cmpSTACKop   >
  #define STACKblock_(type,n)  (((type*)STACK)[-1-(sintP)(n)])
#endif
#define pushSTACK(obj)  (STACK_(-1) = (obj), STACK skipSTACKop -1)
  /* Almost equivalent with *--STACK = obj  resp.  *STACK++ = obj  , but
   Careful: first enter the object into STACK_(-1), THEN modify the STACK! */
#define popSTACK()  (STACK skipSTACKop 1, STACK_(-1))
#define skipSTACK(n)  (STACK skipSTACKop (sintP)(n))

#if defined(GNU) && defined(M68K) && !defined(NO_ASM) && !defined(WIDE) && defined(STACK_register)
  /* With GNU and a M68K, STACK is in a register. Access and
   modification of the STACK are an atomic unit that cannot be interrupted. */
  #undef pushSTACK
  #undef popSTACK
  #ifdef STACK_DOWN
    /* define pushSTACK(obj)  (*--STACK = (obj)) */
    #define pushSTACK(obj)  \
      ({ __asm__ __volatile__ ("movel %0,"REGISTER_PREFIX""STACK_register"@-" : : "g" ((object)(obj)) : STACK_register ); })
    /* define popSTACK()  (*STACK++) */
    #define popSTACK()  \
      ({var object __result;                                                                                         \
        __asm__ __volatile__ ("movel "REGISTER_PREFIX""STACK_register"@+,%0" : "=g" (__result) : : STACK_register ); \
        __result;                                                                                                    \
       })
  #endif
  #ifdef STACK_UP
    /* define pushSTACK(obj)  (*STACK++ = (obj)) */
    #define pushSTACK(obj)  \
      ({ __asm__ __volatile__ ("movel %0,"REGISTER_PREFIX""STACK_register"@+" : : "g" ((object)(obj)) : STACK_register ); })
    /* define popSTACK()  (*--STACK) */
    #define popSTACK()  \
      ({var object __result;                                                                                         \
        __asm__ __volatile__ ("movel "REGISTER_PREFIX""STACK_register"@-,%0" : "=g" (__result) : : STACK_register ); \
        __result;                                                                                                    \
       })
  #endif
#endif
#if defined(SPARC) && !defined(GNU) && !defined(__SUNPRO_C) && !defined(MULTITHREAD) && (SAFETY < 2)
  #undef pushSTACK
  #undef popSTACK
  #undef skipSTACK
  #define pushSTACK(obj)  (STACK_(-1) = (obj), _setSTACK(STACK STACKop -1))
  #define popSTACK()  (_setSTACK(STACK STACKop 1), STACK_(-1))
  #define skipSTACK(n)  (_setSTACK(STACK STACKop (sintP)(n)))
#endif
%% export_def(STACK_(n));
%% export_def(skipSTACKop);
%% export_def(STACKop);
%% export_def(pushSTACK(obj));
%% export_def(popSTACK());
%% export_def(skipSTACK(n));

#define STACK_0  (STACK_(0))
#define STACK_1  (STACK_(1))
#define STACK_2  (STACK_(2))
#define STACK_3  (STACK_(3))
#define STACK_4  (STACK_(4))
#define STACK_5  (STACK_(5))
#define STACK_6  (STACK_(6))
#define STACK_7  (STACK_(7))
#define STACK_8  (STACK_(8))
#define STACK_9  (STACK_(9))
#define STACK_10  (STACK_(10))
/* etc. */
%% { int i;
%%   for (i=0; i<=10; i++)
%%     printf("#define STACK_%d  (STACK_(%d))\n",i,i);
%% }

/* Values:

 Highest number of multiple values + 1 */
#define mv_limit  128

/* Values are always passed in the MULTIPLE_VALUE_SPACE mv_space:
 uintC mv_count : number of values, >=0, <mv_limit
 object mv_space [mv_limit-1] : the values.
   For mv_count>0 the first mv_count elements are occupied.
   For mv_count=0 the first value = NIL.
   The values in mv_space are not subject to the Garbage Collection! */
/* VTZ: g++ is very strict about forward declarations. */
#if !defined(MULTITHREAD)
  #if !defined(mv_count_register)
    /* a global Variable */
    extern  uintC mv_count;
  #else
    /* a global register */
    register uintC mv_count __asm__(mv_count_register);
  #endif
  extern  object mv_space [mv_limit-1];
#endif
/* Synonyms: */
#if !defined(value1_register)
  #define value1  mv_space[0]
#else
  /* The first value mv_space[0] is stored permanently in a register: */
  register object value1 __asm__(value1_register);
  #define VALUE1_EXTRA /* and thus has to be treated extra every time... */
#endif
#define value2  mv_space[1]
#define value3  mv_space[2]
#define value4  mv_space[3]
#define value5  mv_space[4]
#define value6  mv_space[5]
#define value7  mv_space[6]
#define value8  mv_space[7]
#define value9  mv_space[8]
/* You might need global variables to pass with setjmp/longjmp: */
#ifdef NEED_temp_mv_count
  extern  uintC temp_mv_count;
  #define LONGJMP_SAVE_mv_count()  temp_mv_count = mv_count
  #define LONGJMP_RESTORE_mv_count()  mv_count = temp_mv_count
#else
  #define LONGJMP_SAVE_mv_count()
  #define LONGJMP_RESTORE_mv_count()
#endif
#ifdef NEED_temp_value1
  extern  object temp_value1;
  #define LONGJMP_SAVE_value1()  temp_value1 = value1
  #define LONGJMP_RESTORE_value1()  value1 = temp_value1
#else
  #define LONGJMP_SAVE_value1()
  #define LONGJMP_RESTORE_value1()
#endif
/* is used by EVAL, CONTROL,
                    Macros LIST_TO_MV, MV_TO_LIST, STACK_TO_MV, MV_TO_STACK */
%% #if !defined(MULTITHREAD)
%% #if notused
%% export_def(mv_limit);
%% #endif
%% #if !defined(mv_count_register)
%%   exportV(uintC,mv_count);
%% #else
%%   puts("#ifndef IN_MODULE_CC");
%%   printf("register uintC mv_count __asm__(\"%s\");\n",mv_count_register);
%%   puts("#endif");
%% #endif
%% printf("%s object mv_space [%d];\n",STRINGIFY(modimp),mv_limit-1);
%% emit_dll_def("mv_space");
%% #if !defined(value1_register)
%%   emit_define("value1","mv_space[0]");
%% #else
%%   puts("#ifndef IN_MODULE_CC");
%%   printf("register object value1 __asm__(\"%s\");\n",value1_register);
%%   puts("#endif");
%% #endif
%% { int i = 2;
%%   for (; i <=9 ; i++)
%%     printf("#define value%d  mv_space[%d]\n",i,i-1);
%% }
%% #endif

/* Returns the bottom objects from the STACK as multiple values.
 STACK_to_mv(count)
 count: number of objects, < mv_limit. */
#if !defined(VALUE1_EXTRA)
  #define STACK_to_mv(countx)                                   \
    do { var uintC count = (countx);                            \
      mv_count = count;                                         \
      if (count == 0) value1 = NIL;                             \
      else { /* pointer behind space for last value               */\
       object* mvp = &mv_space[count];                          \
       dotimespC(count,count, { *--mvp = popSTACK(); } );       \
    }  } while(0)
#else
  #define STACK_to_mv(countx)                                   \
    do { var uintC count = (countx);                            \
      mv_count = count;                                         \
      if (count == 0) value1 = NIL;                             \
      else {                                                    \
        count--;                                                \
        if (count > 0) { /* pointer behind space for last value   */\
          object* mvp = &mv_space[1+count];                     \
          dotimespC(count,count, { *--mvp = popSTACK(); } );    \
        }                                                       \
        value1 = popSTACK();                                    \
    }  } while(0)
#endif
/* is used by EVAL, CONTROL */
%% export_def(STACK_to_mv(countx));

/* Puts all values onto the STACK.
 mv_to_STACK()
 > mv_count/mv_space : values
 < values on the Stack (first value on top)
 STACK-Overflow is checked.
 modifies STACK */
#if !defined(VALUE1_EXTRA)
  #define mv_to_STACK()                                         \
    do { var uintC count = mv_count;                            \
         if (count!=0) { /* no values-> nothing onto the STACK    */\
           var object* mvp = &mv_space[0];                      \
           get_space_on_STACK(count);                           \
           dotimespC(count,count, { pushSTACK(*mvp++); } );     \
    }  } while(0)
#else
  #define mv_to_STACK()                                         \
    do { var uintC count = mv_count;                            \
         if (count!=0) { /* no values -> nothing onto the STACK   */\
           get_space_on_STACK(count);                           \
           pushSTACK(value1);                                   \
           count--;                                             \
           if (count > 0) {                                     \
             var object* mvp = &mv_space[1];                    \
             dotimespC(count,count, { pushSTACK(*mvp++); } );   \
           }                                                    \
    }  } while(0)
#endif
/* is used by EVAL, CONTROL */

/* Returns the elements of a list as multiple values.
 list_to_mv(list,error_statement)
 error_statement: if there's an error (too many values). */
#define NEXT_MV  *mvp++ = Car(l); l = Cdr(l); count++
#if !defined(VALUE1_EXTRA)
  #define list_to_mv(lst,error_statement)                               \
    do { var object l = (lst);                                          \
     var uintC count = 0;                                               \
     if (atomp(l)) value1 = NIL;                                        \
     else {                                                             \
       var object* mvp = &mv_space[0];                                  \
       NEXT_MV; if (atomp(l)) goto mv_done;                             \
       NEXT_MV; if (atomp(l)) goto mv_done;                             \
       NEXT_MV; if (atomp(l)) goto mv_done;                             \
       do { if (count==mv_limit-1) { error_statement; } NEXT_MV;        \
       } while (consp(l));                                              \
     }                                                                  \
     mv_done:                                                           \
     if (!nullp(l)) error_proper_list_dotted(S(values_list),l);         \
     mv_count = count;                                                  \
    } while(0)
#else
  #define list_to_mv(lst,error_statement)                               \
    do { var object l = (lst);                                          \
     var uintC count = 0;                                               \
     if (atomp(l)) value1 = NIL;                                        \
     else {                                                             \
       value1 = Car(l); l = Cdr(l); count++; if (atomp(l)) goto mv_done; \
       {var object* mvp = &mv_space[1];                                 \
        NEXT_MV; if (atomp(l)) goto mv_done;                            \
        NEXT_MV; if (atomp(l)) goto mv_done;                            \
        do { if (count==mv_limit-1) { error_statement; } NEXT_MV;       \
        } while (consp(l));                                             \
     }}                                                                 \
     mv_done:                                                           \
     if (!nullp(l)) error_proper_list_dotted(S(values_list),l);         \
     mv_count = count;                                                  \
    } while(0)
#endif
/* is used by EVAL, CONTROL */

/* Gives the list of the multiple values on -(STACK).
 mv_to_list()
 can trigger GC */
#define mv_to_list()                                                  \
  do {                                                                \
    mv_to_STACK(); /* at first all values onto the stack                */\
    GCTRIGGER();                                                      \
    pushSTACK(NIL); /* head of the list                                 */\
    { var uintC count;                                                \
      dotimesC(count,mv_count, { /* until all values have been used:    */\
        var object l = allocate_cons(); /* new cell                     */\
        Cdr(l) = popSTACK(); /* list so far                             */\
        Car(l) = STACK_0; /* next value                                 */\
        STACK_0 = l; /* save new cons                                   */\
      });                                                             \
  } } while(0)
/* is used by EVAL, CONTROL, DEBUG */

/* Error message if there are too many values
 error_mv_toomany(caller);
 > caller: caller, a Symbol */
extern _Noreturn void error_mv_toomany (object caller);
/* is used by EVAL, CONTROL, LISPARIT */
%% #if notused
%% exportE(error_mv_toomany,(object caller));
%% #endif

#if !defined(back_trace_register)
  extern  p_backtrace_t back_trace;
#else
  register p_backtrace_t back_trace __asm__(back_trace_register);
#endif
#define subr_self  back_trace->bt_function
%% #if defined(back_trace_register)
%%   puts("#ifndef IN_MODULE_CC");
%%   printf("register p_backtrace_t back_trace __asm__(\"%s\");\n",back_trace_register);
%%   puts("#endif");
%% #elif !defined(back_trace)
%%   exportV(p_backtrace_t,back_trace);
%% #endif
%% export_def(subr_self);

/* Within the body of a SUBR: Access to the arguments.
 A SUBR with a fixed number of arguments can access them through the STACK:
   STACK_0 = last argument, STACK_1 = second to last argument etc.
   Clean STACK: with skipSTACK(number of arguments) .
 A SUBR with arbitrarily many arguments (&REST-Parameter) gets passed:
     uintC argcount                    the number of the remaining arguments
     gcv_object_t* rest_args_pointer   Pointer above the remaining arguments
   Additionally:
     gcv_object_t* args_end_pointer    Pointer below all arguments, depends on the STACK
   Additionally possible:
     gcv_object_t* args_pointer = rest_args_pointer STACKop (fixed number of arguments);
                                       Pointer above the first argument
   Typical Loop-Processing:
     from the front:
       while (argcount != 0) {
         var object arg = NEXT(rest_args_pointer); ...; argcount--;
       }
       while (rest_args_pointer != args_end_pointer) {
         var object arg = NEXT(rest_args_pointer); ...;
       }
     from the back:
       while (argcount != 0) {
         var object arg = BEFORE(args_end_pointer); ...; argcount--;
       }
       while (rest_args_pointer != args_end_pointer) {
         var object arg = BEFORE(args_end_pointer); ...;
       }
   The macros NEXT and BEFORE modify their arguments!
   Clean STACK: with set_args_end_pointer(args_pointer)
     or skipSTACK((fixed number of arguments) + (uintL) (number of remainung arguments)) . */
#define args_end_pointer  STACK
#define set_args_end_pointer(new_args_end_pointer)  \
  setSTACK(STACK = (new_args_end_pointer))
#ifdef STACK_DOWN
  #define NEXT(argpointer)  (*(--(argpointer)))
  #define BEFORE(argpointer)  (*((argpointer)++))
#endif
#ifdef STACK_UP
  #define NEXT(argpointer)  (*((argpointer)++))
  #define BEFORE(argpointer)  (*(--(argpointer)))
#endif
/* Next(pointer) yields the same value as NEXT(pointer),
 but without changing the value of pointer.
 Before(pointer) yields the same value as BEFORE(pointer),
 but without changing the value of pointer. */
#define Next(pointer)  (*(STACKpointable(pointer) STACKop -1))
#define Before(pointer)  (*(STACKpointable(pointer) STACKop 0))
%% #if !defined(MULTITHREAD)
%% emit_define("args_end_pointer","STACK");
%% #if notused
%% emit_define("set_args_end_pointer(new_args_end_pointer)","STACK = (new_args_end_pointer)");
%% export_def(NEXT(argpointer));
%% export_def(BEFORE(argpointer));
%% emit_define("Next(pointer)","(*(STACKpointable(pointer) STACKop -1))");
%% emit_define("Before(pointer)","(*(STACKpointable(pointer) STACKop 0))");
%% #endif
%% #endif

/* Environments: */

typedef struct {
  object var_env;   /* Variable-Bindings-Environment */
  object fun_env;   /* Function-Bindings-Environment */
  object block_env; /* Block-Environment */
  object go_env;    /* Tagbody/Go-Environment */
  object decl_env;  /* Declarations-Environment */
} environment_t;
typedef struct {
  gcv_object_t var_env;   /* Variable-Bindings-Environment */
  gcv_object_t fun_env;   /* Function-Bindings-Environment */
  gcv_object_t block_env; /* Block-Environment */
  gcv_object_t go_env;    /* Tagbody/Go-Environment */
  gcv_object_t decl_env;  /* Declarations-Environment */
} gcv_environment_t;

/* The current Environment: */
extern  gcv_environment_t aktenv;

/* /usr/include/architecture/i386/fpu.h is not included with C++ */
#if defined(__cplusplus) && defined(UNIX_MACOSX) && !defined(environ)
#define environ (*_NSGetEnviron())
#endif

/* Macro: Puts five single Environments on the STACK
 and makes a single Environment out of them.
 make_STACK_env(venv,fenv,benv,genv,denv, env5 = );
 > object venv,fenv,benv,genv,denv: 5 single Environments
 < gcv_environment_t* env5: pointer to the Environment on the Stack */
#ifdef STACK_UP
  #define make_STACK_env(venv,fenv,benv,genv,denv,env5_assignment)      \
    do { pushSTACK(venv); pushSTACK(fenv); pushSTACK(benv);             \
         pushSTACK(genv); pushSTACK(denv);                              \
         env5_assignment &STACKblock_(gcv_environment_t,0); } while(0)
#endif
#ifdef STACK_DOWN
  #define make_STACK_env(venv,fenv,benv,genv,denv,env5_assignment)      \
    do { pushSTACK(denv); pushSTACK(genv); pushSTACK(benv);             \
         pushSTACK(fenv); pushSTACK(venv);                              \
         env5_assignment &STACKblock_(gcv_environment_t,0); } while(0)
#endif

/* Frameinfobits in Frames:
 in the Frame-Info-Byte (tint): */
#if (oint_type_len>=7) && 0 /* provisionally?? */
  /* Bit numbers in the Frame-Info-Byte:
   occupy Bits 6..0 (resp. Bits 7,5..0 if garcol_bit_t=7). */
  #ifdef TYPECODES
    #define FB7  garcol_bit_t
    #define FB6  (garcol_bit_t>TB5 ? TB5 : TB6)
    #define FB5  (garcol_bit_t>TB4 ? TB4 : TB5)
    #define FB4  (garcol_bit_t>TB3 ? TB3 : TB4)
    #define FB3  (garcol_bit_t>TB2 ? TB2 : TB3)
    #define FB2  (garcol_bit_t>TB1 ? TB1 : TB2)
    #define FB1  (garcol_bit_t>TB0 ? TB0 : TB1)
  #else
    #define FB7  garcol_bit_o
    #define FB6  30
    #define FB5  29
    #define FB4  28
    #define FB3  27
    #define FB2  26
    #define FB1  25
  #endif
  /* depending on it: */
  #define frame_bit_t    FB7  /* garcol_bit as FRAME-identifier */
  /* define skip2_limit_t  ...  (below)
    frame_info < skip2_limit_t  if the GC has to skip two long words
    frame_info >= skip2_limit_t  if the GC has to skip only the frame bottom word */
  #define unwind_bit_t   FB5  /* set if there's something to do while */
                              /* unwinding the frame */
  /* frame_info >= skip2_limit_t ==> unwind-Bit=1. */
  /* for further Information within the Frames with frame_info >= skip2_limit_t: */
  #define envbind_bit_t  FB4  /* Bit set for ENV-Frames. */
                              /* Bit is unset for DYNBIND-Frames. */
  /* for further identification of the ENV-Frames: */
  #define envbind_case_mask_t  (bit(FB3)|bit(FB2)|bit(FB1))
  /* for further discrimination within the Frames with frame_info < skip2_limit_t: */
  #define entrypoint_bit_t  FB4  /* Bit is set, if FRAME contains */
  /* a non-local entrypoint, with Offset SP_, SP is on the STACK.
   Bit is unset for VAR/FUN-Frame and CALLBACK-Frame. */
  /* for further discrimination in BLOCK/TAGBODY/APPLY/EVAL/CATCH/UNWIND_PROTECT/HANDLER/C_HANDLER/DRIVER: */
  /* define blockgo_max_t  (below)
     >= all BLOCK/TAGBODY frame infos,
     < UNWIND_PROTECT_frame_info, DRIVER_frame_info. */
  /* for further discrimination in BLOCK/TAGBODY: */
  #define nested_bit_t unwind_bit_t /* set for IBLOCK and ITAGBODY, */
                                    /* if Exitpoint resp. Tags were nested */
  /* for further discrimination in APPLY/EVAL/CATCH/UNWIND_PROTECT/HANDLER/C_HANDLER/DRIVER: */
  #define dynjump_mask_t  bit(FB2)  /* unset for APPLY and EVAL, */
                                /* partially set for UNWIND_PROTECT/DRIVER-Frames */
  #define trapped_bit_t unwind_bit_t /* set for APPLY and EVAL, if */
                                /* interrupted while unwinding the Frame */
  /* unwind-Bit set for UNWIND_PROTECT/DRIVER/TRAPPED_APPLY/TRAPPED_EVAL,
   else unset. */
  #define eval_bit_t     FB1    /* set for EVAL-Frames, */
                                /* unset for APPLY-Frames */
  #define driver_bit_t   FB1    /* set for DRIVER-Frames, */
                                /* unset for UNWIND_PROTECT-Frames */
  #define handler_bit_t  FB2    /* set for HANDLER/C_HANDLER-Frames, */
                                /* unset for CATCH-Frames */
  /* for further discrimination in VAR/FUN/CALLBACK: */
  #define callback_bit_t   FB3  /* Bit is unset for CALLBACK-Frames. */
                                /* Bit is set for VAR/FUN-Frames. */
  /* for further discrimination in VAR/FUN: */
  #define fun_bit_t      FB2    /* set for FUN-Frame, unset for VAR-Frame */
  /* on objects on the STACK (oint): */
  #define      frame_bit_o      (frame_bit_t+oint_type_shift)
  #define     unwind_bit_o     (unwind_bit_t+oint_type_shift)
  #define    envbind_bit_o    (envbind_bit_t+oint_type_shift)
  #define   callback_bit_o   (callback_bit_t+oint_type_shift)
  #define entrypoint_bit_o (entrypoint_bit_t+oint_type_shift)
  #define     nested_bit_o     (nested_bit_t+oint_type_shift)
  #define    trapped_bit_o    (trapped_bit_t+oint_type_shift)
  #define       eval_bit_o       (eval_bit_t+oint_type_shift)
  #define     driver_bit_o     (driver_bit_t+oint_type_shift)
  #define    handler_bit_o    (handler_bit_t+oint_type_shift)
  #define        fun_bit_o        (fun_bit_t+oint_type_shift)
  /* single Frame-Info-Bytes: */
  #define DYNBIND_frame_info          /* %1110... */ (bit(FB7)|bit(FB6)|bit(FB5))
  #define ENV1V_frame_info            /* %1111000 */ (bit(FB7)|bit(FB6)|bit(FB5)|bit(FB4))
  #define ENV1F_frame_info            /* %1111001 */ (bit(FB7)|bit(FB6)|bit(FB5)|bit(FB4)|bit(FB1))
  #define ENV1B_frame_info            /* %1111010 */ (bit(FB7)|bit(FB6)|bit(FB5)|bit(FB4)|bit(FB2))
  #define ENV1G_frame_info            /* %1111011 */ (bit(FB7)|bit(FB6)|bit(FB5)|bit(FB4)|bit(FB2)|bit(FB1))
  #define ENV1D_frame_info            /* %1111100 */ (bit(FB7)|bit(FB6)|bit(FB5)|bit(FB4)|bit(FB3))
  #define ENV2VD_frame_info           /* %1111101 */ (bit(FB7)|bit(FB6)|bit(FB5)|bit(FB4)|bit(FB3)|bit(FB1))
  #define ENV5_frame_info             /* %1111110 */ (bit(FB7)|bit(FB6)|bit(FB5)|bit(FB4)|bit(FB3)|bit(FB2))
  #define skip2_limit_t                              (bit(FB7)|bit(FB6)|bit(FB5))
  #ifdef HAVE_SAVED_REGISTERS
    #define CALLBACK_frame_info       /* %10100.. */ (bit(FB7)|bit(FB5))
  #endif
  #define VAR_frame_info              /* %101010. */ (bit(FB7)|bit(FB5)|bit(FB3))
  #define FUN_frame_info              /* %101011. */ (bit(FB7)|bit(FB5)|bit(FB3)|bit(FB2))
  #define IBLOCK_frame_info           /* %1001000 */ (bit(FB7)|bit(FB4))
  #define NESTED_IBLOCK_frame_info    /* %1011000 */ (bit(FB7)|bit(FB5)|bit(FB4))
  #define ITAGBODY_frame_info         /* %1001010 */ (bit(FB7)|bit(FB4)|bit(FB2))
  #define NESTED_ITAGBODY_frame_info  /* %1011010 */ (bit(FB7)|bit(FB5)|bit(FB4)|bit(FB2))
  #define CBLOCK_CTAGBODY_frame_info  /* %1011001 */ (bit(FB7)|bit(FB5)|bit(FB4)|bit(FB1))
  #define APPLY_frame_info            /* %1001100 */ (bit(FB7)|bit(FB4)|bit(FB3))
  #define TRAPPED_APPLY_frame_info    /* %1011100 */ (bit(FB7)|bit(FB5)|bit(FB4)|bit(FB3))
  #define EVAL_frame_info             /* %1001101 */ (bit(FB7)|bit(FB4)|bit(FB3)|bit(FB1))
  #define TRAPPED_EVAL_frame_info     /* %1011101 */ (bit(FB7)|bit(FB5)|bit(FB4)|bit(FB3)|bit(FB1))
  #define CATCH_frame_info            /* %1101000 */ (bit(FB7)|bit(FB6)|bit(FB4))
  #define HANDLER_frame_info          /* %1101010 */ (bit(FB7)|bit(FB6)|bit(FB4)|bit(FB2))
  #define C_HANDLER_frame_info        /* %1101011 */ (bit(FB7)|bit(FB6)|bit(FB4)|bit(FB2)|bit(FB1))
  #define UNWIND_PROTECT_frame_info   /* %1011110 */ (bit(FB7)|bit(FB5)|bit(FB4)|bit(FB3)|bit(FB2))
  #define DRIVER_frame_info           /* %1011111 */ (bit(FB7)|bit(FB5)|bit(FB4)|bit(FB3)|bit(FB2)|bit(FB1))
  #define blockgo_max_t                              (bit(FB7)|bit(FB5)|bit(FB4)|bit(FB2))
#endif
#if (oint_type_len==6) || 1 /* provisionally?? */
  /* bit numbers in Frame-Info-Byte:
   occupy Bits 5..0 (resp. Bits 7,4..0 if garcol_bit_t=7). */
  #ifdef TYPECODES
    #define FB6  garcol_bit_t
    #define FB5  (garcol_bit_t>TB4 ? TB4 : TB5)
    #define FB4  (garcol_bit_t>TB3 ? TB3 : TB4)
    #define FB3  (garcol_bit_t>TB2 ? TB2 : TB3)
    #define FB2  (garcol_bit_t>TB1 ? TB1 : TB2)
    #define FB1  (garcol_bit_t>TB0 ? TB0 : TB1)
  #else /* HEAPCODES */
    #define FB6  garcol_bit_o
    #ifdef ONE_FREE_BIT_HEAPCODES
      #define FB5  (garcol_bit_o-1)
      #define FB4  (garcol_bit_o-2)
      #define FB3  (garcol_bit_o-3)
      #define FB2  (garcol_bit_o-4)
      #define FB1  (garcol_bit_o-5)
    #endif
    #if defined(KERNELVOID32_HEAPCODES) || defined(GENERIC64_HEAPCODES)
      #define FB5  5
      #define FB4  4
      #define FB3  3
      #define FB2  2
      #define FB1  1
    #endif
  #endif
  /* depending on it: */
  #define frame_bit_t    FB6  /* garcol_bit as FRAME-indicator */
  /* define skip2_limit_t  ...  (below)
    frame_info < skip2_limit_t  if the GC has to skip two long words
    frame_info >= skip2_limit_t  if the GC has to skip only the frame bottom word */
  /* define unwind_limit_t  ...  (below)
    frame_info >= unwind_limit_t  if there's something to be done while unwinding the frame
    Note: skip2_limit_t >= unwind_limit_t. */
  /* for further information within the Frames with frame_info >= skip2_limit_t: */
  #define envbind_bit_t  FB4  /* Bit is set for ENV-Frames. */
                              /* Bit unset for DYNBIND-Frames. */
  /* for further identification within the ENV-Frames: */
  #define envbind_case_mask_t  (bit(FB3)|bit(FB2)|bit(FB1))
  /* for further discrimination with the Frames with frame_info < skip2_limit_t:
   define entrypoint_limit_t  ...  (below)
   frame_info < entrypoint_limit_t
     if FRAME contains a non-local entry point
     with Offset SP_ SP is on the STACK.
   frame_info >= entrypoint_limit_t
     for VAR/FUN-Frame and CALLBACK-Frame. */
  /* for further discrimination in BLOCK/TAGBODY/APPLY/EVAL/CATCH/UNWIND_PROTECT/HANDLER/C_HANDLER/DRIVER: */
  /* define blockgo_max_t  (below)
     >= all BLOCK/TAGBODY frame infos,
     < UNWIND_PROTECT_frame_info, DRIVER_frame_info. */
  /* for further discrimination in BLOCK/TAGBODY: */
  #define nested_bit_t   FB4  /* set for IBLOCK and ITAGBODY, */
                              /* if exit point or Tags have been nested */
  /* for further discrimination in APPLY/EVAL/CATCH/UNWIND_PROTECT/HANDLER/C_HANDLER/DRIVER: */
  #define dynjump_mask_t  bit(FB3)  /* unset for APPLY and EVAL, */
                              /* partially set for UNWIND_PROTECT/DRIVER-Frames */
  #define trapped_bit_t  FB4  /* set for APPLY and EVAL, if interrupted while */
                              /* unwinding the Frames */
  /* >= unwind_limit_t for UNWIND_PROTECT/DRIVER/TRAPPED_APPLY/TRAPPED_EVAL,
   < unwind_limit_t else. */
  #define eval_bit_t     FB1  /* set for EVAL-Frames, */
                              /* unset for APPLY-Frames */
  #define driver_bit_t   FB1  /* set for DRIVER-Frames, */
                              /* unset for UNWIND_PROTECT-Frames */
  #define handler_bit_t  FB2  /* set for HANDLER/C_HANDLER-Frames, */
                              /* unset for CATCH-Frames */
  /* for further discrimination in VAR/FUN/CALLBACK: */
  #define callback_bit_t FB2  /* Bit is unset for CALLBACK-Frames. */
                              /* Bit is set for VAR/FUN-Frames. */
  /* for further discrimination in VAR/FUN: */
  #define fun_bit_t      FB1  /* set for FUN-Frame, unset for VAR-Frame */
  /* in Objects on the STACK (oint): */
  #define    frame_bit_o    (frame_bit_t+oint_type_shift)
  #define  envbind_bit_o  (envbind_bit_t+oint_type_shift)
  #define callback_bit_o (callback_bit_t+oint_type_shift)
  #define   nested_bit_o   (nested_bit_t+oint_type_shift)
  #define  trapped_bit_o  (trapped_bit_t+oint_type_shift)
  #define     eval_bit_o     (eval_bit_t+oint_type_shift)
  #define   driver_bit_o   (driver_bit_t+oint_type_shift)
  #define  handler_bit_o  (handler_bit_t+oint_type_shift)
  #define      fun_bit_o      (fun_bit_t+oint_type_shift)
  /* single Frame-Info-Bytes: */
  #define CATCH_frame_info            /* %100000 */ (bit(FB6))
  #define APPLY_frame_info            /* %100010 */ (bit(FB6)|bit(FB2))
  #define EVAL_frame_info             /* %100011 */ (bit(FB6)|bit(FB2)|bit(FB1))
  #define HANDLER_frame_info          /* %100100 */ (bit(FB6)|bit(FB3))
  #define C_HANDLER_frame_info        /* %100101 */ (bit(FB6)|bit(FB3)|bit(FB1))
  #define IBLOCK_frame_info           /* %100110 */ (bit(FB6)|bit(FB3)|bit(FB2))
  #define ITAGBODY_frame_info         /* %100111 */ (bit(FB6)|bit(FB3)|bit(FB2)|bit(FB1))
  #define unwind_limit_t                            (bit(FB6)|bit(FB4))
  #define NESTED_IBLOCK_frame_info    /* %101000 */ (bit(FB6)|bit(FB4))
  #define NESTED_ITAGBODY_frame_info  /* %101001 */ (bit(FB6)|bit(FB4)|bit(FB1))
  #define TRAPPED_APPLY_frame_info    /* %101010 */ (bit(FB6)|bit(FB4)|bit(FB2))
  #define TRAPPED_EVAL_frame_info     /* %101011 */ (bit(FB6)|bit(FB4)|bit(FB2)|bit(FB1))
  #define CBLOCK_CTAGBODY_frame_info  /* %101100 */ (bit(FB6)|bit(FB4)|bit(FB3))
  #define blockgo_max_t                             (bit(FB6)|bit(FB4)|bit(FB3))
  #define UNWIND_PROTECT_frame_info   /* %101110 */ (bit(FB6)|bit(FB4)|bit(FB3)|bit(FB2))
  #define DRIVER_frame_info           /* %101111 */ (bit(FB6)|bit(FB4)|bit(FB3)|bit(FB2)|bit(FB1))
  #define entrypoint_limit_t                        (bit(FB6)|bit(FB5))
  #ifdef HAVE_SAVED_REGISTERS
    #define CALLBACK_frame_info       /* %110001 */ (bit(FB6)|bit(FB5)|bit(FB1))
  #endif
  #define VAR_frame_info              /* %110010 */ (bit(FB6)|bit(FB5)|bit(FB2))
  #define FUN_frame_info              /* %110011 */ (bit(FB6)|bit(FB5)|bit(FB2)|bit(FB1))
  #define skip2_limit_t                             (bit(FB6)|bit(FB5)|bit(FB3))
  #define DYNBIND_frame_info          /* %1101.. */ (bit(FB6)|bit(FB5)|bit(FB3))
  #define ENV1V_frame_info            /* %111000 */ (bit(FB6)|bit(FB5)|bit(FB4))
  #define ENV1F_frame_info            /* %111001 */ (bit(FB6)|bit(FB5)|bit(FB4)|bit(FB1))
  #define ENV1B_frame_info            /* %111010 */ (bit(FB6)|bit(FB5)|bit(FB4)|bit(FB2))
  #define ENV1G_frame_info            /* %111011 */ (bit(FB6)|bit(FB5)|bit(FB4)|bit(FB2)|bit(FB1))
  #define ENV1D_frame_info            /* %111100 */ (bit(FB6)|bit(FB5)|bit(FB4)|bit(FB3))
  #define ENV2VD_frame_info           /* %111101 */ (bit(FB6)|bit(FB5)|bit(FB4)|bit(FB3)|bit(FB1))
  #define ENV5_frame_info             /* %111110 */ (bit(FB6)|bit(FB5)|bit(FB4)|bit(FB3)|bit(FB2))
#endif
#define CBLOCK_frame_info  CBLOCK_CTAGBODY_frame_info
#define CTAGBODY_frame_info  CBLOCK_CTAGBODY_frame_info
%% #ifdef HAVE_SAVED_REGISTERS
%%   export_def(CALLBACK_frame_info);
%% #endif

/* Bits for Symbols in VAR-Frames:
 bit(active_bit),bit(dynam_bit),bit(svar_bit) must fit into one uintB: */
#if !((active_bit<intBsize) && (dynam_bit<intBsize) && (svar_bit<intBsize))
  #error Symbol bits do not fit in a single byte -- Symbol-Bits passen nicht in ein Byte!
#endif
#ifdef NO_symbolflags
  /* Bits are separatly stored on the Stack as Fixnums. */
  #undef oint_symbolflags_shift
  #define oint_symbolflags_shift  oint_data_shift
#else
  #if (oint_symbolflags_shift==oint_addr_shift)
    /* bit(active_bit),bit(dynam_bit),bit(svar_bit) must be true divisors
     of varobject_alignment: */
    #if (varobject_alignment % bit(active_bit+1)) || (varobject_alignment % bit(dynam_bit+1)) || (varobject_alignment % bit(svar_bit+1))
      #error No more room for three bits in a symbol -- Kein Platz fuer drei Bits in der Adresse eines Symbols!
    #endif
  #endif
#endif
#define active_bit_o  (active_bit+oint_symbolflags_shift)  /* set: binding is active */
#define dynam_bit_o   (dynam_bit+oint_symbolflags_shift)   /* set: binding is dynamic */
#define svar_bit_o    (svar_bit+oint_symbolflags_shift)    /* set: next parameter is supplied-p-parameter for this */

/* Offsets for data in Frames, to be addressed via STACK_(Offset) */
#define frame_form      2  /* EVAL */
#define frame_closure   2  /* APPLY, HANDLER */
#define frame_count     1  /* VAR, FUN */
#define frame_SP        1  /* IBLOCK, CBLOCK, ITAGBODY, CTAGBODY, */
                           /* EVAL, CATCH, UNWIND-PROTECT, HANDLER, C_HANDLER, DRIVER */
#define frame_next_env  2  /* VAR, FUN, IBLOCK, ITAGBODY */
#define frame_ctag      2  /* CBLOCK, CTAGBODY */
#define frame_tag       2  /* CATCH */
#define frame_handlers  3  /* HANDLER, C_HANDLER */
#define frame_name      3  /* IBLOCK */
#define frame_args      3  /* APPLY */
#define frame_bindings  3  /* VAR, FUN, ITAGBODY */
/* Structure of the different bindings in VAR-Frames: */
#ifdef NO_symbolflags
  #define varframe_binding_size  3
  #define varframe_binding_mark   0
  #define varframe_binding_sym    1
  #define varframe_binding_value  2
  #define pushSTACK_symbolwithflags(symbol,flags)  \
    pushSTACK(symbol); pushSTACK(as_object(as_oint(Fixnum_0) | (oint)(flags)))
#else
  #define varframe_binding_size  2
  #define varframe_binding_mark   0
  #define varframe_binding_sym    0
  #define varframe_binding_value  1
  #define pushSTACK_symbolwithflags(symbol,flags)  \
    pushSTACK(as_object(as_oint(symbol) | (oint)(flags)))
#endif

/* Special value to mark BLOCK- and TAGBODY-references that are not 'live'
   anymore (replaces the Frame-Pointer in the CDR of the corresponding Cons) */
#define disabled  make_system(0xDDDDDDUL)

/* Value to mark specially declared references */
#define specdecl  make_system(0xECDECDUL)

/* Value to mark implementation-dependent references */
#define impdependent  make_system(0xDE6114UL)

/* Handling Frames:
 A local variable FRAME contains the value of STACK after
 creating a Frame. Then you can access with FRAME_(n) just like
 with likeSTACK_(n): */
#ifdef STACK_DOWN
  #define FRAME_(n)  (FRAME[(sintP)(n)])
#endif
#ifdef STACK_UP
  #define FRAME_(n)  (FRAME[-1-(sintP)(n)])
#endif
/* make_framepointer(FRAME) is the Frame-Pointer as LISP-object.
 framecode(FRAME_(0)) is the Frame-Info-Byte (of Type fcint),
 topofframe(FRAME_(0)) is a Pointer above the Frame.
 FRAME = uTheFramepointer(obj) is a Frame-Pointer as pointer into the Stack.
         [uTheFramepointer is the exact opposite of make_framepointer.]
 FRAME = TheFramepointer(obj) as well, but possibly still with type info!
         [An attenuation of uTheFramepointer, that is enough for access.] */
#ifdef TYPECODES
  #if !defined(SINGLEMAP_MEMORY_STACK)
    #define make_framepointer(stack_ptr)  type_pointer_object(system_type,stack_ptr)
    #define topofframe(bottomword)  (gcv_object_t*)upointer(bottomword)
    #define uTheFramepointer(obj)  (gcv_object_t*)upointer(obj)
  #else
    #define make_framepointer(stack_ptr)  (as_object((oint)(stack_ptr)))
    #define topofframe(bottomword)  (gcv_object_t*)as_oint(type_pointer_object(system_type,upointer(bottomword)))
    #define uTheFramepointer(obj)  TheFramepointer(obj) /* = (gcv_object_t*)(obj) */
  #endif
  #define framecode(bottomword)  mtypecode(bottomword)
  typedef tint fcint;
#else
  /* Here the bottomword consists of the frame size, not the top of frame itself.
   This leaves room for the frame info byte. */
  #define make_framepointer(stack_ptr)  make_machine(stack_ptr)
  #ifdef ONE_FREE_BIT_HEAPCODES
    #define makebottomword(type,size)  as_object((oint)(type)+(oint)(size))
    #define framecode(bottomword)  (as_oint(bottomword) & minus_wbit(FB1))
    #define framesize(bottomword)  (as_oint(bottomword)&(wbit(FB1)-1))
  #endif
  #if defined(KERNELVOID32_HEAPCODES) || defined(GENERIC64_HEAPCODES)
    #define makebottomword(type,size)  as_object((oint)(type)+((oint)(size)<<6))
    #define framecode(bottomword)  (as_oint(bottomword) & 0x3F)
    #define framesize(bottomword)  (as_oint(bottomword) >> 6)
  #endif
  #ifdef STACK_UP
    #define topofframe(bottomword)  \
      (gcv_object_t*)((uintP)(&(bottomword))-(uintP)framesize(bottomword)+sizeof(gcv_object_t))
  #endif
  #ifdef STACK_DOWN
    #define topofframe(bottomword)  \
      (gcv_object_t*)((uintP)(&(bottomword))+(uintP)framesize(bottomword))
  #endif
  #define uTheFramepointer(obj)  TheFramepointer(obj) /* = (gcv_object_t*)(obj) */
  typedef oint fcint;
#endif
/* is used by EVAL, CONTROL, DEBUG */
%% #ifdef HEAPCODES
%%  export_def(makebottomword(type,size));
%% #endif
%% export_def(framecode(bottomword));

/* To determine the size of a frame:
 STACK_item_count(new_STACK_ptr,old_STACK_ptr)
 calculates the number of STACK-elements between an older stack pointer
 old_STACK_ptr and a new one new_STACK_ptr.
 (That's count with old_STACK_ptr = new_STACK_ptr STACKop count .) */
#ifdef STACK_DOWN
  #define STACK_item_count(new_STACK_ptr,old_STACK_ptr)  \
    (uintL)((old_STACK_ptr) - (new_STACK_ptr))
#endif
#ifdef STACK_UP
  #define STACK_item_count(new_STACK_ptr,old_STACK_ptr)  \
    (uintL)((new_STACK_ptr) - (old_STACK_ptr))
#endif

/* Finishes a frame.
 finish_frame(frametype);
 > gcv_object_t* top_of_frame: pointer to the top of the frame
 decreases STACK by 1 */
#ifdef TYPECODES
  #if !defined(SINGLEMAP_MEMORY_STACK)
    #define framebottomword(type,top_of_frame,bot_of_frame)  \
      type_pointer_object(type,top_of_frame)
  #else /* top_of_frame has already Typinfo system_type */
    #define framebottomword(type,top_of_frame,bot_of_frame)  \
      as_object(type_zero_oint(type)-type_zero_oint(system_type)+(oint)(top_of_frame))
  #endif
  #define finish_frame(frametype)  \
    pushSTACK(framebottomword(frametype##_frame_info,top_of_frame,bot_of_frame_ignored))
#else
  #ifdef STACK_UP
    #define framebottomword(type,top_of_frame,bot_of_frame)  \
      makebottomword(type,(uintP)(bot_of_frame)-(uintP)(top_of_frame))
  #endif
  #ifdef STACK_DOWN
    #define framebottomword(type,top_of_frame,bot_of_frame)  \
      makebottomword(type,(uintP)(top_of_frame)-(uintP)(bot_of_frame))
  #endif
  #define finish_frame(frametype)  \
    (STACK_(-1) = framebottomword(frametype##_frame_info,top_of_frame,STACK STACKop -1), skipSTACK(-1))
#endif
/* is used by EVAL, CONTROL */
%% export_def(framebottomword(type,top_of_frame,bot_of_frame));
%% export_def(finish_frame(frametype));

/* Makes a Frame for all 5 Environments
 make_ENV5_frame();
 decreases STACK by 5 */
#define make_ENV5_frame()                       \
  do { var gcv_object_t* top_of_frame = STACK;  \
       pushSTACK(aktenv.decl_env);              \
       pushSTACK(aktenv.go_env);                \
       pushSTACK(aktenv.block_env);             \
       pushSTACK(aktenv.fun_env);               \
       pushSTACK(aktenv.var_env);               \
       finish_frame(ENV5);                      \
  } while(0)
/* is used by EVAL, CONTROL, DEBUG */

/* Finishes a Frame with entry point and places jump-point here.
 finish_entry_frame(frametype,returner,retval_assignment,reentry_statement);
 > gcv_object_t* top_of_frame: pointer to the top of the frame
 > sp_jmp_buf* returner: longjmp-Buffer for re-entry
 > retval_assignment: allocated of the setjmp()-value to a variable
 > reentry_statement: what is to be done immediately after re-entry.
 decreases STACK by 1 */
#define finish_entry_frame(frametype,returner,retval_assignment,reentry_statement)  \
  do { pushSTACK(fake_gcv_object((aint)(returner))); /* SP onto the Stack       */\
    pushSTACK(nullobj); /* dummy onto the Stack, until re-entry is permitted    */\
    begin_setjmp_call();                                                      \
    if ((retval_assignment setjmpspl(returner))!=0) /* set point for returner   */\
      { end_longjmp_call(); LONGJMP_RESTORE_mv_count(); LONGJMP_RESTORE_value1(); reentry_statement } /* after re-entry  */\
    else                                                                      \
      { end_setjmp_call(); STACK_0 = framebottomword(frametype##_frame_info,top_of_frame,STACK); } \
  } while(0)
/* is used by EVAL, CONTROL, DEBUG */

#ifdef MULTITHREAD
  /* unwind pinned objects up to the current stack location in thread.
     executes statement for each object that has been unpinned */
  #define unwind_pinned_objects(thread)                                 \
    do {                                                                \
      var clisp_thread_t *thr = thread;                                 \
      var pinned_chain_t **p = &(thr->_pinned);                         \
      while (*p && !((aint)(thr->_STACK) cmpSTACKop (aint)(*p)->pc_unwind_stack_ptr)) { \
        *p = (*p)->pc_next;                                             \
      }                                                                 \
    } while(0)
#else /* !MULTITHREAD */
  #define unwind_pinned_objects(thread)
#endif

/* Jumps to a Frame with entry point that starts at STACK.
 (Important: The STACK has to have the same values it had when the
 frame was created, since the STACK might not be saved at setjmp/longjmp)
 Never returns and cleans the SP!!
 The multiple values are passed.
 enter_frame_at_STACK(); */
#define enter_frame_at_STACK()                                do {      \
  /* the returner of finish_entry_frame: */                             \
  var sp_jmp_buf* returner = (sp_jmp_buf*)(aint)as_oint(STACK_(frame_SP)); \
  unwind_back_trace(back_trace,STACK);                                  \
  unwind_pinned_objects(current_thread());                              \
  LONGJMP_SAVE_value1(); LONGJMP_SAVE_mv_count();                       \
  begin_longjmp_call();                                                 \
  longjmpspl(*returner,(aint)returner);/* jump there, pass own addess (/=0) */\
  NOTREACHED;                                                           \
 } while(0)
/* is used by EVAL */

/* Makes a C_HANDLER-Frame.
 make_C_HANDLER_frame(types_labels_vector_list,handler,sp_arg);
 make_C_HANDLER_entry_frame(types_labels_vector_list,handler,returner,reentry_statement);
 > object types_labels_vector_list: a list containing a simple-vector: (#(type1 label1 ... typem labelm))
 > handler: void (*) (void* sp, gcv_object_t* frame, object label, object condition)
 > sp_arg: a pointer into the C stack, or NULL
 > sp_jmp_buf* returner: longjmp-Buffer for re-entry
 > reentry_statement: what is to be done right after the re-entry. */
#define make_C_HANDLER_frame(types_labels_vector_list,handler,sp_arg)  \
  do { var gcv_object_t* top_of_frame = STACK;             \
       pushSTACK(types_labels_vector_list);                \
       pushSTACK(fake_gcv_object((aint)(void*)(handler))); \
       pushSTACK(fake_gcv_object((aint)(sp_arg)));         \
       finish_frame(C_HANDLER);                            \
  } while(0)
#define make_C_HANDLER_entry_frame(types_labels_vector_list,handler,returner,reentry_statement)  \
  do { var gcv_object_t* top_of_frame = STACK;                    \
       pushSTACK(types_labels_vector_list);                       \
       pushSTACK(fake_gcv_object((aint)(void*)(handler)));        \
       finish_entry_frame(C_HANDLER,returner,,reentry_statement); \
  } while(0)
#define unwind_C_HANDLER_frame()  skipSTACK(4)

/* UP: Applies a function to its arguments.
 apply(function,args_on_stack,other_args);
 > function: function
 > Arguments: args_on_stack arguments on the STACK,
              remaining argument-list in other_args
 < STACK: cleaned (ie. STACK is increased by args_on_stack)
 < mv_count/mv_space: values
 modifies STACK, can trigger GC */
extern maygc Values apply (object fun, uintC args_on_stack, object other_args);
/* is used by EVAL, CONTROL, IO, PATHNAME, ERROR */
%% #if notused
%% exportF(Values,apply,(object fun, uintC args_on_stack, object other_args));
%% #endif

/* UP: Applies a function to its arguments.
 funcall(function,argcount);
 > function: function
 > Arguments: argcount arguments on the STACK
 < STACK: cleaned (ie. STACK is increased by argcount)
 < mv_count/mv_space: values
 modifies STACK, can trigger GC */
extern maygc Values funcall (object fun, uintC argcount);
/* is used by all Modules */
%% exportF(Values,funcall,(object fun, uintC argcount));

/* UP: Evaluates a Form in the current Environment.
 eval(form);
 > form: Form
 < mv_count/mv_space: values
 can trigger GC */
extern maygc Values eval (object form);
/* is used by CONTROL, DEBUG */
%% #if notused
%% exportF(Values,eval,(object form));
%% #endif

/* UP: Evaluates a Form in a given Environment.
 eval_5env(form,var,fun,block,go,decl);
 > var_env: Value for VAR_ENV
 > fun_env: Value for FUN_ENV
 > block_env: Value for BLOCK_ENV
 > go_env: Value for GO_ENV
 > decl_env: Value for DECL_ENV
 > form: Form
 < mv_count/mv_space: Values
 can trigger GC */
extern maygc Values eval_5env (object form, object var_env, object fun_env, object block_env, object go_env, object decl_env);
/* is used by */

/* UP: Evaluates a Form in an empty Environment.
 eval_noenv(form);
 > form: Form
 < mv_count/mv_space: Values
 can trigger GC */
extern maygc Values eval_noenv (object form);
/* is used by CONTROL, IO, DEBUG, SPVW */

/* UP: Evaluates a Form in the current Environment. Doesn't care about
 *EVALHOOK* and *APPLYHOOK*.
 eval_no_hooks(form);
 > form: Form
 < mv_count/mv_space: Values
 can trigger GC */
extern maygc Values eval_no_hooks (object form);
/* is used by CONTROL */

/* UP: signal an error on a dotted form in EVAL
 error_dotted_form(form,fun)
 > form: full form being evaluated
 > fun: caller (car form) */
global _Noreturn void error_dotted_form (object form, object fun);
/* is used by CONTROL */

/* UP: binds *EVALHOOK* and *APPLYHOOK* dynamically to the given values.
 bindhooks(evalhook_value,applyhook_value);
 > evalhook_value: Value for *EVALHOOK*
 > applyhook_value: Value for *APPLYHOOK*
 modifies STACK */
extern void bindhooks (object evalhook_value, object applyhook_value);
/* is used by CONTROL */

/* UP: Unwinds a Frame, to which STACK points.
 unwind();
 The values mv_count/mv_space aren't changed.
 If it is no Unwind-Protect-Frame: returns normally.
 If it is an Unwind-Protect-Frame:
   saves the values, climbs up the STACK and SP
   and then jumps to unwind_protect_to_save.fun.
 modifies STACK
 can trigger GC */
typedef _GL_NORETURN_FUNCPTR /*maygc*/ void (*restartf_t) (gcv_object_t* upto_frame);
typedef struct {
  restartf_t fun;
  gcv_object_t* upto_frame;
} unwind_protect_caller_t;
extern  unwind_protect_caller_t unwind_protect_to_save;
extern /*maygc*/ void unwind (void);
/* is used by CONTROL, DEBUG, SPVW */

/* UP: "unwinds" the STACK to the next DRIVER_FRAME and
 jumps to the corresponding Top-Level-loop
 if count=0, unwind to TOP; otherwise reset that many times */
extern _GL_NORETURN_FUNC void reset (uintL count);
/* is used by SPVW, CONTROL */

/* UP: binds the symbols of the list symlist dynamically
 to the values of the list vallist.
 progv(symlist,vallist);
 > symlist, vallist: two lists
 Exactly one variable-bindings-frame is created.
 modifies STACK
 can trigger GC */
extern maygc void progv (object symlist, object vallist);
/* used by CONTROL, EVAL */

/* UP: Unwinds the dynamic nesting on the STACK until the frame
 (exclusively), to which upto points, and jumps to it.
 unwind_upto(upto);
 > upto: Pointer to a Frame (into the Stack, without type-info).
 Saves the values mv_count/mv_space.
 modifies STACK,SP
 can trigger GC
 Jumps to the found Frame. */
extern _GL_NORETURN_FUNC /*maygc*/ void unwind_upto (gcv_object_t* upto_frame);
/* is used by CONTROL, DEBUG */

/* UP: throws to the Tag tag and passes the values mv_count/mv_space.
 Only returns, if there is no CATCH-Frame of this tag.
 throw_to(tag); */
extern void throw_to (object tag);
/* is used by CONTROL */

/* UP: Invokes all handlers for the condition cond. Only returns, if none
 of the handlers feels responsible (ie. if every handler returns).
 invoke_handlers(cond);
 can trigger GC */
extern maygc void invoke_handlers (object cond);
typedef struct {
  object condition;
  gcv_object_t* stack;
  SPint* sp;
  object spdepth;
} handler_args_t;
extern  handler_args_t handler_args;
typedef struct stack_range_t {
  struct stack_range_t * next;
  gcv_object_t* low_limit;
  gcv_object_t* high_limit;
} stack_range_t;
extern  stack_range_t* inactive_handlers;
/* is used by ERROR */

/* UP: Determines, whether an Object is a function name, ie. a Symbol or
 a list of the form (SETF symbol).
 funnamep(obj)
 > obj: Objekt
 < result: true if function name */
extern bool funnamep (object obj);
/* is used by CONTROL */

/* Gives the block-name that belongs to the function name.
 funname_blockname(obj)
 > obj: a Symbol or (SETF symbol)
 < result: Block-name, a Symbol */
#define funname_blockname(obj)  \
  (atomp(obj) ? (object)obj : (object)Car(Cdr(obj)))

/* UP: Determines, whether a Symbol is a Macro in the current Environment.
 sym_macrop(symbol)
 > symbol: Symbol
 < result: true if sym is a Symbol-Macro
 can trigger GC */
extern maygc bool sym_macrop (object sym);
/* is used by CONTROL */

/* UP: Sets the value of a Symbol in the current Environment.
 setq(symbol,value);
 > symbol: Symbol, not a constant
 > value: desired value of the Symbol in the current Environment
 < result: value
 can trigger GC */
extern maygc object setq (object sym, object value);
/* used by CONTROL */

/* UP: Gives the definition of the function for a Symbol in an Environment
 sym_function(sym,fenv)
 > sym: name of the function (a Symbol for example)
 > fenv: a Functions- and Macrobindings-Environment
 < result: Definition of the function, either unbound (if function is undefined)
             or Closure/SUBR/FSUBR/Macro/FunctionMacro. */
extern object sym_function (object sym, object fenv);
/* is used by CONTROL */

/* UP: "nests" an FUN-Environment, ie. writes all active bindings
 from the Stack to freshly allocated Vectors..
 nest_fun(env)
 > env: FUN-Env
 < result: same Environment, no pointer into the Stack
 can trigger GC */
extern maygc object nest_fun (object env);
/* is used by CONTROL */

/* UP: Nests the Environments in *env (ie. write all information
 to Stack-independent structures) and pushes it onto the STACK.
 nest_env(env)
 > gcv_environment* env: Pointer to five single Environments
 < gcv_environment* result: Pointer to the Environments on the STACK
 modifies STACK, can trigger GC */
extern maygc gcv_environment_t* nest_env (gcv_environment_t* env);
/* is used by Macro nest_aktenv */

/* UP: Nests the current environments (ie. writes all information
 to Stack-independent structures) and pushes them onto the STACK.
 (The values VAR_ENV, FUN_ENV, BLOCK_ENV, GO_ENV, DECL_ENV aren't
 modified, since there might be inactive bindings in frames that cannot
 be activated without modifying VAR_ENV .)
 nest_aktenv()
 < gcv_environment* result: Pointer to the Environments on the STACK
 modifies STACK, can trigger GC
 extern gcv_environment* nest_aktenv (void); */
#define nest_aktenv()  nest_env(&aktenv)
/* is used by CONTROL */

/* UP: Augments a Declarations-Environment with one decl-spec.
 augment_decl_env(declspec,env)
 > declspec: Declarations-Specifier, a Cons
 > env: Declarations-Environment
 < result: new (possibly augmented) Declarations-Environment
 can trigger GC */
extern maygc object augment_decl_env (object new_declspec, object env);
/* is used by CONTROL */

/* UP: expands a Form, if possible, (but not, if FSUBR-call
 or Symbol or FunctionMacro-call) in an Environment
 macroexp(form,venv,fenv);
 > form: Form
 > venv: a Variable- and Symbolmacro-Environment
 > fenv: a Function- and Macrobindings-Environment
 < value1: the expansion
 < value2: NIL, if not expanded,
           T, if expanded
 can trigger GC */
extern maygc void macroexp (object form, object venv, object fenv);
/* is used by CONTROL */

/* UP: expands a Form if possible, (also, if FSUBR-call or
 Symbol, but not if FunctionMacro-call) in an environment
 macroexp0(form,env);
 > form: Form
 > env: a macro-expansion environment
 < value1: the expansion
 < value2: NIL, if not expanded,
           T, if expanded
 can trigger GC */
extern maygc void macroexp0 (object form, object env);
/* is used by CONTROL */

/* UP: Parse-Declarations-Docstring. Detaches from a Form-list those,
 that must be viewed as Declarations resp. Documentation-string.
 parse_dd(formlist)
 > formlist: ( {decl|doc-string} . body )
 < value1: body
 < value2: list of decl-specs
 < value3: Doc-String or NIL
 < result: name if a (COMPILE name)-declaration occurred,
           unbound if a (COMPILE)-declaration occurred, else Fixnum_0
 can trigger GC */
extern maygc object parse_dd (object formlist);
/* is used by CONTROL */

/* UP: Creates a corresponding Closure for a Lambda-body by disassembling
 the Lambda-list and possibly macro-expanding of all forms.
 get_closure(lambdabody,name,blockp,env)
 > lambdabody: (lambda-list {decl|doc} {form})
 > name: Name, a Symbol or (SETF symbol)
 > blockp: whether an implicit BLOCK is to be added
 > env: Pointer to the five individual environments:
        env->var_env = VENV, env->fun_env = FENV,
        env->block_env = BENV, env->go_env = GENV,
        env->decl_env = DENV.
 < result: Closure
 can trigger GC */
extern maygc object get_closure (object lambdabody, object name, bool blockp, gcv_environment_t* env);
/* is used by CONTROL, SYMBOL, PREDTYPE */

/* UP: Converts an argument to a function.
 coerce_function(obj)
 > obj: Object
 < result: Object as function (SUBR or Closure)
 can trigger GC */
extern maygc object coerce_function (object obj);
/* is used by IO, FOREIGN */

/* Binds a Symbol dynamically to a value.
 Creates a dynamic variable-bindings frame for 1 variable.
 dynamic_bind(var,val)
 > var: a Symbol
 > val: the new value
 decreases STACK by 3 entries
 modifies STACK */
#define dynamic_bind(variable,val_to_use)      \
  do { var gcv_object_t* top_of_frame = STACK; \
    var object sym_to_bind = (variable);       \
    /* Create frame :                            */\
    pushSTACK(Symbol_thread_value(sym_to_bind));      \
    pushSTACK(sym_to_bind);                    \
    finish_frame(DYNBIND);                     \
    /* modify value                              */\
    Symbol_thread_value(sym_to_bind) = (val_to_use);  \
  } while(0)
/* is used by IO, EVAL, DEBUG, ERROR */

/* Unbinds a dynamic variable-bindings frame for one variable.
 dynamic_unbind_g()  - generic
 dynamic_unbind(sym) - with an additional check when STACKCHECKB
 increases STACK by 3 entries
 modifies STACK */
#if STACKCHECKB
  #define CHECK_DYNBIND                                                 \
    if (!((as_oint(STACK_0) & wbit(frame_bit_o)) && framecode(STACK_0))) \
      abort()
#else
  #define CHECK_DYNBIND
#endif
#define dynamic_unbind_g()    do {                                      \
  CHECK_DYNBIND;                                                        \
  Symbol_value(STACK_(1)) = STACK_(2); /* restore the value */          \
  skipSTACK(3);    /* dismantle Frame */                                \
} while(0)
#if STACKCHECKB
  #define dynamic_unbind(sym)   do {              \
    if (!eq(sym,STACK_1)) abort();                \
    dynamic_unbind_g();                           \
  } while(0)
#else
  #define dynamic_unbind(sym)   dynamic_unbind_g()
#endif
/* is used by IO, DEBUG, ERROR, EVAL, PREDTYPE, SPVW */

/* Executes "implicit PROGN" .
 implicit_progn(body,default)
 Executes body as implicit PROGN.
  If the body is empty, the value is the default one.
 can trigger GC */
#define implicit_progn(body,default)                                   \
  do {                                                                 \
    var object rest = (body);                                          \
    if (atomp(rest)) {                                                 \
      VALUES1(default); /* default as value                              */\
    } else                                                             \
      do { pushSTACK(Cdr(rest)); eval(Car(rest)); rest = popSTACK(); } \
      while (consp(rest));                                             \
  } while(0)
/* is used by EVAL, CONTROL */

/* Highest number of parameters in a lambda-list (< bitm(intCsize))
 (= value of LAMBDA-PARAMETERS-LIMIT - 1) */
#define lp_limit_1  ((uintL)(bitm(12)-1))

/* Highest number of arguments for a function call
 (= value of CALL-ARGUMENTS-LIMIT - 1) */
#define ca_limit_1  lp_limit_1

/* The macro LISPSPECFORM initiates the declaration of a LISP-Special-Form.
 LISPSPECFORM(name,req_count,opt_count,body_flag)
 > name: C-name of the function and the Symbol
 > req_count: number of required parameters
 > opt_count: number of optional parameters
 > body_flag: body or nobody, depending on whether &BODY exists or not
 See FSUBR.D */
#define LISPSPECFORM  LISPSPECFORM_B
/* is used by CONTROL */

/* The macro LISPFUN initiates a declaration of a LISP functions.
 LISPFUN(name,seclass,req_count,opt_count,rest_flag,key_flag,key_count,keywords)
 > name: the name of the function (a C-Identifier)
 > seclass: the side-effect class (seclass_t, see above)
 > req_count: number of required parameters (a number)
 > opt_count: number of optional parameters (a number)
 > rest_flag: either norest or rest, depending on whether &REST exists or not
 > key_flag: either nokey or key or key_allow, depending on whether &KEY
             exists or not and whether &ALLOW-OTHER-KEYS is present
 > key_count: number of keyword-parameters, a number (0 if nokey)
 > keywords: either NIL or an expression of the form
             v(kw(keyword1),...,kw(keywordn))   (NIL if nokey)
 See SUBR.D */
#define LISPFUN  LISPFUN_B
/* used by all modules */

/* The macro LISPFUNN initiates a simple declaration of a LISP-function.
 LISPFUNN(name,req_count)
 > name: the function-name (a C-Identifier)
 > req_count: the (fixed) number of arguments (a number)
 LISPFUNNF - ditto, but seclass_foldable instead of seclass_default
 LISPFUNNR - ditto, but seclass_read instead of seclass_default
 LISPFUNNS - ditto, but seclass_rd_sig instead of seclass_default
 See SUBR.D
 used by all modules */

/* UP: initialize hand-made compiled closures
 init_cclosures();
 can trigger GC */
extern maygc void init_cclosures (void);

#if defined(USE_JITC)
#if defined(TYPECODES)
  #error USE_JITC requires HEAPCODES
#endif
/* GC hooks for JIT code */
extern void gc_mark_jitc_object (void *ptr);
extern void gc_scan_jitc_objects (void);
extern bool gc_drop_jitc;
#endif

/* ##################### CTRLBIBL for CONTROL.D ############################ */

/* the variables declared special appear on the stack twice:
   with binding SPECDECL (added when processing declarations)
   and the actual value (added when processing bindings).
 here we activate the SPECDECL bindings */
#define specdecled_p(sym,ptr,nn) (nn>0 ? specdecled_(sym,ptr,nn) : NULL)
/* Find the SPECDECL binding for the symbol
 > spec_pointer & spec_count are returned by make_variable_frame()
 < return the pointer to the flags (or symbol+flags)
 i.e., something suitable to SET_BIT,
 or NULL if no such binding is found */
extern gcv_object_t* specdecled_ (object symbol, gcv_object_t* spec_pointer,
                                  uintL spec_count);
/* used by CONTROL, EVAL */

/* activate the SPECDECL binding if found */
#define activate_specdecl(sym,ptr,nn) do {                      \
  var gcv_object_t *spec = specdecled_p(sym,ptr,nn);            \
  if (spec)                                                     \
    *spec = SET_BIT(*spec,active_bit_o); /* activate binding */ \
 } while(0)

/* activate all SPECDECL and IMPDEPENDENT declarations */
extern void activate_specdecls (gcv_object_t* spec_ptr, uintC spec_count);
/* used by CONTROL, EVAL */

/* Error if a block has already been left.
 error_block_left(name);
 > name: Block-name */
extern _Noreturn void error_block_left (object name);
/* is used by EVAL */

/* convert the numeric side-effect class as stored in subr_t or cclosure_t
 to the object - CONS or NIL - as used in compiler.lisp and for #Y i/o */
#ifndef COMPILE_STANDALONE
static inline object seclass_object (seclass_t sec) {
  switch (sec) {
    case seclass_foldable: { return NIL; }
    case seclass_no_se:    { return O(seclass_no_se); }
    case seclass_read:     { return O(seclass_read); }
    case seclass_rd_sig:   { return O(seclass_rd_sig); }
    case seclass_write:    { return O(seclass_write); }
    case seclass_default:  { return O(seclass_default); }
    default: NOTREACHED;
  }
}
#endif
/* used by IO and CONTROL */

/* ########################## for ENCODING.D ############################### */

/* Initialize the encodings.
 init_encodings(); */
extern void init_encodings_1 (void);
extern void init_encodings_2 (void);
/* is used by SPVW */

/* Initialize the encodings which depend on environment variables.
 init_dependent_encodings(); */
extern void init_dependent_encodings (void);
/* is used by SPVW */

/* Maximum number of bytes needed to form a character, over all encodings.
 max_bytes_per_chart */
#ifdef ENABLE_UNICODE
  #define max_bytes_per_chart  8  /* 6 for JAVA, 7 for ISO-2022-KR, 8 for ISO-2022-CN[-EXT] */
#else
  #define max_bytes_per_chart  1
#endif
/* is used by STREAM */

/* UP: Creates a LISP-String with given contents.
 n_char_to_string(charptr,len,encoding)
 > char* charptr: address of a character sequence
 > uintL len: length of the sequence
 > object encoding: Encoding
 < result: Normal-Simple-String with len characters starting from charptr as contents
 can trigger GC */
#ifdef ENABLE_UNICODE
  extern maygc object n_char_to_string (const char* charptr, uintL len, object encoding);
#else
  #define n_char_to_string(charptr,len,encoding)  n_char_to_string_(charptr,len)
  extern maygc object n_char_to_string_ (const char* charptr, uintL len);
#endif
/* is used by PATHNAME */
%% #ifdef ENABLE_UNICODE
%%   exportF(object,n_char_to_string,(const char* charptr, uintL len, object encoding));
%% #else
%%   emit_define("n_char_to_string(charptr,len,encoding)","n_char_to_string_(charptr,len)");
%%   exportF(object,n_char_to_string_,(const char* charptr, uintL len));
%% #endif

/* UP: Converts an ASCIZ-String to a LISP-String.
 asciz_to_string(asciz,encoding)
 ascii_to_string(asciz)
 > char* asciz: ASCIZ-String
       (address of a null-terminated character-sequence)
 > object encoding: Encoding
 < result: Normal-Simple-String with the character sequence (without null-byte) as contents.
 can trigger GC */
#ifdef ENABLE_UNICODE
  extern maygc object asciz_to_string (const char * asciz, object encoding);
#else
  #define asciz_to_string(asciz,encoding)  asciz_to_string_(asciz)
  extern maygc object asciz_to_string_ (const char * asciz);
#endif
extern maygc object ascii_to_string (const char * asciz);
/* is used by SPVW/CONSTSYM, STREAM, PATHNAME, PACKAGE, GRAPH */
%% #ifdef ENABLE_UNICODE
%%   exportF(object,asciz_to_string,(const char * asciz, object encoding));
%% #else
%%   emit_define("asciz_to_string(asciz,encoding)","asciz_to_string_(asciz)");
%%   exportF(object,asciz_to_string_,(const char * asciz));
%% #endif
%% exportF(object,ascii_to_string,(const char * asciz));

/* UP: Converts a String to an ASCIZ-String.
 string_to_asciz(obj,encoding)
 > object obj: String
 > object encoding: Encoding
 < result: Simple-Bit-Vector with the same characters as bytes and one
             additional null-byte at the end
 < TheAsciz(result): address of the byte-sequence contained in there
 can trigger GC */
#ifdef ENABLE_UNICODE
  extern maygc object string_to_asciz (object obj, object encoding);
#else
  #define string_to_asciz(obj,encoding)  string_to_asciz_(obj)
  extern maygc object string_to_asciz_ (object obj);
#endif
#define TheAsciz(obj)  ((char*)(&TheSbvector(obj)->data[0]))
/* is used by STREAM, PATHNAME */
%% #ifdef ENABLE_UNICODE
%%   exportF(object,string_to_asciz,(object obj, object encoding));
%% #else
%%   export_def(string_to_asciz(obj,encoding));
%%   exportF(object,string_to_asciz_,(object obj));
%% #endif
%% export_def(TheAsciz(obj));

/* Converts a String to an ASCIZ-String on the C-Stack.
 with_string_0(string,encoding,asciz,statement);
 with_sstring_0(simple_string,encoding,asciz,statement);
 copies the contents of string (which should be a Lisp string) to a safe area
 (zero-terminating it), binds the variable asciz pointing to it, and
 executes the statement. */
#if 0
  #define with_string_0(string,encoding,ascizvar,statement)  \
    do { var char* ascizvar = TheAsciz(string_to_asciz(string,encoding)); \
         statement                                                        \
    } while(0)
  #define with_sstring_0  with_string_0
#else
  #define with_string_0_help_(string,encoding,ascizvar,statement,ascizvar_len,ascizvar_offset,ascizvar_string,ascizvar_bytelen,ascizvar_data,asserter,notreached) \
    do { var uintL ascizvar_len;                                        \
      var uintL ascizvar_offset;                                        \
      var object ascizvar_string = unpack_string_ro(string,&ascizvar_len,&ascizvar_offset); \
      var const chart* ptr1;                                            \
      unpack_sstring_alloca_help_(ascizvar_string,ascizvar_len,ascizvar_offset, ptr1=,notreached); \
     {var uintL ascizvar_bytelen = cslen(encoding,ptr1,ascizvar_len);   \
      var DYNAMIC_ARRAY(ascizvar_data,uintB,ascizvar_bytelen+1);        \
      cstombs_help_(encoding,ptr1,ascizvar_len,&ascizvar_data[0],ascizvar_bytelen,asserter); \
      ascizvar_data[ascizvar_bytelen] = '\0';                           \
     {var char* ascizvar = (char*) &ascizvar_data[0];                   \
      statement}                                                        \
      FREE_DYNAMIC_ARRAY(ascizvar_data);                                \
    }} while(0)
  #define with_string_0(string,encoding,ascizvar,statement) \
    with_string_0_help_(string,encoding,ascizvar,statement,ascizvar##_len,ascizvar##_offset,ascizvar##_string,ascizvar##_bytelen,ascizvar##_data,ASSERT,NOTREACHED)
  #define with_sstring_0_help_(string,encoding,ascizvar,statement,ascizvar_len,ascizvar_string,ascizvar_bytelen,ascizvar_data,asserter,notreached) \
    do { var object ascizvar_string = (string);                         \
      sstring_un_realloc(ascizvar_string);                              \
     {var uintL ascizvar_len = Sstring_length(ascizvar_string);         \
      var const chart* ptr1;                                            \
      unpack_sstring_alloca_help_(ascizvar_string,ascizvar_len,0, ptr1=,notreached); \
     {var uintL ascizvar_bytelen = cslen(encoding,ptr1,ascizvar_len);   \
      var DYNAMIC_ARRAY(ascizvar_data,uintB,ascizvar_bytelen+1);        \
      cstombs_help_(encoding,ptr1,ascizvar_len,&ascizvar_data[0],ascizvar_bytelen,asserter); \
      ascizvar_data[ascizvar_bytelen] = '\0';                           \
     {var char* ascizvar = (char*) &ascizvar_data[0];                   \
      statement}                                                        \
      FREE_DYNAMIC_ARRAY(ascizvar_data);                                \
    }}} while(0)
  #define with_sstring_0(string,encoding,ascizvar,statement) \
    with_sstring_0_help_(string,encoding,ascizvar,statement,ascizvar##_len,ascizvar##_string,ascizvar##_bytelen,ascizvar##_data,ASSERT,NOTREACHED)
#endif
/* is used by PATHNAME, MISC, FOREIGN */
%% export_def(with_string_0_help_(string,encoding,ascizvar,statement,ascizvar_len,ascizvar_offset,ascizvar_string,ascizvar_bytelen,ascizvar_data,asserter,notreached));
%% export_def(with_sstring_0_help_(string,encoding,ascizvar,statement,ascizvar_len,ascizvar_string,ascizvar_bytelen,ascizvar_data,asserter,notreached));
%% /* cannot use emit_define because Rectype_* is not a define in lispbibl.d */
%% puts("#define with_string_0(string,encoding,ascizvar,statement) with_string_0_help_(string,encoding,ascizvar,statement,ascizvar##_len,ascizvar##_offset,ascizvar##_string,ascizvar##_bytelen,ascizvar##_data,ASSERT,NOTREACHED)");
%% puts("#define with_sstring_0(string,encoding,ascizvar,statement) with_sstring_0_help_(string,encoding,ascizvar,statement,ascizvar##_len,ascizvar##_string,ascizvar##_bytelen,ascizvar##_data,ASSERT,NOTREACHED)");

/* In some foreign modules, we call library functions that can do callbacks.
 When we pass a parameter to such a library function, maybe it first does a
 callback - which may involve garbage collection - and only then looks at
 the parameter. Therefore all the parameters, especially strings, must be
 located in areas that are not moved by garbage collection. The following
 macro helps achieving this. */

/* Converts a String to a String on the C-Stack.
 with_string(string,encoding,charptr,len,statement);
 with_sstring(simple_string,encoding,charptr,len,statement);
 copies the contents of string (which should be a Lisp string) to a safe area,
 binds the variable charptr pointing to it and the variable len to its length,
 and executes the statement. */
#define with_string(string,encoding,charptrvar,lenvar,statement)  \
  do { var uintL charptrvar##_len;                                        \
    var uintL charptrvar##_offset;                                        \
    var object charptrvar##_string = unpack_string_ro(string,&charptrvar##_len,&charptrvar##_offset); \
    var const chart* ptr1;                                                \
    unpack_sstring_alloca(charptrvar##_string,charptrvar##_len,charptrvar##_offset, ptr1=); \
   {var uintL lenvar = cslen(encoding,ptr1,charptrvar##_len);             \
    var DYNAMIC_ARRAY(charptrvar##_data,uintB,lenvar);                    \
    cstombs(encoding,ptr1,charptrvar##_len,&charptrvar##_data[0],lenvar); \
    {var char* charptrvar = (char*) &charptrvar##_data[0];                \
     statement                                                            \
    }                                                                     \
    FREE_DYNAMIC_ARRAY(charptrvar##_data);                                \
  }} while(0)
#define with_sstring(string,encoding,charptrvar,lenvar,statement)  \
  do { var object charptrvar##_string = (string);                         \
    sstring_un_realloc(charptrvar##_string);                              \
   {var uintL charptrvar##_len = Sstring_length(charptrvar##_string);     \
    var const chart* ptr1;                                                \
    unpack_sstring_alloca(charptrvar##_string,charptrvar##_len,0, ptr1=); \
   {var uintL lenvar = cslen(encoding,ptr1,charptrvar##_len);             \
    var DYNAMIC_ARRAY(charptrvar##_data,uintB,lenvar);                    \
    cstombs(encoding,ptr1,charptrvar##_len,&charptrvar##_data[0],lenvar); \
    {var char* charptrvar = (char*) &charptrvar##_data[0];                \
     statement                                                            \
    }                                                                     \
    FREE_DYNAMIC_ARRAY(charptrvar##_data);                                \
  }}} while(0)
/* is used by PATHNAME */

/* Error, when a character cannot be converted to an encoding.
 error_unencodable(encoding,ch); */
extern _Noreturn void error_unencodable (object encoding, chart ch);
/* is used by STREAM */

/* ####################### ARRBIBL for ARRAY.D ############################# */

/* ARRAY-TOTAL-SIZE-LIMIT is chosen as large as possible, respecting the
 constraint that the total-size of any array is a fixnum and (from ANSI CL)
 that ARRAY-TOTAL-SIZE-LIMIT itself is also a fixnum.
 (>=0, <2^oint_data_len): */
#if (oint_data_len<=intLsize)
  #define arraysize_limit_1  ((uintV)(vbitm(oint_data_len)-2))
#else
  #if defined(UNIX_MACOSX) && defined(WIDE_HARD)
    /* on 64 bit Darwin HEAPCODES are used - limit the size of arrays.*/
    #define arraysize_limit_1  ((uintV)(vbitm(24)-2))
  #else
    /* Respect the constraint that the total-size of any array is an uintL. */
    #define arraysize_limit_1  ((uintV)(vbitm(intLsize)-1))
  #endif
#endif

/* ARRAY-RANK-LIMIT is chosen as large as possible, respecting the constraint
 that the rank of any array is an uintWC:
  #define arrayrank_limit_1  ((uintL)(bitm(intWCsize)-1))
 array dimensions are pushed on STACK in array_dimensions()
 so we are limited like with LAMBDA-PARAMETERS-LIMIT */
#define arrayrank_limit_1  lp_limit_1

/* Macro: Follows the Sistring chain, to get from a simple array (actually,
 a string) to its storage vector.
 sstring_un_realloc(array);
 sstring_un_realloc1(array); [when at most one Sistring is involved]
 > array: a simple array
 < array: its storage vector */
#ifdef HAVE_SMALL_SSTRING
  #ifdef TYPECODES
    #define sarray_reallocstringp(array)  \
      (typecode(array) == sstring_type                                  \
       && (sstring_flags(TheSstring(array)) & sstringflags_forwarded_B) \
      )
  #else
    #define sarray_reallocstringp(array)  \
      (Record_type(array) == Rectype_reallocstring)
  #endif
  #define sstring_un_realloc(array)  \
    while (sarray_reallocstringp(array)) \
      (array) = TheSistring(array)->data/*;*/
  #define sstring_un_realloc1(array)  \
    if (sarray_reallocstringp(array)) \
      (array) = TheSistring(array)->data/*;*/
#else
  #define sstring_un_realloc(array)  (void)0 /*nop*/
  #define sstring_un_realloc1(array)  (void)0 /*nop*/
#endif

/* Function: Copies a simple-vector.
 copy_svector(vector)
 > vector: simple-vector
 < result: fresh simple-vector with the same contents
 can trigger GC */
extern maygc object copy_svector (object vector);
/* used by IO */

/* Function: allocate a simple-bit/byte-vector and copy data there
 > atype: array type
 > vec_len: the length of the new array
 > data: pointer to the memory area to be copied
 > byte_len: the memory size to be copied, in bytes
 < returns a fresh simple-bit/byte-vector with the same contents
 can trigger GC */
extern maygc object data_to_sbvector (uintB atype, uintL vec_len,
                                      const void *data, uintL byte_len);
#define data_to_sb8vector(data,bytelen)  data_to_sbvector(Atype_8Bit,bytelen,data,bytelen)
%% exportF(object,data_to_sbvector,(uintB atype, uintL vec_len, const void *data, uintL byte_len));
%% export_def(data_to_sb8vector(data,bytelen));
/* used by RAWSOCK, NEW-CLX, DIRKEY, GDBM, SYSCALLS, BERKELEY-DB, PCRE */

/* Function: Copies a simple-bit/byte-vector.
 copy_sbvector(vector)
 > vector: simple-bit/byte-vector
 < result: fresh simple-bit/byte-vector with the same contents
 can trigger GC */
extern maygc object copy_sbvector (object vector);
/* used by RECORD */

/* Function: Returns the active length of a vector (same as LENGTH).
 vector_length(vector)
 > vector: a vector
 < result: its length */
extern uintL vector_length (object vector);
/* used by many modules */
%% exportF(uintL,vector_length,(object vector));

/* Function: Canonicalizes an array element-type and returns its
 element type code.
 eltype_code(element_type)
 > element_type: type specifier
 < result: element type code Atype_xxx
 The canonicalized types are the possible results of ARRAY-ELEMENT-TYPE
 (symbols T, BIT, CHARACTER and lists (UNSIGNED-BYTE n)).
 The result type is a supertype of element_type.
 can trigger GC */
extern maygc uintB eltype_code (object element_type);
/* is used by SEQUENCE */

/* Function: Creates a simple-vector with given elements.
 vectorof(len)
 > uintC len: desired vector length
 > STACK_(len-1), ..., STACK_(0): len objects
 < result: simple-vector containing these objects
 Pops n objects off STACK.
 can trigger GC */
extern maygc object vectorof (uintC len);
/* used by PREDTYPE */
%% exportF(object,vectorof,(uintC len));

/* Function: For an indirect array, returns the storage vector and the offset.
 Also verifies that all elements of the array are physically present.
 iarray_displace_check(array,size,&index)
 > object array: indirect array
 > uintL size: size
 < result: storage vector
 < index: is incremented by the offset into the storage vector */
extern object iarray_displace_check (object array, uintL size, uintL* index);
/* used by IO, CHARSTRG, HASHTABL, PREDTYPE, STREAM, SEQUENCE */

/* Function: For an array, returns the storage vector and the offset.
 Also verifies that all elements of the array are physically present.
 array_displace_check(array,size,&index)
 > object array: array
 > uintV size: size
 < result: storage vector
 < index: is incremented by the offset into the storage vector */
extern object array_displace_check (object array, uintV size, uintL* index);
/* used by HASHTABL, PREDTYPE, IO, FOREIGN */
%% exportF(object,array_displace_check,(object array, uintV size, uintL* index));

/* Tests for the storage vector of an array of element type NIL.
 simple_nilarray_p(obj) */
#define simple_nilarray_p(obj)  nullp(obj)
%% export_def(simple_nilarray_p(obj));

/* error-message
 > array: array (usually a Vector)
 > STACK_0: (erroneous) Index */
extern _Noreturn void error_index_range (object array, uintL bound);
/* used by SEQUENCE */

/* error message: attempt to retrieve a value from (ARRAY NIL) */
extern _Noreturn void error_nilarray_retrieve (void);
/* used by PREDTYPE */
%% exportE(error_nilarray_retrieve,(void));

/* error message: attempt to store a value in (ARRAY NIL) */
extern _Noreturn void error_nilarray_store (void);

/* error message: attempt to access a value from (ARRAY NIL) */
extern _Noreturn void error_nilarray_access (void);

/* Function: Performs an AREF access.
 storagevector_aref(storagevector,index)
 > storagevector: a storage vector (simple vector or semi-simple byte vector)
 > index: (already checked) index into the storage vector
 < result: (AREF storagevector index)
 can trigger GC - if the element type is (UNSIGNED-BYTE 32) */
extern /*maygc*/ object storagevector_aref (object storagevector, uintL index);
/* used by IO */

/* Error when attempting to store an invalid value in an array.
 error_store(array,value); */
extern _Noreturn void error_store (object array, object value);
/* used by SEQUENCE */

/* Macro: Tests a bit in a simple-bit-vector.
 if (sbvector_btst(sbvector,index)) ...
 > sbvector: a simple-bit-vector
 > index: index (a variable, must be < (length sbvector)) */
#define sbvector_btst(sbvector,index)  \
  ( /* in byte number (index div 8), the bit number 7 - (index mod 8) :  */\
   TheSbvector(sbvector)->data[(uintL)(index)/8]                       \
     & bit((~(uintL)(index)) % 8)                                      \
  )
/* used by SEQUENCE, IO */

/* Macro: Clears a bit in a simple-bit-vector.
 sbvector_bclr(sbvector,index);
 > sbvector: a simple-bit-vector
 > index: index (a variable, must be < (length sbvector)) */
#define sbvector_bclr(sbvector,index)  \
  ( /* in byte number (index div 8), the bit number 7 - (index mod 8) :  */\
    TheSbvector(sbvector)->data[(uintL)(index)/8]                      \
      &= ~bit((~(uintL)(index)) % 8)                                   \
  )
/* used by IO */

/* Macro: Sets a bit in a simple-bit-vector.
 sbvector_bset(sbvector,index);
 > sbvector: a simple-bit-vector
 > index: index (a variable, must be < (length sbvector)) */
#define sbvector_bset(sbvector,index)  \
  ( /* in byte number (index div 8), the bit number 7 - (index mod 8) :  */\
    TheSbvector(sbvector)->data[(uintL)(index)/8]                      \
      |= bit((~(uintL)(index)) % 8)                                    \
  )
/* used by SEQUENCE, IO */

/* return Atype for the given array */
global uintBWL array_atype (object array);
/* used by socket.d and modules */
%% exportF(uintBWL,array_atype,(object array));

/* Function: Returns the element-type of an array.
 array_element_type(array)
 > array: an array
 < result: element-type, one of the symbols T, BIT, CHARACTER, or a list
 can trigger GC */
extern maygc object array_element_type (object array);
/* used by PREDTYPE, IO */

/* Returns the rank of an array.
 array_rank(array)
 > array: an array
 < uintL result: its rank = number of dimensions */
extern uintL array_rank (object array);
/* used by modules */
%% exportF(uintL,array_rank,(object array));

/* Returns the dimensions of an array.
 get_array_dimensions(array,rank,&dimensions[]);
 > array: an array
 > uintL rank: = array_rank(array)
 > uintL dimensions[0..rank-1]: room for rank dimensions
 < uintL dimensions[0..rank-1]: the array's dimensions */
extern void get_array_dimensions (object array, uintL rank, uintL* dimensions);
/* used by modules */
%% exportF(void,get_array_dimensions,(object array, uintL rank, uintL* dimensions));

/* Function: Returns the list of dimensions of an array.
 array_dimensions(array)
 > array: an array
 < result: list of its dimensions
 can trigger GC */
extern maygc object array_dimensions (object array);
/* used by PREDTYPE, IO */

/* Function: Returns the dimensions of an array and their partial products.
 iarray_dims_sizes(array,&dims_sizes);
 > array: indirect array of rank r
 > struct { uintL dim; uintL dimprod; } dims_sizes[r]: room for the result
 < for i=1,...r:  dims_sizes[r-i] = { Dim_i, Dim_i * ... * Dim_r } */
typedef struct { uintL dim; uintL dimprod; }  array_dim_size_t;
extern void iarray_dims_sizes (object array, array_dim_size_t* dims_sizes);
/* used by IO */

/* Function: Returns the total-size of an array.
 array_total_size(array)
 > array: an array (a variable)
 < uintL result: its total-size */
#ifndef COMPILE_STANDALONE
static inline uintL array_total_size (object array) {
  if (array_simplep(array)) {
    sstring_un_realloc(array);
    if (simple_string_p(array))
      return Sstring_length(array); /* simple string: total length */
    else
      return Sarray_length(array); /* simple vector: total length */
  } else
    return TheIarray(array)->totalsize; /* indirect array: contains totalsize */
}
#endif
/* used by ARRAY, SEQUENCE, FOREIGN */

/* Function: Compares two slices of simple-bit-vectors.
 bit_compare(array1,index1,array2,index2,count)
 > array1: first simple-bit-vector
 > index1: absolute index into array1
 > array2: second simple-bit-vector
 > index2: absolute index into array2
 > count: number of bits to be compared, > 0
 < result: true, if both slices are the same, bit for bit, else false. */
extern bool bit_compare (object array1, uintL index1,
                         object array2, uintL index2,
                         uintL bitcount);
/* used by PREDTYPE */

/* Function: Copies a slice of an array array1 into another array array2.
 elt_copy(dv1,index1,dv2,index2,count);
 > dv1: source storage-vector
 > index1: start index in dv1
 > dv2: destination storage-vector
 > index2: start index in dv2
 > count: number of elements to be copied, > 0
 can trigger GC - if dv1 and dv2 have different element types or
                  if both are strings and dv1 is wider than dv2 */
extern /*maygc*/ void elt_copy (object dv1, uintL index1, object dv2, uintL index2, uintL count);
/* used by SEQUENCE, STREAM */

/* Function: Copies a slice of an array array1 into another array array2 of
 the same element type. Handles overlapping arrays correctly.
 elt_move(dv1,index1,dv2,index2,count);
 > dv1: source storage-vector
 > index1: start index in dv1
 > dv2: destination storage-vector
 > index2: start index in dv2
 > count: number of elements to be copied, > 0
 can trigger GC - if both are strings and dv1 is wider than dv2 */
extern /*maygc*/ void elt_move (object dv1, uintL index1, object dv2, uintL index2, uintL count);
/* used by SEQUENCE */

/* Function: Fills a slice of an array with an element.
 elt_fill(dv,index,count,element)
 > dv: destination storage-vector
 > index: start index in dv
 > count: number of elements to be filled
 < result: true if element does not fit, false when done
 can trigger GC */
extern maygc bool elt_fill (object dv, uintL index, uintL count, object element);
/* used by SEQUENCE */

/* Function: Reverses a slice of an array, copying it into another array
 of the same element type.
 elt_reverse(dv1,index1,dv2,index2,count);
 > dv1: source storage-vector
 > index1: start index in dv1
 > dv2: destination storage-vector
 > index2: start index in dv2
 > count: number of elements to be copied, > 0
 can trigger GC */
extern maygc void elt_reverse (object dv1, uintL index1, object dv2, uintL index2, uintL count);
/* used by SEQUENCE */

/* Function: Reverses a slice of an array destructively.
 elt_nreverse(dv,index,count);
 > dv: storage-vector
 > index: start index in dv
 > count: number of elements to be reversed, > 0 */
extern void elt_nreverse (object dv, uintL index, uintL count);
/* used by SEQUENCE */

/* Function: Tests whether an array has a fill-pointer.
 array_has_fill_pointer_p(array)
 > array: ein Array
 < result: true, if it has a fill-pointer, else false. */
extern bool array_has_fill_pointer_p (object array);
/* used by SEQUENCE, STREAM, IO */

/* Function: Allocates a new simple-bit-vector, filled with zeroes.
 allocate_bit_vector_0(len)
 > uintL len: length of the desired bit-vector (number of bits)
 < result: fresh simple-bit-vector, filled with zeroes
 can trigger GC */
extern maygc object allocate_bit_vector_0 (uintL len);
/* used by SEQUENCE */
%% #if notused
%% exportF(object,allocate_bit_vector_0,(uintL len));
%% #endif

/* The following functions work on "semi-simple string"s.
 That are CHARACTER arrays with FILL-POINTER, (pro forma) not adjustable and
 not displaced, whose storagevector is a normal-simple-string. When their
 length is exceeded, the length is doubled (so that the resizing effort
 becomes unimportant: adding a character is still O(1) on average.) */

/* Function: Returns a fresh semi-simple-string of given length, with
 fill-pointer = 0.
 make_ssstring(len)
 > uintL len: desired length, must be >0
 < result: fresh semi-simple-string of the given length
 can trigger GC */
extern maygc object make_ssstring (uintL len);
#define SEMI_SIMPLE_DEFAULT_SIZE 50
/* used by STREAM, IO */

/* Function: Adds a character to a semi-simple-string, thereby possibly
 extending it.
 ssstring_push_extend(ssstring,ch)
 > ssstring: a semi-simple-string
 > ch: a character
 < result: the same semi-simple-string
 can trigger GC */
extern maygc object ssstring_push_extend (object ssstring, chart ch);
/* used by STREAM, IO */

/* Function: Ensures that a semi-simple-string has at least a given length,
 possibly extending it.
 ssstring_extend(ssstring,size)
 > ssstring: a semi-simple-string
 > size: desired minimum length
 < result: the same semi-simple-string
 can trigger GC */
extern maygc object ssstring_extend (object ssstring, uintL needed_len);
/* used by STREAM */

/* Function: Adds a substring to a semi-simple-string, thereby possibly
 extending it.
 ssstring_append_extend(ssstring,srcstring,start,len)
 > ssstring: a semi-simple-string
 > srcstring: a simple-string
 > start: the start index into the sstring
 > len: the number of characters to be pushed, starting from start
 < result: the same semi-simple-string
 can trigger GC */
extern maygc object ssstring_append_extend (object ssstring, object srcstring, uintL start, uintL len);
/* used by STREAM */

/* The following functions work on "semi-simple byte-vector"s.
 That are bit vectors with FILL-POINTER, (pro forma) not adjustable and
 not displaced, whose storagevector is a simple-bit-vector. When their
 length is exceeded, the length is doubled (so that the resizing effort
 becomes unimportant: adding a character is still O(1) on average.) */

/* Function: Returns a fresh semi-simple byte-vector of given length, with
 fill-pointer = 0.
 make_ssbvector(len)
 > uintL len: length (number of bytes!), must be >0
 < fresh: fresh semi-simple byte-vector of the given length
 can trigger GC */
extern maygc object make_ssbvector (uintL len);
/* used by IO */

/* Function: Adds a byte to a semi-simple byte vector, thereby possibly
 extending it.
 ssbvector_push_extend(ssbvector,b)
 > ssbvector: a semi-simple byte-vector
 > b: byte
 < result: the same semi-simple byte-vector
 can trigger GC */
extern maygc object ssbvector_push_extend (object ssbvector, uintB b);
/* used by IO */

/* ##################### CHARBIBL for CHARSTRG.D ########################### */

/* Special Characters: (refer to above)
 #define BEL   7  #  #\Bell
 #define BS    8  #  #\Backspace
 #define TAB   9  #  #\Tab
 #define LF   10  #  #\Linefeed
 #define CR   13  #  #\Return
 #define PG   12  #  #\Page */
#define NL   10  /*  #\Newline */
#define NLstring  "\n"  /* C-String, that contains #\Newline */
#define ESC  27  /*  #\Escape */
#define ESCstring  "\033"  /* C-String, that contains #\Escape */

/* Converts Byte ch to upcase
 up_case(ch) */
extern chart up_case (chart ch);
/* is used by IO, PREDTYPE, PATHNAME */
%% #if notused
%% exportF(chart,up_case,(chart ch));
%% #endif

/* Converts Byte ch to downcase
 down_case(ch) */
extern chart down_case (chart ch);
/* is used by IO, PATHNAME */
%% #if notused
%% exportF(chart,down_case,(chart ch));
%% #endif

/* Checks whether a Character is alphanumeric.
 alphanumericp(ch)
 > ch: Character-Code
 < result: true if alphanumeric, else false. */
extern bool alphanumericp (chart ch);
/* is used by IO, PATHNAME */

/* Checks, whether a Character is a Graphic-Character ("printing").
 graphic_char_p(ch)
 > ch: Character-Code
 < result: true if printing, else false. */
extern bool graphic_char_p (chart ch);
/* is used by STREAM, PATHNAME */

/* Returns the screen display width of a character.
 char_width(ch)
 > ch: character code
 < result: number of output columns occupied by ch */
extern uintL char_width (chart ch);
/* is used by IO, STREAM */

#if !defined(ENABLE_UNICODE) || defined(HAVE_SMALL_SSTRING)
/* Copies an array of uint8 to an array of uint8.
 copy_8bit_8bit(src,dest,len);
 > uint8* src: source
 > uint8* dest: destination
 > uintL len: number of elements to be copied, > 0 */
extern void copy_8bit_8bit (const uint8* src, uint8* dest, uintL len);
#endif

#if defined(HAVE_SMALL_SSTRING)
/* Copies an array of uint8 to an array of uint16.
 copy_8bit_16bit(src,dest,len);
 > uint8* src: source
 > uint16* dest: destination
 > uintL len: number of elements to be copied, > 0 */
extern void copy_8bit_16bit (const uint8* src, uint16* dest, uintL len);
#endif
%% #ifdef HAVE_SMALL_SSTRING
%%   exportF(void,copy_8bit_16bit,(const uint8* src, uint16* dest, uintL len));
%% #endif

#if defined(HAVE_SMALL_SSTRING)
/* Copies an array of uint8 to an array of uint32.
 copy_8bit_32bit(src,dest,len);
 > uint8* src: source
 > uint32* dest: destination
 > uintL len: number of elements to be copied, > 0 */
extern void copy_8bit_32bit (const uint8* src, uint32* dest, uintL len);
#endif
%% #ifdef HAVE_SMALL_SSTRING
%%   exportF(void,copy_8bit_32bit,(const uint8* src, uint32* dest, uintL len));
%% #endif

#if defined(HAVE_SMALL_SSTRING)
/* Copies an array of uint16 to an array of uint8.
 All source elements must fit into uint8.
 copy_16bit_8bit(src,dest,len);
 > uint16* src: source
 > uint8* dest: destination
 > uintL len: number of elements to be copied, > 0 */
extern void copy_16bit_8bit (const uint16* src, uint8* dest, uintL len);
#endif
%% #ifdef HAVE_SMALL_SSTRING
%%   exportF(void,copy_16bit_8bit,(const uint16* src, uint8* dest, uintL len));
%% #endif

#if defined(HAVE_SMALL_SSTRING)
/* Copies an array of uint16 to an array of uint16.
 copy_16bit_16bit(src,dest,len);
 > uint16* src: source
 > uint16* dest: destination
 > uintL len: number of elements to be copied, > 0 */
extern void copy_16bit_16bit (const uint16* src, uint16* dest, uintL len);
#endif
%% #ifdef HAVE_SMALL_SSTRING
%%   exportF(void,copy_16bit_16bit,(const uint16* src, uint16* dest, uintL len));
%% #endif

#if defined(HAVE_SMALL_SSTRING)
/* Copies an array of uint16 to an array of uint32.
 copy_16bit_32bit(src,dest,len);
 > uint16* src: source
 > uint32* dest: destination
 > uintL len: number of elements to be copied, > 0 */
extern void copy_16bit_32bit (const uint16* src, uint32* dest, uintL len);
#endif
%% #ifdef HAVE_SMALL_SSTRING
%%   exportF(void,copy_16bit_32bit,(const uint16* src, uint32* dest, uintL len));
%% #endif

#if defined(HAVE_SMALL_SSTRING)
/* Copies an array of uint32 to an array of uint8.
 All source elements must fit into uint8.
 copy_32bit_8bit(src,dest,len);
 > uint32* src: source
 > uint8* dest: destination
 > uintL len: number of elements to be copied, > 0 */
extern void copy_32bit_8bit (const uint32* src, uint8* dest, uintL len);
#endif
%% #ifdef HAVE_SMALL_SSTRING
%%   exportF(void,copy_32bit_8bit,(const uint32* src, uint8* dest, uintL len));
%% #endif

#if defined(HAVE_SMALL_SSTRING)
/* Copies an array of uint32 to an array of uint16.
 All source elements must fit into uint16.
 copy_32bit_16bit(src,dest,len);
 > uint32* src: source
 > uint16* dest: destination
 > uintL len: number of elements to be copied, > 0 */
extern void copy_32bit_16bit (const uint32* src, uint16* dest, uintL len);
#endif
%% #ifdef HAVE_SMALL_SSTRING
%%   exportF(void,copy_32bit_16bit,(const uint32* src, uint16* dest, uintL len));
%% #endif

#if defined(ENABLE_UNICODE)
/* Copies an array of uint32 to an array of uint32.
 copy_32bit_32bit(src,dest,len);
 > uint32* src: source
 > uint32* dest: destination
 > uintL len: number of elements to be copied, > 0 */
extern void copy_32bit_32bit (const uint32* src, uint32* dest, uintL len);
#endif

#if defined(HAVE_SMALL_SSTRING)

/* Determines the smallest string element type capable of holding a
 set of 8-bit characters. */
#define smallest_string_flavour8(src,len)  \
  (unused (src), unused(len), Sstringtype_8Bit)

/* Determines the smallest string element type capable of holding a
 set of 16-bit characters.
 smallest_string_flavour16(src,len)
 > uint16* src: source
 > uintL len: number of characters at src
 < result: Sstringtype_8Bit or Sstringtype_16Bit */
extern uintBWL smallest_string_flavour16 (const uint16* src, uintL len);

/* Determines the smallest string element type capable of holding a
 set of 32-bit characters.
 smallest_string_flavour32(src,len)
 > uint32* src: source
 > uintL len: number of characters at src
 < result: Sstringtype_8Bit or Sstringtype_16Bit or Sstringtype_32Bit */
extern uintBWL smallest_string_flavour32 (const uint32* src, uintL len);

/* Determines the smallest string element type capable of holding a
 set of characters.
 smallest_string_flavour(src,len)
 > chart* src: source
 > uintL len: number of characters at src
 < result: Sstringtype_8Bit or Sstringtype_16Bit or Sstringtype_32Bit */
#ifndef COMPILE_STANDALONE
static inline uintBWL smallest_string_flavour (const chart* src, uintL len) {
  return smallest_string_flavour32((const uint32*)src,len);
}
#endif

#endif

/* Dispatches among S8string, S16string, S32string and nilvector.
 SstringCase(string,s8string_statement,s16string_statement,s32string_statement,nilvector_statement);
 > string: a not-reallocated simple-string or simple-nilvector (i.e. NIL)
 Executes one of the three statement, depending on the element size of string. */
#ifdef ENABLE_UNICODE
  #ifdef HAVE_SMALL_SSTRING
    #define SstringCase(string,s8string_statement,s16string_statement,s32string_statement,nilvector_statement)  \
      if (Array_type(string) == Array_type_snilvector) { nilvector_statement } else \
      if (sstring_eltype(TheSstring(string)) == Sstringtype_8Bit) { s8string_statement } else \
      if (sstring_eltype(TheSstring(string)) == Sstringtype_16Bit) { s16string_statement } else \
      if (sstring_eltype(TheSstring(string)) == Sstringtype_32Bit) { s32string_statement } else \
      NOTREACHED;
  #else
    #define SstringCase(string,s8string_statement,s16string_statement,s32string_statement,nilvector_statement)  \
      if (Array_type(string) == Array_type_snilvector) { nilvector_statement } else \
      { s32string_statement }
  #endif
#else
  /* In this case we take the s32string_statement, not the s8string_statement,
   because the s32string_statement is the right one for normal simple strings. */
  #define SstringCase(string,s8string_statement,s16string_statement,s32string_statement,nilvector_statement)  \
    if (Array_type(string) == Array_type_snilvector) { nilvector_statement } else \
    { /*s8string_statement*/ s32string_statement }
#endif
/* is used by CHARSTRG, ARRAY, HASHTABL, PACKAGE, PATHNAME, PREDTYPE, STREAM */

/* Dispatches among S8string, S16string, S32string and nilvector.
 SstringDispatch(string,suffix,statement)
 > string: a not-reallocated simple-string or simple-nilvector (i.e. NIL)
 Executes the statement with cint##suffix being bound to the appropriate
 integer type (cint8, cint16 or cint32) and with Sstring being bound to the
 appropriate struct pointer type (S8string, S16string or S32string).
 Gives an error for simple-nilvector; must therefore only be called if the
 contents of the string is really to be accessed. */
#define SstringDispatch(string,suffix,statement)  \
  SstringCase(string,                                                 \
    { typedef cint8 cint##suffix; typedef S8string Sstring##suffix;   \
      statement                                                       \
    },                                                                \
    { typedef cint16 cint##suffix; typedef S16string Sstring##suffix; \
      statement                                                       \
    },                                                                \
    { typedef cint32 cint##suffix; typedef S32string Sstring##suffix; \
      statement                                                       \
    },                                                                \
    { error_nilarray_access();                                       \
    })
/* is used by CHARSTRG, ARRAY, HASHTABL, PACKAGE, PATHNAME, PREDTYPE, STREAM */

/* Tests whether a simple-string is a normal-simple-string.
 sstring_normal_p(string)
 > string: a not-reallocated simple-string */
#ifdef HAVE_SMALL_SSTRING
  #define sstring_normal_p(string)  \
    (sstring_eltype(TheSstring(string)) == Sstringtype_32Bit)
#else
  #define sstring_normal_p(string)  1
#endif

/* Makes a string contents available.
 unpack_sstring_alloca(string,len,offset, charptr = );
 > object string: a not-reallocated simple-string
 > uintL len: the number of characters to be accessed
 > uintL offset: where the characters to be accessed start
 < const chart* charptr: pointer to the characters
   (may be in string, may be on the stack) */
#ifdef HAVE_SMALL_SSTRING
  #define unpack_sstring_alloca_help_(string,len,offset,charptr_assignment,notreached) \
    if (simple_nilarray_p(string)) {                                           \
      if ((len) > 0) error_nilarray_retrieve();                                \
      charptr_assignment NULL;                                                 \
    } else if (sstring_eltype(TheSstring(string)) == Sstringtype_32Bit) {      \
      charptr_assignment (const chart*) &TheS32string(string)->data[offset];   \
    } else {                                                                   \
      var chart* _unpacked_ = (chart*)alloca((len)*sizeof(chart));             \
      if ((len) > 0) {                                                         \
        if (sstring_eltype(TheSstring(string)) == Sstringtype_16Bit)           \
          copy_16bit_32bit(&TheS16string(string)->data[offset],(cint32*)_unpacked_,len);\
        else if (sstring_eltype(TheSstring(string)) == Sstringtype_8Bit)       \
          copy_8bit_32bit(&TheS8string(string)->data[offset],(cint32*)_unpacked_,len);\
        else                                                                   \
          notreached;                                                          \
      }                                                                        \
      charptr_assignment (const chart*) _unpacked_;                            \
    }
#else
  #define unpack_sstring_alloca_help_(string,len,offset,charptr_assignment,notreached) \
    if (simple_nilarray_p(string)) {                                           \
      if ((len) > 0) error_nilarray_retrieve();                                \
      charptr_assignment NULL;                                                 \
    } else {                                                                   \
      charptr_assignment (const chart*) &TheSnstring(string)->data[offset];    \
    }
#endif
#define unpack_sstring_alloca(string,len,offset,charptr_assignment)     \
  unpack_sstring_alloca_help_(string,len,offset,charptr_assignment,NOTREACHED)
/* is used by */
%% export_def(unpack_sstring_alloca_help_(string,len,offset,charptr_assignment,notreached));
%% puts("#define unpack_sstring_alloca(s,l,o,c) unpack_sstring_alloca_help_(s,l,o,c,NOTREACHED)");

/* UP: Fetches a character from a simple string.
 schar(string,index)
 > object string: a not-reallocated simple-string or simple-nilvector (i.e. NIL)
 > uintL index: >= 0, < length of string
 < chart result: character at the given position */
#ifndef COMPILE_STANDALONE
static inline chart schar (object string, uintL index) {
  SstringDispatch(string,X, {
    return as_chart(((SstringX)TheVarobject(string))->data[index]);
  });
  return as_chart(0); /* not reached - just pacify the compiler */
}
#endif
/* is used by PATHNAME, STREAM */

/* UP: unpacks a String.
 unpack_string_ro(string,&len,&offset)  [for read-only access]
 > object string: a String.
 < uintL len: number of characters of the String.
 < uintL offset: offset into the datastorage vector
 < object result: datastorage vector, a simple-string or NIL */
extern object unpack_string_ro (object string, uintL* len, uintL* offset);
/* is used by STREAM, HASHTABL, PACKAGE, SEQUENCE, ENCODING */
%% exportF(object,unpack_string_ro,(object string, uintL* len, uintL* offset));

/* UP: tests two Strings for equality
 string_eq(string1,string2)
 > string1: String
 > string2: simple-string
 < result: /=0, if equal */
extern bool string_eq (object string1, object string2);
/* is used by PACKAGE, STREAM, IO */

/* UP: tests two Strings for equality, case-insensitive
 string_equal(string1,string2)
 > string1: String
 > string2: simple-string
 < result: /=0, if equal */
extern bool string_equal (object string1, object string2);
/* is used by IO, PATHNAME */
%% exportF(bool,string_equal,(object string1, object string2));

/* UP: Stores a character in a string.
 > string: a mutable string that is or was simple
 > index: (already checked) index into the string
 > element: a character
 < result: the possibly reallocated string
 can trigger GC */
  extern maygc object sstring_store (object string, uintL index, chart element);
/* is used by STREAM */

/* UP: Stores an array of characters in a string.
 > string: a mutable string that is or was simple
 > offset: (already checked) offset into the string
 > charptr[0..len-1]: a character array, not GC affected
 < result: the possibly reallocated string
 can trigger GC */
  extern maygc object sstring_store_array (object string, uintL offset,
                                           const chart *charptr, uintL len);
/* is used by FFI */

#ifdef ENABLE_UNICODE
/* UP: Creates a Simple-String with given elements.
 stringof(len)
 > uintL len: desired length of vector
 > on STACK: len Characters, first one on top
 < result: Simple-String with these objects
 increases STACK
 modifies STACK, can trigger GC */
  extern maygc object stringof (uintL len);
/* is used by ENCODING, STREAM */
#endif

/* UP: copies a String and turns it into a Simple-String.
 copy_string_normal(string)
 > string: String
 < result: mutable Normal-Simple-String with the same characters
 can trigger GC */
  extern maygc object copy_string_normal (object string);
/* is used by IO, PATHNAME */

/* UP: copies a String and turns it into a Simple-String.
 copy_string(string)
 > string: String
 < result: mutable Simple-String with the same characters
 can trigger GC */
#ifdef HAVE_SMALL_SSTRING
  extern maygc object copy_string (object string);
#else
  #define copy_string(string)  copy_string_normal(string)
#endif
/* is used by IO, PATHNAME */

/* UP: Converts a String to a Simple-String.
 coerce_ss(obj)
 > obj: Lisp-Object, should be a String.
 < result: Simple-String with the same characters.
 can trigger GC */
extern maygc object coerce_ss (object obj);
/* is used by STREAM, PATHNAME */

/* UP: Converts a String to a immutable Simple-String.
 coerce_imm_ss(obj)
 > obj: Lisp-Object, should be a String.
 < result: immutable Simple-String with the same characters.
 can trigger GC */
extern maygc object coerce_imm_ss (object obj);
/* is used by PACKAGE */

/* UP: Converts a String to a Normal-Simple-String.
 coerce_normal_ss(obj)
 > obj: Lisp-Object, should be a String.
 < result: Normal-Simple-String with the same characters.
 can trigger GC */
#ifndef HAVE_SMALL_SSTRING
  #define coerce_normal_ss coerce_ss
#else
  extern maygc object coerce_normal_ss (object obj);
#endif
/* is used by PATHNAME */

#if 0 /* unused */
/* UP: converts a String to an immutable Normal-Simple-String.
 coerce_imm_normal_ss(obj)
 > obj: Lisp-Object, should be a String.
 < result: immutable Normal-Simple-String with the same characters
 can trigger GC */
  #ifndef HAVE_SMALL_SSTRING
    #define coerce_imm_normal_ss coerce_imm_ss
  #else
    extern maygc object coerce_imm_normal_ss (object obj);
  #endif
/* is used by */
#endif

/* UP: Converts Object to a Character
 coerce_char(obj)
 > obj: Lisp-Object
 < result: Character or NIL */
extern object coerce_char (object obj);
/* is used by PREDTYPE */

/* UP: Returns the name of a character
 char_name(code)
 > chart code: Code of a character
 < result: Simple-String (this char's name) or NIL
 can trigger GC */
extern maygc object char_name (chart code);
/* is used by IO */

/* UP: Determines the Character with a given Name
 name_char(string)
 > string: String
 < result: Character with this Name, or NIL if none exists */
extern object name_char (object string);
/* is used by IO */

/* Converts a character to opposite case.
 invert_case(ch)
 > ch: a character
 < result: a character, either ch or up_case(ch) or down_case(ch)
 Note that always invert_case(invert_case(ch)) == ch. */
extern chart invert_case (chart ch);
/* is used by PACKAGE */

/* UP: compares two strings for equality modulo case-invert
 string_eq_inverted(string1,string2)
 > string1: string
 > string2: simple-string
 < result: /=0, if equal modulo case-invert */
extern bool string_eq_inverted (object string1, object string2);
/* is used by PACKAGE */

/* UP: converts a string to opposite case
 string_invertcase(string)
 > string: string
 < result: new normal-simple-string
 can trigger GC */
extern object string_invertcase (object string);
/* is used by SYMBOL, PACKAGE */

/* UP: tests the limits for a String argument
 test_string_limits_ro(&arg)  [for read-only access]
 > STACK_2: String-Argument
 > STACK_1: optional :start-Argument
 > STACK_0: optional :end-Argument
 < stringarg arg: description of the argument
 < result: String-Argument
 increases STACK by 3
 can trigger GC */
typedef struct stringarg {
  object string; /* data vector - not-reallocated simple-string or -array */
  uintL offset;                 /* offset into this string */
  uintL index;                  /* :start index */
  uintL len;                    /* :end - :start */
} stringarg;
%% emit_typedef("struct stringarg { object string; uintL offset; uintL index; uintL len; }","stringarg");
extern maygc object test_string_limits_ro (stringarg* arg);
/* used by STREAM, PATHNAME, IO, ENCODING */

/* UP: checks :START and :END limits for a vector argument
 > STACK_1: optional :start-argument
 > STACK_0: optional :end-argument
 > stringarg arg: arg.string its data vector,
                  [arg.offset .. arg.offset+arg.len-1] the range within the
                  data vector corresponding to the entire vector-argument
 < stringarg arg: arg.string and arg.offset unchanged,
                  [arg.offset+arg.index .. arg.offset+arg.index+arg.len-1] the
                  range within the data vector corresponding to the selected
                  vector slice
 removes 2 elements from STACK */
extern void test_vector_limits (stringarg* arg);
/* used by ENCODING */
%% exportF(void,test_vector_limits,(stringarg* arg));
/* used by RAWSOCK, NEW-CLX */

/* UP: checks a string/symbol/character-argument
 test_stringsymchar_arg(obj,invert)
 > obj: argument
 > invert: whether to implicitly case-invert a symbol's printname
 < result: argument as string
 can trigger GC */
extern maygc object test_stringsymchar_arg (object obj, bool invert);
/* used by IO, PACKAGE */

/* UP: tests two equally long strings for equality
 > string1,offset1: Chars in String1 start from here
 > string2,offset2: Chars in String2 start from here
 > len: number of chars in String1 and String2, > 0
 < result: true if equal, else false. */
extern bool string_eqcomp (object string1, uintL offset1, object string2, uintL offset2, uintL len);
/* is used by PREDTYPE */

/* UP: compares two equally long strings, case-insensitive
 > string1,offset1: Chars in String1 start from here
 > string2,offset2: Chars in String2 start from here
 > len: number of chars in String1 and String2, > 0
 < result: true if equal, else false. */
extern bool string_eqcomp_ci (object string1, uintL offset1, object string2, uintL offset2, uintL len);
/* is used by PREDTYPE */

/* UP: converts the Characters of a partial string to upcase
 nstring_upcase(dv,offset,len);
 > object dv: the character storage vector
 > uintL offset: index of first affected character
 > uintL len: number of affected characters
 can trigger GC */
extern maygc void nstring_upcase (object dv, uintL offset, uintL len);
/* is not used at this time (except in CHARSTRG, of course) */

/* UP: converts the Characters of a partial string to downcase
 nstring_downcase(dv,offset,len);
 > object dv: the character storage vector
 > uintL offset: index of first affected character
 > uintL len: number of affected characters
 can trigger GC */
extern maygc void nstring_downcase (object dv, uintL offset, uintL len);
/* is used by PATHNAME */

/* UP: changes the words of a part of a string so they start
 with capital letters and continue with lowercase ones
 nstring_capitalize(dv,offset,len);
 > object dv: the character storage vector
 > uintL offset: index of first affected character
 > uintL len: number of affected characters
 can trigger GC */
extern maygc void nstring_capitalize (object dv, uintL offset, uintL len);
/* is used by PATHNAME */

/* UP: converts a String to upcase
 string_upcase(string)
 > string: String
 < result: new Normal-Simple-String, in upcase
 can trigger GC */
extern maygc object string_upcase (object string);
/* is used by MISC, PATHNAME */

/* UP: converts a String to downcase
 string_downcase(string)
 > string: String
 < result: new Normal-Simple-String, in downcase
 can trigger GC */
extern maygc object string_downcase (object string);
/* is used by PATHNAME */

/* Returns a substring of a simple-string.
 subsstring(string,start,end)
 > object string: a simple-string
 > uintL start: start index
 > uintL end: end index
 with 0 <= start <= end <= Sstring_length(string)
 < object result: (subseq string start end),
                  a freshly created normal-simple-string
 can trigger GC */
extern maygc object subsstring (object string, uintL start, uintL end);
/* is used by CHARSTRG, PATHNAME */

/* UP: Concatenates several Strings to one String.
 string_concat(argcount)
 > uintC argcount: Number of arguments
 > on STACK: the argument (should be Strings)
 < result: newly created string
 < STACK: cleaned
 can trigger GC */
extern maygc object string_concat (uintC argcount);
/* is used by PACKAGE, PATHNAME, DEBUG, SYMBOL */
%% exportF(object,string_concat,(uintC argcount));

/* ###################### DEBUGBIB for DEBUG.D ########################### */

/* Starts the normal driver (Read-Eval-Print-Loop)
 driver(); */
extern void driver (void);
/* is used by SPVW */

/* Starts a secondary driver (Read-Eval-Print-Loop)
 break_driver(continuable_p);
 > continuable_p == can be continued after the driver finishes
 can trigger GC */
extern maygc void break_driver (bool continuable_p);
/* is used by ERROR, EVAL */

/* ##################### HASHBIBL for HASHTABL.D ######################### */

/* UP: Gets the hash of an object from a hash-table.
 gethash(obj,ht,allowgc)
 > obj: Object, as key
 > ht: hash-table
 > allowgc: whether GC is allowed during hash lookup
            (should be true if the hash-table has a user-defined test)
 < result: corresponding value, if found, else nullobj
 can trigger GC - if allowgc is true */
extern /*maygc*/ object gethash (object obj, object ht, bool allowgc);
/* is used by EVAL, RECORD, PATHNAME, FOREIGN */
%% exportF(object,gethash,(object obj, object ht, bool allowgc));

/* UP: Locates a key in a hash-table and gives the older value.
 shifthash(ht,obj,value) == (SHIFTF (GETHASH obj ht) value)
 > ht: hash-table
 > obj: object
 > value: new value
 > allowgc: whether GC is allowed during hash lookup
            (should be true if the hash-table has a user-defined test or
             if the hash-table is not known to already contain a value for obj)
 < result: old value
 can trigger GC - if allowgc is true */
extern /*maygc*/ object shifthash (object ht, object obj, object value, bool allowgc);
/* is used by SEQUENCE, PATHNAME, FOREIGN */

/* hash_table_weak_type(ht)
 > ht: hash-table
 < result: symbol NIL/:KEY/:VALUE/:KEY-AND-VALUE/:KEY-OR-VALUE */
extern object hash_table_weak_type (object ht);
/* used by PREDTYPE */

/* HASH-TABLE-TEST (EQ/EQL/EQUAL/EQUALP)
 > ht: hash-table
 < result: symbol EQ/EQL/EQUAL/EQUALP or cons (TEST . HASH)
 can trigger GC - for user-defined ht_test */
extern maygc object hash_table_test (object ht);
/* used by HASHTABL, IO */

/* Macro: Runs through a Hash-Tabelle.
 map_hashtable(ht,key,value,statement)
 map_hashtable_nogc(ht,key,value,statement)
 > ht: Hash-Tabelle
 Calls 'statement', where key and value are a pair from the table.
 The first form is necessary, if the statement can trigger GC. */
#define map_hashtable(ht,key,value,statement)                           \
  do {                                                                  \
    var object ht_from_map_hashtable = (ht);                            \
    var uintL index_from_map_hashtable =                                \
      3*posfixnum_to_V(TheHashtable(ht_from_map_hashtable)->ht_maxcount); \
    pushSTACK(TheHashtable(ht_from_map_hashtable)->ht_kvtable);         \
    while (1) {                                                         \
      if (index_from_map_hashtable==0) break;                           \
      index_from_map_hashtable -= 3;                                    \
      {var gcv_object_t* KVptr_from_map_hashtable =                     \
         &TheHashedAlist(STACK_0)->hal_data[index_from_map_hashtable];  \
       var object key = KVptr_from_map_hashtable[0];                    \
       if (boundp(key)) {                                               \
         var object value = KVptr_from_map_hashtable[1];                \
              statement;                                                \
      } }  }                                                            \
    skipSTACK(1);                                                       \
  } while(0)
#define map_hashtable_nogc(ht,key,value,statement)                      \
  do {                                                                  \
    var object ht_from_map_hashtable = (ht);                            \
    var uintL index_from_map_hashtable =                                \
      posfixnum_to_V(TheHashtable(ht_from_map_hashtable)->ht_maxcount); \
    var gcv_object_t* KVptr_from_map_hashtable =                        \
      &TheHashedAlist(TheHashtable(ht_from_map_hashtable)->ht_kvtable)->hal_data[3*index_from_map_hashtable]; \
    while (1) {                                                         \
      if (index_from_map_hashtable==0) break;                           \
      index_from_map_hashtable--; KVptr_from_map_hashtable -= 3;        \
      { var object key = KVptr_from_map_hashtable[0];                   \
        if (boundp(key)) {                                              \
          var object value = KVptr_from_map_hashtable[1];               \
          statement;                                                    \
    } } }                                                               \
  } while(0)
/* is used by IO */

/* ######################### IOBIBL for IO.D ############################# */

/* check a cint for being a whitespace */
#define cint_white_p(c)   \
  ((c)==' ' || (c)=='\n' || (c)=='\r' || (c)=='\t' || (c)=='\v' || (c)=='\f')

/* special Object, that indicates EOF */
#define eof_value  make_system(0xE0FE0FUL)
/* is used by IO, STREAM, DEBUG, SPVW */

/* aux. value to recognize certain Dots */
#define dot_value  make_system(0xD0DD0DUL)
/* is used by IO, SPVW */

/* UP: Initializes the reader.
 init_reader();
 can trigger GC */
extern maygc void init_reader (void);
#if defined(MULTITHREAD)
#define INIT_READER_LOW_ARGS  struct clisp_thread_t* thr
#define INIT_READER_LOW_OTAB  thr->_object_tab
#define INIT_READER_LOW()  init_reader_low(current_thread())
#else
#define INIT_READER_LOW_ARGS  void
#define INIT_READER_LOW_OTAB  object_tab
#define INIT_READER_LOW()  init_reader_low()
#endif
extern maygc void init_reader_low (INIT_READER_LOW_ARGS);
/* is used by SPVW */

/* UP:
 (setf (strm-pphelp-strings *stream_)
    (list* (make-Semi-Simple-String 50)
           (cons nl_type *PRIN-INDENTATION*)
           (strm-pphelp-strings *stream_))) */
extern object cons_ssstring (const gcv_object_t* stream_, object nl_type);
/* used by io.d and stream.d */

/* UP: Reads an object.
 stream_read(&stream,recursive-p,whitespace-p)
 > recursive-p: tells whether there's a recursive call of READ,
                with error at EOF
 > whitespace-p: tells, whether whitespace is to be consumed afterwards
 > stream: Stream
 < stream: Stream
 < result: read object (eof_value at EOF, dot_value for single dot)
 can trigger GC */
extern maygc object stream_read (const gcv_object_t* stream_, object recursive_p, object whitespace_p);
/* is used by SPVW, DEBUG */

/* UP: Write a Simple-String to a Stream element by element.
 write_sstring(&stream,string);
 > string: Simple-String
 > stream: Stream
 < stream: Stream
 can trigger GC */
extern maygc void write_sstring (const gcv_object_t* stream_, object string);
/* is used by EVAL, DEBUG, ERROR, PACKAGE, SPVW */

/* UP: Writes a String to a Stream element by element.
 write_string(&stream,string);
 > string: String
 > stream: Stream
 < stream: Stream
 can trigger GC */
extern maygc void write_string (const gcv_object_t* stream_, object string);
/* is used by PACKAGE, DEBUG */

/* UP: Writes an object to a Stream.
 prin1(&stream,obj);
 > obj: Objekt
 > stream: Stream
 < stream: Stream
 can trigger GC */
extern maygc void prin1 (const gcv_object_t* stream_, object obj);
/* is used by EVAL, DEBUG, PACKAGE, ERROR, SPVW */

/* UP: Writes a Newline to a Stream.
 terpri(&stream);
 > stream: Stream
 < stream: Stream
 can trigger GC
 extern maygc void terpri (const gcv_object_t* stream_); */
#define terpri(stream_)  write_ascii_char(stream_,NL)
/* is used by IO, DEBUG, PACKAGE, ERROR, SPVW */

/* ####################### Functional arguments for FUNARG.D ################ #
 used by LIST, WEAK, SEQUENCE */

/* UP: Checks the :KEY argument
 check_key_arg()
 > *pkey_arg: optional argument
 < *pkey_arg: correct KEY function */
global void check_key_arg (gcv_object_t *pkey_arg);
/* used by LIST, SEQUENCE, WEAK */

/* Applies a :KEY argument.
 funcall_key(key,item);
 > key: value of the :KEY argument
 > item: object being considered
 < value1: (FUNCALL key item) */
#define funcall_key(key,item) do {                    \
  var object _key = (key);                            \
  var object _item = (item);                          \
  GCTRIGGER2(_key,_item);                             \
  /* shortcut for :KEY #'IDENTITY, very common */     \
  if (!eq(_key,L(identity))) {                        \
    pushSTACK(_item); funcall(_key,1);                \
  } else {                                            \
    value1 = _item;                                   \
  }                                                   \
 } while(0)

/* Subroutine to compute the test :TEST & :TEST-NOT
 call_test(fun,item,x)
 > *fun: the test function
 > item: the item to compare with
 > x: the argument
 < result: true if the test is okay, otherwise false.
 can trigger GC */
typedef maygc bool funarg_t (const gcv_object_t* fun, object item, object x);
funarg_t call_if, call_if_not;

/* UP: Check the :TEST, :TEST-NOT - arguments
 check_test_args()
 > stackptr: Pointer to the STACK
 > *(stackptr+1): :TEST argument
 > *(stackptr+0): :TEST-NOT argument
 < *(stackptr+1): computed :TEST argument
 < *(stackptr+0): computed :TEST-NOT argument
 < call_test: Adress of a test function */
extern funarg_t* check_test_args (gcv_object_t* stackptr);

/* ####################### LISTBIBL for LIST.D ############################# */

/* UP: Copies a list
 copy_list(list)
 > list: list
 < result: copy of the list
 can trigger GC */
extern maygc object copy_list (object list);
/* is used by PACKAGE */
%% exportF(object,copy_list,(object old_list));

/* UP: Reverses a list constructively.
 reverse(list)
 > list: list (x1 ... xm)
 < result: reversed list (xm ... x1)
 can trigger GC */
extern maygc object reverse (object list);
/* is used by SEQUENCE, PACKAGE, PATHNAME */

/* UP: get the length of a list and the last atom
 llength1(obj,last)
 > obj: object
 < uintL result: length of obj, interpreted as list
 < last: the last atom
 Does not test for circular lists. */
extern uintL llength1 (object obj, object* last);
/* used in SEQUENCE */
#define llength(obj)  llength1(obj,NULL)
/* used by CONTROL, EVAL, RECORD, IO, PACKAGE, HASHTABL, STREAM */
%% exportF(uintL,llength1,(object obj, object* last));

/* UP: Makes a list with exactly len elements
 make_list(len)
 > STACK_0: Initial value for the elements
 > uintL len: desired list length
 < result: list with len elements
 can trigger GC */
extern maygc object make_list (uintL len);
/* is used by */
%% #if notused
%% exportF(object,make_list,(uintL len));
%% #endif

/* UP: reverses a list destructively.
 nreverse(list)
 > list: list (x1 ... xm)
 < result: list (xm ... x1), EQ to the old one */
extern object nreverse (object list);
/* is used by SEQUENCE, EVAL, CONTROL, IO, PATHNAME, ERROR, DEBUG, PACKAGE */
%% exportF(object,nreverse,(object list));

/* UP: A0 := (nreconc A0 A1)
 nreconc(list,obj)
 > list: list
 > obj: object
 < result: (nreconc A0 A1) */
extern object nreconc (object list, object obj);
/* is used by SEQUENCE, IO, PATHNAME, CONTROL, DEBUG */

/* UP: Build (delete obj (the list list) :test #'EQ)
 deleteq(list,obj)
 Remove all elements that are EQ to obj from the list.
 > obj: element to be removed
 > list: list
 < result: modified list */
extern object deleteq (object list, object obj);
/* is used by PACKAGE, STREAM */
%% exportF(object,deleteq,(object list, object obj));

/* UP: check whether OBJ ends a proper list
 endp(obj)
 > obj: object
 < result: true if obj is the list end NIL,
           false if obj is a Cons.
           error otherwise */
extern bool endp (object obj);
/* used by CONTROL */
%% exportF(bool,endp,(object obj));

/* Finds the length of a possibly circular or dotted list.
 list_length(list,&dotted)
 > list: an object
 < result: the length (integer >= 0, or NIL for circular lists)
 < dotted: if non-circular, the last atom, i.e., the indicator whether the list
           is dotted
 can trigger GC */
extern maygc object list_length (object list, object *dottedp);
/* used by SEQUENCE */

/* proper_list_p(obj)
   returns true if obj is a proper list, i.e. a list which is neither dotted
   nor circular, i.e. a list which ends in NIL. */
extern bool proper_list_p (object obj);
/* used by PREDTYPE */

/* UP: Creates a list with given elements.
 listof(len)
 > uintC len: desired list length
 > auf STACK: len objects, first one on top
 < result: list of those objects
 Increases STACK
 modifies STACK, can trigger GC */
extern maygc object listof (uintC len);
/* used by STREAM, PATHNAME, PACKAGE, ARRAY, EVAL, PREDTYPE, ERROR, SPVW */
%% exportF(object,listof,(uintC len));

/* UP: find OBJ in LIS: (MEMBER OBJ LIS :TEST #'EQ) */
extern object memq (const object obj, const object lis);
/* used by RECORD */
%% exportF(object,memq,(const object obj, const object lis));

/* ####################### MISCBIBL for MISC.D ############################# */

/* for modules */
typedef struct { long c_const; gcv_object_t *l_const; } c_lisp_pair_t;
typedef struct {
  const c_lisp_pair_t *table;         /* C <--> Lisp */
  const unsigned int size;            /* table length */
  const long default_value; /* what to use when Lisp value is missing */
  const bool have_default_value_p;   /* use default_value? */
  const char *name;                  /* map name for error messages */
} c_lisp_map_t;
extern maygc long map_lisp_to_c (object obj, const c_lisp_map_t *map);
extern maygc object map_c_to_lisp (long val, const c_lisp_map_t *map);
extern maygc object map_c_to_list (long val, const c_lisp_map_t *map);
global maygc long map_list_to_c (object obj, const c_lisp_map_t *map);
%% emit_typedef("struct { long c_const; gcv_object_t *l_const; }","c_lisp_pair_t");
%% emit_typedef("struct { const c_lisp_pair_t *table; const unsigned int size; const long default_value; const bool have_default_value_p;  const char *name; }","c_lisp_map_t");
%% exportF(long,map_lisp_to_c,(object obj, const c_lisp_map_t *map));
%% exportF(object,map_c_to_lisp,(long val, const c_lisp_map_t *map));
%% exportF(object,map_c_to_list,(long val, const c_lisp_map_t *map));
%% exportF(long,map_list_to_c,(object obj, const c_lisp_map_t *map));
global maygc void push_string_array (char **arr);
%% exportF(void,push_string_array,(char **arr));
global maygc object safe_to_string (const char *asciz);
%% exportF(object,safe_to_string,(const char *asciz));

/* ####################### ERRBIBL for ERROR.D ############################# */

/* Classification of the known condition-types:
 (More precisely, all these are the SIMPLE-... types.) */
typedef enum {
  condition, /* all kinds of conditions */
    serious_condition, /* conditions that require interactive intervention */
      error_condition, /* serious conditions that occur deterministically */
        program_error, /* mostly statically detectable errors of a program */
          source_program_error, /* statically detectable errors of a program, */
                                /* source available */
        control_error, /* not statically detectable errors in program control */
        arithmetic_error, /* errors that occur while doing arithmetic operations */
          division_by_zero, /* eval a mathematical function at a singularity */
          floating_point_overflow, /* trying to get too close to infinity and... */
          floating_point_underflow, /* trying to get too close to zero */
                                    /* in the floating point domain */
        cell_error, /* trying to access a location which contains #<UNBOUND> */
          unbound_variable, /* trying to get the value of an unbound variable */
          undefined_function, /* trying to get the global function definition */
                              /* of an undefined function */
          unbound_slot, /* trying to get the value of an unbound slot */
        type_error, /* some datum does not belong to the expected type */
          keyword_error, /* a keyword is not one of the allowed keywords */
          charset_type_error, /* a character does not belong to a character set */
          argument_list_dotted, /* an argument list in APPLY is dotted */
        package_error, /* errors during operation on packages */
        print_not_readable, /* attempted violation of *PRINT-READABLY* */
        parse_error, /* errors related to parsing */
        stream_error, /* errors while doing stream I/O */
          end_of_file, /* unexpected end of stream */
          reader_error, /* parsing/tokenization error during READ */
        file_error, /* errors with pathnames, OS level errors with streams */
      storage_condition, /* "Virtual memory exhausted" */
      interrupt_condition, /* "User break" */
    warning, /* conditions for which user notification is appropriate */
  number_of_conditions_defined_in_c
} condition_t;
%% printf("typedef enum { condition=%d, serious_condition=%d, error_condition=%d, program_error=%d, source_program_error=%d, control_error=%d, arithmetic_error=%d, division_by_zero=%d, floating_point_overflow=%d, floating_point_underflow=%d, cell_error=%d, unbound_variable=%d, undefined_function=%d, unbound_slot=%d, type_error=%d, keyword_error=%d, charset_type_error=%d, package_error=%d, print_not_readable=%d, parse_error=%d, stream_error=%d, end_of_file=%d, reader_error=%d, file_error=%d, storage_condition=%d, interrupt_condition=%d, warning=%d } condition_t;\n",condition, serious_condition, error_condition, program_error, source_program_error, control_error, arithmetic_error, division_by_zero, floating_point_overflow, floating_point_underflow, cell_error, unbound_variable, undefined_function, unbound_slot, type_error, keyword_error, charset_type_error, package_error, print_not_readable, parse_error, stream_error, end_of_file, reader_error, file_error, storage_condition, interrupt_condition, warning);

/* Error with error-string. Does not return.
 error(errortype,errorstring);
 > errortype: condition-type
 > errorstring: constant ASCIZ-String, in UTF-8 Encoding.
   At every tilde-S, a LISP-object is taken from the STACK and printed
   instead of the tilde-S.
 > on the STACK: initial values for the Condition, depending on error-type */
extern _Noreturn void error (condition_t errortype, const char * errorstring);
/* used by all modules */
%% exportE(error,(condition_t errortype, const char * errorstring));

/* Report an error and try to recover by asking the user to supply a value.
 check_value(errortype,errorstring);
 > errortype: condition-type
 > errorstring: constant ASCIZ-String, in UTF-8 Encoding.
   At every tilde-S, a LISP-object is taken from the STACK and printed
   instead of the tilde-S.
 > on the STACK: PLACE (form to be shown to the user) or NIL, then
   the initial values for the Condition, depending on error-type
 < value1, value2: return values from CHECK-VALUE:
   value1 = value supplied by the user,
   value2 = indicates whether PLACE should be filled
 < STACK: cleaned up
 can trigger GC */
extern maygc void check_value (condition_t errortype, const char * errorstring);
/* used by all modules */
%% exportF(void,check_value,(condition_t errortype, const char * errorstring));

/* Report an error and try to recover by asking the user to choose among some
 alternatives.
 correctable_error(errortype,errorstring);
 > errortype: condition-type
 > errorstring: constant ASCIZ-String, in UTF-8 Encoding.
   At every tilde-S, a LISP-object is taken from the STACK and printed
   instead of the tilde-S.
 > on the STACK: list of alternatives
   ((restart-name restart-help-string . value-returned-by-the-restart)*),
   then the initial values for the Condition, depending on error-type
 < value1: return value from CORRECTABLE-ERROR, one of the CDDRs of the
   alternatives
 < STACK: cleaned up
 can trigger GC */
extern maygc void correctable_error (condition_t errortype, const char* errorstring);
/* use by PACKAGE, new-clx */

/* Just like OS_error, but signal an error of type ETYPE.
 OS_file_error(pathname);
 > etype: symbolic name of the error type
 > arg: error argument
 > end_system_call() already called */
extern _Noreturn void OS_error_arg (object etype, object arg);
#if defined(DEBUG_OS_ERROR)
  /* Show the file and line number of the caller of OS_file_error().
   For debugging. */
#define OS_error_arg(etype,arg)                                         \
  (fprintf(stderr,"\n[%s:%d] ",__FILE__,__LINE__), (OS_error_arg)(etype,arg))
#endif
#define OS_file_error(path)   OS_error_arg(S(os_file_error),path)
%% exportE(OS_error_arg,(object etype, object arg));
%% puts("#define OS_file_error(path) OS_error_arg(S(os_file_error),path)");

/* Just like OS_error, but takes a channel stream and signals a FILE-ERROR.
 OS_filestream_error(stream);
 > stream: a channel stream
 > end_system_call() already called */
extern _Noreturn void OS_filestream_error (object stream);
#if defined(DEBUG_OS_ERROR)
  /* Show the file and line number of the caller of OS_filestream_error(). For debugging. */
  #define OS_filestream_error(stream)  \
    (fprintf(stderr,"\n[%s:%d] ",__FILE__,__LINE__), (OS_filestream_error)(stream))
#endif
%% exportE(OS_filestream_error,(object stream));

/* Prints error directly via the OS:  errno_out_low(errorcode,FILE,LINE);
 > errorcode: error code
 > FILE: Filename (with quotation marks) as constant ASCIZ-String
 > LINE: line number */
#if defined(UNIX)
extern void errno_out_low (int errorcode, const char* file, uintL line);
#endif
#if defined(WIN32_NATIVE)
extern void errno_out_low (DWORD errorcode, const char* file, uintL line);
#endif
/* Show the file and line number of the caller of errno_out(). */
#define errno_out(e)   errno_out_low(e,__FILE__,__LINE__)

/* UP: Executes break-loop because of a keyboard-interrupt.
 > -(STACK) : calling funtion
 modifies STACK, can trigger GC */
extern maygc void tast_break (void);
/* is used by EVAL, IO, SPVW, STREAM */

/* check_classname(obj,type)
 > obj: an object
 > classname: a symbol expected to name a class with "proper name" classname
 < result: an object of the given type, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_classname (object obj, object type);
%% exportF(object,check_classname,(object obj, object classname));

#ifdef FOREIGN
/* check_fpointer(obj,restart_p)
 > obj: an object
 > restart_p: flag whether to allow entering a replacement
 < result: a valid foreign pointer, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_fpointer_replacement (object obj, bool restart_p);
#ifndef COMPILE_STANDALONE
static inline maygc object check_fpointer (object obj, bool restart_p) {
  if (!(fpointerp(obj) && fp_validp(TheFpointer(obj))))
    obj = check_fpointer_replacement(obj,restart_p);
  return obj;
}
#endif
/* used by FOREIGN and REGEXP (amd maybe other non-FFI modules) */
#endif
%% #ifdef FOREIGN
%%   exportF(object,check_fpointer_replacement,(object obj, bool restart_p));
%%   puts("#ifndef COMPILE_STANDALONE");
%%   puts("static inline object check_fpointer (object obj, bool restart_p) {"
%%          " if (!(fpointerp(obj) && fp_validp(TheFpointer(obj))))"
%%            " obj = check_fpointer_replacement(obj,restart_p);"
%%          " return obj;"
%%        " }");
%%   puts("#endif");
%% #endif

/* Error message, if an object isn't a list.
 error_list(obj);
 > obj: non-list */
extern _Noreturn void error_list (object obj);
/* used by LIST, EVAL, STREAM */
%% #if notused
%% exportE(error_list,(object obj));
%% #endif

/* check_list(obj)
 > obj: an object
 < result: a list, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_list_replacement (object obj);

/* create check function for name */
#define MAKE_CHECK_LOW(name,test)                               \
  static inline maygc object check_##name (object obj) {        \
    if (!test)                                                  \
      obj = check_##name##_replacement(obj);                    \
    return obj;                                                 \
  }
#define MAKE_CHECK(name) MAKE_CHECK_LOW(name,name##p(obj))
#define MAKE_CHECK_(name) MAKE_CHECK_LOW(name,name##_p(obj))

#ifndef COMPILE_STANDALONE
MAKE_CHECK(list)
#endif
/* used by PATHNAME */
%% exportF(object,check_list_replacement,(object obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK(list));
%% puts("#endif");

/* Error message, if an object isn't a proper list because it is dotted.
 error_proper_list_dotted(caller,obj);
 > caller: caller (a symbol)
 > obj: End of list, non-list */
extern _Noreturn void error_proper_list_dotted (object caller, object obj);
/* is used by LIST */
%% exportE(error_proper_list_dotted,(object caller, object obj));

/* Error message, if an object isn't a proper list because it is circular.
 error_proper_list_circular(caller,obj);
 > caller: caller (a symbol)
 > obj: circular list */
extern _Noreturn void error_proper_list_circular (object caller, object obj);
/* is used by LIST */

/* check_symbol(obj)
 > obj: an object
 < result: a symbol, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_symbol_replacement (object obj);
#ifndef COMPILE_STANDALONE
static inline maygc object check_symbol (object obj) {
  if (!symbolp(obj))
    obj = check_symbol_replacement(obj);
  return obj;
}
#endif
/* used by CONTROL, EVAL, I18N, PACKAGE, PREDTYPE, RECORD, STREAM, SYMBOL */

/* check_symbol_non_constant(obj,caller)
 > obj: an object
 > caller: a symbol
 < result: a non-constant symbol, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_symbol_non_constant_replacement (object obj, object caller);
#ifndef COMPILE_STANDALONE
static inline maygc object check_symbol_non_constant (object obj, object caller) {
  if (!(symbolp(obj) && !constant_var_p(TheSymbol(obj))))
    obj = check_symbol_non_constant_replacement(obj,caller);
  return obj;
}
#endif
/* used by EVAL, CONTROL */

/* UP: signal an error if a non-symbol was declared (e.g. SPECIAL)
 returns the symbol
 can trigger GC */
extern maygc object check_symbol_in_declaration (object obj, object decl_identifier, object caller);
/* used by EVAL, CONTROL */

/* UP: make sure that the symbol does not name a global symbol-macro
 return the symbol
 can trigger GC */
global maygc object check_symbol_not_symbol_macro (object symbol);
/* use by CONTROL, PACKAGE, SYMBOL */

/* UP: make sure that the symbol does not name a global special variable
 return the symbol
 can trigger GC */
global maygc object check_symbol_not_global_special (object symbol);
/* used by SYMBOL */

/* Error message, if an object isn't a Simple-Vector.
 error_no_svector(caller,obj);
 > caller: caller (a Symbol)
 > obj: non-Svector */
extern _Noreturn void error_no_svector (object caller, object obj);
/* is used by ARRAY, EVAL */
%% #if notused
%% exportE(error_no_svector,(object caller, object obj));
%% #endif

/* Error message, if an object isn't a vector.
 error_vector(obj);
 > obj: non-vector */
extern _Noreturn void error_vector (object obj);
/* is used by ARRAY */
%% #if notused
%% exportE(error_vector,(object obj));
%% #endif

/* check_array(obj)
 > obj: an object
 < result: an array, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_array_replacement (object obj);
#ifndef COMPILE_STANDALONE
MAKE_CHECK(array)
#endif
/* used by ARRAY, modules */
%% exportF(object,check_array_replacement,(object obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK(array));
%% puts("#endif");

/* check_vector(obj)
 > obj: an object
 < result: an vector, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_vector_replacement (object obj);
#ifndef COMPILE_STANDALONE
MAKE_CHECK(vector)
#endif
/* used by ARRAY, ENCODING, modules */
%% exportF(object,check_vector_replacement,(object obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK(vector));
%% puts("#endif");

/* check_byte_vector_replacement(obj)
 > obj: not an (ARRAY (UNSIGNED-BYTE 8) (*))
 < result: an (ARRAY (UNSIGNED-BYTE 8) (*)), a replacement
 can trigger GC */
extern maygc object check_byte_vector_replacement (object obj);
#ifndef COMPILE_STANDALONE
MAKE_CHECK_LOW(byte_vector,bit_vector_p(Atype_8Bit,obj))
#endif
/* used by STREAM, modules */
%% exportF(object,check_byte_vector_replacement,(object obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_LOW(byte_vector,bit_vector_p(Atype_8Bit,obj)));
%% puts("#endif");


/* error-message, if an object is not an environment.
 error_environment(obj);
 > obj: non-vector */
extern _Noreturn void error_environment (object obj);
/* used by EVAL, CONTROL */

/* Error message, if an argument isn't a Fixnum >=0:
 > obj: the faulty argument */
extern _Noreturn void error_posfixnum (object obj);
/* used by DEBUG, ENCODING, FOREIGN, IO, STREAM, TIME */

/* check_posfixnum(obj)
 > obj: an object
 < result: a fixnum >= 0, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_posfixnum_replacement (object obj);
#ifndef COMPILE_STANDALONE
MAKE_CHECK(posfixnum)
#endif
/* used by STREAM, LISPARIT */
%% exportF(object,check_posfixnum_replacement,(object obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK(posfixnum));
%% puts("#endif");

/* check_integer(obj)
 > obj: an object
 < result: an integer, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_integer_replacement (object obj);
#ifndef COMPILE_STANDALONE
MAKE_CHECK(integer)
#endif
/* used by LISPARIT */

/* check_pos_integer(obj)
 > obj: an object
 < result: an integer >= 0, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_pos_integer_replacement (object obj);
#ifndef COMPILE_STANDALONE
MAKE_CHECK_LOW(pos_integer,(integerp(obj)&&!R_minusp(obj)))
#endif
/* used by LISPARIT, LIST */
%% exportF(object,check_pos_integer_replacement,(object obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_LOW(pos_integer,(integerp(obj)&&!R_minusp(obj))));
%% puts("#endif");

/* Error when the argument is not a non-negative integer
 > kw: keyword naming the argument
 > object: bad index */
extern _Noreturn void error_pos_integer (object kw, object obj);
/* used by CHARSTRG, SEQUENCE */

/* Error message, if an argument isn't a Character:
 error_char(obj);
 > obj: the faulty argument */
extern _Noreturn void error_char (object obj);
/* used by IO, STREAM */

/* check_char(obj)
 > obj: an object
 < result: a character, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_char_replacement (object obj);
#ifndef COMPILE_STANDALONE
MAKE_CHECK(char)
#endif
/* used by CHARSTRG, ENCODING, IO */
%% #if notused
%% exportF(object,check_char_replacement,(object obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK(char));
%% puts("#endif");
%% #endif

/* check_string(obj)
 > obj: an object
 < result: a string, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_string_replacement (object obj);
#ifndef COMPILE_STANDALONE
MAKE_CHECK(string)
#endif
/* used by CHARSTRG, FOREIGN, MISC, PACKAGE, PATHNAME, STREAM, SOCKET, I18N */
%% exportF(object,check_string_replacement,(object obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK(string));
%% puts("#endif");

/* Error message, if an argument isn't a Simple-String:
 error_sstring(obj);
 > obj: the faulty argument */
extern _Noreturn void error_sstring (object obj);
/* is used by CHARSTRG */
%% #if notused
%% exportE(error_sstring,(object obj));
%% #endif

/* Checks a simple-string for being mutable.
 check_sstring_mutable(string); */
  #define check_sstring_mutable(obj)  \
    if (sstring_immutable(TheSstring(obj))) \
      error_sstring_immutable(obj);
  /* Error message, if a Simple-String is immutable:
   error_sstring_immutable(obj);
   > obj: the String */
  extern _Noreturn void error_sstring_immutable (object obj);
  /* is used by Macro check_sstring_mutable */

/* Error message, if an argument is not of type (OR STRING INTEGER).
 error_string_integer(obj); */
extern _Noreturn void error_string_integer (object obj);
%% exportE(error_string_integer,(object obj));

/* Error message, if a string size is too big.
 error_stringsize(size);
 > size: the desired string length */
extern _Noreturn void error_stringsize (uintV size);

/* Check a string size, reporting an error when it's too big. */
#define check_stringsize(size)  \
  if ((size) > stringsize_limit_1) \
    error_stringsize(size)/*;*/

/* error message if an argument is not a class.
 error_class(caller,obj);
 > obj: the erroneous argument */
extern _Noreturn void error_class (object obj);

/* Report an error when the argument is not an encoding:
 check_encoding(obj,&default,keyword_p)
 > obj: the (possibly) bad argument
 > default: what to return for :DEFAULT
 > keyword_p: true if the object comes from the :EXTERNAL-FORMAT argument
 < result: an encoding
 can trigger GC */
extern maygc object check_encoding (object obj, const gcv_object_t* e_default,
                                    bool keyword_p);
/* used by ENCODING, FOREIGN */

/* Signal an Error on illegal argument
 > arg: bad object
 > typ: expected type (may be nullobj to signal a regular error
        instead of a type-error)
 > key: the argument name (usually a keyword) */
global _Noreturn void error_illegal_arg (object arg, object typ, object key);
/* used by ENCODING, PATHNAME, STREAM */

/* Error when the property list has odd length
 error_plist_odd(caller,plist);
 > plist: bad plist */
extern _Noreturn void error_plist_odd (object plist);
%% exportE(error_plist_odd,(object plist));

/* error-message for non-paired keyword-arguments
 error_key_odd(argcount,caller);
 > argcount: the number of arguments on the STACK
 > caller: function */
extern _Noreturn void error_key_odd (uintC argcount, object caller);
%% exportE(error_key_odd,(uintC argcount, object caller));

/* error-message for flawed keyword
 error_key_notkw(kw);
 > key: Non-Symbol
 > caller: function */
extern _Noreturn void error_key_notkw (object key, object caller);

/* error-message for flawed keyword
 error_key_badkw(fun,kw,kwlist);
 > fun: function
 > key: illegal keyword
 > val: its value
 > kwlist: list of legal keywords */
extern _Noreturn void error_key_badkw (object fun, object key, object val, object kwlist);
%% exportE(error_key_badkw,(object fun, object key, object val, object kwlist));

/* check_function(obj)
 > obj: an object
 < result: a function, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_function_replacement (object obj);
#ifndef COMPILE_STANDALONE
MAKE_CHECK(function)
#endif
/* used by RECORD, EVAL, SEQUENCE, SYMBOL, FOREIGN */

/* error if funname does not have a function definition
 check_fdefinition(funname,caller)
 > funname: symbol or (setf symbol)
 > caller: symbol
 < a function object, possibly also installed as (FDEFINITION funname)
 can trigger GC */
extern maygc object check_fdefinition (object funname, object caller);
/* used by EVAL, CONTROL */

/* check_funname(obj)
 > errtype: type of condition to signal if obj is not a function name,
            either type_error or source_program_error
 > caller: a symbol
 > obj: an object
 < result: a function name, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_funname_replacement (condition_t errtype, object caller, object obj);
#ifndef COMPILE_STANDALONE
static inline maygc object check_funname (condition_t errtype, object caller, object obj) {
  if (!funnamep(obj))
    obj = check_funname_replacement(errtype,caller,obj);
  return obj;
}
#endif
/* used by EVAL, CONTROL */

/* Error message, if an argument is a lambda-expression instead of a function:
 error_lambda_expression(caller,obj);
 caller: caller (a symbol)
 obj: the faulty argument */
extern _Noreturn void error_lambda_expression (object caller, object obj);
/* is used by EVAL, SYMBOL */

/* too many/few arguments in a function call
 > caller : the function that is reporting the error (unbound == EVAL/APPLY)
 > func   : the function being incorrectly called
 > ngiven : the number of arguments given
 < nmax   : the maximum number of arguments accepted
 < nmin   : the minimum number of arguments required */
extern _Noreturn void error_too_many_args (object caller, object func, uintL ngiven, uintL nmax);
extern _Noreturn void error_too_few_args (object caller, object func, uintL ngiven, uintL nmin);

/* used by EVAL, FOREIGN */

/* error-message, if a symbol has no value.
 > symbol_: unbound symbol
 > restart_p: false if nonreturning
 < value1: bound value
 < value2: T if STORE-VALUE was selected, NIL otherwise
 can trigger GC */
extern maygc void check_variable_value_replacement (gcv_object_t *symbol_,
                                                    bool restart_p);
/* used by EVAL, CONTROL */

/* Error message, if an argument isn't of a given elementary C type.
 error_<ctype>(obj);
 > obj: the faulty argument */
global _Noreturn void error_c_integer (object obj, int tcode, bool signedp);
%% exportE(error_c_integer,(object obj, int tcode, bool signedp));
#define error_uint8(obj)   error_c_integer(obj,0,true)
#define error_sint8(obj)   error_c_integer(obj,0,true)
#define error_uint16(obj)  error_c_integer(obj,1,false)
#define error_sint16(obj)  error_c_integer(obj,1,true)
#define error_uint32(obj)  error_c_integer(obj,2,false)
#define error_sint32(obj)  error_c_integer(obj,2,true)
#define error_uint64(obj)  error_c_integer(obj,3,false)
#define error_sint64(obj)  error_c_integer(obj,3,true)
/* extern _Noreturn void error_uint (object obj);
 extern _Noreturn void error_sint (object obj); */
#if (int_bitsize==16)
  #define error_uint  error_uint16
  #define error_sint  error_sint16
#else /* (int_bitsize==32) */
  #define error_uint  error_uint32
  #define error_sint  error_sint32
#endif
/* extern _Noreturn void error_ulong (object obj);
 extern _Noreturn void error_slong (object obj); */
#if (long_bitsize==32)
  #define error_ulong  error_uint32
  #define error_slong  error_sint32
#else /* (long_bitsize==64) */
  #define error_ulong  error_uint64
  #define error_slong  error_sint64
#endif
/* used by STREAM, ENCODING, modules */
%% export_def(error_uint8(obj));
%% export_def(error_sint8(obj));
%% export_def(error_uint16(obj));
%% export_def(error_sint16(obj));
%% export_def(error_uint32(obj));
%% export_def(error_sint32(obj));
%% export_def(error_uint64(obj));
%% export_def(error_sint64(obj));
%% #if (int_bitsize==16)
%%   emit_define("error_uint","error_uint16");
%%   emit_define("error_sint","error_sint16");
%% #else
%%   emit_define("error_uint","error_uint32");
%%   emit_define("error_sint","error_sint32");
%% #endif
%% #if (long_bitsize==32)
%%   emit_define("error_ulong","error_uint32");
%%   emit_define("error_slong","error_sint32");
%% #else
%%   emit_define("error_ulong","error_uint64");
%%   emit_define("error_slong","error_sint64");
%% #endif

/* Check whether an object can be converted to an elementary C type.
 check_<ctype>(obj)
 > obj: an object
 < result: an object that can be converted to the C type, either the same
           as obj or a replacement
 can trigger GC */
extern maygc object check_c_integer_replacement (object obj, int tcode, bool signedp);
#define check_uint8_replacement(obj)  check_c_integer_replacement(obj,0,false)
#ifndef COMPILE_STANDALONE
MAKE_CHECK_(uint8)
#endif
#define check_sint8_replacement(obj)  check_c_integer_replacement(obj,0,true)
#ifndef COMPILE_STANDALONE
MAKE_CHECK_(sint8)
#endif
#define check_uint16_replacement(obj)  check_c_integer_replacement(obj,1,false)
#ifndef COMPILE_STANDALONE
MAKE_CHECK_(uint16)
#endif
#define check_sint16_replacement(obj)  check_c_integer_replacement(obj,1,true)
#ifndef COMPILE_STANDALONE
MAKE_CHECK_(sint16)
#endif
#define check_uint32_replacement(obj)  check_c_integer_replacement(obj,2,false)
#ifndef COMPILE_STANDALONE
MAKE_CHECK_(uint32)
#endif
#define check_sint32_replacement(obj)  check_c_integer_replacement(obj,2,true)
#ifndef COMPILE_STANDALONE
MAKE_CHECK_(sint32)
#endif
#define check_uint64_replacement(obj)  check_c_integer_replacement(obj,3,false)
#ifndef COMPILE_STANDALONE
MAKE_CHECK_(uint64)
#endif
#define check_sint64_replacement(obj)  check_c_integer_replacement(obj,3,true)
#ifndef COMPILE_STANDALONE
MAKE_CHECK_(sint64)
#endif
extern maygc object check_uint_replacement (object obj);
#ifndef COMPILE_STANDALONE
MAKE_CHECK_(uint)
#endif
extern maygc object check_sint_replacement (object obj);
#ifndef COMPILE_STANDALONE
MAKE_CHECK_(sint)
#endif
extern maygc object check_ulong_replacement (object obj);
#ifndef COMPILE_STANDALONE
MAKE_CHECK_(ulong)
#endif
extern maygc object check_slong_replacement (object obj);
#ifndef COMPILE_STANDALONE
MAKE_CHECK_(slong)
#endif
extern maygc object check_ffloat_replacement (object obj);
#ifndef COMPILE_STANDALONE
MAKE_CHECK_LOW(ffloat,single_float_p(obj))
#endif
extern maygc object check_dfloat_replacement (object obj);
#ifndef COMPILE_STANDALONE
MAKE_CHECK_LOW(dfloat,double_float_p(obj))
#endif
/* is used by STREAM, FFI */
%% exportF(object,check_c_integer_replacement,(object obj, int tcode, bool signedp));
%% export_def(check_uint8_replacement(obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_(uint8));
%% puts("#endif");
%% export_def(check_sint8_replacement(obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_(sint8));
%% puts("#endif");
%% export_def(check_uint16_replacement(obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_(uint16));
%% puts("#endif");
%% export_def(check_sint16_replacement(obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_(sint16));
%% puts("#endif");
%% export_def(check_uint32_replacement(obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_(uint32));
%% puts("#endif");
%% export_def(check_sint32_replacement(obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_(sint32));
%% puts("#endif");
%% export_def(check_uint64_replacement(obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_(uint64));
%% puts("#endif");
%% export_def(check_sint64_replacement(obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_(sint64));
%% puts("#endif");
%% exportF(object,check_uint_replacement,(object obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_(uint));
%% puts("#endif");
%% exportF(object,check_sint_replacement,(object obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_(sint));
%% puts("#endif");
%% exportF(object,check_ulong_replacement,(object obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_(ulong));
%% puts("#endif");
%% exportF(object,check_slong_replacement,(object obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_(slong));
%% puts("#endif");
%% exportF(object,check_ffloat_replacement,(object obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_LOW(ffloat,single_float_p(obj)));
%% puts("#endif");
%% exportF(object,check_dfloat_replacement,(object obj));
%% puts("#ifndef COMPILE_STANDALONE");
%% export_literal(MAKE_CHECK_LOW(dfloat,double_float_p(obj)));
%% puts("#endif");

/* ##################### PACKBIBL for PACKAGE.D ############################ */

/* UP: tests whether a symbol is accessible in a package and isn't hidden
 by a another symbol with the same name.
 accessiblep(sym,pack)
 > sym: Symbol
 > pack: Package
 < result: true if sym is accessible in pack and nod hidden,
             else false */
extern bool accessiblep (object sym, object pack);
/* is used by IO */

/* UP: tests whether a symbol is accessible as external symbol in a package
 externalp(sym,pack)
 > sym: Symbol
 > pack: Package
 < result: true if sym is accessible as external symbol in pack ,
           else false */
extern bool externalp (object sym, object pack);
/* is used by IO */

/* UP: locates an external symbol with a given printname in a package.
 find_external_symbol(string,invert,pack,&sym)
 > string: string
 > invert: whether to implicitly case-invert the string
 > pack: package
 < result: true, if an external symbol with that printname has been found in pack.
 < sym: this symbol, if found. */
extern bool find_external_symbol (object string, bool invert, object pack, object* sym_);
/* is used by IO */

/* UP: locates a package with a given name or nickname
 find_package(string)
 > string: String
 < result: Package with that name or NIL
 can trigger GC in threads builds */
extern maygc object find_package (object string);
/* is used by IO, EVAL */
%% exportF(object,find_package,(object string));

/* UP: Interns a symbol with a given printname in a package.
 intern(string,invert,pack,&sym)
 > string: String
 > invert: whether to implicitly case-invert the string
 > pack: Package
 < sym: Symbol
 < result: 0, if not found but newly created
           1, if found as external symbol
           2, if inherited through use-list
           3, if exists as internal symbol
 can trigger GC */
extern maygc uintBWL intern (object string, bool invert, object pack, object* sym_);
/* is used by IO, SPVW */
%% exportF(uintBWL,intern,(object string, object pack, object* sym_));

/* UP: Interns a symbol with a given printname in the Keyword-Package.
 intern_keyword(string)
 > string: String
 < result: Symbol, a keyword
 can trigger GC */
extern maygc object intern_keyword (object string);
/* is used by IO, EVAL, GRAPH */
%% exportF(object,intern_keyword,(object string));

/* UP: Imports a symbol into a package
 import(&sym,&pack);
 > sym: Symbol (on STACK)
 > pack: Package (on STACK)
 < sym: Symbol, EQ with the old
 < pack: Package, EQ with the old
 can trigger GC */
extern maygc void import (const gcv_object_t* sym_, const gcv_object_t* pack_);
/* is used by SPVW */

/* UP: Exports a symbol from a package
 export(&sym,&pack);
 > sym: Symbol (on STACK)
 > pack: Package (on STACK)
 < sym: Symbol, EQ with the old
 < pack: Package, EQ with the old
 can trigger GC */
extern maygc void export (const gcv_object_t* sym_, const gcv_object_t* pack_);
/* is used by SPVW */

/* UP: gets the current package
 get_current_package()
 < result: current Package
 can trigger GC */
extern maygc object get_current_package (void);
/* is used by IO */

/* check whether package lock prevents assignment to symbol
 can trigger GC */
extern maygc void symbol_value_check_lock (object caller, object symbol);
/* used by EVAL */

/* UP: Initializes the package-management
 init_packages();
 can trigger GC */
extern void init_packages (void);
/* is used by SPVW */

/* ##################### PATHBIBL for PATHNAME.D ########################### */

/* return the file stream truename
 > s: file stream (open or closed) - no type check is done!
 < truename of the file associated with the stream
 can trigger GC
 for syscall module */
extern maygc object file_stream_truename (object s);
%% exportF(object,file_stream_truename,(object s));

/* Check that the namestring for path will be parsed into a similar object
 used by pr_orecord() in io.d
 can trigger GC */
extern maygc bool namestring_correctly_parseable_p (gcv_object_t *path_);

/* Check whether the file exists
 > namestring : path
 > STACK_0 = FILE-ERROR slot PATHNAME
 < resolved : truename (if return is success, i.e., FILE or DIR)
 < fwd: file write date (if return is success and address is supplied)
 < fsize: file size (if return is success and address is supplied)
 < return : */
typedef enum {
  FILE_KIND_FILE,               /* regular file */
  FILE_KIND_DIR,                /* directory */
  FILE_KIND_BAD, /* exists but cannot figure out what it is, check errno */
  FILE_KIND_NONE /* namestring does not name an existing file or directory */
} file_kind_t;
extern /*maygc*/ file_kind_t classify_namestring (const char* namestring, char *resolved, gcv_object_t *fwd, gcv_object_t* fsize);
/* used by spvw_language:init_language */

/* Converts an object into an absolute physical pathname and returns its
   namestring.
 physical_namestring(thing)
 > thing: an object
 < result: the namestring of the pathname denoted by thing
 can trigger GC */
extern maygc object physical_namestring (object thing);
%% exportF(object,physical_namestring,(object obj));

/* Converts a directory pathname to an OS directory specification.
 > pathname: an object
 > use_default: whether to use the current default directory
 < result: a simple-bit-vector containing an ASCIZ string in OS format
 can trigger GC */
extern maygc object pathname_to_OSdir (object pathname, bool use_default);
/* used by modules (I18N) */
%% exportF(object,pathname_to_OSdir,(object pathname, bool use_default));

/* Converts an OS directory specification to a directory pathname.
 > path: a pathname referring to a directory
 < result: a pathname without name and type
 can trigger GC */
extern maygc object OSdir_to_pathname (const char* path);
/* used by modules (I18N) */
%% exportF(object,OSdir_to_pathname,(const char* path));

/* UP: Initializes the pathname-system.
 init_pathnames();
 can trigger GC */
extern maygc void init_pathnames (void);
/* is used by SPVW */

/* Duplicate an open file handle.
 handle_dup(oldfd)
 Similar to dup(oldfd), with error checking.
 To be called only inside begin/end_system_call(). */
extern Handle handle_dup (Handle old_handle);
%% exportF(Handle,handle_dup,(Handle old_handle));

/* Duplicate an open file handle.
 handle_dup2(oldfd,newfd)
 Similar to dup2(oldfd,newfd), with error checking. The result may or may not
 be equal to newfd.
 To be called only inside begin/end_system_call(). */
extern Handle handle_dup2 (Handle old_handle, Handle new_handle);
%% exportF(Handle,handle_dup2,(Handle old_handle, Handle new_handle));

/* Locates the executable program immediately after the program start.
 find_executable(argv[0]) */
extern int find_executable (const char * program_name);
/* is used by SPVW */

/* check the :DIRECTION argument
 return one of the following: */
typedef enum {
  /* see READ_P, RO_P and WRITE_P in <stream.d> regarding the choice of values */
  DIRECTION_PROBE           = 0,
  DIRECTION_INPUT           = 1,
  DIRECTION_INPUT_IMMUTABLE = 3,
  DIRECTION_OUTPUT          = 4,
  DIRECTION_IO              = 5,
  /* Work around a g++-3.4.0 bug, see
     http://gcc.gnu.org/bugzilla/show_bug.cgi?id=15069 */
  DIRECTION_DUMMY_TO_AVOID_GXX_BUG = 100
} direction_t;
extern direction_t check_direction (object dir);
%% printf("typedef enum { DIRECTION_PROBE=%d, DIRECTION_INPUT=%d, DIRECTION_INPUT_IMMUTABLE=%d, DIRECTION_OUTPUT=%d, DIRECTION_IO=%d} direction_t;\n",
%%        DIRECTION_PROBE, DIRECTION_INPUT, DIRECTION_INPUT_IMMUTABLE,
%%        DIRECTION_OUTPUT, DIRECTION_IO);
%% exportF(direction_t,check_direction,(object dir));

/* check the :IF-DOES-NOT-EXIST argument
 check_if_does_not_exist(argument)
 return one of the following: */
typedef enum {
  IF_DOES_NOT_EXIST_UNBOUND,
  IF_DOES_NOT_EXIST_ERROR,
  IF_DOES_NOT_EXIST_NIL,
  IF_DOES_NOT_EXIST_CREATE
} if_does_not_exist_t;
extern if_does_not_exist_t check_if_does_not_exist (object if_not_exist);
%% emit_typedef("enum { IF_DOES_NOT_EXIST_UNBOUND, IF_DOES_NOT_EXIST_ERROR, IF_DOES_NOT_EXIST_NIL, IF_DOES_NOT_EXIST_CREATE }","if_does_not_exist_t");
%% exportF(if_does_not_exist_t,check_if_does_not_exist,(object if_not_exist));

/* Converts a :IF-DOES-NOT-EXIST enum item to a symbol.
 if_does_not_exist_symbol(item) */
extern object if_does_not_exist_symbol (if_does_not_exist_t if_not_exist);
%% exportF(object,if_does_not_exist_symbol,(if_does_not_exist_t if_not_exist));

/* check the :IF-EXISTS argument
 check_if_exists(argument)
 return one of the following: */
typedef enum {
  IF_EXISTS_UNBOUND,
  IF_EXISTS_ERROR,
  IF_EXISTS_NIL,
  IF_EXISTS_RENAME,
  IF_EXISTS_RENAME_AND_DELETE,
  IF_EXISTS_SUPERSEDE,
  IF_EXISTS_APPEND,
  IF_EXISTS_OVERWRITE
} if_exists_t;
extern if_exists_t check_if_exists (object if_exists);
%% emit_typedef("enum { IF_EXISTS_UNBOUND, IF_EXISTS_ERROR, IF_EXISTS_NIL, IF_EXISTS_RENAME, IF_EXISTS_RENAME_AND_DELETE, IF_EXISTS_SUPERSEDE, IF_EXISTS_APPEND, IF_EXISTS_OVERWRITE }","if_exists_t");
%% exportF(if_exists_t,check_if_exists,(object if_exists));

/* Converts a :IF-EXISTS enum item to a symbol.
 if_exists_symbol(item) */
extern object if_exists_symbol (if_exists_t if_exists);
%% exportF(object,if_exists_symbol,(if_exists_t if_exists));

#if defined(WIN32_NATIVE)
/* ------------------- Functions defined in w32shell.c ------------------- */

/* shell_quote() - surround dangerous strings with double quotes.
 escape quotes and backslashes.
 dest should be twice as large as source
  + 2 (for quotes) + 1 for zero byte + 1 for possible endslash */
extern int shell_quote (char * dest, const char * source);
/* used by PATHNAME and the driver clisp.exe */

/* real_path() - the ultimate shortcut megaresolver
   style inspired by directory_search_scandir
 > namein: filename pointing to file or directory
            wildcards (only asterisk) may appear only as filename
 < nameout: filename with directory and file shortcuts resolved
             on failure holds filename resolved so far
 < result:  true if resolving succeeded */
extern BOOL real_path (LPCSTR namein, LPSTR nameout);
/* used by PATHNAME, SPVW [for loadmem()] and the driver clisp.exe */

#endif

/* ##################### PREDBIBL for PREDTYPE.D ########################### */

/* UP: test for atomic equality EQL
 eql(obj1,obj2)
 > obj1,obj2: Lisp-objects
 < result: true, if objects are equal */
extern bool eql (object obj1, object obj2);
/* is used by CONTROL, EVAL, HASHTABL, LISPARIT */
%% #if notused
%% exportF(bool,eql,(object obj1, object obj2));
%% #endif

/* UP: tests for equality EQUAL
 equal(obj1,obj2)
 > obj1,obj2: Lisp-objects
 < result: true, if objects are equal */
extern bool equal (object obj1, object obj2);
/* is used by EVAL, PATHNAME, HASHTABL, MISC */
%% #if notused
%% exportF(bool,equal,(object obj1, object obj2));
%% #endif

/* UP: tests for a more lax equality EQUALP
 equalp(obj1,obj2)
 > obj1,obj2: Lisp-objects
 < result: true, if objects are equal */
extern bool equalp (object obj1, object obj2);
/* is used by PATHNAME, HASHTABL */
%% #if notused
%% exportF(bool,equalp,(object obj1, object obj2));
%% #endif

/* typep_class(obj,clas)
 > obj: an object
 > clas: a class object
 < true if the object is an instance of the class, false otherwise
 clobbers value1, mv_count */
extern bool typep_class (object obj, object clas);
%% exportF(bool,typep_class,(object obj, object clazz));

/* typep_classname(obj,classname)
 > obj: an object
 > classname: a symbol expected to name a class with "proper name" classname
 < true if the object is an instance of the class, false otherwise
 clobbers value1, mv_count */
extern bool typep_classname (object obj, object classname);
%% exportF(bool,typep_classname,(object obj, object classname));

/* UP: expand all DEFTYPE definitions in the type spec
 (recursively, unless once_p is true)
 > type_spec: Lisp object
 < result: the expansion (when not a deftyped type, returns the argument)
 can trigger GC */
extern maygc object expand_deftype (object type_spec, bool once_p);
/* used by predtype.d, sequence.d */

/* UP: Makes a statistic about the action of a GC.
 with_gc_statistics(fun);
 > fun: Function that does a GC */
typedef void gc_function_t (void);
extern void with_gc_statistics (gc_function_t* fun);
/* is used by SPVW */

/* ####################### RECBIBL for RECORD.D ############################ */

/* check_structure(obj)
 > obj: an object
 < result: a structure object, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_structure_replacement (object obj);
#ifndef COMPILE_STANDALONE
static inline maygc object check_structure (object obj) {
  if (!structurep(obj))
    obj = check_structure_replacement(obj);
  return obj;
}
#endif
/* used by IO */

/* instance_un_realloc(obj);
 walks over forward pointers left by instance reallocation (CHANGE-CLASS
 and/or redefined classes).
 > obj: a CLOS instance, possibly a forward pointer
 < obj: the same CLOS instance, not a forward pointer
 Note that the forwarded instance must not be leaked to "userland", because
 the forward pointer and the forwarded instance are not EQ. */
#define instance_un_realloc(obj) \
  if (record_flags(TheInstance(obj)) & instflags_forwarded_B) {        \
    (obj) = TheInstance(obj)->inst_class_version;                      \
    /* We know that there is at most one indirection. */               \
    ASSERT(!(record_flags(TheInstance(obj)) & instflags_forwarded_B)); \
  }

/* update_instance(user_obj,obj)
 updates a CLOS instance after its class or one of its superclasses has been
 redefined.
 > user_obj: a CLOS instance, possibly a forward pointer
 > obj: the same CLOS instance, not a forward pointer
 < result: the same CLOS instance, not a forward pointer
 can trigger GC */
extern maygc object update_instance (object user_obj, object obj);

/* instance_valid_p(obj)
 Tests whether a CLOS instance can be used without first updating it.
 > obj: a CLOS instance, not a forward pointer
 < result: false if its class was redefined since the instance was last used */
#define instance_valid_p(obj) \
  nullp(TheClassVersion(TheInstance(obj)->inst_class_version)->cv_next)

/* instance_update(user_obj,obj);
 performs necessary CLOS instance updates on obj.
 > user_obj: a CLOS instance, possibly a forward pointer
 > obj: the same CLOS instance, not a forward pointer
 < obj: the same CLOS instance, not a forward pointer
 can trigger GC */
#define instance_update(user_obj,obj) \
  if (!instance_valid_p(obj)) \
    (obj) = update_instance(user_obj,obj); \
  GCTRIGGER1(obj)/*;*/

/* Test for CLOS instance of a given class
 > obj: a Lisp object
 > clas: a class that doesn't have obsolete instances */
#ifndef COMPILE_STANDALONE
static inline bool instanceof (object obj, object clas) {
  if (!instancep(obj)) return false;
  var object obj_forwarded = obj;
  instance_un_realloc(obj_forwarded);
  /*instance_update(obj,obj_forwarded); - not needed since we don't access a slot */
  var object cv = TheInstance(obj_forwarded)->inst_class_version;
  var object objclas = TheClassVersion(cv)->cv_newest_class;
  return !eq(gethash(clas,TheClass(objclas)->all_superclasses,false),nullobj);
}
#endif

/* ###################### SEQBIBL for SEQUENCE.D ########################### */

/* UP: Converts an object into a sequence of a given type.
 coerce_sequence(obj, result_type, error_p)
 > obj: Object, should be a sequence
 > result_type: identifier (symbol) of the sequence-type
 > error_p: when result_type does not name a sequence:
              when true, signal an error; when false, return nullobj
 < value: Sequence of type result_type
 can trigger GC */
extern maygc Values coerce_sequence (object sequence, object result_type,
                                     bool error_p);
/* is used by PREDTYPE, EVAL */

/* UP:  Traverses a sequence and calls a function for every element.
 map_sequence(obj,fun,arg);
 > obj: Object, should be a sequence
 > fun: Function, fun(arg,element) may trigger GC
 > arg: arbitrary given argument
 can trigger GC */
typedef maygc void map_sequence_function_t (void* arg, object element);
extern maygc void map_sequence (object obj, map_sequence_function_t* fun, void* arg);
/* used by ARRAY, modules */
/* do not use emit_typedef_f because g++ complains:
 error: invalid application of `sizeof' to a function type */
%% puts("typedef void map_sequence_function_t (void* arg, object element);");
%% exportF(void,map_sequence,(object obj, map_sequence_function_t* fun, void* arg));

/* ###################### STRMBIBL for STREAM.D ############################ */

/* Error message, if an argument isn't a stream:
 check_stream_replacement(obj);
 > obj: not a stream
 < obj: a stream
 can trigger GC */
extern maygc object check_stream_replacement (object obj);
#ifndef COMPILE_STANDALONE
static inline maygc object check_stream (object obj) {
  if (!streamp(obj))
    obj = check_stream_replacement(obj);
  return obj;
}
#endif
/* is used by IO, STREAM, DEBUG */

/* parse timeout argument
 sec = posfixnum or (SEC . USEC) or (SEC USEC) or float or ratio or nil/unbound
 usec = posfixnum or nil/unbound
 can trigger GC */
extern maygc struct timeval * sec_usec (object sec, object usec, struct timeval *tv);
%% exportF(struct timeval *,sec_usec,(object sec, object usec, struct timeval *tv));

/* Convert C sec/usec (struct timeval et al) pair into Lisp number (of seconds)
 if abs_p is true, add UNIX_LISP_TIME_DIFF
 can trigger GC */
#if defined(SIZEOF_STRUCT_TIMEVAL) && SIZEOF_STRUCT_TIMEVAL == 16
global maygc object sec_usec_number (uint64 sec, uint64 usec, bool abs_p);
#else
global maygc object sec_usec_number (uint32 sec, uint32 usec, bool abs_p);
#endif
%% #if defined(SIZEOF_STRUCT_TIMEVAL) && SIZEOF_STRUCT_TIMEVAL == 16
%% exportF(object,sec_usec_number,(uint64 sec, uint64 usec, bool abs_p));
%% #else
%% exportF(object,sec_usec_number,(uint32 sec, uint32 usec, bool abs_p));
%% #endif

/* UP: Initializes the OS dependencies for streams.
 init_stream_osdeps(); */
extern void init_stream_osdeps (void);

/* UP: Initializes the stream variables.
 init_streamvars(batch_p);
 > batch_p: Flag, whether *standard-input*, *standard-output*, *error-output*
            should be initialized to the C stdio handle-streams
            (deviates from the standard)
 can trigger GC */
extern maygc void init_streamvars (bool batch_p);
/* used by SPVW */

/* Error-message, if a stream-operation is not permitted on a stream.
 error_illegal_streamop(caller,stream);
 > caller: Caller (a symbol)
 > stream: Stream */
extern _Noreturn void error_illegal_streamop (object caller, object stream);
/* is used by IO */

/* Reads a byte from a stream.
 read_byte(stream)
 > stream: Stream
 < result: read Integer (eof_value at EOF)
 can trigger GC */
extern maygc object read_byte (object stream);
/* is used by SEQUENCE */

/* Writes a byte onto a stream.
 write_byte(stream,byte);
 > stream: Stream
 > byte: Integer to be written
 can trigger GC */
extern maygc void write_byte(object stream, object byte);
/* is used by SEQUENCE */

/* Reads a character from a stream.
 read_char(&stream)
 > stream: Stream
 < stream: Stream
 < result: read character (eof_value at EOF)
 can trigger GC */
extern maygc object read_char (const gcv_object_t* stream_);
/* is used by IO, DEBUG, SEQUENCE */

/* Pushes the last read character back onto a stream.
 unread_char(&stream,ch);
 > ch: last read character
 > stream: Stream
 < stream: Stream
 can trigger GC */
extern maygc void unread_char (const gcv_object_t* stream_, object ch);
/* is used by IO, DEBUG */

/* Reads a character from a stream without using it.
 peek_char(&stream)
 > stream: Stream
 < stream: Stream
 < result: read character (eof_value at EOF)
 can trigger GC */
extern maygc object peek_char (const gcv_object_t* stream_);
/* is used by IO */

/* Reads a line of characters from a stream.
 read_line(&stream,&buffer)
 > stream: stream
 > buffer: a semi-simple string
 < stream: stream
 < buffer: contains the read characters, excluding the terminating #\Newline
 < result: true is EOF was seen before newline, else false
 can trigger GC */
extern maygc bool read_line (const gcv_object_t* stream_, const gcv_object_t* buffer_);
/* used by IO */

/* Write a character onto a stream.
 write_char(&stream,ch);
 > ch: Character to be written
 > stream: Stream
 < stream: Stream
 can trigger GC */
extern maygc void write_char (const gcv_object_t* stream_, object ch);
/* is used by LISPARIT, IO, ERROR, SEQUENCE */

/* Writes a character onto a stream.
 write_code_char(&stream,ch);
 > ch: a character
 > stream: Stream
 < stream: Stream
 can trigger GC
 extern maygc void write_code_char (const gcv_object_t* stream_, chart ch); */
#define write_code_char(stream_,ch)  write_char(stream_,code_char(ch))
/* is used by LISPARIT, IO */

/* Writes a fixed standard-char onto a stream.
 write_ascii_char(&stream,ch);
 > ch: a standard char, in ASCII encoding
 > stream: Stream
 < stream: Stream
 can trigger GC
 extern maygc void write_ascii_char (const gcv_object_t* stream_, uintB ch); */
#define write_ascii_char(stream_,ch)  write_char(stream_,code_char(as_chart(ch)))
/* is used by LISPARIT, IO, DEBUG, Macro TERPRI */

#ifdef ENABLE_UNICODE
/* Changes a terminal stream's external format.
 > stream: a stream
 > encoding: an encoding
 can trigger GC */
extern maygc void set_terminalstream_external_format (object stream, object encoding);
/* used by ENCODING */
#endif

/* UP: Determines whether a stream is "interactive",
     ie. whether the input from the stream
     depends from a promt that has propably just been printed.
 interactive_stream_p(stream)
 > stream: Stream */
extern bool interactive_stream_p (object stream);
/* is used by DEBUG */

/* UP: Closes a stream.
 builtin_stream_close(&stream,abort);
 > stream: Builtin-Stream
 > abort: flag: non-0 => ignore errors
 < stream: Builtin-Stream
 can trigger GC */
extern maygc void builtin_stream_close (const gcv_object_t* stream_, uintB abort);
/* is used by PATHNAME, SPVW, DEBUG, MISC */
%% exportF(void,builtin_stream_close,(const gcv_object_t* stream_, uintB abort));

/* UP: Closes a list of open files.
 close_some_files(list);
 > list: List of open builtin-streams
 can trigger GC */
extern maygc void close_some_files (object list);
/* is used by SPVW */

/* UP: Closes all open files.
 close_all_files();
 can trigger GC */
extern maygc void close_all_files (void);
/* is used by SPVW */

/* UP: declares all open file-streams closed.
 closed_all_files(); */
extern void closed_all_files (void);
/* is used by SPVW */

typedef enum {
  LISTEN_AVAIL,  /* a character is available */
  LISTEN_EOF,    /* EOF is reached */
  LISTEN_WAIT,   /* no character is available, but not because of EOF */
  LISTEN_ERROR   /* stream is broken, e.g., ECONNRESET */
} listen_t;

/* UP: determines whether a char is available in the Stream stream
 listen_char(stream)
 > stream: Stream
 < result: input availability
 can trigger GC */
extern maygc listen_t listen_char (object stream);
/* is used by IO, DEBUG */

/* UP: clears an already entered interactive input from a Stream stream.
 clear_input(stream)
 > stream: Stream
 < result: true if input has been deleted
 can trigger GC */
extern maygc bool clear_input (object stream);
/* is used by IO, DEBUG */

/* UP: Determines whether a stream has a byte immediately available.
 listen_byte(stream)
 > stream: a stream with element-type ([UN]SIGNED-BYTE 8)
 < result: input availability
 can trigger GC */
extern maygc listen_t listen_byte (object stream);
/* is used by */

/* UP: Finishes waiting output of a Stream stream
 finish_output(stream);
 > stream: Stream
 can trigger GC */
extern maygc void finish_output (object stream);
/* is used by IO */

/* UP: Forces waiting output of a Stream stream
 force_output(stream);
 > stream: Stream
 can trigger GC */
extern maygc void force_output (object stream);
/* is used by IO, DEBUG */

/* UP: clear the waiting output of a stream.
 clear_output(stream);
 > stream: Stream
 can trigger GC */
extern maygc void clear_output (object stream);
/* is used by IO */

/* UP: Gives the line position of a stream:
 get_line_position(stream)
 > stream: Stream
 < result: Line-Position (Fixnum >=0 or NIL)
 can trigger GC */
extern maygc object get_line_position (object stream);
/* is used by IO, DEBUG */

/* Writes a newline on a stream, if it is not already positioned at column 0.
 fresh_line(&stream);
 > stream: Stream
 < stream: Stream
 < result: true if did output a newline
 can trigger GC */
extern maygc bool fresh_line (const gcv_object_t* stream_);
/* is used by IO */

/* Writes a newline on a stream, delayed and nullified if the next character
 written would be a newline anyway.
 elastic_newline(&stream);
 > stream: Stream
 < stream: Stream
 can trigger GC */
extern maygc void elastic_newline (const gcv_object_t* stream_);
/* is used by IO */

/* UP: give away corresponding underlying handle
 making sure buffers were flushed. One can then use the
 handle outside of stream object as far as the latter
 is not used and not GCed.
 stream_lend_handle(stream, inputp, handletype)
 > stream_: stream for handle to extract
 > inputp: whether its input or output side is requested.
 < stream_: corrected stream (if the original argument was not a handle stream)
 < int * handletype 0:reserved, 1:file, 2:socket
 < Handle result - extracted handle
 can trigger GC */
extern maygc Handle stream_lend_handle (gcv_object_t *stream_, bool inputp, int * handletype);
/* used by STREAM */
%% exportF(Handle,stream_lend_handle,(gcv_object_t *stream_, bool inputp, int * handletype));

/* extract the OS file handle from the file stream
 > stream: open Lisp file stream
 < fd: OS file handle
 > permissive_p: return nullobj instead of signaling an error
 < result: either stream, or a corrected stream in case stream was invalid
           or nullobj if permissive_p was true and the stream was invalid
 for syscall module
 can trigger GC */
extern maygc object open_file_stream_handle (object stream, Handle *fd, bool permissive_p);
%% exportF(object,open_file_stream_handle,(object stream, Handle *fd, bool permissive_p));

/* return the OS's idea of the stream length for the file stream
 > stream: for error reporting
 > fd: OS file handle
 < result: the length of the stream
 should be wrapped in begin_system_call()/end_system_call()
 for gdbm module */
extern maygc off_t handle_length (gcv_object_t *stream_, Handle fd);
%% exportF(off_t,handle_length,(gcv_object_t *stream_, Handle fd));

/* Function: Reads several bytes from a stream.
 read_byte_array(&stream,&bytearray,start,len,persev)
 > stream: stream (on the STACK)
 > object bytearray: simple-8bit-vector (on the STACK)
 > uintL start: start index of byte sequence to be filled
 > uintL len: length of byte sequence to be filled
 > perseverance_t persev: how to react on incomplete I/O
 < uintL result: number of bytes that have been filled
 can trigger GC */
extern maygc uintL read_byte_array (const gcv_object_t* stream_, const gcv_object_t* bytearray_, uintL start, uintL len, perseverance_t persev);
/* used by SEQUENCE, PATHNAME */
%% exportF(uintL,read_byte_array,(const gcv_object_t* stream_, const gcv_object_t* bytearray_, uintL start, uintL len, perseverance_t persev));

/* Function: Writes several bytes to a stream.
 write_byte_array(&stream,&bytearray,start,len,no_hang)
 > stream: Stream (on the STACK)
 > object bytearray: simple-8bit-vector (on the STACK)
 > uintL start: start index of byte sequence to be written
 > uintL len: length of byte sequence to be written
 > perseverance_t persev: how to react on incomplete I/O
 < uintL result: number of bytes that have been written
 can trigger GC */
extern maygc uintL write_byte_array (const gcv_object_t* stream_, const gcv_object_t* bytearray_, uintL start, uintL len, perseverance_t persev);
/* is used by SEQUENCE */
%% exportF(uintL,write_byte_array,(const gcv_object_t* stream_, const gcv_object_t* bytearray_, uintL start, uintL len, perseverance_t persev));

/* Function: Reads several characters from a stream.
 read_char_array(&stream,&chararray,start,len)
 > stream: stream (on the STACK)
 > object chararray: a mutable string that is or was simple (on the STACK)
 > uintL start: start index of character sequence to be filled
 > uintL len: length of character sequence to be filled
 < uintL result: number of characters that have been filled
 can trigger GC */
extern maygc uintL read_char_array (const gcv_object_t* stream_, const gcv_object_t* chararray_, uintL start, uintL len);
/* is used by SEQUENCE */

/* Function: Writes several characters to a stream.
 write_char_array(&stream,&chararray,start,len)
 > stream: stream (on the STACK)
 > object chararray: not-reallocated simple-string (on the STACK)
 > uintL start: start index of character sequence to be written
 > uintL len: length of character sequence to be written
 can trigger GC */
extern maygc void write_char_array (const gcv_object_t* stream_, const gcv_object_t* chararray_, uintL start, uintL len);
/* is used by SEQUENCE */

/* UP: Gives the stream that is the value of a variable
 var_stream(sym,streamflags)
 > sym: Variable (symbol)
 > streamflags: Set of operations that should work on the stream
 < result: Stream */
extern object var_stream (object sym, uintB streamflags);
/* is used by IO, PACKAGE, ERROR, DEBUG, SPVW */

/* UP: makes a file-stream
 make_file_stream(direction,append_flag,handle_fresh)
 > STACK_5: Filename, a Pathname or NIL
 > STACK_4: Truename, a Pathname or NIL
 > STACK_3: :BUFFERED argument
 > STACK_2: :EXTERNAL-FORMAT argument
 > STACK_1: :ELEMENT-TYPE argument
 > STACK_0: Handle of the open file
 > direction: Mode (0 = :PROBE, 1 = :INPUT, 4 = :OUTPUT, 5 = :IO,
                    3 = :INPUT-IMMUTABLE)
 > append_flag: true if the stream should immediately be positioned at the end
                ,else false
 > handle_fresh: whether the handle is freshly created.
                 This means 1. that it is currently positioned at position 0,
                 2. if (direction & bit(2)), it is opened for read/write, not
                 only for write.
                 If the handle refers to a regular file, this together means
                 that it supports file_lseek, reading/repositioning/writing
                 and close/reopen.
 If direction==5, handle_fresh must be true.
 < result: File-Stream (or evtl. File-Handle-Stream)
 < STACK: cleaned
 can trigger GC */
extern maygc object make_file_stream (direction_t direction, bool append_flag, bool handle_at_pos_0);
/* is used by PATHNAME */
%% exportF(object,make_file_stream,(direction_t direction, bool append_flag,bool handle_fresh)/*+6 arguments on the STACK!*/);

/* check whether the object is a handle stream or a socket-server
 and return its socket-like handle(s) */
extern void stream_handles (object obj, bool check_open, bool* char_p, SOCKET* in_sock, SOCKET* out_sock);
%% exportF(void,stream_handles,(object obj, bool check_open, bool* char_p, SOCKET* in_sock, SOCKET* out_sock));

#ifdef PIPES
/* mk_pipe_from_handle(pipe,process_id,dir)
 Make a PIPE-OUTPUT-STREAM from pipe handle and a process-id
 > STACK_0: buffered
 > STACK_1: element-type
 > STACK_2: encoding
 > pipe: input or output pipe, depending on direction
 > process_id: PID of the underlying process
 > direction: pipe stream direction
 < result - a PIPE-OUTPUT-STREAM
 Used in LAUNCH
 can trigger GC */
extern maygc object mk_pipe_from_handle (Handle opipe, int process_id, direction_t direction);
/* is used by PATHNAME */
#endif

/* Makes a Broadcast-Stream using a Stream stream.
 make_broadcast1_stream(stream)
 can trigger GC */
extern maygc object make_broadcast1_stream (object stream);
/* is used by IO */

/* Makes a Two-Way-stream using an Input-Stream and an Output-Stream.
 make_twoway_stream(input_stream,output_stream)
 > input_stream : Input-Stream
 > output_stream : Output-Stream
 < result : Two-Way-Stream
 can trigger GC */
extern maygc object make_twoway_stream (object input_stream, object output_stream);
/* is used by SPVW */

/* Makes a string-output-stream.
 make_string_output_stream()
 can trigger GC */
extern maygc object make_string_output_stream (void);
/* is used by IO, EVAL, DEBUG, ERROR */

/* UP: Returns the collected contents of a String-Output-Stream.
 get_output_stream_string(&stream)
 > stream: String-Output-Stream
 < stream: emptied Stream
 < result: the aggregation, a Simple-String
 can trigger GC */
extern maygc object get_output_stream_string (const gcv_object_t* stream_);
/* is used by IO, EVAL, DEBUG, ERROR */

/* UP: Makes a pretty-printer help stream
 make_pphelp_stream()
 can trigger GC */
extern maygc object make_pphelp_stream (void);
/* is used by IO */

/* UP: Tells whether a stream is buffered.
 stream_isbuffered(stream)
 > stream: a channel or socket stream
 < result: bit(1) set if input side is buffered,
           bit(0) set if output side is buffered */
extern uintB stream_isbuffered (object stream);
/* is used by IO */

/* UP: Returns the current line number of a stream.
 stream_line_number(stream)
 > stream: a stream
 < result: an integer or NIL
 can trigger GC */
extern maygc object stream_line_number (object stream);
/* is used by IO */

/* Function: Returns the last character read (and not yet unread) from a stream.
 stream_get_lastchar(stream)
 > stream: a stream
 < result: the last character read, or NIL
 can trigger GC */
extern maygc object stream_get_lastchar (object stream);
/* is used by DEBUG */

/* Function: Returns true if a stream is a FAS stream.
 stream_get_fasl(stream)
 > stream: a stream
 < result: true if the stream is a FAS stream, else false */
extern maygc bool stream_get_fasl (object stream);
/* used by IO */

/* Function: Changes the FAS state of a stream.
 stream_set_fasl(stream,value);
 > stream: a stream
 > value: true if the stream should be a FAS stream, else false */
extern maygc void stream_set_fasl (object stream, bool value);
/* used by */

#if defined(UNIX)
  /* UP: return terminal to normal mode
   terminal_sane(); */
  extern void terminal_sane (void);
  /* is used by SPVW */
#endif

#if defined(SCREEN)
/* UP: reset terminal size on SIGWINCH */
extern void resize_screen (int rows, int columns);
#endif

/* Function: test whether a stream is a terminal stream. */
extern bool terminal_stream_p(object stream);

/* check whether the charset is valid
 signal an error when code is invalid and charset is not nullobj
 return false otherwise */
extern bool check_charset (const char * code, object charset);
/* used in encoding.d */

/* ###################### SOCKBIBL for SOCKET.D ############################ */

#if defined(UNIX) || defined(WIN32_NATIVE)
/* Convert the IP address from C format to Lisp
 > af: address family (AF_INET..)
 > addr: whatever the address is for this address family
 < lisp string representing the address in a human-readable format
 for syscalls & rawsock modules
 can trigger GC */
extern maygc object addr_to_string (int af, const void *addr);
#endif
%% #if defined(UNIX) || defined(WIN32_NATIVE)
%%   exportF(object,addr_to_string,(int af, const void *addr));
%% #endif

#if (defined(UNIX) || defined(WIN32_NATIVE)) && defined(HAVE_GETHOSTBYNAME)
/* A wrapper around the connect() function.
 To be used inside begin/end_system_call() only. */
extern int nonintr_connect (SOCKET fd, const struct sockaddr * name, int namelen);
#endif

#if (defined(UNIX) || defined(WIN32_NATIVE)) && defined(HAVE_GETHOSTBYNAME) && defined(TCPCONN)
/* Convert the IP address from C format to Lisp
 > name: FQDN or dotted quad or IPv6 address
 < lisp string for FQDN or byte vector for IPv[46] numerics
 for syscalls & rawsock modules
 can trigger GC */
extern maygc object string_to_addr (const char* name);
#endif
%% #if (defined(UNIX) || defined(WIN32_NATIVE)) && defined(HAVE_GETHOSTBYNAME) && defined(TCPCONN)
%%   exportF(object,string_to_addr,(const char *name));
%% #endif

#if (defined(UNIX) || defined(WIN32_NATIVE)) && defined(HAVE_GETHOSTBYNAME) && defined(TCPCONN)
/* Return the hostent specified by the host designator
 > arg: host name designator:
        :DEFAULT - current host
        string/symbol: FQDN is resolved (gethostbyname)
        uint32: raw IPv4 address (gethostbyaddr)
        uint128: raw IPv6 address (gethostbyaddr)
        bit vector: raw IPv4[46] address (gethostbyaddr)
 < static hostent descriptor from LIBC
 for syscalls & rawsock modules */
extern struct hostent* resolve_host (object arg);
#endif
%% #if (defined(UNIX) || defined(WIN32_NATIVE)) && defined(HAVE_GETHOSTBYNAME) && defined(TCPCONN)
%%   exportF(struct hostent*,resolve_host,(object arg));
%% #endif

#if (defined(UNIX) || defined(WIN32_NATIVE)) && defined(HAVE_GETHOSTBYNAME)
/* connect_to_x_server(host,display)
 Attempts to connect to server, given host name and display number.
 Returns file descriptor (network socket). Returns -1 and sets errno
 if connection fails.
 An empty hostname is interpreted as the most efficient local connection to
 a server on the same machine (usually a UNIX domain socket).
 hostname="unix" is interpreted as a UNIX domain connection.
 To be used inside begin/end_system_call() only. */
extern SOCKET connect_to_x_server (const char* host, int display);
#endif

#if (defined(UNIX) || defined(WIN32_NATIVE)) && defined(HAVE_GETHOSTBYNAME) && defined(SOCKET_STREAMS)
/* socket_getlocalname(socket_handle,hd)
 Returns the IP name of the localhost for the given socket,
 or NULL in case of error.
 Fills all of *hd.
 To be used inside begin/end_system_call() only. */
extern host_data_t * socket_getlocalname (SOCKET socket_handle, host_data_t * hd, bool resolve_p);
#endif

#if (defined(UNIX) || defined(WIN32_NATIVE)) && defined(HAVE_GETHOSTBYNAME) && defined(SOCKET_STREAMS)
/* socket_getpeername(socket_handle,hd)
 Returns the name of the host to which IP socket fd is connected,
 or NULL in case of error.
 Fills all of *hd.
 To be used inside begin/end_system_call() only. */
extern host_data_t * socket_getpeername (SOCKET socket_handle, host_data_t * hd, bool resolve_p);
#endif

#if (defined(UNIX) || defined(WIN32_NATIVE)) && defined(HAVE_GETHOSTBYNAME) && defined(SOCKET_STREAMS)
/* Creates a socket to which other processes can connect. */
extern SOCKET create_server_socket_by_socket (host_data_t *hd, SOCKET sock,
                                              unsigned int port, int backlog);

extern SOCKET create_server_socket_by_string (host_data_t *hd,
                                              const char *ip_interface,
                                              unsigned int port, int backlog);
#endif

#if (defined(UNIX) || defined(WIN32_NATIVE)) && defined(HAVE_GETHOSTBYNAME) && defined(SOCKET_STREAMS)
/* Waits for a connection from another process. */
extern SOCKET accept_connection (SOCKET socket_handle);
#endif

#if (defined(UNIX) || defined(WIN32_NATIVE)) && defined(HAVE_GETHOSTBYNAME) && defined(SOCKET_STREAMS)
/* Creates a connection to a server (which must be waiting
   on the specified host and port).
   timeout may be NULL or a 'struct timeval *'. In the latter case,
   its value may be modified upon return, to indicate the time not slept. */
extern SOCKET create_client_socket (const char* hostname, unsigned int port, struct timeval* timeout);
#endif

/* ####################### SYMBIBL for SYMBOL.D ############################ */

/* UP: Returns the gobal definition of a symbol's function,
 and tests, whether the symbol is a global function.
 Symbol_function_checked(symbol)
 > symbol: symbol
 < result: the global definition of the function */
extern object Symbol_function_checked (object symbol);
/* is used by */

/* UP: gets a property from a symbol's property list.
 get(symbol,key)
 > symbol: a symbold
 > key: a key that is comparable with EQ
 < value: corresponding value from the property list of 'symbol', or unbound. */
extern object get (object symbol, object key);
/* is used by IO, CONTROL, EVAL, PREDTYPE, SEQUENCE */
%% #if notused
%% exportF(object,get,(object symbol, object key));
%% #endif

/* ##################### ARITBIBL for LISTARIT.D ########################### */

/* check_real(obj)
 > obj: an object
 < result: a real number, either the same as obj or a replacement
 can trigger GC */
extern maygc object check_real_replacement (object obj);
#ifndef COMPILE_STANDALONE
static inline maygc object check_real (object obj) {
  if_realp(obj, ; , { obj = check_real_replacement(obj); });
  return obj;
}
#endif
/* used by IO */

/* UP: Initializes the arithmetics.
 init_arith();
 can trigger GC */
extern maygc void init_arith (void);
/* is used by SPVW */

/* Converts a longword into an Integer.
 L_to_I(val)
 > val: value of the Integer, a signed 32-Bit-Integer.
 < result: Integer with that value.
 can trigger GC */
extern maygc object L_to_I (sint32 val);
/* used by TIME */
%% exportF(object,L_to_I,(sint32 val));

/* Converts an unsigned longword into an Integer >=0.
 UL_to_I(val)
 > val: value of the Integer, an unsigned 32-bit-Integer.
 < result: Integer with that value.
 can trigger GC */
#if (intLsize<=oint_data_len)
  #ifdef DEBUG_GCSAFETY
    static inline maygc object UL_to_I (uintL val) { return fixnum(val); }
  #else
    #define UL_to_I(val)  fixnum((uintL)(val))
  #endif
#else
  extern maygc object UL_to_I (uintL val);
#endif
/* is used by MISC, TIME, STREAM, PATHNAME, HASHTABL, SPVW, ARRAY */
%% #if (intLsize<=oint_data_len)
%%   #ifdef DEBUG_GCSAFETY
%%    puts("static inline object UL_to_I (uintL val) { return fixnum(val); }");
%%   #else
%%     export_def(UL_to_I(val));
%%   #endif
%% #else
%%   exportF(object,UL_to_I,(uintL val));
%% #endif

/* converts a double-longword into an Integer.
 L2_to_I(val_hi,val_lo)
 > val_hi|val_lo: value of the Integer, an signed 64-bit-Integer.
 < result: Integer with that value.
 can trigger GC */
#if (intVsize>32)
  #define L2_to_I(val_hi,val_lo)  \
    Q_to_I(((sint64)(sint32)(val_hi)<<32)|(sint64)(uint32)(val_lo))
#else
  extern maygc object L2_to_I (sint32 val_hi, uint32 val_lo);
#endif
/* is used by TIME, FOREIGN */
%% #if (intVsize>32)
%%   export_def(L2_to_I(val_hi,val_lo));
%% #else
%%   exportF(object,L2_to_I,(sint32 val_hi, uint32 val_lo));
%% #endif

/* Converts an unsigned double-longword into an Integer.
 UL2_to_I(val_hi,val_lo)
 > val_hi|val_lo: value of the Integer, an unsigned 64-bit-Integer.
 < result: Integer with that value.
 can trigger GC */
#if (intVsize>32)
  #define UL2_to_I(val_hi,val_lo)  \
    UQ_to_I(((uint64)(uint32)(val_hi)<<32)|(uint64)(uint32)(val_lo))
#else
  extern maygc object UL2_to_I (uint32 val_hi, uint32 val_lo);
#endif
/* is used by TIME, FOREIGN, and by the FFI */
%% #if (intVsize>32)
%%   export_def(UL2_to_I(val_hi,val_lo));
%% #else
%%   exportF(object,UL2_to_I,(uint32 val_hi, uint32 val_lo));
%% #endif

#if defined(intQsize) || (intVsize>32)
  /* Converts a quadword into an Integer.
   Q_to_I(val)
   > val: value of the Integer, a signed 64-bit-Integer.
   < result: Integer with that value
   can trigger GC */
  extern maygc object Q_to_I (sint64 val);
  /* is used by the FFI */
#endif
%% #if defined(intQsize) || (intVsize>32)
%%   exportF(object,Q_to_I,(sint64 val));
%% #endif

#if defined(intQsize) || (intVsize>32) || defined(WIDE_HARD) || (SIZEOF_OFF_T > 4) || (SIZEOF_INO_T > 4)
  /* Converts an unsigned quadword into an Integer >=0.
   UQ_to_I(val)
   > val: value of the Integer, an unsigned 64-bit-Integer.
   < result: Integer with that value
   can trigger GC */
  extern maygc object UQ_to_I (uint64 val);
  /* is used by MISC, TIME, FFI */
#endif
%% #if defined(intQsize) || (intVsize>32)
%%   exportF(object,UQ_to_I,(uint64 val));
%% #endif

/* Converts a sintV into an Integer.
 V_to_I(val)
 > val: value of the Integer, a signed intVsize-bit-Integer.
 < result: Integer with that value
 can trigger GC
 extern maygc object V_to_I (uintV val); */
#if (intVsize<=32)
  #define V_to_I(val)  L_to_I(val)
#else
  #define V_to_I(val)  Q_to_I(val)
#endif
/* is used by LISPARIT */
%% #if notused
%% #if (intVsize<=32)
%%   emit_define("V_to_I(val)","L_to_I(val)");
%% #else
%%   emit_define("V_to_I(val)","Q_to_I(val)");
%% #endif
%% #endif

/* Converts an uintV into an Integer >=0.
 UV_to_I(val)
 > val: value of the Integer, an unsigned intVsize-bit-Integer.
 < result: Integer with that value
 can trigger GC
 extern maygc object UV_to_I (uintV val); */
#if (intVsize<=32)
  #define UV_to_I(val)  UL_to_I(val)
#else
  #define UV_to_I(val)  UQ_to_I(val)
#endif
/* is used by LISPARIT */
%% #if notused
%% #if (intVsize<=32)
%%   emit_define("UV_to_I(val)","UL_to_I(val)");
%% #else
%%   emit_define("UV_to_I(val)","UQ_to_I(val)");
%% #endif
%% #endif

/* Converts a C-Integer of a given type into an Integer
 val should be a variable */
#define uint8_to_I(val)  fixnum((uint8)(val))
#define sint8_to_I(val)  L_to_I((sint32)(sint8)(val))
#define uint16_to_I(val)  fixnum((uint16)(val))
#define sint16_to_I(val)  L_to_I((sint32)(sint16)(val))
#define uint32_to_I(val)  UL_to_I((uint32)(val))
#define sint32_to_I(val)  L_to_I((sint32)(val))
#if defined(intQsize) || (intVsize>32)
  #define uint64_to_I(val)  UQ_to_I((uint64)(val))
  #define sint64_to_I(val)  Q_to_I((sint64)(val))
#else
  #define uint64_to_I(val)  UL2_to_I((uint32)((val)>>32),(uint32)(val))
  #define sint64_to_I(val)  L2_to_I((sint32)((val)>>32),(uint32)(val))
#endif
#if (int_bitsize==16)
  #define uint_to_I(val)  uint16_to_I(val)
  #define sint_to_I(val)  sint16_to_I(val)
#else /* (int_bitsize==32) */
  #define uint_to_I(val)  uint32_to_I(val)
  #define sint_to_I(val)  sint32_to_I(val)
#endif
#if (long_bitsize==32)
  #define ulong_to_I(val)  uint32_to_I(val)
  #define slong_to_I(val)  sint32_to_I(val)
#else /* (long_bitsize==64) */
  #define ulong_to_I(val)  uint64_to_I(val)
  #define slong_to_I(val)  sint64_to_I(val)
#endif
/* is used by MISC, for FFI */
%% export_def(uint8_to_I(val));
%% export_def(sint8_to_I(val));
%% export_def(uint16_to_I(val));
%% export_def(sint16_to_I(val));
%% export_def(uint32_to_I(val));
%% export_def(sint32_to_I(val));
%% export_def(uint64_to_I(val));
%% export_def(sint64_to_I(val));
%% export_def(uint_to_I(val));
%% export_def(sint_to_I(val));
%% export_def(ulong_to_I(val));
%% export_def(slong_to_I(val));

/* Converts a uintM integer into an Integer. */
#if intMsize <= intLsize
  #define uintM_to_I(val)  UL_to_I(val)
#else
  #define uintM_to_I(val)  UQ_to_I(val)
#endif

/* Converts a sintM integer into an Integer. */
#if intMsize <= intLsize
  #define sintM_to_I(val)  L_to_I(val)
#else
  #define sintM_to_I(val)  Q_to_I(val)
#endif

/* converts off_t to an Integer */
#if defined(WIN32_NATIVE)
  #define off_to_I(val)  L2_to_I((sint32)((val)>>32),(uint32)(val))
#elif SIZEOF_OFF_T > 4
  #define off_to_I  sint64_to_I
#else
  #define off_to_I  sint32_to_I
#endif

/* Converts an Integer >=0 into an unsigned longword.
 I_to_UL(obj)
 > obj: an object, should be an Integer >=0, <2^32
 < result: the Integer's value as unsigned longword */
extern uintL I_to_UL (object obj);
/* is used by TIME, ARRAY */
%% exportF(uintL,I_to_UL,(object obj));

/* Converts an Integer into a signed longword
 I_to_L(obj)
 > obj: an object, should be an Integer >=-2^31, <2^31
 < result: the Integer's value as signed longword */
extern sintL I_to_L (object obj);
/* is used by */
%% exportF(sintL,I_to_L,(object obj));

#if defined(HAVE_LONG_LONG_INT)
  /* Converts an Integer >=0 into an unsigned quadword.
   I_to_UQ(obj)
   > obj: an object, should be an Integer >=0, <2^64
   < result: the Integer's vaulue as unsigned quadword */
  extern uint64 I_to_UQ (object obj);
  /* used by FOREIGN, for FFI, and by modules */
#endif
%% #ifdef HAVE_LONG_LONG_INT
%%   exportF(uint64,I_to_UQ,(object obj));
%% #endif

#if defined(HAVE_LONG_LONG_INT)
  /* Converts an Integer into a signed quadword.
   I_to_Q(obj)
   > obj: an object, should be an Integer >=-2^63, <2^63
   < result: the Integer's value as quadword. */
  extern sint64 I_to_Q (object obj);
  /* used by FOREIGN, for FFI, and by modules */
#endif
%% #ifdef HAVE_LONG_LONG_INT
%%   exportF(sint64,I_to_Q,(object obj));
%% #endif

/* Converts an Integer into a C-Integer of a given type.
 I_to_xintyy(obj) assumes that xintyy_p(obj) has already been checked. */
#define I_to_uint8(obj)  (uint8)(as_oint(obj) >> oint_data_shift)
#define I_to_sint8(obj)  (sint8)(as_oint(obj) >> oint_data_shift)
#define I_to_uint16(obj)  (uint16)(as_oint(obj) >> oint_data_shift)
#define I_to_sint16(obj)  (sint16)(as_oint(obj) >> oint_data_shift)
#if (oint_data_len>=32)
  #define I_to_uint32(obj)  (uint32)(as_oint(obj) >> oint_data_shift)
#else
  #define I_to_uint32(obj)  I_to_UL(obj)
#endif
#if (oint_data_len>=31)
  #define I_to_sint32(obj)  (sint32)(as_oint(obj) >> oint_data_shift)
#else
  #define I_to_sint32(obj)  I_to_L(obj)
#endif
#ifdef HAVE_LONG_LONG_INT
  #define I_to_uint64(obj)  I_to_UQ(obj)
  #define I_to_sint64(obj)  I_to_Q(obj)
#endif
#if (int_bitsize==16)
  #define I_to_uint  I_to_uint16
  #define I_to_sint  I_to_sint16
#else /* (int_bitsize==32) */
  #define I_to_uint  I_to_uint32
  #define I_to_sint  I_to_sint32
#endif
/* always: long_bitsize > oint_data_len ==> I_to_Xlong checks its argument */
#if (long_bitsize==32)
  #define I_to_ulong  I_to_uint32
  #define I_to_slong  I_to_sint32
#else /* (long_bitsize==64) */
  #define I_to_ulong  I_to_uint64
  #define I_to_slong  I_to_sint64
#endif
/* used by FFI, STREAM, modules */
%% export_def(I_to_uint8(obj));
%% export_def(I_to_sint8(obj));
%% export_def(I_to_uint16(obj));
%% export_def(I_to_sint16(obj));
%% export_def(I_to_uint32(obj));
%% export_def(I_to_sint32(obj));
%% export_def(I_to_uint64(obj));
%% export_def(I_to_sint64(obj));
%% export_def(I_to_uint);
%% export_def(I_to_sint);
%% export_def(I_to_ulong);
%% export_def(I_to_slong);

/* Unsigned Digit Sequence to Integer
 UDS_to_I(MSDptr,len)
 convert UDS MSDptr/len/.. into Integer >=0 .
 MSDptr[0] is the most significant digit, MSDptr[len-1] the least significant.
 there must be room for 1 digit below of MSDptr.
 can trigger GC */
extern maygc object UDS_to_I (uintD* MSDptr, uintC len);
/* is used by modules */
%% exportF(object,UDS_to_I,(uintD* MSDptr, uintC len));

/* Digit Sequence to Integer
 DS_to_I(MSDptr,len)
 convert DS MSDptr/len/.. into Integer.
 MSDptr[0] is the most significant digit, MSDptr[len-1] the least significant.
 can trigger GC */
extern maygc object DS_to_I (const uintD* MSDptr, uintC len);
/* is used by modules */
%% exportF(object,DS_to_I,(const uintD* MSDptr, uintC len));

/* I_I_comp(x,y) compares two Integers x and y.
 Result: 0 if x=y, +1 if x>y, -1 if x<y. */
extern signean I_I_comp (object x, object y);
/* is used by SEQUENCE */

/* (1+ x), where x is an Integer. Result Integer.
 I_1_plus_I(x)
 can trigger GC */
extern maygc object I_1_plus_I (object x);
/* is used by SEQUENCE, SPVW, SYMBOL */
%% #if notused
%% exportF(object,I_1_plus_I,(object x));
%% #endif

/* (1- x), where x is an Integer. Result Integer.
 I_minus1_plus_I(x)
 can trigger GC */
extern maygc object I_minus1_plus_I (object x);
/* is used by SEQUENCE */
%% #if notused
%% exportF(object,I_minus1_plus_I,(object x));
%% #endif

/* (+ x y), where x and y are Integers. Result Integer.
 I_I_plus_I(x,y)
 can trigger GC */
extern maygc object I_I_plus_I (object x, object y);
/* is used by SEQUENCE */
%% #if notused
%% exportF(object,I_I_plus_I,(object x, object y));
%% #endif

/* (- x y), where x and y are Integers. Result Integer.
 I_I_minus_I(x,y)
 can trigger GC */
extern maygc object I_I_minus_I (object x, object y);
/* is used by SEQUENCE */
%% #if notused
%% exportF(object,I_I_minus_I,(object x, object y));
%% #endif

/* (ASH x y), where x and y are Integers. Result Integer.
 I_I_ash_I(x,y)
 can trigger GC */
extern maygc object I_I_ash_I (object x, object y);
/* is used by SEQUENCE */

/* (INTEGER-LENGTH x), where x is an Integer. Result uintL.
 I_integer_length(x) */
extern uintL I_integer_length (object x);
/* is used by ARRAY */
%% exportF(uintL,I_integer_length,(object x));

/* Converts a little-endian byte sequence to an unsigned integer.
 > bytesize: number of given 8-bit bytes of the integer,
             < intDsize/8*uintWC_max
 > bufferptr: address of bytesize bytes in GC-invariant memory
 < result: an integer >= 0 with I_integer_length(result) <= 8*bytesize
 can trigger GC */
extern maygc object LEbytes_to_UI (uintL bytesize, const uintB* bufferptr);
%% exportF(object,LEbytes_to_UI,(uintL bytesize, const uintB* bufferptr));

/* Converts a little-endian byte sequence to an unsigned integer.
 > bytesize: number of given 8-bit bytes of the integer,
             < intDsize/8*uintWC_max
 > *buffer_: address of a simple-8bit-vector (or of a fake)
             containing bytesize bytes of memory
 < result: an integer >= 0 with I_integer_length(result) <= 8*bytesize
 can trigger GC */
extern maygc object LESbvector_to_UI (uintL bytesize, const gcv_object_t* buffer_);
/* is used by STREAM */

/* Converts a little-endian byte sequence to an integer.
 > bytesize: number of given 8-bit bytes of the integer, > 0,
             < intDsize/8*uintWC_max
 > bufferptr: address of bytesize bytes in GC-invariant memory
 < result: an integer with I_integer_length(result) < 8*bytesize
 can trigger GC */
extern maygc object LEbytes_to_I (uintL bytesize, const uintB* bufferptr);
%% exportF(object,LEbytes_to_I,(uintL bytesize, const uintB* bufferptr));

/* Converts a little-endian byte sequence to an integer.
 > bytesize: number of given 8-bit bytes of the integer, > 0,
             < intDsize/8*uintWC_max
 > *buffer_: address of a simple-8bit-vector (or of a fake)
             containing bytesize bytes of memory
 < result: an integer with I_integer_length(result) < 8*bytesize
 can trigger GC */
extern maygc object LESbvector_to_I (uintL bytesize, const gcv_object_t* buffer_);
/* is used by STREAM */

/* Converts an unsigned integer to a little-endian byte sequence.
 > obj: an integer
 > bitsize: maximum number of bits of the integer
 > bufferptr: pointer to bytesize = ceiling(bitsize,8) bytes of memory
 < false and bufferptr[0..bytesize-1] filled, if obj >= 0 and
                                              I_integer_length(obj) <= bitsize;
   true, if obj is out of range */
extern bool UI_to_LEbytes (object obj, uintL bitsize, uintB* bufferptr);
/* is used by STREAM */
%% exportF(bool,UI_to_LEbytes,(object obj, uintL bitsize, uintB* bufferptr));

/* Converts an integer to a little-endian byte sequence.
 > obj: an integer
 > bitsize: maximum number of bits of the integer, including the sign bit
 > bufferptr: pointer to bytesize = ceiling(bitsize,8) bytes of memory
 < false and bufferptr[0..bytesize-1] filled, if I_integer_length(obj) < bitsize;
   true, if obj is out of range */
extern bool I_to_LEbytes (object obj, uintL bitsize, uintB* bufferptr);
/* is used by STREAM */
%% exportF(bool,I_to_LEbytes,(object obj, uintL bitsize, uintB* bufferptr));

/* c_float_to_FF(&val) converts an IEEE-single-float val into an single-float.
 can trigger GC */
extern maygc object c_float_to_FF (const ffloatjanus* val_);
%% exportF(object,c_float_to_FF,(const ffloatjanus* val_));

/* FF_to_c_float(obj,&val);
 converts single-float obj into an IEEE-single-float val. */
extern void FF_to_c_float (object obj, ffloatjanus* val_);
%% exportF(void,FF_to_c_float,(object obj, ffloatjanus* val_));

/* c_double_to_DF(&val) converts an IEEE-double-float val into a double-float.
 can trigger GC */
extern maygc object c_double_to_DF (const dfloatjanus* val_);
%% exportF(object,c_double_to_DF,(const dfloatjanus* val_));

/* DF_to_c_double(obj,&val);
 converts a double-float obj into an IEEE-double-float val. */
extern void DF_to_c_double (object obj, dfloatjanus* val_);
%% exportF(void,DF_to_c_double,(object obj, dfloatjanus* val_));

/* hash-code of a Long-Float: mixture of exponent, length, first 32 bits */
extern uint32 hashcode_lfloat (object obj);

/* (complex x (float 0 x)) */
extern object F_complex_C (object x);

/* UP: turns a string with Integer syntax into an Integer number
 Points will be ignored
 read_integer(base,sign,string,index1,index2)
 > base: read base(>=2, <=36)
 > sign: sign (/=0 if negative)
 > string: simple-string (contains digits with values
   <base and eventually a point)
 > index1: Index of the first digit
 > index2: Index after the last digit
   (thus index2-index1 digits, incl. a decimal point that can be at the end)
 < result: Integer
 can trigger GC */
extern maygc object read_integer (uintWL base, signean sign, object string, uintL index1, uintL index2);
/* is used by IO */

/* UP: turns a string with rational syntax into a rational number
 read_rational(base,sign,string,index1,index3,index2)
 > base: read base (>=2, <=36)
 > sign: sign (/=0 if negative)
 > string: Normal-Simple-String (contains digits with values
           <base and fraction bar)
 > index1: Index of the first digit
 > index3: Index of '/'
 > index2: Index after the last digit
   (thus index3-index1 digits of the numerator,
    index2-index3-1 digits of the denominator)
 < result: rational number
 can trigger GC */
extern maygc object read_rational (uintWL base, signean sign, object string, uintL index1, uintL index3, uintL index2);
/* is used by IO */

/* UP: turns a string with float-syntax into a float
 read_float(base,sign,string,index1,index4,index2,index3)
 > base: read base (=10)
 > sign: Sign (/=0 if negative)
 > string: normal-simple-string (contains digits and eventually
           point and exponent marker)
 > index1: Index of the beginning of the mantissa (without sign)
 > index4: Index after the end of the mantissa
 > index2: Index at the end of the character
 > index3: Index after the decimal point (=index4 if there is none)
   (thus mantissa with index4-index1 characters: digits and max. 1 '.')
   (thus index4-index3 digits after the decimal point)
   (thus at index4<index2: index4 = index of the exponent marker,
    index4+1 = index of the exponent's sign or the first digit
    of the exponent)
 < result: Float
 can trigger GC */
extern maygc object read_float (uintWL base, signean sign, object string, uintL index1, uintL index4, uintL index2, uintL index3);
/* is used by IO */

/* UP: prints an Integer
 print_integer(z,base,&stream);
 > z: Integer
 > base: base (>=2, <=36)
 > stream: Stream
 < stream: Stream
 can trigger GC */
extern maygc void print_integer (object z, uintWL base, const gcv_object_t* stream_);
/* is used by IO */

/* UP: prints a float
 print_float(z,&stream);
 > z: Float
 > stream: Stream
 < stream: Stream
 can trigger GC */
extern maygc void print_float (object z, const gcv_object_t* stream_);
/* is used by IO */

/* UP: Multiply an Integer by 10 and add another digit
 mult_10_plus_x(y,x)
 > y: Integer Y (>=0)
 > x: digit value X (>=0,<10)
 < result: Integer Y*10+X (>=0)
 can trigger GC */
extern maygc object mult_10_plus_x (object y, uintB x);
/* is used by IO */

/* UP: decides whether two numbers are equal
 number_equal(x,y)
 > x,y: two numbers
 < result: true, if (= x y) holds */
extern bool number_equal (object x, object y);
/* is used by PREDTYPE */

/* UP: Converts an object into a float of a given type
 coerce_float(obj,type)
 > obj: Object
 > type: one of the symbols
         FLOAT, SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT
 < result: (coerce obj type)
 can trigger GC */
extern maygc object coerce_float (object obj, object type);
/* is used by PREDTYPE, FOREIGN */

/* Converts a function's argument to a C 'double'.
 to_double(obj)
 > obj: an object, usually a real number
 < result: its value as a C 'double'
 can trigger GC */
extern maygc double to_double (object x);
%% exportF(double,to_double,(object obj));

/* Converts a function's argument to a C 'int'.
 to_int(obj)
 > obj: an object, usually an integer
 < result: its value as a C 'int'
 can trigger GC */
extern maygc int to_int (object x);
%% exportF(int,to_int,(object obj));

/* UP: Returns the decimal string representation of an integer >= 0.
 decimal_string(x)
 > object x: an integer >= 0
 < object result: a normal-simple-string containing the digits
 can trigger GC */
extern maygc object decimal_string (object x);
/* is used by PATHNAME */

/* ###################### FOREIGNBIBL for FOREIGN.D ######################### */

#ifdef DYNAMIC_FFI
%% #ifdef DYNAMIC_FFI
%%  puts("#define HAVE_FFI");

/* Return the pointer encoded by a Foreign-Pointer. */
  #define Fpointer_value(obj) TheFpointer(obj)->fp_pointer

/* Return the pointer encoded by a Foreign-Address. obj a variable */
  #define Faddress_value(obj)  \
   ((void*)((uintP)Fpointer_value(TheFaddress(obj)->fa_base) + TheFaddress(obj)->fa_offset))

/* Allocate a foreign address.
 make_faddress(base,offset)
 > base: base address
 > offset: offset relative to the base address
 < result: Lisp object
 can trigger GC */
extern maygc object make_faddress (object base, uintP offset);
/* used by FOREIGN & modules (see foreign1.lisp:convert-from-foreign) */
%%   exportF(object,make_faddress,(object base, uintP offset));

/* ensure that the Faddress is valid
 < fa: foreign address (not checked!)
 can trigger GC */
extern maygc object check_faddress_valid (object fa);
/* usd by FOREIGN, MISC */

/* Registers a foreign variable.
 register_foreign_variable(address,name,flags,size);
 > address: address of a variable in memory
 > name: its name
 > flags: fv_readonly for read-only variables
 > size: its size in bytes
 can trigger GC */
  extern maygc void register_foreign_variable (void* address, const char * name, uintBWL flags, uintL size);
/* Specifies that the variable will not be written to. */
#define fv_readonly  bit(0)
/* Specifies that when the value is replaced and the variable contains pointers,
 the old storage will be free()d and new storage will be allocated via malloc(). */
#define fv_malloc    bit(1)
%%   exportF(void,register_foreign_variable,(void* address, const char * name, uintBWL flags, uintL size));

/* Registers a foreign function.
 register_foreign_function(address,name,flags);
 > address: address of the function in memory
 > name: its name
 > flags: its language and parameter passing convention
 can trigger GC */
  extern maygc void register_foreign_function (void* address, const char * name, uintWL flags);
/* Flags for language: */
#define ff_lang_asm       bit(8)  /* no argument passing conventions */
#define ff_lang_c         bit(9)  /* K&R C, with argument type promotions */
#define ff_lang_ansi_c    bit(10) /* ANSI C, without argument type promotions */
/* define ff_lang_pascal   bit(11) # not yet supported */
#define ff_lang_stdcall   bit(15) /* `stdcall' calling convention */
/* Varargs functions are not supported.
 Set this if pointers within the arg should point to alloca()ed data, i.e.
 have dynamic extent: are valid for this call only. */
#define ff_alloca         bit(0)
/* Set this if pointers within the arg should point to malloc()ed data. The
 function takes over responsibility for that storage. For return values,
 set this if free() shall be called for pointers within the resulting value. */
#define ff_malloc         bit(1)
/* Set this if the arg should point to a place where a return value can be
 stored. */
#define ff_out            bit(4)
/* Set this if the arg is also treated as a return value. */
#define ff_inout          bit(5)
%%   exportF(void,register_foreign_function,(void* address, const char * name, uintWL flags));

/* Registers a foreign int type.
 register_foreign_inttype (const char * name_asciz, uintL size, bool signed_p)
 > name_asciz: C type name
 > size : sizeof(name_asciz)
 > signed_p : signed?
 can trigger GC */
extern maygc void register_foreign_inttype (const char * name_asciz, uintL size, bool signed_p);
%%   exportF(void,register_foreign_inttype,(const char * name_asciz, uintL size, bool signed_p));

/* Convert foreign data to Lisp data.
 can trigger GC */
extern maygc object convert_from_foreign (object fvd, const void* data);
%% exportF(object,convert_from_foreign,(object fvd, const void* data));

/* Convert Lisp data to foreign data. */
typedef void* converter_malloc_t (void* old_data, uintL size, uintL alignment, void** state);
global converter_malloc_t mallocing, nomalloc;
%% puts("typedef void* converter_malloc_t (void* old_data, uintL size, uintL alignment, void** state);");
%% exportV(converter_malloc_t,nomalloc);
%% exportV(converter_malloc_t,mallocing);
/* Convert Lisp data to foreign data.
   Storage is allocated through converter_malloc().
 Only the toplevel storage must already exist; its address is given.
 can trigger GC */
extern void convert_to_foreign (object fvd, object obj, void* data, converter_malloc_t *converter_malloc, void** state);
%% exportF(void,convert_to_foreign,(object fvd, object obj, void* data, converter_malloc_t *converter_malloc, void** state));

/* Initialize the FFI. */
  extern maygc void init_ffi (void);
/* used by SPVW */

/* De-Initialize the FFI. */
  extern void exit_ffi (void);
/* used by SPVW */

#endif
%% #endif

/* ######################## THREADBIBL for THREAD.D ######################## */

#ifdef MULTITHREAD
%% #ifdef MULTITHREAD

/* thread-local object table */
struct object_tab_tl_ {
#define LISPOBJ_TL(name,initstring)  gcv_object_t name;
  #include "constobj_tl.c"
 #undef LISPOBJ_TL
};

  /* every thread keeps chain of pinned objects.
   usually there will be just a single one (if any), but it is
   possible with signal handlers to have real chain.*/
  typedef struct pinned_chain_t {
    gcv_object_t pc_varobject; /* GC invariant - GC will mark it */
    gcv_object_t *pc_unwind_stack_ptr; /* pointer above the STACK when object
                                          was pinned */
    struct pinned_chain_t *pc_next;
  } pinned_chain_t;

  /* Structure containing all the per-thread global variables.*/
  typedef struct clisp_thread_t {
    /* Most often used (also used by modules - so should be exported) : */
    gcv_object_t* _STACK;
    uintC _mv_count;
    p_backtrace_t _back_trace;
    struct object_tab_tl_ _object_tab;
   #ifdef DEBUG_GCSAFETY
    uintL _alloccount; /* alloccount for this thread */
   #endif
   #if defined(DEBUG_SPVW) && (defined(CAN_ALLOCATE_8BIT_VECTORS_ON_C_STACK) || defined(CAN_ALLOCATE_STRINGS_ON_C_STACK))
    /* in debug builds that allocate lisp objects on C stack we want to assert
       from GC if there is something wrong. So before going in suspend
       state for GC - the thread will save here the current stack pointer.*/
    void *_SP_before_suspend;
   #endif
    /* GC suspend/resume machinery */
    spinlock_t _gc_suspend_request; /*always signalled unless there is a suspend request. */
    spinlock_t _gc_suspend_ack; /* always signalled unless it can be assumed the thread is suspended */
    xmutex_raw_t _gc_suspend_lock; /* the mutex on which the thread waits. */
    uintC _suspend_count; /* how many times this thread has been suspended ? */
    /* The values of per-thread symbols: */
    gcv_object_t *_ptr_symvalues; /* allocated separately */
   #if (int_bitsize < long_bitsize)
    long _jmpl_value;
   #endif
   #if defined(HAVE_SIGNALS) && defined(SIGPIPE)
    /* Set ONLY during IO calls to pipes directed to subprocesses. */
    bool _writing_to_subprocess;
   #endif
    /* is the thread waiting to be resumed (on _gc_suspend_lock) */
    xmutex_raw_t *_raw_wait_mutex;
    /* count of pending interrupts */
    volatile uintC _pending_interrupts;
    /* chain of pinned objects for this thread */
    pinned_chain_t * _pinned;
    object _mv_space [mv_limit-1];
    /* everything till here is exported to modules */
    /* The lexical environment: */
    gcv_environment_t _aktenv;
    /* Used for exception handling only: */
    handler_args_t _handler_args;
    stack_range_t* _inactive_handlers;
    unwind_protect_caller_t _unwind_protect_to_save;
   #ifndef NO_SP_CHECK
    void* _SP_bound;
   #endif
    void* _SP_anchor;
    gcv_object_t* _STACK_bound;
    gcv_object_t* _STACK_start;
    bool _running_handle_directory_encoding_error; /* used in pathname.d */
    bool _running_handle_close_errors;             /* used in stream.d */
    /* do not rely on SA_NODEFER for signal nesting */
    spinlock_t _signal_reenter_ok;
    /* Following are related to thread interruption  */
    /* condvar on which thread waits currently (in GC_SAFE way) */
    xcondition_t *_wait_condition;
    /* mutex on which thread waits currently (in GC_SAFE way) */
    xmutex_t *_wait_mutex;
    bool _own_stack; /* who owns our lisp stack. should it be freed? */
    /* true when thread is in final cleanup and should not be interrupted */
    bool _thread_is_dying;
    /* the current thread. NOT GC VISIBLE. */
    gcv_object_t _lthread;
    /* real time when the thread started */
    internal_time_t thr_realstart_time;
    /* previous and next thread. all active threads are kept in double
       linked list*/
    struct clisp_thread_t *thr_prev;
    struct clisp_thread_t *thr_next;
  } clisp_thread_t;

  /* following macro is "called" before thread can be suspended in debug
     builds with possible object allocated on C stack */
  #if defined(DEBUG_SPVW) && (defined(CAN_ALLOCATE_8BIT_VECTORS_ON_C_STACK) || defined(CAN_ALLOCATE_STRINGS_ON_C_STACK))
    #define SET_SP_BEFORE_SUSPEND(thr)                 \
      do {                                             \
        var int dummy;                                 \
        thr->_SP_before_suspend = (void *)&dummy;      \
      } while(0)
  #else
    #define SET_SP_BEFORE_SUSPEND(thr)
  #endif

  #define GC_SAFE_SPINLOCK_ACQUIRE(s)                  \
    do {                                               \
      while (!spinlock_tryacquire(s)) {                \
        GC_SAFE_POINT_IF(GC_SAFE_ACK_SUSPEND_REQUEST_(),xthread_yield()); \
      }                                                \
    } while(0)

  /* helper macro for locking mutex that allows GC and thread interrupts while
     waiting. To be used only here and in zthread.d. In all other places
     WITH_OS_MUTEX_LOCK() should be used since it guarantees correct unlocking
     in case of non-local exit and thread interrupt
    > mutex: mutex to lock
    > locked: pointer to bool filled with true in case the lock
    has been acquired (before handling of pending interrupts) */
  #define GC_SAFE_MUTEX_LOCK(mutex,locked)                     \
    do {                                                       \
      xmutex_t *m=mutex; /* get pointer before we allow GC */  \
      current_thread()->_wait_mutex = m;                       \
      begin_blocking_system_call();                            \
      xmutex_lock(m);                                          \
      *locked = true;                                          \
      current_thread()->_wait_mutex=NULL;                      \
      end_blocking_system_call(); /* nb: pending interrupt are handled here */ \
    } while(0)

  /* unlocks mutex. preserves mv_space (i.e. does not allow gc or thread
     interruption)*/
  #define GC_SAFE_MUTEX_UNLOCK(m)           \
    do {                                    \
      begin_system_call();                  \
      xmutex_unlock(m);                     \
      end_system_call();                    \
    } while (0)

  /* try to use the compiler support for thread local storage */
  #if defined(__GNUC__)
    #if defined(UNIX_LINUX) || defined(UNIX_FREEBSD) || (defined(UNIX_MACOSX) && (defined(__clang__) || (((__GNUC__ == 4) && (__GNUC_MINOR__ >= 5)) || (__GNUC__ > 4))))
      #define per_thread __thread
    #endif
  #elif defined(__WIN32__) && defined (MICROSOFT)
    #define  per_thread __declspec(thread)
  #endif
  /* If asked for USE_CUSTOM_TLS - it overrides the compiler TLS
   support - if any. Warn the user. */
  #if defined(per_thread) && defined(USE_CUSTOM_TLS)
   #warning "USE_CUSTOM_TLS overrides the compiler per_thread support."
   #undef per_thread
  #endif

  #ifdef per_thread
    extern per_thread clisp_thread_t* _current_thread; /* current_thread pointer */
    #define current_thread() _current_thread
    #define set_current_thread(thread) _current_thread=thread
  #else
   /* We want MT, but our compiler does not provide built in support for TLS.
   USE_CUSTOM_TLS={1,2,3}
     1 - using xthread_key_get/set - slowest one (and probably safest)
     2 - using slightly modified version of TLS found in Boehm GC for C/C++.
     3 - using full page map of address space (4 MB).
     Basically it is trade-off between performance/memory usage:
     {1} - xthread_key_get/set is 200-500% slower then native compiler TLS.
     {2} is about 50-100% slower than compiler TLS support and uses
     just 8 KB.
     {3} is almost as fast as single threaded and compiler TLS but uses 4 MB.

     NB: {2} and {3} assume 32 bit address space and 4 KB page size (anything other
     will cause problems).
   */

   /* If there is no prefered way to perform TLS - fall back to the slowest
      one (and probably safest).*/
   #if !defined(USE_CUSTOM_TLS)
    #define USE_CUSTOM_TLS 1
   #endif

   /* custom TLS == 3 is available only on 32 bit platforms */
   #if USE_CUSTOM_TLS == 3 && defined(WIDE_HARD)
    #error "USE_CUSTOM_TLS == 3 asked on 64 bit machine."
   #endif

   /* for {2} and {3} we will need access to the stack pointer */
   #if USE_CUSTOM_TLS >=2
    #define TLS_SP_SHIFT  12

    #if defined(ASM_get_SP_register)
     /* means we have also GNU - so can use the extension */
     #define roughly_SP() \
       ({ var aint __SP; __asm__ ASM_get_SP_register(__SP); __SP;})
    #elif defined(GNU)
     /* may be use SP() as well ?? */
     #define roughly_SP()  (aint)__builtin_frame_address(0)
    #else
     /* this may expand to function call !!! */
     /* MSVC falls here (and all other non-gcc 32 bit compilers) */
     #define roughly_SP()  (aint)SP()
    #endif /* ASM_get_SP_register) / GNU */
   #endif  /* USE_CUSTOM_TLS */

   /* xthread_key_get/set - slowest way to do things.*/
   #if USE_CUSTOM_TLS == 1
     extern xthread_key_t current_thread_tls_key;
     #define set_current_thread(thread) \
       xthread_key_set(current_thread_tls_key,(void *)thread)
     #ifdef WIN32_NATIVE
       /* TlsGetValue() changes GetLastError() - this is quite bad and we
          should preserve the old value. Otherwise GC_SAFE_SYSTEM_CALL()
          will "return" with bad last error (which causes really weird
          problems). */
       static inline clisp_thread_t *current_thread_impl() {
         DWORD err=GetLastError();
         clisp_thread_t *thr=((clisp_thread_t *)xthread_key_get(current_thread_tls_key));
         SetLastError(err);
         return thr;
       }
       #define current_thread() current_thread_impl()
     #else
       #define current_thread() \
         ((clisp_thread_t *)xthread_key_get(current_thread_tls_key))
     #endif

   /* modified version of the code in Boehm C/C++ GC.
      much faster just 16 KB mem usage.*/
   #elif USE_CUSTOM_TLS == 2
     #define TS_CACHE_SIZE 1024
     #define TSD_CACHE_HASH(n) (((((long)n) >> 8) ^ (long)n) & (TS_CACHE_SIZE - 1))
     #define TS_HASH_SIZE 1024
     #define TSD_HASH(n) (((((long)n) >> 8) ^ (long)n) & (TS_HASH_SIZE - 1))
     #define INVALID_QTID ((unsigned long)0)

     typedef struct thread_specific_entry {
       volatile long qtid; /*quick thread id, only for cache - atomic store*/
       void *value; /* clisp_thread_t actually */
       struct thread_specific_entry *next;
       xthread_t thread;
     } tse;
     typedef struct thread_specific_data {
       /* A faster index to the hash table */
       tse * volatile cache[TS_CACHE_SIZE];
       tse *hash[TS_HASH_SIZE];
       spinlock_t lock;
     } tsd;
     /* global variable the keeps all active threads TLS values */
     extern tsd threads_tls;
     /* the slow version for accessing the TLS when the quick cache
        misses (when the thread stack crosses the VM page boundary). */
     global void* tsd_slow_getspecific(unsigned long qtid,
                                       tse * volatile *cache_ptr);
     /* UP: removes the TLS for current thread - should be called on
        thread exit. */
     global void tsd_remove_specific (void);
     /* initializes the current thread storage with supplied value.
       entry should be pre-allocated. May reside on the stack as
       well - but we have to be sure that it will be valid during
       the thread lifespan. */
     global void tsd_setspecific(tse *entry, void *value);
     /* quick TLS lookup. If there is cache miss - falls back to the slow
       version (which updates the cache as well). */
     static inline void *tsd_getspecific()
     {
       long qtid = roughly_SP() >> TLS_SP_SHIFT;
       unsigned hash_val = TSD_CACHE_HASH(qtid);
       tse * volatile * entry_ptr = threads_tls.cache + hash_val;
       tse * entry = *entry_ptr;   /* Must be loaded only once. */
       if (entry->qtid == qtid) {
         return entry->value;
       }
       return tsd_slow_getspecific(qtid, entry_ptr);
     }
     #define current_thread() \
       ((clisp_thread_t *)tsd_getspecific())
     /* NB: really nasty thing in order to have nice build.
      the __thread_tse_entry should be declared before using the
      set_current_thread macro !!!! So actually on the entry point of
      any LISP thread we have to declare it if needed. Fortunately there
      are just 3 places and no plans for more. */
     #define set_current_thread(thread)         \
       tsd_setspecific(__thread_tse_entry,(void *)thread)

    /* fastest TLS - almost matches compiler provided TLS support.
      maps the SP >> 12 to clisp_thread_t. */
   #elif USE_CUSTOM_TLS == 3
     #define TLS_PAGE_SIZE 4096
     /* the array below is indexed by SP >> 12 (TLS_SP_SHIFT)*/
     extern clisp_thread_t *threads_map[];
     #define current_thread() threads_map[roughly_SP() >> TLS_SP_SHIFT]
     global void set_current_thread(clisp_thread_t *thr);
   #else
     #error "USE_CUSTOM_TLS should be defined as 1,2 or 3. See comment."
   #endif
  #endif /* !defined(per_thread)*/

/* just the beginning of the structure is exported -
   what modules want to know about (in order to build) */
%% puts("struct object_tab_tl_ {");
%% #define LISPOBJ_TL(name,initstring) printf("  gcv_object_t %s;\n",STRING(name));
%%  #include "constobj_tl.c"
%% #undef LISPOBJ_TL
%% puts("};");
%% puts("typedef struct pinned_chain_t {");
%% puts("  gcv_object_t pc_varobject;");
%% puts("  gcv_object_t *pc_unwind_stack_ptr;");
%% puts("  struct pinned_chain_t *pc_next;");
%% puts("} pinned_chain_t;");
%% puts("typedef struct {");
%% puts("  gcv_object_t* _STACK;");
%% puts("  uintC _mv_count;");
%% puts("  p_backtrace_t _back_trace;");
%% puts("  struct object_tab_tl_ _object_tab;");
%% #ifdef DEBUG_GCSAFETY
%%  puts(" uintL _alloccount;");
%% #endif
%% #if defined(DEBUG_SPVW) && (defined(CAN_ALLOCATE_8BIT_VECTORS_ON_C_STACK) || defined(CAN_ALLOCATE_STRINGS_ON_C_STACK))
%%  puts(" void *_SP_before_suspend;");
%% #endif
%% puts("  spinlock_t _gc_suspend_request;");
%% puts("  spinlock_t _gc_suspend_ack;");
%% puts("  xmutex_raw_t _gc_suspend_lock;");
%% puts("  uintC _suspend_count;");
%% puts("  gcv_object_t *_ptr_symvalues;");
%% #if (int_bitsize < long_bitsize)
%%  puts(" long _jmpl_value;");
%% #endif
%% #if defined(HAVE_SIGNALS) && defined(SIGPIPE)
%%  puts(" bool _writing_to_subprocess;");
%% #endif
%% puts("  xmutex_raw_t *_raw_wait_mutex;");
%% puts("  volatile uintC _pending_interrupts;");
%% puts("  pinned_chain_t * _pinned;");
%% puts("  object _mv_space [unspecified];");
%% puts("} clisp_thread_t;");

%% #ifdef per_thread
%%  export_def(per_thread);
%%  exportV(per_thread clisp_thread_t*,_current_thread);
%% #else
%%  #if USE_CUSTOM_TLS == 1
%%   exportV(xthread_key_t,current_thread_tls_key);
%%   #ifdef WIN32_NATIVE
%%    puts("static inline clisp_thread_t *current_thread_impl() {");
%%    puts("  DWORD err=GetLastError();");
%%    puts("  clisp_thread_t *thr=((clisp_thread_t *)TlsGetValue(current_thread_tls_key));");
%%    puts("  SetLastError(err); return thr;\n}");
%%   #endif
%%  #elif USE_CUSTOM_TLS == 2
%%   export_def(TS_CACHE_SIZE);
%%   export_def(TS_HASH_SIZE);
%%   export_def(TSD_CACHE_HASH(qtid));
%%   export_def(roughly_SP());
%%   export_def(TLS_SP_SHIFT);
%%   emit_typedef("struct thread_specific_entry { volatile long qtid; void *value; struct thread_specific_entry *next; xthread_t thread; }","tse");
%%   emit_typedef("struct thread_specific_data { tse * volatile cache[TS_CACHE_SIZE]; tse *hash[TS_HASH_SIZE]; spinlock_t lock; }","tsd");
%%   exportV(tsd,threads_tls);
%%   exportF(void,tsd_setspecific,(tse *entry, void *value));
%%   exportF(void*,tsd_slow_getspecific,(unsigned long qtid,tse * volatile *cache_ptr));
%%   puts("static inline void *tsd_getspecific() {");
%%   puts("  long qtid = roughly_SP() >> 12;");
%%   puts("  unsigned hash_val = TSD_CACHE_HASH(qtid);");
%%   puts("  tse * volatile * entry_ptr = threads_tls.cache + hash_val;");
%%   puts("  tse * entry = *entry_ptr;");
%%   puts("  if (entry->qtid == qtid) {");
%%   puts("    return entry->value;");
%%   puts("  }");
%%   puts("  return tsd_slow_getspecific(qtid, entry_ptr);");
%%   puts("}");
%%  #elif USE_CUSTOM_TLS == 3
%%   export_def(TLS_SP_SHIFT);
%%   exportF(clisp_thread_t*,threads_map,[]);
%%  #endif
%% #endif

  #define inactive_handlers current_thread()->_inactive_handlers
  #define handler_args current_thread()->_handler_args
  #define unwind_protect_to_save current_thread()->_unwind_protect_to_save
  #define aktenv current_thread()->_aktenv
  #define STACK_bound current_thread()->_STACK_bound
  #define STACK_start current_thread()->_STACK_start
  #define mv_space current_thread()->_mv_space
  #define STACK current_thread()->_STACK
  #define TLO(name)  current_thread()->_object_tab.name
  #define mv_count current_thread()->_mv_count
  #define back_trace current_thread()->_back_trace
  #define SP_bound current_thread()->_SP_bound
  #define SP_anchor current_thread()->_SP_anchor
  #define break_sems current_thread()->_break_sems
  #if defined(HAVE_SIGNALS) && defined(SIGPIPE)
    #define writing_to_subprocess current_thread()->_writing_to_subprocess
  #endif
  #define running_handle_directory_encoding_error \
    current_thread()->_running_handle_directory_encoding_error
  #define running_handle_close_errors \
    current_thread()->_running_handle_close_errors
  #if (int_bitsize < long_bitsize)
    #define jmpl_value current_thread()->_jmpl_value
  #endif

/* needed for building modules */
%% export_def(current_thread());
%% export_def(value1);
%% export_def(value2);
%% export_def(value3);
%% export_def(value4);
%% export_def(value5);
%% export_def(value6);
%% export_def(value7);
%% export_def(value8);
%% export_def(value9);
%% export_def(mv_space);
%% export_def(mv_count);
%% export_def(back_trace);
%% #if defined(HAVE_SIGNALS) && defined(SIGPIPE)
%%  export_def(writing_to_subprocess);
%% #endif
%% export_def(SET_SP_BEFORE_SUSPEND(thr));
%% #ifdef export_unwind_protect_macros
%%   #if (int_bitsize < long_bitsize)
%%     export_def(jmpl_value);
%%   #endif
%% #endif

/* allocates,initializes and returns clisp_thread_t structure.
   Does not register it in the global thread array.
   When called the global thread lock should be held.*/
global clisp_thread_t* create_thread(uintM lisp_stack_size);
/* UP: removes the current_thread from the list (array) of threads.
   Also frees any allocated resource.
 > thread: thread to be removed */
global void delete_thread(clisp_thread_t *thread);
/* UP: creates initial bindings in thread context from alist
 > initial_bindings: alist of (symbol . form) elements */
global void initialize_thread_bindings(gcv_object_t *initial_bindings);
/* UP: Suspends all running threads /besides the current/ at GC safe
   points/regions.
 > lock_heap: if false - the caller already owns the heap lock
 At the end the heap lock is released since the GC itself may want
 to allocate. */
global void gc_suspend_all_threads(bool lock_heap);
/* UP: Resumes all suspended threads after GC (or world stop)
 > unlock_heap: if true - the heap lock will be released at the end
 should match a call to gc_suspend_all_threads()*/
global void gc_resume_all_threads(bool unlock_heap);
/* UP: Suspends single thread
 > thread: the thread to be suspended
 > have_locks: is the caller holding the heap and threads locks ?
 < returns true of the thread has been suspended. false in case it has exited
   meanwhile
 Called from signal handler thread and THREAD-INTERRUPT
 Upon exit we hold threads lock. It is released in resume_thread(). This prevents
 race condition when several threads try to THREAD-INTERRUPT another thread. */
global maygc bool suspend_thread(object thread, bool have_locks);
/* UP: Resumes single thread (or just decreases it's _suspend_count).
 > thread: the thread to be suspended
 > release_threads_lock: should we unlock threads lock
 Called from signal handler thread and from THREAD-INTERRUPT
 When called we should be the owner of threads lock and if specified we should
 release it.
 Should match a call to suspend_thread */
global void resume_thread(object thread, bool release_threads_lock);
/* UP: interrupts thread "safely"
 > thr: the thread
 The thread should be suspended (safe for GC).
 Caller should hold the thread _signal_reenter_ok. On failure
 (or when the thread will not be signalled) it will be released here*/
global bool interrupt_thread(clisp_thread_t *thr);
/* UP: signals that there is new timeout call (CALL-WITH-TIMEOUT)
   handles both POSIX and WIN32 threads */
global int signal_timeout_call (void);
/* UP: handles any pending interrupt (currently just one).
   arguments are on the STACK */
global maygc void handle_pending_interrupts (void);
%% exportF(void,handle_pending_interrupts,(void));
/* releases the clisp_thread_t memory of the list of Thread records */
global void release_threads (object list);
/* releases the OS mutexes for mutex objects in the list */
global void release_mutexes(object list);
/* releases the OS condition variables for exemption objects in the list */
global void release_exemptions(object list);
/* called at thread exiting. performs cleanup/checks. */
global maygc void thread_cleanup(void);
/* signals an error if obj is not thread. returns the thread */
global maygc object check_thread(object obj);
/* add per thread special symbol value - initialized to SYMVALUE_EMPTY.
 symbol: the symbol
 returns: the new index in the _symvalues thread array */
global maygc uintL add_per_thread_special_var(object symbol);
/* Clears any per thread value for symbol. Also set tls_index
   of the symbol to invalid. */
global maygc void clear_per_thread_symvalues(object symbol);
/* O(open_files) needs a global locks when accessed/modified */
extern xmutex_t open_files_lock;
/* O(open_files) needs a global locks when accessed/modified */
extern xmutex_t all_finalizers_lock;
/* mutex for guarding access to O(all_mutexes) */
extern xmutex_t all_mutexes_lock;
/* mutex for guarding access to O(all_exemptions) */
extern xmutex_t all_exemptions_lock;
/* mutex for guarding access to O(all_weakpointers) */
extern xmutex_t all_weakpointers_lock;
/* mutex for guarding access to O(all_packages) */
extern xmutex_t all_packages_lock;
/* mutex protecting the O(all_threads) and list of clisp_thread_t structs
 NB: when it is hold - any heap allocation will cause deadlock */
extern xmutex_t allthreads_lock;
/* mutex serializing gensym (only when *gensym-counter* is not
   per thread bound) */
extern xmutex_t gensym_lock;
/* mutex guarding internal counter used by gentemp */
extern xmutex_t gentemp_lock;

/* operations on a lisp stack that is not the current one (NC)
   - ie. belongs to other not yet started threads */
#ifdef STACK_DOWN
  #define NC_STACK_(non_current_stack,n)  (non_current_stack[(sintP)(n)])
#endif
#ifdef STACK_UP
  #define NC_STACK_(non_current_stack,n)  (non_current_stack[-1-(sintP)(n)])
#endif
#define NC_pushSTACK(non_current_stack,obj)  \
  (NC_STACK_(non_current_stack,-1) = (obj), non_current_stack skipSTACKop -1)

/* every CALL-WITH-TIMEOUT adds an item in the chain below.
   upon timeout the thread is interrupted with (THROW TAG) */
typedef struct timeout_call {
  clisp_thread_t *thread; /* thread to be interrupted */
  gcv_object_t *throw_tag; /* pointer to thread STACK */
  bool failed; /* true if the thread signal has failed */
  struct timeval *expire;  /* timeout expire time */
  struct timeout_call *next; /* next timeout call */
} timeout_call;
/* lock for the timeout_call_chain */
extern spinlock_t timeout_call_chain_lock;
/* chain of sorted by expire time timeout_calls */
extern timeout_call *timeout_call_chain;
/* returns true if p1 is before p2 */
global bool timeval_less(struct timeval *p1, struct timeval *p2);

#define GC_STOP_WORLD(lock_heap) \
  gc_suspend_all_threads(lock_heap)
#define GC_RESUME_WORLD(unlock_heap) \
  gc_resume_all_threads(unlock_heap)

/* all calls to GC should be via this macro.
 The statement is executed. If lock_heap is true the heap is locked first.
 (this is needed since GC may be called from allocation or explicitly - when
 the heap lock is not held). */
#define WITH_STOPPED_WORLD(lock_heap,statement) \
  do {                                          \
    var bool lh=lock_heap;                      \
    GC_STOP_WORLD(lh);                          \
    statement;                                  \
    GC_RESUME_WORLD(lh);                        \
  } while(0)

  #ifndef DEBUG_GCSAFETY
    #define PERFORM_GC(statement,lock_heap)             \
      do {                                              \
        SET_SP_BEFORE_SUSPEND(current_thread());        \
        WITH_STOPPED_WORLD(lock_heap,statement);        \
      } while(0)
  #else /* DEBUG_GCSAFETY */
    /* if we trigger GC from allocate_xxxx, than we already have
     stopped the world and will resume it at exit.*/
    #define PERFORM_GC(statement,lock_heap) \
      do {                                  \
        SET_SP_BEFORE_SUSPEND(current_thread()); \
        if (lock_heap) WITH_STOPPED_WORLD(true,statement); else statement; \
      }while(0)
    extern uintL* current_thread_alloccount (void);
  #endif

  #if defined(GENERATIONAL_GC) && defined(SPVW_MIXED)
   #define unprotect_heap_range(vo,access)                              \
     handle_fault_range(access, (aint)TheVarobject(vo),                 \
                        (aint)TheVarobject(vo) + varobject_bytelength(vo))
  #else
   #define unprotect_heap_range(vo,access)
  #endif

  #ifdef DEBUG_SPVW
    /* aborts if we want to pin object while we are in GC safe region.
       prevents bad pin_varobject() usage. */
    #define ASSERT_SAFE_TO_PIN()                                \
      do {                                                      \
        if (current_thread()->_gc_suspend_ack == 0) abort();    \
      } while(0)
    /* aborts if we want to unpin object that is not in the front of pinned
       object chain. since we have proper unwind semantic - this should
       never happen.*/
    #define ASSERT_VALID_UNPIN(pc,vo)                        \
      do {                                                   \
        if (!eq((*pc)->pc_varobject, vo)) abort(); \
      } while(0)
  #else
    #define ASSERT_SAFE_TO_PIN()
    #define ASSERT_VALID_UNPIN(pc,vo)
  #endif

  /* UP: pins varobject by using pinned_chain_t pointed by pc.
     > pc: pointer to C stack allocated pinned_chain_t struct
     > varobj: varobject to be pinned
    NB: pc should be C stack allocated (auto scope or alloca()).
    usage should be matched with unpin_varobject(). In case of non-local
    exit before the control reaches unpin_varobject() - cleanup is performed
    in enter_frame_at_STACK() (same like the backtrace).
    does not modify the STACK */
  #define pin_varobject_with_pc(pc, varobj) do {                        \
    ASSERT_SAFE_TO_PIN();                                               \
    (pc)->pc_varobject = varobj;                                        \
    (pc)->pc_unwind_stack_ptr = &STACK_(-1);/* above current stack */   \
    (pc)->pc_next = current_thread()->_pinned;                          \
    current_thread()->_pinned = (pc);                                   \
  } while(0)

  /* UP: add object to the pinned chain.
     > varobj: the varobject to be pinned
     allocates pinned_chain_t struct on C stack and uses it */
  #define pin_varobject(varobj)                      \
    var pinned_chain_t GENTAG(pc);                   \
    pin_varobject_with_pc(&GENTAG(pc), varobj)

  /* UP: unpin varobject in lisp heap. */
  #define unpin_varobject(varobj)    do {                  \
      var pinned_chain_t **p=&(current_thread()->_pinned); \
      ASSERT_VALID_UNPIN(p,varobj);                        \
      *p = (*p)->pc_next;                                  \
    } while(0)

  /* Macro: pin varobj, execute code, unpin it */
  #define with_pinned_varobject(varobj,code)    do {   \
      var pinned_chain_t GENTAG(pc);                   \
      pin_varobject_with_pc(&GENTAG(pc), varobj);      \
      code;                                            \
      unpin_varobject(GENTAG(pc).pc_varobject);        \
    } while(0)

  /* UP: unpins specified number of pinned objects. will abort if there are
     less pinned objects than asked (SEGFAULT).
     > count: how many object to remove from the pinned chain */
  #define unpin_varobjects(count)  do {                  \
    var uintC cnt;                                       \
    var pinned_chain_t **p=&(current_thread()->_pinned); \
    dotimespC(cnt, count, { *p = (*p)->pc_next; });      \
  } while(0)

/* UP: executes body while thread interrupts are deferred. after body
   finishes - deferred interrupts are executed (if any) */
#define WITH_DEFERRED_INTERRUPTS(body) do {                 \
  dynamic_bind(S(defer_interrupts), T);                     \
  body;                                                     \
  dynamic_unbind(S(defer_interrupts));                      \
  if (nullp(Symbol_thread_value(S(defer_interrupts))))      \
    while (!nullp(Symbol_thread_value(S(deferred_interrupts)))) {         \
      var object intr = Car(Symbol_thread_value(S(deferred_interrupts))); \
      Symbol_thread_value(S(deferred_interrupts)) =         \
        Cdr(Symbol_thread_value(S(deferred_interrupts)));   \
      apply(Car(intr), 0, nreverse(Cdr(intr)));             \
    }                                                       \
 } while (0)

/* UP: helper macro for executing body in unwind_protect frame with
   mutex lock held. body should not call return or goto outside of
   itself.
 > stack_count: number of object to copy from the stack after the
   UNWIND_PROTECT frame is established. at the end the same count of
   objects are removed from the stack.
 > keep_mv_space: whether the mv_space should be preserved
 > mutex: the mutex object
 > locker_vars: local variables used by locker (for thread interrupt safety)
 > locker: statement to execute for locking the mutex
 > unlocker: statement to execute for unlocking the mutex
 > body: the statement(s) to be executed with lock held */
#define WITH_MUTEX_LOCK_HELP_(stack_count,keep_mv_space,mutex,locker_vars,locker,unlocker,body) \
  do {                                                                  \
    locker_vars;                                                        \
    var gcv_object_t* top_of_frame = STACK;                             \
    var sp_jmp_buf returner;                                            \
    finish_entry_frame(UNWIND_PROTECT,returner,, {                      \
      var restartf_t fun = unwind_protect_to_save.fun;                  \
      var gcv_object_t* upto = unwind_protect_to_save.upto_frame;       \
      skipSTACK(2);                                                     \
      unlocker(mutex,true);                                             \
      fun(upto);                                                        \
    });                                                                 \
    locker(mutex);                                                      \
    if (stack_count > 0) {                                              \
      var gcv_object_t *args=&STACK_(stack_count+1);                    \
      var uintC count;                                                  \
      get_space_on_STACK(stack_count);                                  \
      dotimespC(count,stack_count, { pushSTACK(*args++); });            \
    }                                                                   \
    body;                                                               \
    /* skip the args and unwind_protect frame */                        \
    skipSTACK(stack_count+2);                                           \
    unlocker(mutex,keep_mv_space);                                      \
  } while (0)

#define OS_MUTEX_LOCK_DECLARE_LOCALS var volatile bool locked=false
#define OS_MUTEX_LOCK_HELP_(mutex) GC_SAFE_MUTEX_LOCK(mutex, &locked)
#define OS_MUTEX_UNLOCK_HELP_(mutex,keep_mv_space) do { \
  if (locked)                                           \
    GC_SAFE_MUTEX_UNLOCK(mutex);                        \
 } while(0)
#define WITH_OS_MUTEX_LOCK(stack_count,mutex,body)      \
  WITH_MUTEX_LOCK_HELP_(stack_count,true,mutex,OS_MUTEX_LOCK_DECLARE_LOCALS,OS_MUTEX_LOCK_HELP_,OS_MUTEX_UNLOCK_HELP_,body)

#define LISP_MUTEX_LOCK_DECLARE_LOCALS                                  \
  var volatile bool we_owned = false;                                   \
  var volatile uintL rec_count=0
#define LISP_MUTEX_LOCK_HELP_(mutex) do {                               \
  we_owned = eq(TheMutex(*(mutex))->xmu_owner, current_thread()->_lthread); \
  rec_count = TheMutex(*(mutex))->xmu_recurse_count;                    \
  pushSTACK(*(mutex)); funcall(L(mutex_lock),1);                        \
 } while(0)
/* also preserves values */
#define LISP_MUTEX_UNLOCK_HELP_(mutex,keep_mv_space)                    \
  do {                                                                  \
    var bool we_own = eq(TheMutex(*(mutex))->xmu_owner, current_thread()->_lthread); \
    var uintL rc = TheMutex(*(mutex))->xmu_recurse_count;               \
    if (we_own && (!we_owned || rc > rec_count)) {                      \
      var uintC cnt=mv_count;                                           \
      if (keep_mv_space) mv_to_STACK();                                 \
      WITH_DEFERRED_INTERRUPTS({                                        \
        pushSTACK(*(mutex));                                            \
        funcall(L(mutex_unlock),1);                                     \
      });                                                               \
      if (keep_mv_space) STACK_to_mv(cnt);                              \
    }                                                                   \
  } while(0)

/* mutex should be pointer to GC safe location. */
#define WITH_LISP_MUTEX_LOCK(stack_count,keep_mv_space,pmutex,body)     \
  WITH_MUTEX_LOCK_HELP_(stack_count,keep_mv_space,pmutex,LISP_MUTEX_LOCK_DECLARE_LOCALS,LISP_MUTEX_LOCK_HELP_,LISP_MUTEX_UNLOCK_HELP_,body)

#else /* ! MULTITHREAD */
%% #else
  #define pin_varobject_with_pc(pc,vo)
  #define pin_varobject(vo)
  #define unprotect_heap_range(vo,access)
  #define unpin_varobject(vo)
  #define with_pinned_varobject(vo,code)  code
  #define unpin_varobjects(count)
  #define GC_STOP_WORLD(lock_heap)
  #define GC_RESUME_WORLD(unlock_heap)
  #define PERFORM_GC(statement,lock_heap) statement
  #define WITH_MUTEX_LOCK_HELP_(body)                    \
    do {                                                 \
      body;                                              \
    } while(0);
  #define WITH_OS_MUTEX_LOCK(stack_count,mutex,body)     \
    WITH_MUTEX_LOCK_HELP_(body)
  #define WITH_LISP_MUTEX_LOCK(stack_count,keep_mv_space,pmutex,body)     \
    WITH_MUTEX_LOCK_HELP_(body)
#endif
%% #endif

#define pin_unprotect_varobject(vo,access)              \
  pin_varobject(vo); unprotect_heap_range(vo,access)

%% export_def(unprotect_heap_range(vo,access));
%% export_def(pin_varobject_with_pc(pc,vo));
%% export_def(pin_varobject(vo));
%% export_def(unpin_varobject(vo));
%% export_def(with_pinned_varobject(vo,code));
%% export_def(unpin_varobjects(count));
%% export_def(WITH_OS_MUTEX_LOCK(stack_count,mutex,body));
%% export_def(WITH_LISP_MUTEX_LOCK(stack_count,keep_mv_space,pmutex,body));

#if defined(HAVE_SIGNALS) && defined(SIGPIPE)
 #define START_WRITING_TO_SUBPROCESS  writing_to_subprocess=true
 #define STOP_WRITING_TO_SUBPROCESS  writing_to_subprocess=false
#else
 #define START_WRITING_TO_SUBPROCESS /*noop*/
 #define STOP_WRITING_TO_SUBPROCESS /*noop*/
#endif
%% export_def(START_WRITING_TO_SUBPROCESS);
%% export_def(STOP_WRITING_TO_SUBPROCESS);

#ifdef DEBUG_GCSAFETY
  /* Add support for the 'mv_space' expression to the GCTRIGGER1/2/... macros.*/
  inline void inc_allocstamp (object (&mvsp)[mv_limit-1]) {
    inc_allocstamp(value1);
    var uintC count = mv_count;
    if (count > 1) {
      var object* mvp = &mv_space[1];
      dotimespC(count,count-1, { inc_allocstamp(*mvp++); });
    }
  }
#endif


/* ######################## BUILTBIBL for BUILT.D ######################## */

/* Returns a multiline string containing some info about the flags with which
   the executable was built. */
extern object built_flags (void);

/* ####################### FOR DEBUGGING UNDER GDB ####################### */

#ifdef GENERATIONAL_GC
/* Put a breakpoint here if you want to catch CLISP just before it dies. */
extern void sigsegv_handler_failed (void* address);
#endif

/* For debugging: From within gdb, type: call ext_show_stack().
   Equivalent to (ext:show-stack) from the Lisp prompt. */
extern void gdb_show_stack (void);

/* Fore debugging: From within gdb, type: call gdb_disassemble_closure(obj).
   Equivalent to (sys::disassemble-closures (list obj) *standard-output*). */
extern void gdb_disassemble_closure (object obj);

/*************************************************************************/
