# Hilfsfunktionen für CLISP auf AmigaOS
# Jörg Höhle 2.9.1997

#include "lispbibl.c"

# ==============================================================================
# a wrapper for Read
global long read_helper (Handle handle, void* bufarea, long nbyte,
                         bool partial_p) {
  var char* buf = (char*) bufarea;
  var long done = 0;
  while (nbyte!=0) {
    var long retval = Read(handle,(APTR)buf,nbyte);
    if (retval == 0)
      break; # EOF
    else if (retval < 0) {
      return retval;
    } else {
      buf += retval; done += retval; nbyte -= retval;
      if (partial_p)
        break;
    }
  }
  return done;
}

# Ein Wrapper um die Write-Funktion.
global long write_helper (Handle handle, const void* bufarea, long nbyte,
                          bool no_hang)
{
  var CONST char* buf = (CONST char*) bufarea;
  var long done = 0;
  while (nbyte!=0) {
    var long retval = Write(handle,(CONST APTR)buf,nbyte);
    if (retval == 0)
      break; # Wann passiert das?? Wenn Platte voll!
    else if (retval < 0) {
      return retval;
    } else {
      buf += retval; done += retval; nbyte -= retval;
      if (no_hang)
        break;
    }
  }
  return done;
}

# ==============================================================================

# Sofortiger Programmabbruch, Sprung in den Debugger
  global void abort (void);
  global void abort()
    {
      #if defined(GNU) && 0 # Jörg mag das nicht so sehr bis überhaupt nicht
        __asm__ __volatile__ (" .word 0x4AFC "); # illegaler Befehl
      #else
        # Je préfère Wait(0L) car ainsi le programme se met en attente infinie
        # et on peut essayer de savoir pourquoi en analysant la mémoire. Je ne
        # considère pas qu'une sortie de programme soit sûre puisque la mémoire
        # peut se trouver dans un mauvais état, il peut y avoir des fichiers
        # non fermés, des «Lock» alloués, etc.                    Jörg 7.1.1993
        fprintf(stderr,"\nCLISP panic! (halting)\n");
        Wait(0L);
      #endif
    }

# ==============================================================================

# Eigenes malloc(), free() nötig wegen Resource Tracking.

  # Flag, das anzeigt, ob der Prozessor ein 68000 ist.
  local bool cpu_is_68000;
  #if defined(MC68000)
    #define CPU_IS_68000  true
  #elif defined(MC680Y0)
    #define CPU_IS_68000  false
  #else
    #define CPU_IS_68000  cpu_is_68000
  #endif

  # Flag für AllocMem().
  global uintL default_allocmemflag = MEMF_ANY;
  global uintL retry_allocmemflag;  # wird in init_amiga() gesetzt.
  #if !(defined(WIDE_SOFT) || defined(MC68000) || !defined(TYPECODES))
    # Es kann sein, dass wir mit MEMF_ANY Speicher außerhalb des
    # 24/26-Bit-Adressraums bekommen, den wir nicht nutzen können.
    # Dann versuchen wir's nochmal.
  #endif

  # Doppelt verkettete Liste aller bisher belegten Speicherblöcke führen:
  typedef struct MemBlockHeader {
    struct MemBlockHeader * next;
    #ifdef SPVW_PAGES
    struct MemBlockHeader * * prev;
    #endif
    uintL size;
    oint usable_memory[unspecified]; # "oint" erzwingt Alignment
  } MemBlockHeader;
  local MemBlockHeader* allocmemblocks = NULL;
  # Für alle p = allocmemblocks{->next}^n (n=0,1,...) mit !(p==NULL) gilt
  # *(p->prev) = p.

  # Speicher vom Betriebssystem holen:
  global void* allocmem (uintL amount, uintL allocmemflag);
  global void* allocmem(amount,allocmemflag)
    var uintL amount;
    var uintL allocmemflag;
    {
      amount = round_up(amount+offsetofa(MemBlockHeader,usable_memory),4);
      var void* address = AllocMem(amount,allocmemflag);
      if (!(address==NULL)) {
        ((MemBlockHeader*)address)->size = amount;
        ((MemBlockHeader*)address)->next = allocmemblocks;
        ((MemBlockHeader*)address)->prev = &allocmemblocks;
        if (!(allocmemblocks == NULL)) {
          if (allocmemblocks->prev == &allocmemblocks) { # Sicherheits-Check
            allocmemblocks->prev = &((MemBlockHeader*)address)->next;
          } else {
            abort();
          }
        }
        allocmemblocks = (MemBlockHeader*)address;
        address = &((MemBlockHeader*)address)->usable_memory[0];
      }
      return address;
    }

  # Speicher dem Betriebssystem zurückgeben:
  global void freemem (void* address);
  global void freemem(address)
    var void* address;
    {
      var MemBlockHeader* ptr = (MemBlockHeader*)((aint)address - offsetofa(MemBlockHeader,usable_memory));
      if (*(ptr->prev) == ptr) { # Sicherheits-Check
        var MemBlockHeader* ptrnext = ptr->next;
        *(ptr->prev) = ptrnext; # ptr durch ptr->next ersetzen
        if (!(ptrnext == NULL))
          ptrnext->prev = ptr->prev;
        FreeMem(ptr,ptr->size);
        return;
      } else {
        abort();
      }
    }

  # ANSI C compliant
  global void* malloc (uintL amount);
  global void* malloc(amount)
    var uintL amount;
    {
      return allocmem(amount,default_allocmemflag);
    }
  global void free (void* address);
  global void free(address)
    var void* address;
    {
      freemem(address);
    }

/* ======================================================================== */

# Diese beiden Variablen werden, wenn man Glück hat, vom Startup-System
# (von dem main() aufgerufen wird) sinnvoll vorbesetzt:
global Handle stdin_handle = Handle_NULL;    # low-level stdin Eingabekanal
global Handle stdout_handle = Handle_NULL;   # low-level stdout Ausgabekanal
extern Handle stderr_handle = Handle_NULL; /* low-level stderr ouput channel */

  global BPTR orig_dir_lock = BPTR_NONE; # das Current Directory beim Programmstart
  # wird verwendet von PATHNAME

  # Initialisierung, ganz zuerst in main() durchzuführen:
    global void init_amiga (void);
    global void init_amiga()
      {
        cpu_is_68000 = ((SysBase->AttnFlags & (AFF_68020|AFF_68030|AFF_68040)) == 0);
        #ifdef MC68000
        # Diese Version benötigt einen 68000. (Wegen addressbus_mask.)
        if (!cpu_is_68000) {
          exit(RETURN_FAIL);
        }
        #endif
        #ifdef MC680Y0
        # Diese Version benötigt mindestens einen 68020, läuft nicht auf 68000.
        # (Wegen ari68020.d, einiger asm()s und wegen gcc-Option -m68020.)
        if (cpu_is_68000) {
          exit(RETURN_FAIL);
        }
        #endif
        # Wir wollen uns nicht mehr mit OS Version 1.x beschäftigen
        if (SysBase->LibNode.lib_Version < 36) {
          exit(RETURN_FAIL);
        }
        if (stdin_handle==Handle_NULL)
          stdin_handle = Input();
        if (stdout_handle==Handle_NULL)
          stdout_handle = Output();
        if (stderr_handle==Handle_NULL)
          stderr_handle = Error();
        # Abfrage, ob Workbench-Aufruf ohne besonderen Startup:
        if ((stdin_handle==Handle_NULL) || (stdout_handle==Handle_NULL)) {
          exit(RETURN_FAIL);
        }
        # Benutzter Speicher muss in [0..2^oint_addr_len-1] liegen:
        #if defined(TYPECODES) && !defined(WIDE_SOFT)
        #define pointable_usable_test(a)  ((void*)pointable(type_pointer_object(0,a)) == (void*)(a))
        if (!(pointable_usable_test((aint)&init_amiga) # Code-Segment überprüfen
              && pointable_usable_test((aint)&symbol_tab) # Daten-Segment überprüfen
           ) ) {
          fprintf(stderr,GETTEXTL("This version of CLISP runs only in low address memory."));
          fprintf(stderr,"\nCODE: %x, DATA: %x.\n", (aint)&init_amiga, (aint)&symbol_tab);
          exit(RETURN_FAIL);
        }
        #undef pointable_usable_test
        #endif
        # Ein Flag, das uns hilft, Speicher mit niedrigen Adressen zu bekommen:
        retry_allocmemflag =
          (CPU_IS_68000              # der 68000 hat nur 24 Bit Adressbereich,
           ? MEMF_ANY                # nie ein zweiter Versuch nötig
           : MEMF_24BITDMA           # sonst Flag MEMF_24BITDMA
          );
      }

  # Rückgabe aller Ressourcen und Programmende:
  nonreturning_function(global, exit_amiga, (sintL code))
    {
      begin_system_call();
      # Zurück ins Verzeichnis, in das wir beim Programmstart waren:
      if (!(orig_dir_lock == BPTR_NONE)) { # haben wir das Verzeichnis je gewechselt?
        var BPTR lock = CurrentDir(orig_dir_lock); # zurück ins alte
        UnLock(lock); # dieses nun freigeben
      }
      # Speicher freigeben:
      {
        var MemBlockHeader* memblocks = allocmemblocks;
        until (memblocks==NULL) {
          var MemBlockHeader* next = memblocks->next;
          FreeMem(memblocks,memblocks->size);
          memblocks = next;
        }
      }
      # Programmende:
      exit(code);
    }

# ==============================================================================

