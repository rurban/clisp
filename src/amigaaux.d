# Hilfsfunktionen für CLISP auf AmigaOS
# Jörg Höhle 2.9.1997

#include "lispbibl.c"

# ==============================================================================

# Ein Wrapper um die Read-Funktion.
global long full_read (Handle handle, RW_BUF_T bufarea, long nbyte);
global long full_read(handle,bufarea,nbyte)
  var Handle handle;
  var RW_BUF_T bufarea;
  var long nbyte;
  { var char* buf = (char*) bufarea;
    var long done = 0;
    until (nbyte==0)
      { var long retval = Read(handle,(APTR)buf,nbyte);
        if (retval == 0) break; # EOF
        elif (retval < 0) { return retval; }
        else { buf += retval; done += retval; nbyte -= retval; }
      }
    return done;
  }

# Ein Wrapper um die Write-Funktion.
global long full_write (Handle handle, const RW_BUF_T bufarea, long nbyte);
global long full_write(handle,bufarea,nbyte)
  var Handle handle;
  var const RW_BUF_T bufarea;
  var long nbyte;
  { var CONST char* buf = (CONST char*) bufarea;
    var long done = 0;
    until (nbyte==0)
      { var long retval = Write(handle,(CONST APTR)buf,nbyte);
        if (retval == 0) break; # Wann passiert das?? Wenn Platte voll!
        elif (retval < 0) { return retval; }
        else { buf += retval; done += retval; nbyte -= retval; }
      }
    return done;
  }

# ==============================================================================

