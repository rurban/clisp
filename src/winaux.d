# Auxiliary functions for MS-Windows


#ifdef WIN_DEBUG
  local void event_out(caller,hWnd,w,message,wParam,lParam)
    var const char * caller;
    var HWND hWnd;
    var mywindow w;
    var UINT message;
    var WPARAM wParam;
    var LPARAM lParam;
    { asciz_out(NLstring "{"); asciz_out(caller); asciz_out(":");
      dez_out(hWnd); asciz_out(":");
      hex_out(w); asciz_out(":");
      switch (message)
        { default: hex_out(message); break;
          case WM_ACTIVATE:          asciz_out("WM_ACTIVATE"); break;
          case WM_ACTIVATEAPP:       asciz_out("WM_ACTIVATEAPP"); break;
          case WM_ASKCBFORMATNAME:   asciz_out("WM_ASKCBFORMATNAME"); break;
          case WM_CANCELMODE:        asciz_out("WM_CANCELMODE"); break;
          case WM_CHANGECBCHAIN:     asciz_out("WM_CHANGECBCHAIN"); break;
          case WM_CHAR:              asciz_out("WM_CHAR"); break;
          case WM_CHARTOITEM:        asciz_out("WM_CHARTOITEM"); break;
          case WM_CHILDACTIVATE:     asciz_out("WM_CHILDACTIVATE"); break;
          case WM_CLEAR:             asciz_out("WM_CLEAR"); break;
          case WM_CLOSE:             asciz_out("WM_CLOSE"); break;
          case WM_COALESCE_FIRST:    asciz_out("WM_COALESCE_FIRST"); break;
          case WM_COALESCE_LAST:     asciz_out("WM_COALESCE_LAST"); break;
          case WM_COMMAND:           asciz_out("WM_COMMAND"); break;
          case WM_COMMNOTIFY:        asciz_out("WM_COMMNOTIFY"); break;
          case WM_COMPACTING:        asciz_out("WM_COMPACTING"); break;
          case WM_COMPAREITEM:       asciz_out("WM_COMPAREITEM"); break;
          case WM_COPY:              asciz_out("WM_COPY"); break;
          case WM_CREATE:            asciz_out("WM_CREATE"); break;
          case WM_CTLCOLOR:          asciz_out("WM_CTLCOLOR"); break;
          case WM_CUT:               asciz_out("WM_CUT"); break;
          case WM_DEADCHAR:          asciz_out("WM_DEADCHAR"); break;
          case WM_DELETEITEM:        asciz_out("WM_DELETEITEM"); break;
          case WM_DESTROY:           asciz_out("WM_DESTROY"); break;
          case WM_DESTROYCLIPBOARD:  asciz_out("WM_DESTROYCLIPBOARD"); break;
          case WM_DEVMODECHANGE:     asciz_out("WM_DEVMODECHANGE"); break;
          case WM_DRAWCLIPBOARD:     asciz_out("WM_DRAWCLIPBOARD"); break;
          case WM_DRAWITEM:          asciz_out("WM_DRAWITEM"); break;
          case WM_DROPFILES:         asciz_out("WM_DROPFILES"); break;
          case WM_ENABLE:            asciz_out("WM_ENABLE"); break;
          case WM_ENDSESSION:        asciz_out("WM_ENDSESSION"); break;
          case WM_ENTERIDLE:         asciz_out("WM_ENTERIDLE"); break;
          case WM_ERASEBKGND:        asciz_out("WM_ERASEBKGND"); break;
          case WM_FONTCHANGE:        asciz_out("WM_FONTCHANGE"); break;
          case WM_GETDLGCODE:        asciz_out("WM_GETDLGCODE"); break;
          case WM_GETFONT:           asciz_out("WM_GETFONT"); break;
          case WM_GETMINMAXINFO:     asciz_out("WM_GETMINMAXINFO"); break;
          case WM_GETTEXT:           asciz_out("WM_GETTEXT"); break;
          case WM_GETTEXTLENGTH:     asciz_out("WM_GETTEXTLENGTH"); break;
          case WM_HSCROLL:           asciz_out("WM_HSCROLL"); break;
          case WM_HSCROLLCLIPBOARD:  asciz_out("WM_HSCROLLCLIPBOARD"); break;
          case WM_ICONERASEBKGND:    asciz_out("WM_ICONERASEBKGND"); break;
          case WM_INITDIALOG:        asciz_out("WM_INITDIALOG"); break;
          case WM_INITMENU:          asciz_out("WM_INITMENU"); break;
          case WM_INITMENUPOPUP:     asciz_out("WM_INITMENUPOPUP"); break;
          case WM_KEYDOWN:           asciz_out("WM_KEYDOWN"); break;
          case WM_KEYUP:             asciz_out("WM_KEYUP"); break;
          case WM_KILLFOCUS:         asciz_out("WM_KILLFOCUS"); break;
          case WM_LBUTTONDBLCLK:     asciz_out("WM_LBUTTONDBLCLK"); break;
          case WM_LBUTTONDOWN:       asciz_out("WM_LBUTTONDOWN"); break;
          case WM_LBUTTONUP:         asciz_out("WM_LBUTTONUP"); break;
          case WM_MBUTTONDBLCLK:     asciz_out("WM_MBUTTONDBLCLK"); break;
          case WM_MBUTTONDOWN:       asciz_out("WM_MBUTTONDOWN"); break;
          case WM_MBUTTONUP:         asciz_out("WM_MBUTTONUP"); break;
          case WM_MDIACTIVATE:       asciz_out("WM_MDIACTIVATE"); break;
          case WM_MDICASCADE:        asciz_out("WM_MDICASCADE"); break;
          case WM_MDICREATE:         asciz_out("WM_MDICREATE"); break;
          case WM_MDIDESTROY:        asciz_out("WM_MDIDESTROY"); break;
          case WM_MDIGETACTIVE:      asciz_out("WM_MDIGETACTIVE"); break;
          case WM_MDIICONARRANGE:    asciz_out("WM_MDIICONARRANGE"); break;
          case WM_MDIMAXIMIZE:       asciz_out("WM_MDIMAXIMIZE"); break;
          case WM_MDINEXT:           asciz_out("WM_MDINEXT"); break;
          case WM_MDIRESTORE:        asciz_out("WM_MDIRESTORE"); break;
          case WM_MDISETMENU:        asciz_out("WM_MDISETMENU"); break;
          case WM_MDITILE:           asciz_out("WM_MDITILE"); break;
          case WM_MEASUREITEM:       asciz_out("WM_MEASUREITEM"); break;
          case WM_MENUCHAR:          asciz_out("WM_MENUCHAR"); break;
          case WM_MENUSELECT:        asciz_out("WM_MENUSELECT"); break;
          case WM_MOUSEACTIVATE:     asciz_out("WM_MOUSEACTIVATE"); break;
          case WM_MOUSEMOVE:         asciz_out("WM_MOUSEMOVE"); break;
          case WM_MOVE:              asciz_out("WM_MOVE"); break;
          case WM_NCACTIVATE:        asciz_out("WM_NCACTIVATE"); break;
          case WM_NCCALCSIZE:        asciz_out("WM_NCCALCSIZE"); break;
          case WM_NCCREATE:          asciz_out("WM_NCCREATE"); break;
          case WM_NCDESTROY:         asciz_out("WM_NCDESTROY"); break;
          case WM_NCHITTEST:         asciz_out("WM_NCHITTEST"); break;
          case WM_NCLBUTTONDBLCLK:   asciz_out("WM_NCLBUTTONDBLCLK"); break;
          case WM_NCLBUTTONDOWN:     asciz_out("WM_NCLBUTTONDOWN"); break;
          case WM_NCLBUTTONUP:       asciz_out("WM_NCLBUTTONUP"); break;
          case WM_NCMBUTTONDBLCLK:   asciz_out("WM_NCMBUTTONDBLCLK"); break;
          case WM_NCMBUTTONDOWN:     asciz_out("WM_NCMBUTTONDOWN"); break;
          case WM_NCMBUTTONUP:       asciz_out("WM_NCMBUTTONUP"); break;
          case WM_NCMOUSEMOVE:       asciz_out("WM_NCMOUSEMOVE"); break;
          case WM_NCPAINT:           asciz_out("WM_NCPAINT"); break;
          case WM_NCRBUTTONDBLCLK:   asciz_out("WM_NCRBUTTONDBLCLK"); break;
          case WM_NCRBUTTONDOWN:     asciz_out("WM_NCRBUTTONDOWN"); break;
          case WM_NCRBUTTONUP:       asciz_out("WM_NCRBUTTONUP"); break;
          case WM_NEXTDLGCTL:        asciz_out("WM_NEXTDLGCTL"); break;
          case WM_NULL:              asciz_out("WM_NULL"); break;
          case WM_PAINT:             asciz_out("WM_PAINT"); break;
          case WM_PAINTCLIPBOARD:    asciz_out("WM_PAINTCLIPBOARD"); break;
          case WM_PALETTECHANGED:    asciz_out("WM_PALETTECHANGED"); break;
          case WM_PALETTEISCHANGING: asciz_out("WM_PALETTEISCHANGING"); break;
          case WM_PARENTNOTIFY:      asciz_out("WM_PARENTNOTIFY"); break;
          case WM_PASTE:             asciz_out("WM_PASTE"); break;
          case WM_PENWINFIRST:       asciz_out("WM_PENWINFIRST"); break;
          case WM_PENWINLAST:        asciz_out("WM_PENWINLAST"); break;
          case WM_POWER:             asciz_out("WM_POWER"); break;
          case WM_QUERYDRAGICON:     asciz_out("WM_QUERYDRAGICON"); break;
          case WM_QUERYENDSESSION:   asciz_out("WM_QUERYENDSESSION"); break;
          case WM_QUERYNEWPALETTE:   asciz_out("WM_QUERYNEWPALETTE"); break;
          case WM_QUERYOPEN:         asciz_out("WM_QUERYOPEN"); break;
          case WM_QUEUESYNC:         asciz_out("WM_QUEUESYNC"); break;
          case WM_QUIT:              asciz_out("WM_QUIT"); break;
          case WM_RBUTTONDBLCLK:     asciz_out("WM_RBUTTONDBLCLK"); break;
          case WM_RBUTTONDOWN:       asciz_out("WM_RBUTTONDOWN"); break;
          case WM_RBUTTONUP:         asciz_out("WM_RBUTTONUP"); break;
          case WM_RENDERALLFORMATS:  asciz_out("WM_RENDERALLFORMATS"); break;
          case WM_RENDERFORMAT:      asciz_out("WM_RENDERFORMAT"); break;
          case WM_SETCURSOR:         asciz_out("WM_SETCURSOR"); break;
          case WM_SETFOCUS:          asciz_out("WM_SETFOCUS"); break;
          case WM_SETFONT:           asciz_out("WM_SETFONT"); break;
          case WM_SETREDRAW:         asciz_out("WM_SETREDRAW"); break;
          case WM_SETTEXT:           asciz_out("WM_SETTEXT"); break;
          case WM_SHOWWINDOW:        asciz_out("WM_SHOWWINDOW"); break;
          case WM_SIZE:              asciz_out("WM_SIZE"); break;
          case WM_SIZECLIPBOARD:     asciz_out("WM_SIZECLIPBOARD"); break;
          case WM_SPOOLERSTATUS:     asciz_out("WM_SPOOLERSTATUS"); break;
          case WM_SYSCHAR:           asciz_out("WM_SYSCHAR"); break;
          case WM_SYSCOLORCHANGE:    asciz_out("WM_SYSCOLORCHANGE"); break;
          case WM_SYSCOMMAND:        asciz_out("WM_SYSCOMMAND"); break;
          case WM_SYSDEADCHAR:       asciz_out("WM_SYSDEADCHAR"); break;
          case WM_SYSKEYDOWN:        asciz_out("WM_SYSKEYDOWN"); break;
          case WM_SYSKEYUP:          asciz_out("WM_SYSKEYUP"); break;
          case WM_SYSTEMERROR:       asciz_out("WM_SYSTEMERROR"); break;
          case WM_TIMECHANGE:        asciz_out("WM_TIMECHANGE"); break;
          case WM_TIMER:             asciz_out("WM_TIMER"); break;
          case WM_UNDO:              asciz_out("WM_UNDO"); break;
          case WM_USER:              asciz_out("WM_USER"); break;
          case WM_VKEYTOITEM:        asciz_out("WM_VKEYTOITEM"); break;
          case WM_VSCROLL:           asciz_out("WM_VSCROLL"); break;
          case WM_VSCROLLCLIPBOARD:  asciz_out("WM_VSCROLLCLIPBOARD"); break;
          case WM_WINDOWPOSCHANGED:  asciz_out("WM_WINDOWPOSCHANGED"); break;
          case WM_WINDOWPOSCHANGING: asciz_out("WM_WINDOWPOSCHANGING"); break;
          case WM_WININICHANGE:      asciz_out("WM_WININICHANGE"); break;
        }
      asciz_out(",");
      dez_out(lParam); asciz_out(",");
      dez_out(wParam); asciz_out("}");
    }
#endif

#ifdef WATCOM
# We need our own malloc() since the default one only works
# for blocks of size < 64 KB.
#if 0 # Oops, WATCOM doesn't have Global32Alloc().
  global void* malloc32 (unsigned int size);
  global void* malloc32(size)
    var unsigned int size;
    { var WORD selector;
      if (Global32Alloc(size, # size of block to allocate
                        &selector,
                        size, # maximum size (for Global32Realloc)
                        0     # sharing flag
         )             )
        return NULL;
      return MAKELP(selector,0);
    }
  global void free32 (void* ptr);
  global void free32(ptr)
    var void* ptr;
    { Global32Free(SELECTOROF(ptr)); }
#endif
#if 1
  typedef struct { HGLOBAL handle; uintB usable_memory[unspecified]; }
          memblockheader;
  global void* malloc32 (unsigned int size);
  global void* malloc32(size)
    var unsigned int size;
    { var HGLOBAL hglobal =
        GlobalAlloc(GPTR,size+offsetofa(memblockheader,usable_memory));
asciz_out("{malloc32@"); hex_out(&malloc32); asciz_out(":"); hex_out(hglobal); asciz_out("}");
      if (!hglobal) return NULL;
     {var void* address = GlobalLock(hglobal);
asciz_out("{malloc32@"); hex_out(&malloc32); asciz_out(";"); hex_out(address); asciz_out("}");
      ((memblockheader*)address)->handle = hglobal; # <-- here it crashes
asciz_out("{malloc32 done}");
      return &((memblockheader*)address)->usable_memory[0];
    }}
  global void free32 (void* address);
  global void free32(address)
    var void* address;
    { var memblockheader* ptr =
        (memblockheader*)((long)address - offsetofa(memblockheader,usable_memory));
      var HGLOBAL hglobal = ptr->handle;
asciz_out("{free32:"); hex_out(hglobal); asciz_out("}");
      GlobalUnlock(hglobal);
      GlobalFree(hglobal);
    }
#endif
#endif

# Call external programs.
global int system (CONST char* command);
global int system(command)
  var CONST char* command;
  { return !(WinExec(command,SW_SHOWNORMAL) >= 32); }

