# Stuff for MS-Windows 3.1
# Bruno Haible 21.4.1995

#include "lispbibl.c"

# Any Windows program needs this:
#define _STDDEF_H_INCLUDED  # WATCOM: avoid clash because of offsetof()
#define _STDDEF_H           # EMX: avoid warning because of offsetof()
#define INCLUDE_SHELLAPI_H  # include <shellapi.h> for drag&drop support
#include <windows.h>

# Some useful definitions:
# define min(x,y)  ((x) < (y) ? (x) : (y))
# define max(x,y)  ((x) > (y) ? (x) : (y))
struct xy { int x; int y; };

# A couple of magic identifiers used to access the elements of the resource:
#include "clispwin.h"

# Debugging support:
/* #define WIN_DEBUG */
#ifdef WIN_DEBUG
  #define WINDEBUG(x) x
  local void event_out (const char * caller, HWND hWnd, mywindow w, UINT message, WPARAM wParam, LPARAM lParam);
#else
  #define WINDEBUG(x)
#endif


# Accessing the "command line":
# There is no such thing. (Well, the RSXWIN startup code saves something as
# __argc and __argv, but I don't know what it is.)
# We load the arguments from a file called CLISPWIN.INI, in the Windows
# directory.
#
# Its layout is roughly like this:
#   [CLISP]
#   Arglist="[the command line arguments]"
#   Language="english"
#   Lines=25
#   Columns=80
#   Font="Fixedsys"
#   Fontsize=16
#   Leading=-1
# Fonts should be fixed width (such as "Courier New", "Fixedsys",
# or "Lucinda Sans Typewriter").
# Font sizes should be in the 10..20 point range.
# Leading specifies an adjustment for inter-line spacing.

#define ini_filename  "CLISPWIN.INI"
#define ini_section   "CLISP"

# Retrieve the value of a "ini file environment variable".
local char* getenv_ini_string (char* name);
local int getenv_ini_int (char* name);
local char* getenv_ini_string(name)
  var char* name;
  { var uintL bufsize = 50;
    var char* buf;
    while (1)
      { buf = malloc(bufsize);
        if (!buf) return NULL;
        if (GetPrivateProfileString(ini_section, name, "",
                                    buf, bufsize,
                                    ini_filename
                                   )
            < bufsize-1
           )
          return buf;
        free(buf);
        bufsize *= 2; # retry with larger buffer
      }
  }
local int getenv_ini_int(name)
  var char* name;
  { return GetPrivateProfileInt(ini_section, name, 0,
                                ini_filename
                               );
  }


# The program's "instance" and "command line".
local HINSTANCE global_hInstance;
local LPSTR global_lpszCmdLine;

# Our (modified) command line arguments.
local char* * argv;
local int argc;

# Get the command line args, maybe from the ini file.
local void extend_args (void);
local void extend_args()
  { local var char* dummy_argv[] = { "CLISPWIN" };
    var char* arglist;
    argv = dummy_argv; argc = 1;
    if (!(global_lpszCmdLine[0]=='\0'))
      # If args have been given, they override the INI file args.
      { begin_system_call();
        arglist = malloc(strlen(global_lpszCmdLine)+1);
        end_system_call();
        if (!arglist) return;
        strcpy(arglist,global_lpszCmdLine);
      }
      else
      { begin_system_call();
        arglist = getenv_ini_string("Arglist");
        end_system_call();
        if (!arglist) return;
      }
    # Split up into arguments, separated by whitespace:
    #define whitespacep(c)  (((c)==' ') || ((c)=='\t') || ((c)=='\n'))
    { var char* ptr = arglist;
      var uintC count = 0;
      loop
        { if (*ptr=='\0') break;
          if (whitespacep(*ptr))
            { ptr++; }
            else
            { count++;
              until ((*ptr=='\0') || whitespacep(*ptr))
                { ptr++; }
            }
        }
      if (count==0)
        return; # no new arguments
      begin_system_call();
     {var int new_argc = 1 + count;
      var char* * new_argv = malloc(new_argc*sizeof(char*));
      end_system_call();
      if (!new_argv) return;
      new_argv[0] = argv[0]; count = 1;
      ptr = arglist;
      loop
        { if (*ptr=='\0') break;
          if (whitespacep(*ptr))
            { ptr++; }
            else
            { new_argv[count] = ptr;
              until ((*ptr=='\0') || whitespacep(*ptr))
                { ptr++; }
              if (*ptr=='\0') break;
              *ptr = '\0';
              ptr++;
              count++;
            }
        }
      argc = new_argc; argv = new_argv;
    }}
    #undef whitespacep
  }


# The type of our event handler functions:
typedef long (*eventhandler) (mywindow w, UINT message, WPARAM wParam, LPARAM lParam);

# The type of our events:
typedef enum { ev_key, ev_menu, ev_drop, ev_paste, ev_eof } event_type;
typedef struct event { event_type type;
                       union { cint key; WPARAM wParam; char* pastebuf; } u;
                     }
        event;

# The type of our windows:
struct mywindow
       { HWND hWnd; # its MS-Windows handle
         boolean in_focus; # whether this window holds the focus
         boolean for_output; # whether this window is an output stream
                             # or only presents static text
         boolean for_input; # whether this window is an input stream
                            # or only presents output
         # a list of event handlers, most specific first
         eventhandler eventhandler1, eventhandler2, eventhandler3, eventhandler4;
         # data for text windows:
         struct {
                  int width;  # text width, in characters
                  int height; # text height, in characters
                  # The text displayed in the window:
                  char* * contents; # an array of length height,
                                    # each of length width
                  # User feedback during Cut&Paste operation:
                  # Some of the text may be selected, inverted on the screen.
                  struct xy selpoint1, selpoint2;
                  enum { none, # nothing selected, selpoint1/2 invalid
                         notmoved, # one mouse click, no move, selpoint1 = selpoint2
                         marking, # mouse is moving
                         marked # selection done
                       }
                       in_marking;
         # data for text windows which support output:
                  # To focus the user's attention, we place a text cursor at
                  # a point we call the "current line/column".
                  struct xy cursor;
                  boolean cursor_visible; # is the cursor currently visible?
                }
                text;
         # data for windows which support input:
         struct { # keyboard typeahead buffer, event queue
                  event* ev_fifo;
                  uintL ev_fifo_size;
                  event* ev_fifo_out;
                  event* ev_fifo_in;
                  # The current state of a finite state machine:
                  boolean in_paste; # getting our input from a paste buffer
                  event paste_ev; # the current paste event
                  char* paste_ptr; # pointer into the paste buffer
                }
                input;
         # data for text windows which support input:
         struct {
                  char* line;      # current line
                  uintL line_size; # its malloc() size
                  uintL count;     # number of characters in the line
                  uintL index;     # number of characters already consumed
                }
                textinput;
         # linked list of windows:
         struct mywindow * next;
       };


# This is the list of all windows we are managing.
local mywindow all_windows = NULL;

# Add a window to the list all_windows:
local void register_window (mywindow w);
local void register_window(w)
  var mywindow w;
  { w->next = all_windows; all_windows = w; }
# The converse:
local void unregister_window (mywindow w);
local void unregister_window(w)
  var mywindow w;
  { var mywindow * p = &all_windows;
    until (*p == NULL) if (*p == w) { *p = w->next; return; }
  }

# Lookup a window, given its handle:
local mywindow find_window (HWND hWnd);
local mywindow find_window(hWnd)
  var HWND hWnd;
  { var mywindow w;
    for (w = all_windows; !(w==NULL); w = w->next)
      { if (w->hWnd == hWnd)
          { return w; }
      }
    # If hWnd wasn't found, it belongs to a window that is being created.
    return NULL;
  }

# The event handler for the whole application, see below.
local long _export FAR PASCAL all_event (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);

# The default event handler, see below.
local long default_event (mywindow w, UINT message, WPARAM wParam, LPARAM lParam);


# A library for text windows:

#include "wintext.c"


# A status window:

#include "winstat.c"


# The main window:

#include "winmain.c"


# A default event handler:
local long default_event (w,message,wParam,lParam)
  var mywindow w;
  var UINT message;
  var WPARAM wParam;
  var LPARAM lParam;
  { switch (message)
      { case WM_SETFOCUS:
          in_focus = TRUE;
          show_mouse_cursor();
          break;
        case WM_KILLFOCUS:
          in_focus = FALSE;
          break;
        default:
          break;
      }
    return DefWindowProc(w->hWnd,message,wParam,lParam);
  }

# The event handler for the whole application.
local long _export FAR PASCAL all_event(hWnd,message,wParam,lParam)
  var HWND hWnd;
  var UINT message;
  var WPARAM wParam;
  var LPARAM lParam;
  { var mywindow w = find_window(hWnd);
    WINDEBUG( event_out("all_event",hWnd,w,message,wParam,lParam); )
    if (w)
      return w->eventhandler1(w,message,wParam,lParam);
    # If hWnd wasn't found, it belongs to a window that is being created.
    # We haven't yet had a chance to call register_window().
    return DefWindowProc(hWnd,message,wParam,lParam);
  }


# This is the main program, called by Windows.
# You may also call it the application's INITIALIZE-INSTANCE method.
global int PASCAL WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCmdLine, int nCmdShow);
extern int clisp_main (int argc, char* argv[]);
global int PASCAL WinMain(hInstance,hPrevInstance,lpszCmdLine,nCmdShow)
  var HINSTANCE hInstance;
  var HINSTANCE hPrevInstance;
  var LPSTR lpszCmdLine;
  var int nCmdShow;
  { var int retval;

    #ifdef EMUNIX
      # Initialize SP_start so that begin_system_call() will work.
      # SP() = %esp = 0x0000FE38 here.
      SP_start = (void*)(SP() - 0x4000);
      # This is preliminary:
      # The system stack is 16 KB below the normal stack, for now.
    #endif

    global_hInstance = hInstance;
    global_lpszCmdLine = lpszCmdLine;
    # hPrevInstance will be NULL since we have only one thread running.

    # Look up the INI environment.
    extend_args();

    # Mouse cursors.
    init_cursors();
    in_focus = FALSE; in_wait = FALSE; in_gc = FALSE; mouse_hidden = FALSE;

    # Load accelerator table.
    begin_system_call();
    hAccel = LoadAccelerators(global_hInstance,"ACCELERATORS_1");
    end_system_call();

    if (!init_text_class())
      return FALSE;

    # Determine character sizes and colours.
    get_char_info();

    # Main window.
    { var int main_width, main_height;
      begin_system_call();
      main_width = getenv_ini_int("Columns");
      if (!main_width) { main_width = 80; }
      main_height = getenv_ini_int("Lines");
      if (!main_height) { main_height = 25; }
      end_system_call();
      if (!main_create(main_width,main_height))
        return FALSE;
    }

    # Status window.
    status_create(main_window->hWnd);

    # Display the main window.
    begin_system_call();
    ShowWindow(main_window->hWnd,nCmdShow);
    UpdateWindow(main_window->hWnd);
    end_system_call();

    # Go, CLISP, go!
    retval = clisp_main(argc,argv);

    begin_system_call();
    DestroyWindow(main_window->hWnd);
    end_system_call();

    return retval;
  }


# Other auxiliary functions:

#include "winaux.c"


# TODO:
# Fix Cut&Paste. It does not work yet.
# ANSI to OEM and back?!?!? This is a mess:
#   - files may be in ISO or DOS character set. What do we assume?
#   - Type Alt-128 at the keyboard, get C-cedilla, which has code 199.
#     The up-/downcase tables in io.d do not correctly downcase this.
#   - The mode of a font can be enquired using GetTextMetrics.
# Support for several/switchable font.
# ev_eof shall terminate the application. Currently (sys::%exit) is the only way.
# Endless loop WM_QUIT -> end_of_main -> WM_DESTROY -> WM_QUIT ...
# About and Copyright boxes are too large, choose help text or something else.
# Update status_window more regularly, use interruptp() macro for this.
# Define an "interrupt event" which is acknowledged by interruptp().
# Mouse click into the line during winterm_readchar shall position the cursor.
# Menu points for LOAD, DRIBBLE, STEP, ABORT, CONTINUE and many more.
# CLOSE rectangle for the status window.
# SCREEN::MAKE-WINDOW crashes.
# Translate the menu into 3 languages, choose it at runtime.

