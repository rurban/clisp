# A status window, just like Xlisp's one.
# It displays the memory usage.

#include <stdio.h> # declares sprintf()

local boolean status_active; # TRUE if status window on screen
local mywindow status; # status window, or NULL when not on screen

#define status_width  22
#define status_height  2

# Update the status window's contents.
local void status_update (void);
local void status_update()
  { var mywindow w = status;
    if (!w) return;
   {var boolean modified = FALSE;
    { var char buf[status_width+1];
      sprintf(buf," Bytes used: %8ld ",used_space());
      if (memcmp(w->text.contents[0],buf,status_width))
        { memcpy(w->text.contents[0],buf,status_width); modified = TRUE; }
    }
    { var char buf[status_width+1];
      sprintf(buf," Bytes free: %8ld ",free_space());
      if (memcmp(w->text.contents[1],buf,status_width))
        { memcpy(w->text.contents[1],buf,status_width); modified = TRUE; }
    }
    # No update is needed if the data to be displayed didn't change.
    if (modified)
      { # Force status window update:
        begin_system_call();
        InvalidateRect(w->hWnd,NULL,TRUE);
        UpdateWindow(w->hWnd);
        end_system_call();
      }
  }}

# The status window uses a timer to periodically update its contents.
#define timer_id  1

# Switch status window active/inactive.
local void status_on (void);
local void status_off (void);
local void status_on()
  { status_active = TRUE;
    begin_system_call();
    if (!SetTimer(status->hWnd, # window which gets timer messages
                  timer_id,     # timer identifier
                  1000,         # timeout duration / ms
                  NULL          # timer procedure
       )         )
      { status_active = FALSE; end_system_call(); return; } # failure to set the timer
    ShowWindow(status->hWnd,SW_SHOWNOACTIVATE);
    end_system_call();
  }
local void status_off()
  { begin_system_call();
    KillTimer(status->hWnd, # window associated with the timer
              timer_id      # timer identifier
             );
    status_active = FALSE;
    ShowWindow(status->hWnd,SW_HIDE);
    end_system_call();
  }

# The event handler for the status window.
local long status_event (mywindow w, UINT message, WPARAM wParam, LPARAM lParam);
local long status_event(w,message,wParam,lParam)
  var mywindow w;
  var UINT message;
  var WPARAM wParam;
  var LPARAM lParam;
  { WINDEBUG( event_out("status_event",w->hWnd,w,message,wParam,lParam); )
    switch (message)
      { case WM_TIMER:
          if (status_active) { status_update(); }
          return 0;
        case WM_CLOSE:
          if (status_active) { status_off(); }
          return 0;
        default:
          break;
      }
    return w->eventhandler3(w,message,wParam,lParam);
  }

# Create the status window.
local boolean status_create (HWND parent_hWnd);
local boolean status_create(parent_hWnd)
  var HWND parent_hWnd;
  { var mywindow w;
    status = NULL;
    begin_system_call();
    w = (struct mywindow *) malloc(sizeof(struct mywindow));
    if (!w) { MessageBeep(MB_OK); end_system_call(); return FALSE; }
   {var HWND hWnd =
      CreateWindow(text_class_name,          # registered class name
                   "CLISP (room)",           # window title
                   WS_OVERLAPPED|WS_CAPTION, # window style
                   CW_USEDEFAULT,            # horizontal position
                   CW_USEDEFAULT,            # vertical position
                   status_width*text_char_size.x # window width
                    + 2*GetSystemMetrics(SM_CXBORDER),
                   status_height*text_char_size.y # window height
                    + 2*GetSystemMetrics(SM_CYBORDER)
                    + GetSystemMetrics(SM_CYCAPTION),
                   parent_hWnd,              # parent window
                   NULL,                     # menu oder child window id
                   global_hInstance,         # application instance
                   global_lpszCmdLine        # window-creation data
                  );
    if (!hWnd)
      { free(w); MessageBeep(MB_OK); end_system_call(); return FALSE; }
    SetMenu(hWnd,NULL); # no menu on status window
    # Set the background brush. (See the doc of WNDCLASS for the +1.)
    SetClassWord(hWnd,GCW_HBRBACKGROUND,COLOR_WINDOW+1);
    # Fill in w:
    w->hWnd = hWnd;
    w->in_focus = FALSE;
    w->for_output = w->for_input = FALSE;
    w->eventhandler1 = text_event;
    w->eventhandler2 = status_event;
    w->eventhandler3 = default_event;
    w->text.width = status_width; w->text.height = status_height;
    w->text.contents = malloc(status_height*sizeof(char*)); # check??
    { var uintL y;
      for (y = 0; y < status_height; y++)
        { var uintL x;
          var char* line = (char*)malloc(status_width*sizeof(char)); # check??
          for (x = 0; x < status_width; x++) line[x] = ' ';
          w->text.contents[y] = line;
    }   }
    w->text.in_marking = none;
    end_system_call();
    register_window(w);
    status = w;
    return TRUE;
  }}

