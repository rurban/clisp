# The main window has input, output and menu.

global mywindow main_window; # main window

# The event handler for the main window.
local long main_event (mywindow w, UINT message, WPARAM wParam, LPARAM lParam);
local long main_event(w,message,wParam,lParam)
  var mywindow w;
  var UINT message;
  var WPARAM wParam;
  var LPARAM lParam;
  { WINDEBUG( event_out("main_event",w->hWnd,w,message,wParam,lParam); )
    switch (message)
      { case WM_INITMENUPOPUP:
          if (HIWORD(lParam)==0)
            { var HMENU hmenu = (HMENU) wParam;
              EnableMenuItem(hmenu,M_COPY, main_window->text.in_marking==marked ? MF_ENABLED : MF_GRAYED);
              EnableMenuItem(hmenu,M_PASTE, IsClipboardFormatAvailable(CF_TEXT) ? MF_ENABLED : MF_GRAYED);
              EnableMenuItem(hmenu,M_STATUS, status ? MF_ENABLED : MF_GRAYED);
              CheckMenuItem(hmenu,M_STATUS, status_active ? MF_CHECKED : MF_UNCHECKED);
              return 0;
            }
          break;
        case WM_COMMAND: # Menu selection
          switch (wParam)
            { case M_EXIT:
                SendMessage(w->hWnd,WM_CLOSE,0,0);
                return 0;
              case M_ABOUT:
                # display a mix of the files README, SUMMARY and ANNOUNCE
                MessageBox(w->hWnd,
                           #include "winabout.c"
                           ,
                           "About CLISP",
                           MB_ICONINFORMATION | MB_OK
                          );
                return 0;
              case M_COPYRIGHT:
                # display the files COPYRIGHT and GNU-GPL
                MessageBox(w->hWnd,
                           #include "wincopyr.c"
                           ,
                           "Copyright of CLISP",
                           MB_ICONINFORMATION | MB_OK
                          );
                return 0;
              case M_STATUS:
                if (status)
                  { if (!status_active)
                      status_on();
                    else
                      status_off();
                  }
                return 0;
              default:
                break;
            }
          break;
        case WM_DESTROY: # while destroying the main window
          if (status_active) { status_off(); } # destroy subwindows
          PostQuitMessage(final_exitcode); # application goes into WM_QUIT state
          return 0;
        default:
          break;
      }
    return w->eventhandler4(w,message,wParam,lParam);
  }

# Create the main window.
local boolean main_create (int main_width, int main_height);
local boolean main_create(main_width,main_height)
  var int main_width;
  var int main_height;
  { var mywindow w;
    begin_system_call();
    w = (struct mywindow *) malloc(sizeof(struct mywindow));
    if (!w) { MessageBeep(MB_OK); end_system_call(); return FALSE; }
   {var HWND hWnd =
      CreateWindow(text_class_name,           # registered class name
                   "CLISP",                   # window title
                   WS_OVERLAPPED | WS_CAPTION # window style
                    | WS_SYSMENU | WS_MINIMIZEBOX,
                   CW_USEDEFAULT,             # horizontal position
                   CW_USEDEFAULT,             # vertical position
                   main_width*text_char_size.x         # window width
                    + 2*GetSystemMetrics(SM_CXBORDER),
                   main_height*text_char_size.y        # window height
                    + 2*GetSystemMetrics(SM_CYBORDER)
                    + GetSystemMetrics(SM_CYCAPTION)
                    + GetSystemMetrics(SM_CYMENU),
                   HWND_DESKTOP,              # parent window
                   NULL,                      # menu oder child window id
                   global_hInstance,          # application instance
                   global_lpszCmdLine         # window-creation data
                  );
    # If we didn't succeed in creating a window, abort immediately.
    if (!hWnd)
      { free(w); MessageBeep(MB_OK); end_system_call(); return FALSE; }
    # Set the background brush. (See the doc of WNDCLASS for the +1.)
    SetClassWord(hWnd,GCW_HBRBACKGROUND,COLOR_WINDOW+1);
    # Fill in w:
    w->hWnd = hWnd;
    w->in_focus = FALSE;
    w->for_output = w->for_input = TRUE;
    w->eventhandler1 = text_event;
    w->eventhandler2 = textio_event;
    w->eventhandler3 = main_event;
    w->eventhandler4 = default_event;
    w->text.width = main_width; w->text.height = main_height;
    w->text.contents = malloc(main_height*sizeof(char*)); # check??
    { var uintL y;
      for (y = 0; y < main_height; y++)
        w->text.contents[y] = malloc(main_width*sizeof(char)); # check??
    }
    w->text.in_marking = none;
    w->text.cursor.x = 0; w->text.cursor.y = 0;
    w->text.cursor_visible = FALSE;
    w->input.ev_fifo = (event*) malloc((w->input.ev_fifo_size = 16)*sizeof(event));
    w->input.ev_fifo_out = w->input.ev_fifo_in = &w->input.ev_fifo[0];
    w->input.in_paste = FALSE;
    w->textinput.line = (char*) malloc((w->textinput.line_size = 80)*sizeof(char));
    w->textinput.count = 0; w->textinput.index = 0;
    end_system_call();
    register_window(w);
    text_clear(w);
    main_window = w;
    return TRUE;
  }}


# Keyboard input from the main window:
global boolean win_main_kbhit (void);
global cint win_main_getch (void);
global boolean win_main_kbhit()
  { return win_kbhit(main_window); }
global cint win_main_getch()
  { return win_getch(main_window); }

