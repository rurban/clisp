# Text windows for MS-Windows

# TODO: scrollbar, multiline, resizability, ISO/DOS character set switch,
#       choosing font


# Our mouse cursor policy is like this:
# The arrow while the program awaits input.
# The GC panel while garbage collection.
# The busy bee (hour glass) while the program is computing otherwise.
# No mouse cursor after keyboard input, until the mouse moves again.

# Of course, we are allowed to change the cursor shape only while
# we own the focus.

local HCURSOR arrow_cursor;
local HCURSOR busy_cursor;
local HCURSOR gc_cursor;
#define no_cursor  NULL

local void init_cursors (void);
local void init_cursors()
  { begin_system_call();
    arrow_cursor = LoadCursor(NULL,IDC_ARROW);
    busy_cursor = LoadCursor(NULL,IDC_WAIT);
    gc_cursor = LoadCursor(global_hInstance,"CURSOR_1");
    end_system_call();
  }

# This flag is set while one of our windows owns the focus.
local boolean in_focus;

# This flag is set while one of our windows awaits input.
local boolean in_wait;

# This flag is set during garbage collection.
local boolean in_gc;

# This flag is set after keyboard input and before mouse movement.
local boolean mouse_hidden;

local void show_mouse_cursor (void);
local void show_mouse_cursor()
  { if (in_focus)
      { begin_system_call();
        if (mouse_hidden) { SetCursor(no_cursor); }
        elif (in_wait) { SetCursor(arrow_cursor); }
        elif (in_gc) { SetCursor(gc_cursor); }
        else { SetCursor(busy_cursor); }
        end_system_call();
  }   }

# We get notified about garbage collection.
global void windows_note_gc_start (void);
global void windows_note_gc_end (void);
global void windows_note_gc_start()
  { if (in_focus)
      { begin_system_call(); SetCursor(gc_cursor); end_system_call(); }
    in_gc = TRUE;
  }
global void windows_note_gc_end()
  { in_gc = FALSE;
    if (in_focus)
      { begin_system_call(); SetCursor(busy_cursor); end_system_call(); }
  }


# PART 1 : Text windows for static display, no scrolling

# For simplicity, we assume all text windows share the same font and colors.

local HFONT text_font; # our text font
local struct xy text_char_size; # size of a single text character
local int text_char_ascent;
local COLORREF text_colour; # text colour
local COLORREF bg_colour;   # background colour

# Get font and character size info, given a window handle:
local void get_char_info (void);
local void get_char_info()
  { var HDC DC = GetDC(NULL); # the screen's device context
    var char* fontname;
    var int fontsize;
    begin_system_call();
    fontname = getenv_ini_string("Font");
    if (!fontname || (strlen(fontname)==0))
      { fontname = "FIXEDSYS"; }
    fontsize = getenv_ini_int("FontSize");
    if (!fontsize)
      { fontsize = 16; }
    text_font = CreateFont( -fontsize, # font height
                            0,         # character width
                            0,         # escapement of line text
                            0,         # angle of base line and x-axis
                            0,         # font weight
                            0,         # italic
                            0,         # underline
                            0,         # strikeout
                            0,         # character set
                            OUT_DEFAULT_PRECIS, # output precision
                            0,         # clipping precision
                            0,         # output quality
                            FIXED_PITCH|FF_DONTCARE, # pitch, quality
                            fontname   # typeface name
                          );
    # or: text_font = GetStockObject(OEM_FIXED_FONT);
    { var HFONT saved_font = SelectObject(DC,text_font);
      var TEXTMETRIC tm;
      var int leading;
      GetTextMetrics(DC,&tm);
      text_char_size.x = tm.tmAveCharWidth;
      leading = getenv_ini_int("Leading");
      if (!leading)
        { leading = tm.tmExternalLeading; }
      text_char_size.y = tm.tmHeight + leading;
      text_char_ascent = tm.tmAscent;
      SelectObject(DC,saved_font);
    }
    ReleaseDC(NULL,DC);
    text_colour = GetSysColor(COLOR_WINDOWTEXT);
    bg_colour = GetSysColor(COLOR_WINDOW);
    end_system_call();
  }

# Get an auxiliary array text_spacing, of length >= count.
local short* get_text_spacing (uintL count);
local short* text_spacing_array = NULL; # the array
local uintL text_spacing_len;           # its length
local short* get_text_spacing(count)
  var uintL count;
  { if (!text_spacing_array || text_spacing_len < count)
      { begin_system_call();
        if (text_spacing_array) { free(text_spacing_array); }
        text_spacing_array = (short*) malloc(count*sizeof(short)); # check??
        end_system_call();
        text_spacing_len = count;
       {var uintL i;
        for (i = 0; i < count; i++)
          { text_spacing_array[i] = text_char_size.x; }
      }}
    return text_spacing_array;
  }

# Invert a region of text between two points.
local void invert_text (mywindow w, struct xy endpoint1, struct xy endpoint2);
local void invert_text(w,endpoint1,endpoint2)
  var mywindow w;
  var struct xy endpoint1;
  var struct xy endpoint2;
  { var HDC DC;
    var struct xy point1;
    var struct xy point2;
    if ((endpoint1.y < endpoint2.y)
        || ((endpoint1.y == endpoint2.y) && (endpoint1.x <= endpoint2.x))
       )
      { point1 = endpoint1; point2 = endpoint2; }
      else
      { point1 = endpoint2; point2 = endpoint1; }
    # Split into 3 rectangles like this:
    #                *********
    #    *********************
    #    *********************
    #    ******
    begin_system_call();
    DC = GetDC(w->hWnd);
    { var RECT r;
      if (point1.y == point2.y)
        { r.left   = point1.x * text_char_size.x;
          r.right  = point2.x * text_char_size.x;
          r.top    = point1.y * text_char_size.y;
          r.bottom = (point2.y+1) * text_char_size.y;
          InvertRect(DC,&r);
        }
        else
        { r.left   = point1.x * text_char_size.x;
          r.right  = w->text.width * text_char_size.x;
          r.top    = point1.y * text_char_size.y;
          r.bottom = (point1.y+1) * text_char_size.y;
          InvertRect(DC,&r);
          if (point1.y+1 < point2.y)
            { r.left   = 0 * text_char_size.x;
              r.right  = w->text.width * text_char_size.x;
              r.top    = (point1.y+1) * text_char_size.y;
              r.bottom = point2.y * text_char_size.y;
              InvertRect(DC,&r);
            }
          r.left   = 0 * text_char_size.x;
          r.right  = point2.x * text_char_size.x;
          r.top    = point2.y * text_char_size.y;
          r.bottom = (point2.y+1) * text_char_size.y;
          InvertRect(DC,&r);
        }
    }
    ReleaseDC(w->hWnd,DC);
    end_system_call();
  }

# Invert the marked region.
local void invert_marked_region (mywindow w);
local void invert_marked_region(w)
  var mywindow w;
  { if ((w->text.in_marking == none) || (w->text.in_marking == notmoved))
      return;
    invert_text(w,w->text.selpoint1,w->text.selpoint2);
  }

# Unmark the marked region.
local void unmark_marked_region (mywindow w);
local void unmark_marked_region(w)
  var mywindow w;
  { if (!(w->text.in_marking == none))
      { invert_marked_region(w); w->text.in_marking = none; }
  }

# Update the entire text area.
local void text_event_paint (mywindow w);
local void text_event_paint(w)
  var mywindow w;
  { begin_system_call();
   {var PAINTSTRUCT PS;
    var HDC DC = BeginPaint(w->hWnd,&PS);
    var HFONT saved_font = SelectObject(DC,text_font);
    SetTextColor(DC,text_colour); SetBkColor(DC,bg_colour);
    { var int xmin = floor(PS.rcPaint.left,text_char_size.x);
      var int xmax = ceiling(PS.rcPaint.right,text_char_size.x);
      var int xlen = xmax - xmin;
      var int ymin = floor(PS.rcPaint.top,text_char_size.y);
      var int ymax = ceiling(PS.rcPaint.bottom,text_char_size.y);
      var int y;
      if (ymax > w->text.height) { ymax = w->text.height; }
      for (y = ymin; y < ymax; y++)
        ExtTextOut(DC,                         # handle of device context
                   xmin * text_char_size.x,    # x of starting position
                   y * text_char_size.y,       # y of starting position
                   0,                          # rectangle type
                   NULL,                       # rectangle
                   &w->text.contents[y][xmin], # string
                   xlen,                       # length of string
                   get_text_spacing(xlen)      # spacing between characters
                  );
    }
    SelectObject(DC,saved_font);
    invert_marked_region(w);
    EndPaint(w->hWnd,&PS);
    end_system_call();
  }}

# Update a line within given bounds, and leave marking mode.
local void text_event_paintline (mywindow w, int y, int xleft, int xright);
local void text_event_paintline(w,y,xleft,xright)
  var mywindow w;
  var int y;
  var int xleft;
  var int xright;
  { unmark_marked_region(w);
    if (xleft < xright)
      { begin_system_call();
       {var HDC DC = GetDC(w->hWnd);
        var HFONT saved_font = SelectObject(DC,text_font);
        SetTextColor(DC,text_colour); SetBkColor(DC,bg_colour);
        {var int xlen = xright - xleft;
         ExtTextOut(DC,                          # handle of device context
                    xleft * text_char_size.x,    # x of starting position
                    y * text_char_size.y,        # y of starting position
                    0,                           # rectangle type
                    NULL,                        # rectangle
                    &w->text.contents[y][xleft], # string
                    xlen,                        # length of string
                    get_text_spacing(xlen)       # spacing between characters
                   );
        }
        SelectObject(DC,saved_font);
        ReleaseDC(w->hWnd,DC);
        end_system_call();
      }}
  }

# The selection.

# Convert screen coordinates to text coordinates
local void screen_to_text_xy (mywindow w, int* x, int* y);
local void screen_to_text_xy(w,x_,y_)
  var mywindow w;
  var int* x_;
  var int* y_;
  { var int x = *x_;
    var int y = *y_;
    if (x < 0) { x = 0; } # better safe than sorry
    if (y < 0) { y = 0; } # better safe than sorry
    x = floor(x,text_char_size.x);
    y = floor(y,text_char_size.y);
    if (x >= w->text.width) { x = w->text.width-1; } # better safe than sorry
    if (y >= w->text.height) { y = w->text.height-1; } # better safe than sorry
    *x_ = x; *y_ = y;
  }

local void begin_selection (mywindow w, int x, int y);
local void begin_selection(w,x,y)
  var mywindow w;
  var int x;
  var int y;
  { if (!(w->text.in_marking == none))
      { invert_marked_region(w); }
    screen_to_text_xy(w,&x,&y);
    w->text.selpoint1.x = x; w->text.selpoint1.y = y;
    w->text.selpoint2.x = x; w->text.selpoint2.y = y;
    w->text.in_marking = notmoved;
  }

local void adjust_selection (mywindow w, int x, int y);
local void adjust_selection(w,x,y)
  var mywindow w;
  var int x;
  var int y;
  { if ((w->text.in_marking == marking) || (w->text.in_marking == notmoved))
      { screen_to_text_xy(w,&x,&y);
        if (!((w->text.selpoint2.x==x) && (w->text.selpoint2.y==y)))
          {
            #if 0 # unoptimized
              invert_marked_region(w);
              w->text.in_marking = marking;
              w->text.selpoint2.x = x; w->text.selpoint2.y = y;
              invert_marked_region(w);
            #else # optimized
              # Invert only the region between the new and old selpoint2:
              var struct xy new_selpoint2;
              new_selpoint2.x = x; new_selpoint2.y = y;
              invert_text(w,w->text.selpoint2,new_selpoint2);
              w->text.in_marking = marking;
              w->text.selpoint2 = new_selpoint2;
            #endif
  }   }   }

local void finish_selection (mywindow w);
local void finish_selection(w)
  var mywindow w;
  { if (w->text.in_marking==marking) { w->text.in_marking = marked; }
    elif (w->text.in_marking==notmoved) { w->text.in_marking = none; }
  }

local void copy_selection (mywindow w);
local void copy_selection(w)
  var mywindow w;
  { var struct xy point1;
    var struct xy point2;
    if ((w->text.selpoint1.y < w->text.selpoint2.y)
        || ((w->text.selpoint1.y == w->text.selpoint2.y)
            && (w->text.selpoint1.x <= w->text.selpoint2.x)
       )   )
      { point1 = w->text.selpoint1; point2 = w->text.selpoint2; }
      else
      { point1 = w->text.selpoint2; point2 = w->text.selpoint1; }
    begin_system_call();
   {# Each line is to be terminated by CR/LF, except for the last.
    var int totalsize = (point2.y - point1.y) * (w->text.width + 2) + point2.x - point1.x + 1;
    # Fill up a segment up global memory:
    var HANDLE hGlobalMemory = GlobalAlloc(GHND,totalsize);
    if (hGlobalMemory)
      { var char FAR * cp = GlobalLock(hGlobalMemory);
        var int x = point1.x;
        var int y = point1.y;
        while (y < point2.y)
          { while (x < w->text.width) { *cp++ = w->text.contents[y][x]; x++; }
            *cp++ = '\r'; *cp++ = '\n';
            y++;
          }
        while (x < point2.x) { *cp++ = w->text.contents[y][x]; x++; }
        *cp++ = '\0';
        GlobalUnlock(hGlobalMemory);
        # Place it in the clipboard:
        OpenClipboard(w->hWnd);
        EmptyClipboard();
        SetClipboardData(CF_TEXT,hGlobalMemory);
        CloseClipboard();
      }
    end_system_call();
  }}

# The event handler for a text window.
local long text_event (mywindow w, UINT message, WPARAM wParam, LPARAM lParam);
local long text_event(w,message,wParam,lParam)
  var mywindow w;
  var UINT message;
  var WPARAM wParam;
  var LPARAM lParam;
  { WINDEBUG( event_out("text_event",w->hWnd,w,message,wParam,lParam); )
    switch (message)
      { case WM_PAINT:
          text_event_paint(w);
          return 0;
        case WM_LBUTTONDOWN:
          begin_selection(w,LOWORD(lParam),HIWORD(lParam));
          mouse_hidden = FALSE; show_mouse_cursor();
          return 0;
        case WM_LBUTTONUP:
          finish_selection(w);
          mouse_hidden = FALSE; show_mouse_cursor();
          return 0;
        case WM_MOUSEMOVE:
          adjust_selection(w,LOWORD(lParam),HIWORD(lParam));
          mouse_hidden = FALSE; show_mouse_cursor();
          return 0;
        case WM_CHAR: # regular key
        case WM_KEYDOWN: # special key
          if (!w->for_input)
            { MessageBeep(MB_OK);
              return 0;
            }
          break;
        case WM_COMMAND: # Menu selection
          switch (wParam)
            { case M_COPY:
                copy_selection(w);
                return 0;
              default:
                break;
            }
          break;
        case WM_SETFOCUS:
          w->in_focus = TRUE;
          break;
        case WM_KILLFOCUS:
          w->in_focus = FALSE;
          break;
        default:
          break;
      }
    return w->eventhandler2(w,message,wParam,lParam);
  }

# The name of the class of the windows we create.
local char text_class_name[] = "CLISP_TEXT";

# Initialize the class of text windows.
local boolean init_text_class (void);
local boolean init_text_class()
  { static boolean first_time = TRUE;
    if (first_time)
      # Before being able to create a window, we must first register
      # its window class.
      { var WNDCLASS wc;
        begin_system_call();
        wc.style = CS_HREDRAW | CS_VREDRAW;
        wc.lpfnWndProc = (LPVOID) all_event;
        wc.cbClsExtra = 0;
        wc.cbWndExtra = 0;
        wc.hInstance = global_hInstance;
        wc.hIcon = LoadIcon(global_hInstance,"ICON_1");
        wc.hCursor = NULL;
        wc.hbrBackground = 0;
        wc.lpszMenuName = "MENU_1";
        wc.lpszClassName = text_class_name;
        if (!RegisterClass(&wc))
          # If we didn't succeed in creating a window class, abort immediately.
          { end_system_call(); return FALSE; }
        end_system_call();
        first_time = FALSE;
      }
    return TRUE;
  }


# PART 2 : Text windows for output only, scrolling

# The text cursor can only be visible in one text window at most, at any time.
# This is a limitation of CreateCaret() and ShowCaret().

# Our text cursor policy is like this: The text cursor shall only be
# visible when the main windows owns the focus (i.e. is the distinguished
# window on screen) and the program is awaiting input.

# Make the text cursor visible.
local void show_text_cursor (mywindow w);
local void show_text_cursor(w)
  var mywindow w;
  { begin_system_call();
    CreateCaret(w->hWnd,          # handle of owner window
                0,                # handle of bitmap for caret shape
                text_char_size.x, # caret width
                2                 # caret height
               );
    SetCaretPos(w->text.cursor.x * text_char_size.x,
                w->text.cursor.y * text_char_size.y + text_char_ascent
               );
    ShowCaret(w->hWnd); # make the cursor visible, it begins flashing
    end_system_call();
  }

# Make the text cursor invisible.
local void hide_text_cursor (void);
local void hide_text_cursor()
  { begin_system_call();
    DestroyCaret();
    end_system_call();
  }

# Make the cursor visible or invisible, depending on w->text.cursor_visible.
local void text_cursor (mywindow w);
local void text_cursor(w)
  var mywindow w;
  { if (w->in_focus)
      { if (w->text.cursor_visible)
          { show_text_cursor(w); }
          else
          { hide_text_cursor(); }
  }   }
global void text_cursor_on (mywindow w);
global void text_cursor_on(w)
  var mywindow w;
  { w->text.cursor_visible = TRUE; text_cursor(w); }
global void text_cursor_off (mywindow w);
global void text_cursor_off(w)
  var mywindow w;
  { w->text.cursor_visible = FALSE; text_cursor(w); }

# Get and set the cursor position:
global void get_text_size (mywindow w, int* width, int* height);
global void get_text_cursor_position (mywindow w, int* x, int* y);
global void set_text_cursor_position (mywindow w, int x, int y);
global void get_text_size(w,width,height)
  var mywindow w;
  var int* width;
  var int* height;
  { *width = w->text.width; *height = w->text.height; }
global void get_text_cursor_position(w,x,y)
  var mywindow w;
  var int* x;
  var int* y;
  { *x = w->text.cursor.x; *y = w->text.cursor.y; }
global void set_text_cursor_position(w,x,y)
  var mywindow w;
  var int x;
  var int y;
  { if ((w->text.cursor.x == x) && (w->text.cursor.y == y)) return;
    if (w->text.cursor_visible) hide_text_cursor();
    if ((x >= 0) && (x < w->text.width)) { w->text.cursor.x = x; }
    if ((y >= 0) && (y < w->text.height)) { w->text.cursor.y = y; }
    if (w->text.cursor_visible) show_text_cursor(w);
  }

# Write some text to the screen.
# The only special characters we understand are backspace and newline.
# noscroll inhibits scrolling. Returns number of scrolls.
local int text_write (mywindow w, const char* buf, uintL len, boolean noscroll);
local int text_write(w,buf,len,noscroll)
  var mywindow w;
  var const char* buf;
  var uintL len;
  var boolean noscroll;
  { var int scrolls = 0;
    # To save time, we update the screen only once per line.
    var int xmin = w->text.cursor.x;
    var int xmax = w->text.cursor.x;
    if (len==0) return 0;
    if (w->text.cursor_visible) hide_text_cursor();
    dotimespL(len,len,
      { var char c = *buf++;
        switch (c)
          { case '\b': # Backspace
              if (w->text.cursor.x > 0)
                w->text.cursor.x--;
              break;
            case '\r': # Carriage Return
              text_event_paintline(w, w->text.cursor.y, xmin,xmax);
              xmin = xmax = w->text.cursor.x = 0;
              break;
            default:
              if ((unsigned char)c < ' ') break;
              # graphic character
              w->text.contents[w->text.cursor.y][w->text.cursor.x] = c;
              if (w->text.cursor.x < xmin) { xmin = w->text.cursor.x; }
              w->text.cursor.x++;
              if (w->text.cursor.x > xmax) { xmax = w->text.cursor.x; }
              if (w->text.cursor.x < w->text.width)
                break;
              # Immediately wrap to the next line. This saves us
              # from questionable cursor positioning in column w->text.width.
            case '\n': # Newline
              text_event_paintline(w, w->text.cursor.y, xmin,xmax);
              w->text.cursor.x = 0;
              if (++w->text.cursor.y == w->text.height)
                # scroll
                { w->text.cursor.y--;
                  if (noscroll) goto done;
                  { var char* text_line = w->text.contents[0];
                    { var int y;
                      for (y = 1; y < w->text.height; y++)
                        w->text.contents[y-1] = w->text.contents[y];
                    }
                    { var int x;
                      for (x = 0; x < w->text.width; x++)
                        text_line[x] = ' ';
                    }
                    w->text.contents[w->text.height-1] = text_line;
                  }
                  scrolls++;
                  # No need to update selpoint1.y and selpoint2.y
                  # since nothing is selected now.
                  begin_system_call();
                  ScrollWindow(w->hWnd,   # handle of window to scroll
                               0,         # amount of horizontal scrolling
                               -text_char_size.y, # vertical scrolling
                               NULL,      # scroll rectangle
                               NULL       # clip rectangle
                              );
                  UpdateWindow(w->hWnd);  # blank out last line
                  end_system_call();
                }
              xmin = xmax = w->text.cursor.x;
              break;
      }   });
    text_event_paintline(w, w->text.cursor.y, xmin,xmax);
    done:
    if (w->text.cursor_visible) show_text_cursor(w);
    return scrolls;
  }

# Write a single character to the screen.
global void text_writechar (mywindow w, char c);
global void text_writechar(w,c)
  var mywindow w;
  var char c;
  { text_write(w,&c,1,FALSE); }

# Some more routines for the SCREEN package in stream.d:
global void text_clear (mywindow w);
global void text_clear(w)
  var mywindow w;
  { set_text_cursor_position(w,0,0);
    { var int y;
      for (y = 0; y < w->text.height; y++)
        { var int x;
          for (x = 0; x < w->text.width; x++)
            { w->text.contents[y][x] = ' '; }
          text_event_paintline(w,y,0,w->text.width);
  } }   }
global void text_clear_to_eol (mywindow w);
global void text_clear_to_eol(w)
  var mywindow w;
  { var int y = w->text.cursor.y;
    var int x;
    for (x = w->text.cursor.x; x < w->text.width; x++)
      { w->text.contents[y][x] = ' '; }
    text_event_paintline(w,y,w->text.cursor.x,w->text.width);
  }
global void text_clear_to_eot (mywindow w);
global void text_clear_to_eot(w)
  var mywindow w;
  { text_clear_to_eol(w);
    { var int y;
      for (y = w->text.cursor.y+1; y < w->text.height; y++)
        { var int x;
          for (x = 0; x < w->text.width; x++)
            { w->text.contents[y][x] = ' '; }
          text_event_paintline(w,y,0,w->text.width);
  } }   }
global void text_delete_line (mywindow w);
global void text_delete_line(w)
  var mywindow w;
  { unmark_marked_region(w); # unmark selection
    { var char* text_line = w->text.contents[w->text.cursor.y];
      { var int y;
        for (y = w->text.cursor.y+1; y < w->text.height; y++)
          w->text.contents[y-1] = w->text.contents[y];
      }
      { var int x;
        for (x = 0; x < w->text.width; x++)
          text_line[x] = ' ';
      }
      w->text.contents[w->text.height-1] = text_line;
    }
    begin_system_call();
    { var RECT rect;
      rect.top = w->text.cursor.y * text_char_size.y;
      rect.bottom = w->text.height * text_char_size.y;
      rect.left = 0 * text_char_size.x;
      rect.right = w->text.width * text_char_size.x;
      ScrollWindow(w->hWnd,   # handle of window to scroll
                   0,         # amount of horizontal scrolling
                   -text_char_size.y, # vertical scrolling
                   &rect,     # scroll rectangle
                   &rect      # clip rectangle
                  );
      UpdateWindow(w->hWnd);  # blank out last line
    }
    end_system_call();
  }
global void text_insert_line (mywindow w);
global void text_insert_line(w)
  var mywindow w;
  { unmark_marked_region(w); # unmark selection
    { var char* text_line = w->text.contents[w->text.height-1];
      { var int y;
        for (y = w->text.height-1; y > w->text.cursor.y; y--)
          w->text.contents[y] = w->text.contents[y-1];
      }
      { var int x;
        for (x = 0; x < w->text.width; x++)
          text_line[x] = ' ';
      }
      w->text.contents[w->text.cursor.y] = text_line;
    }
    begin_system_call();
    { var RECT rect;
      rect.top = w->text.cursor.y * text_char_size.y;
      rect.bottom = w->text.height * text_char_size.y;
      rect.left = 0 * text_char_size.x;
      rect.right = w->text.width * text_char_size.x;
      ScrollWindow(w->hWnd,   # handle of window to scroll
                   0,         # amount of horizontal scrolling
                   text_char_size.y, # vertical scrolling
                   &rect,     # scroll rectangle
                   &rect      # clip rectangle
                  );
      UpdateWindow(w->hWnd);  # blank out inserted line
    }
    end_system_call();
  }
global mywindow text_create (int width, int height);
global mywindow text_create(width,height)
  var int width;
  var int height;
  { var mywindow w;
    begin_system_call();
    w = (struct mywindow *) malloc(sizeof(struct mywindow));
    if (!w) { MessageBeep(MB_OK); end_system_call(); return NULL; }
   {var HWND hWnd =
      CreateWindow(text_class_name,           # registered class name
                   "CLISP SCREEN",            # window title
                   WS_OVERLAPPED | WS_CAPTION # window style
                    | WS_MINIMIZEBOX,
                   CW_USEDEFAULT,             # horizontal position
                   CW_USEDEFAULT,             # vertical position
                   width*text_char_size.x         # window width
                    + 2*GetSystemMetrics(SM_CXBORDER),
                   height*text_char_size.y        # window height
                    + 2*GetSystemMetrics(SM_CYBORDER)
                    + GetSystemMetrics(SM_CYCAPTION),
                   NULL,                      # parent window
                   NULL,                      # menu oder child window id
                   global_hInstance,          # application instance
                   global_lpszCmdLine         # window-creation data
                  );
    # If we didn't succeed in creating a window, abort immediately.
    if (!hWnd)
      { free(w); MessageBeep(MB_OK); end_system_call(); return NULL; }
    # Set the background brush. (See the doc of WNDCLASS for the +1.)
    SetClassWord(hWnd,GCW_HBRBACKGROUND,COLOR_WINDOW+1);
    # Fill in w:
    w->hWnd = hWnd;
    w->in_focus = FALSE;
    w->for_output = TRUE;
    w->for_input = FALSE;
    w->eventhandler1 = text_event;
    w->eventhandler2 = default_event;
    w->text.width = width; w->text.height = height;
    w->text.contents = malloc(height*sizeof(char*)); # check??
    { var uintL y;
      for (y = 0; y < height; y++)
        w->text.contents[y] = malloc(width*sizeof(char)); # check??
    }
    w->text.in_marking = none;
    w->text.cursor.x = 0; w->text.cursor.y = 0;
    w->text.cursor_visible = FALSE;
    end_system_call();
    register_window(w);
    text_clear(w);
    return w;
  }}
global void text_destroy (mywindow w);
global void text_destroy(w)
  var mywindow w;
  { begin_system_call();
    DestroyWindow(w->hWnd);
    end_system_call();
    unregister_window(w);
    begin_system_call();
    { var uintL y;
      for (y = 0; y < w->text.height; y++) { free(w->text.contents[y]); }
      free(w->text.contents);
    }
    free(w);
    end_system_call();
  }


# PART 3 : Windows for input

# Input comes through the event handler.

# Discard an event when we are done processing it.
local void ev_done (event ev);
local void ev_done(ev)
  var event ev;
  { switch (ev.type)
      { case ev_drop:
          begin_system_call();
          DragFinish((HANDLE)ev.u.wParam);
          end_system_call();
          break;
        case ev_paste:
          begin_system_call();
          free(ev.u.pastebuf);
          end_system_call();
          break;
        default:
          break;
  }   }

# Low level input: get keystroke and other events.
# Since we are not able to handle all events immediately, we store
# the "interesting" ones in a queue.

# In w->input: This is a type-ahead buffer.
# event* ev_fifo; # keyboard typeahead buffer, event queue
# uintL ev_fifo_size;
# event* ev_fifo_out;
# event* ev_fifo_in;
  # ev_fifo_out and ev_fifo_in both rotate through ev_fifo,
  # the keys in the range between ev_fifo_out and ev_fifo_in are
  # awaiting to be handled.
  #
  #   [_______XXXXXXXXXXXXXXXXX___________________________]
  #           |                |
  #           ev_fifo_out      ev_fifo_in
  #
  # When ev_fifo_out reaches ev_fifo_in, the buffer is empty.
  # Never is ev_fifo_in allowed to reach ev_fifo_out from below.

#define ev_fifo_empty(w)  (w->input.ev_fifo_out == w->input.ev_fifo_in)
local event ev_from_fifo (mywindow w); # assumes !ev_fifo_empty(w)
local event ev_from_fifo(w)
  var mywindow w;
  { var event ev = *(w->input.ev_fifo_out)++;
    if (w->input.ev_fifo_out == &w->input.ev_fifo[w->input.ev_fifo_size])
      w->input.ev_fifo_out = &w->input.ev_fifo[0];
    return ev;
  }
local void ev_into_fifo (mywindow w, event ev);
local void ev_into_fifo(w,ev)
  var mywindow w;
  var event ev;
  { var event* new_ev_fifo_in = w->input.ev_fifo_in + 1;
    if (new_ev_fifo_in == &w->input.ev_fifo[w->input.ev_fifo_size])
      new_ev_fifo_in = &w->input.ev_fifo[0];
    if (new_ev_fifo_in == w->input.ev_fifo_out)
      # FIFO full, increase its size
      { var uintL new_ev_fifo_size = 2*w->input.ev_fifo_size;
        begin_system_call();
       {var event* new_ev_fifo = (event*) malloc(new_ev_fifo_size*sizeof(event));
        if (!new_ev_fifo) { MessageBeep(MB_OK); end_system_call(); ev_done(ev); return; }
        end_system_call();
        {var event* ptr = new_ev_fifo;
         until (ev_fifo_empty(w)) { *ptr++ = ev_from_fifo(w); }
         w->input.ev_fifo_size = new_ev_fifo_size;
         w->input.ev_fifo = new_ev_fifo;
         w->input.ev_fifo_out = &new_ev_fifo[0]; w->input.ev_fifo_in = ptr;
         new_ev_fifo_in = w->input.ev_fifo_in + 1;
      }}}
    *(w->input.ev_fifo_in) = ev; w->input.ev_fifo_in = new_ev_fifo_in;
  }

# The event handler for a text window capable of doing input.
local long textio_event (mywindow w, UINT message, WPARAM wParam, LPARAM lParam);
local long textio_event(w,message,wParam,lParam)
  var mywindow w;
  var UINT message;
  var WPARAM wParam;
  var LPARAM lParam;
  { WINDEBUG( event_out("textio_event",w->hWnd,w,message,wParam,lParam); )
    switch (message)
      { case WM_COMMAND: # Menu selection
          switch (wParam)
            { case M_PASTE:
                OpenClipboard(w->hWnd);
                { var HANDLE hClipboardMemory = GetClipboardData(CF_TEXT);
                  if (hClipboardMemory)
                    { var uintL pastelen = GlobalSize(hClipboardMemory);
                      var char* pastebuf = malloc(pastelen);
                      if (pastebuf)
                        { { var char* p = GlobalLock(hClipboardMemory);
                            memcpy(pastebuf,p,pastelen);
                            GlobalUnlock(hClipboardMemory);
                          }
                          { var event ev;
                            ev.type = ev_paste;
                            ev.u.pastebuf = pastebuf;
                            ev_into_fifo(w,ev);
                        } }
                        else
                        { MessageBeep(MB_OK); }
                }   }
                CloseClipboard();
                return 0;
              default:
                break;
            }
          break;
        case WM_DROPFILES: # dropping file icons from the file manager
          { var event ev;
            ev.type = ev_drop;
            ev.u.wParam = wParam;
            ev_into_fifo(w,ev);
          }
          return 0;
        case WM_CHAR: # regular key
          { var event ev;
            ev.type = ev_key; ev.u.key = wParam;
            ev_into_fifo(w,ev);
          }
          return 0;
        case WM_KEYDOWN: # special key
          { var event ev;
            switch (wParam)
              { case VK_HOME:   ev.u.key = char_hyper_c|23 ; break;
                case VK_END:    ev.u.key = char_hyper_c|17 ; break;
                case VK_LEFT:   ev.u.key = char_hyper_c|20 ; break;
                case VK_RIGHT:  ev.u.key = char_hyper_c|22 ; break;
                case VK_UP:     ev.u.key = char_hyper_c|24 ; break;
                case VK_DOWN:   ev.u.key = char_hyper_c|18 ; break;
                case VK_DELETE: ev.u.key = char_hyper_c|127; break;
                default:
                  goto defolt;
              }
            ev.type = ev_key;
            ev_into_fifo(w,ev);
          }
          return 0;
        case WM_CLOSE: # user requests to close the window
          { var event ev;
            ev.type = ev_eof;
            ev_into_fifo(w,ev);
          }
          return 0;
        case WM_SETFOCUS:
          if (w->text.cursor_visible) show_text_cursor(w);
          break;
        case WM_KILLFOCUS:
          if (w->text.cursor_visible) hide_text_cursor();
          break;
        defolt:
        default:
          break;
      }
    return w->eventhandler3(w,message,wParam,lParam);
  }


# The events enter our program through PeekMessage() and GetMessage().
# After some translation they are sent back to Windows using DispatchMessage(),
# which in effect calls all_event(). Then we sort the events according to
# their destination window and stuff them into a FIFO, from where they
# eventually will be processed.

# An event translation table.
local HANDLE hAccel;

# Acknowlegde all the pending events, put them into our queue.
local void look_for_events (void);
local void look_for_events()
  { var MSG msg;
    begin_system_call();
    while (PeekMessage(&msg,
                       NULL, # look for events belonging to all of our windows
                       0, 0, # in any order
                       PM_REMOVE # we take over responsibility for the event
          )           )
      { # Translate the accelerator keys defined in our accelerator table:
        if (!TranslateAccelerator(msg.hwnd,hAccel,&msg))
          { TranslateMessage(&msg);
            DispatchMessage(&msg);
            # Hope that all_event() does the real work...
      }   }
    end_system_call();
  }


# Mid level input: Yield keystroke, process other events.
# These routines can call other Lisp functions.
# kann GC auslösen

local boolean win_kbhit (mywindow w);
local cint win_getch (mywindow w);

# The current state of a finite state machine:
# boolean in_paste; # getting our input from a paste buffer
# event paste_ev;   # the current paste event
# char* paste_ptr;  # pointer into the paste buffer

# Process all events until we hit a key event:
local void process_nonkey_events (mywindow w);
local void process_nonkey_events(w)
  var mywindow w;
  { look_for_events();
    until (!w->input.in_paste && ev_fifo_empty(w))
      { if (w->input.in_paste)
          { if (!(*(w->input.paste_ptr)=='\0')) # some keys in the paste buffer?
              return;
            else
              { ev_done(w->input.paste_ev); w->input.in_paste = FALSE; }
          }
       {var event ev;
        ev = *(w->input.ev_fifo_out);
        switch (ev.type)
          { case ev_key:
              return; # found a key
            case ev_eof:
              return; # found eof
            case ev_menu:
              # Not yet.
              abort();
            case ev_drop:
              # Call LOAD on all dropped files
              ev_from_fifo(w); # consume ev
              begin_system_call();
              { var HDROP hdrop = (HANDLE)ev.u.wParam;
                var uintL numfiles = DragQueryFile(hdrop,-1,NULL,0);
                var uintL i;
                for (i=0; i<numfiles; i++)
                  { var uintL filenamelen = DragQueryFile(hdrop,i,NULL,0);
                    var DYNAMIC_ARRAY(filenamebuf,char,filenamelen+1);
                    DragQueryFile(hdrop,i,filenamebuf,filenamelen);
                    filenamebuf[filenamelen] = '\0';
                    # Convert the filename to a string:
                    pushSTACK(asciz_to_string(filenamebuf));
                    FREE_DYNAMIC_ARRAY(filenamebuf);
                    # Call LOAD:
                    funcall(S(load),1);
                  }
                DragFinish(hdrop);
              }
              end_system_call();
              break;
            case ev_paste:
              ev_from_fifo(w); # consume ev
              # begin paste operation
              w->input.in_paste = TRUE;
              w->input.paste_ev = ev;
              w->input.paste_ptr = ev.u.pastebuf;
              break;
          }
  }   }}

global boolean win_kbhit(w)
  var mywindow w;
  { process_nonkey_events(w);
    return (w->input.in_paste || !ev_fifo_empty(w));
  }

global cint win_getch(w)
  var mywindow w;
  { if (!win_kbhit(w))
      { # Have to wait for a keypress.
        in_wait = TRUE; show_mouse_cursor();
        text_cursor_on(w);
        do { var MSG msg;
             begin_system_call();
             if (GetMessage(&msg,
                            NULL, # look for events belonging to all of our windows
                            0, 0  # in any order
                )          )
               { # Translate the accelerator keys defined in our accelerator table:
                 if (!TranslateAccelerator(msg.hwnd,hAccel,&msg))
                   { TranslateMessage(&msg);
                     DispatchMessage(&msg);
                     # Hope that all_event() does the real work...
                   }
                 end_system_call();
               }
               else
               # received WM_QUIT which means we have to exit immediately
               { end_system_call();
                 # transfer control to the end of clisp_main() -> end of WinMain()
                 final_exitcode = msg.wParam; quit();
               }
           }
           until (win_kbhit(w));
        in_wait = FALSE; show_mouse_cursor();
        text_cursor_off(w);
      }
    if (w->input.in_paste)
      return (unsigned char) (*(w->input.paste_ptr)++);
    if (!ev_fifo_empty(w))
      { var event ev;
        ev = ev_from_fifo(w);
        if (!(ev.type==ev_key)) abort();
        return ev.u.key;
      }
    abort();
  }


# PART 4 : Text windows for input and output

# Basic terminal stream functions for STREAM:
# Output:
global void winterm_writechar (mywindow w, uintB c);
# High level input, with echo and line editing:
global signean winterm_listen (mywindow w);
global boolean winterm_clear_input (mywindow w);
global cint winterm_readchar (mywindow w);

# Output:

global void winterm_writechar(w,c)
  var mywindow w;
  var uintB c;
  { WINDEBUG( asciz_out_1("{winterm_writechar:%x}",c); )
    if (c=='\t')
      { var int x,y;
        get_text_cursor_position(w,&x,&y);
        text_write(w,"        ", 8 - (x % 8), FALSE);
      }
      else
      { text_writechar(w,c); }
  }

# Input:

global signean winterm_listen(w)
  var mywindow w;
  { WINDEBUG( asciz_out("{winterm_listen}"); )
    if (w->textinput.index < w->textinput.count) return signean_null;
    if (win_kbhit(w)) return signean_null;
    return signean_plus;
  }

global boolean winterm_clear_input(w)
  var mywindow w;
  { WINDEBUG( asciz_out("{winterm_clear_input}"); )
   {var boolean cleared_something = (w->textinput.index < w->textinput.count);
    w->textinput.index = w->textinput.count;
    while (win_kbhit(w)) { win_getch(w); cleared_something = TRUE; }
    return cleared_something;
  }}

# Big assumption: Every character in the input line occupies exactly one place
# on screen.

# Prepare for adding one char to the input line.
local void add1char (mywindow w);
local void add1char(w)
  var mywindow w;
  { if (w->textinput.count >= w->textinput.line_size)
      { var uintL new_size = 2 * w->textinput.line_size;
        begin_system_call();
       {var char* new_line = (char*) realloc(w->textinput.line,new_size*sizeof(char));
        end_system_call();
        if (!new_line) abort();
        w->textinput.line = new_line;
        w->textinput.line_size = new_size;
      }}
    w->textinput.count++;
  }

# Add a normal char to the input line.
local void normalchar (mywindow w, char c);
local void normalchar(w,c)
  var mywindow w;
  var char c;
  { add1char(w);
    { var uintL i;
      for (i = w->textinput.count-1; i > w->textinput.index; i--)
        { w->textinput.line[i] = w->textinput.line[i-1]; }
    }
    w->textinput.line[w->textinput.index] = c;
    # Much like  text_write(w, &w->textinput.line[w->textinput.index],
    #                          w->textinput.count - w->textinput.index);
    # but be careful about the cursor position and scrolling:
    { var int x,y;
      get_text_cursor_position(w,&x,&y);
      y -= text_write(w, &w->textinput.line[w->textinput.index],
                         w->textinput.count - w->textinput.index,
                         y==0 # no scrolling if y=0
                     );
      if (y<0) { y=0; } # this shouldn't happen, but better safe than sorry
      set_text_cursor_position(w,x,y);
    }
    text_writechar(w,c); # move the cursor to the right
    w->textinput.index++;
  }

# Delete a char from the input line.
# It is assumed that 0 <= w->textinput.index < w->textinput.count .
local void deletechar (mywindow w);
local void deletechar(w)
  var mywindow w;
  { { var uintL i;
      for (i = w->textinput.index+1; i < w->textinput.count; i++)
        { w->textinput.line[i-1] = w->textinput.line[i]; }
    }
    w->textinput.line[w->textinput.count-1] = ' ';
    # Much like  text_write(w, &w->textinput.line[w->textinput.index],
    #                          w->textinput.count - w->textinput.index);
    # but be careful about the cursor position and scrolling:
    { var int x,y;
      get_text_cursor_position(w,&x,&y);
      text_write(w, &w->textinput.line[w->textinput.index],
                    w->textinput.count - w->textinput.index,
                    TRUE # no scrolling
                );
      set_text_cursor_position(w,x,y);
    }
    w->textinput.count--;
  }

# Move to a new position in the same line.
local void movetox (mywindow w, uintL new_index);
local void movetox(w,new_index)
  var mywindow w;
  var uintL new_index;
  { if (new_index == w->textinput.index)
      return;
    if (new_index < w->textinput.index)
      # move backward
      { var uintL dist = w->textinput.index - new_index;
        var int x,y;
        get_text_cursor_position(w,&x,&y);
        # position (x,y) corresponds to w->textinput.index
        while (dist >= w->text.width) { y--; dist -= w->text.width; }
        x -= dist; if (x < 0) { y--; x += w->text.width; }
        # position (x,y) corresponds to new_index, but y may be < 0
        if (y < 0)
          { var int scrolls = -y;
            var sintL index0 = (sintL)new_index - x;
            var int x0 = 0;
            if (index0 < 0) { x0 = -index0; index0 = 0; }
            # position (x0,y) corresponds to index0
            set_text_cursor_position(w,x0,0);
            while (y < 0) { text_insert_line(w); y++; }
            # position (x0,y=0) corresponds to index0
            text_write(w, &w->textinput.line[index0],
                          scrolls * w->text.width - x0,
                          TRUE # no scrolling
                      );
          }
        set_text_cursor_position(w,x,y);
      }
      else
      # move forward
      { var uintL dist = new_index - w->textinput.index;
        var int x,y;
        get_text_cursor_position(w,&x,&y);
        # position (x,y) corresponds to w->textinput.index
        while (dist >= w->text.width) { y++; dist -= w->text.width; }
        x += dist; if (x >= w->text.width) { y++; x -= w->text.width; }
        # position (x,y) corresponds to new_index, but y may be >= w->text.height
        if (y >= w->text.height)
          { var int scrolls = y - w->text.height + 1;
            set_text_cursor_position(w,0,0);
            while (y >= w->text.height) { text_delete_line(w); y--; }
            # position (x,y=w->text.height-1) corresponds to new_index
            if (scrolls >= w->text.height) { scrolls = w->text.height; }
           {var uintL index0 = new_index - x - (scrolls-1) * w->text.width;
            # position (0,w->text.height-scrolls) corresponds to index0
            set_text_cursor_position(w,0,w->text.height-scrolls);
            text_write(w, &w->textinput.line[index0],
                          w->textinput.count - index0,
                          TRUE # no scrolling
                      );
          }}
        set_text_cursor_position(w,x,y);
      }
    w->textinput.index = new_index;
  }
#define move1left(w)  movetox(w, w->textinput.index - 1)
#define move1right(w)  movetox(w, w->textinput.index + 1)

global cint winterm_readchar(w)
  var mywindow w;
  { if (w->textinput.index >= w->textinput.count)
      # read a new line
      { w->textinput.index = 0;
        w->textinput.count = 0;
        loop
          { var cint c = win_getch(w);
            mouse_hidden = TRUE; show_mouse_cursor();
            if (!(c & char_hyper_c))
              { c &= char_code_mask_c;
                switch (c)
                  { case BS: # Backspace
                      # Delete the character to the left of the cursor
                      if (w->textinput.index == 0) goto bad_input;
                      move1left(w);
                      deletechar(w);
                      break;
                    case TAB: # Tab
                      # Insert spaces until the cursor's column is a multiple of 8
                      { var int x,y;
                        do { normalchar(w,' ');
                             get_text_cursor_position(w,&x,&y);
                           }
                           until ((x % 8) == 0);
                      }
                      break;
                    case CR: # Return/Enter
                      do_CR:
                      movetox(w,w->textinput.count);
                      add1char(w); w->textinput.line[w->textinput.index] = NL;
                      goto loopend;
                    default:
                      if (c < ' ') goto bad_input;
                      normalchar(w,c);
                      break;
              }   }
              else
              # special key
              { switch (c)
                  { case char_hyper_c|CR : # key 'Enter'
                      goto do_CR;
                    case char_hyper_c|127 : # key 'Delete'
                      # Delete the character at the cursor
                      if (w->textinput.index == w->textinput.count) goto bad_input;
                      deletechar(w);
                      break;
                    case char_hyper_c|20 : # key '<-'
                      # move the cursor to the left by one position
                      if (w->textinput.index == 0) goto bad_input;
                      move1left(w);
                      break;
                    case char_hyper_c|char_super_c|20 : # key Shift '<-'
                    case char_hyper_c|23 : # key 'Home'
                      # Go to beginning of line
                      movetox(w,0);
                      break;
                    case char_hyper_c|22 : # key '->'
                      # move the cursor to the right by one position
                      if (w->textinput.index == w->textinput.count) goto bad_input;
                      move1right(w);
                      break;
                    case char_hyper_c|char_super_c|22 : # key Shift '->'
                    case char_hyper_c|17 : # key 'End'
                      # Go to end of line
                      movetox(w,w->textinput.count);
                      break;
                    default:
                    bad_input:
                      begin_system_call();
                      MessageBeep(MB_OK);
                      end_system_call();
              }   }
          }
        loopend: # done reading a line
        mouse_hidden = FALSE; show_mouse_cursor();
        w->textinput.index = 0;
      }
    # Here w->textinput.index < w->textinput.count .
    return (uintB) w->textinput.line[w->textinput.index++];
  }

