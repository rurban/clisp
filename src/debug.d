/* top level loop, aux functions for the debugger, stepper for CLISP
 * Bruno Haible 1990-2002
 * ILISP friendliness: Marcus Daniels 8.4.1994
 * Sam Steingold 2001-2003
 */

#include "lispbibl.c"


/* ----------------------------------------------------------------------- */
/* Top-Level-Loop */

local Values read_form(void)
{ /* (SYS::READ-FORM ostream istream prompt [commandlist])
 read one form (interactively) from the input stream.
 instead of the form, we also recognize special commands from commandlist
 (a fresh alist) or SYS::*KEY-BINDINGS*
 > STACK_1: prompt, a string
 > STACK_0: commandlist (fresh aliste) or #<UNBOUND>
 < STACK_1: Output-Stream *standard-output*
 < STACK_0: Input-Stream *standard-input*
 < mv_space/mv_count: value = form, NIL or (on EOF) T, T
 can trigger GC
 (defun read-form (ostream istream prompt &optional (command-list nil))
   (loop
     (let ((raw (terminal-raw istream nil)))
       (when (interactive-stream-p istream)
         (terpri ostream)
         (write-string prompt ostream)
         (force-output ostream))
       (let* ((eof-value "EOF")
              (form (let ((*read-suppress* nil)
                          (*key-bindings* (nreconc command-list
                                                   *key-bindings*)))
                      (read istream nil eof-value nil))))
         (terminal-raw istream raw)
         (if (eql form eof-value)
           (progn (clear-input istream) (setq istream *debug-io*))
           (progn (clear-input-upto-newline istream)
                  (return (values form nil))))))))   */
 #if STACKCHECKR
  var gcv_object_t* STACKbefore = STACK; /* retain STACK for later */
 #endif
  pushSTACK(STACK_1); pushSTACK(STACK_1);
  STACK_3 = var_stream(S(standard_output),strmflags_wr_ch_B); /* ostream := *STANDARD-OUTPUT* */
  STACK_2 = var_stream(S(standard_input),strmflags_rd_ch_B); /* istream := *STANDARD-INPUT* */
  /* stack layout: ostream, istream, prompt, command-list. */
  pushSTACK(STACK_2);
  pushSTACK(STACK_3); pushSTACK(NIL); funcall(L(terminal_raw),2);
  pushSTACK(value1);
  /* stack layout: ostream, istream, prompt, command-list, inputstream, raw. */
  var signean status = listen_char(STACK_4); # horchen
  if (ls_eof_p(status) && !boundp(Symbol_value(S(terminal_read_stream))))
    goto eof;
  /* already have characters available (and not in ilisp_mode) -> no prompt */
  if (ilisp_mode || interactive_stream_p(STACK_4)) {
    /* interactive input-stream -> prompt output: */
   #if 0
    terpri(&STACK_5); /* (TERPRI ostream) */
   #else
    /* the same, but avoiding infinite recursion
     (let ((*recurse-count-standard-output*
            (1+ *recurse-count-standard-output*)))
       (when (> *recurse-count-standard-output* 3)
         (setq *recurse-count-standard-output* 0)
         (makunbound (quote *standard-output*))
         (let ((*recurse-count-debug-io* (1+ *recurse-count-debug-io*)))
           (when (> *recurse-count-debug-io* 3)
             (setq *recurse-count-debug-io* 0)
             (makunbound (quote *debug-io*))
             (symbol-stream (quote *debug-io*) :io))
           (symbol-stream (quote *standard-output*) :output)))
       (terpri *standard-output*)) */
    /* (incf sys::*recurse-count-standard-output*) */
    dynamic_bind(S(recurse_count_standard_output),
                 fixnum_inc(Symbol_value(S(recurse_count_standard_output)),1));
    if (!posfixnump(Symbol_value(S(recurse_count_standard_output))))
      /* should be fixnum >=0; otherwise emergency correction */
      Symbol_value(S(recurse_count_standard_output)) = Fixnum_0;
    if (posfixnum_to_L(Symbol_value(S(recurse_count_standard_output))) > 3) {
      /* too many nested i/o errors. */
      Symbol_value(S(recurse_count_standard_output)) = Fixnum_0;
      Symbol_value(S(standard_output)) = unbound;
       /* (incf sys::*recurse-count-debug-io*): */
      dynamic_bind(S(recurse_count_debug_io),
                   fixnum_inc(Symbol_value(S(recurse_count_debug_io)),1));
      if (!posfixnump(Symbol_value(S(recurse_count_debug_io))))
        /* should be fixnum >=0; otherwise emergency correction */
        Symbol_value(S(recurse_count_debug_io)) = Fixnum_0;
      if (posfixnum_to_L(Symbol_value(S(recurse_count_debug_io))) > 3) {
        /* too many nested i/o errors. */
        Symbol_value(S(recurse_count_debug_io)) = Fixnum_0;
        Symbol_value(S(debug_io)) = unbound;
        var_stream(S(debug_io),strmflags_rd_ch_B|strmflags_wr_ch_B);
      }
      STACK_(5+3+3) = var_stream(S(standard_output),strmflags_wr_ch_B); /* ostream := *STANDARD-OUTPUT* */
      dynamic_unbind(S(recurse_count_debug_io));
    }
    terpri(&STACK_(5+3)); /* (TERPRI ostream) */
    dynamic_unbind(S(recurse_count_standard_output));
   #endif
    write_string(&STACK_5,STACK_3); /* (WRITE-STRING prompt ostream) */
    force_output(STACK_5);
  } /* Prompt OK */
  {
    var gcv_object_t* inputstream_ = &STACK_1;
  #if 0
    /* That proves nevertheless awkward: If one presses CTRL-C during input,
       then one has some commands then in the BREAK loop doubly in the list */
    {
      var object list = Symbol_value(S(key_bindings)); /* old Key-Bindings */
      if (boundp(STACK_2)) /* command-list supplied? */
        list = nreconc(STACK_2,list); /* add in front */
      dynamic_bind(S(key_bindings),list); /* bind SYS::*KEY-BINDINGS* */
    }
   #else
    {
      var object list = (!boundp(STACK_2) ? NIL : (object)STACK_2);
      dynamic_bind(S(key_bindings),list); /* bind SYS::*KEY-BINDINGS* */
    }
   #endif
   #if !defined(TERMINAL_USES_KEYBOARD) /*  Atari - function keys */
    var bool terminal_read_stream_bound = false;
    if (!ls_avail_p(status) /* only for interactive input streams */
        && !boundp(Symbol_value(S(terminal_read_stream)))) {
      /* look for commands, not forms:
       (multiple-value-bind (line flag) (read-line istream)
         (let ((h (assoc line *key-bindings* :test (function string-equal))))
           (when h (funcall (cdr h)) (return t)))
         (setq istream
               (make-concatenated-stream
                (make-string-input-stream
                 (if flag line
                     (concatenate (quote string) line (string #\Newline))))
                istream))) */
      do {
        /* this loop is for win32 and its C-z RET abomination: after
           C-z (EOF) is processed, there is an empty line in the stream */
        pushSTACK(*inputstream_); pushSTACK(NIL); pushSTACK(NIL);
        funcall(L(read_line),3); /* (READ-LINE istream nil nil) */
        if (nullp(value1)) { /* EOF at line start? */
          dynamic_unbind(S(key_bindings));
          goto eof;
        }
      } while (Sstring_length(value1) == 0);
      var object line = value1; /* non-trivial line */
      /* NB: READ-LINE returns a SIMPLE-STRING in CLISP, so line is simple */
      { /* search for line in *KEY-BINDINGS*: */
        var object alist = Symbol_value(S(key_bindings));
        var uintL input_len = Sstring_length(line);
        for (;consp(alist);alist = Cdr(alist))
          if (mconsp(Car(alist)) && simple_string_p(Car(Car(alist)))) {
            var object key = Car(Car(alist));
            simple_array_to_storage(key);
            var uintL len = Sstring_length(key);
            /* check whether the line starts with the key and a whitespace */
            if ((len <= input_len) && string_eqcomp_ci(line,0,key,0,len)) {
              if (len == input_len) goto found;
              /* now len < input_len */
              { var chart ch = schar(line,len);
                if (cint_white_p(as_cint(ch))) goto found;
              }
              if (false) {
               found:
                funcall(Cdr(Car(alist)),0); /* call the appropriate function */
                dynamic_unbind(S(key_bindings));
                goto eof;
              }
            }
          }
      }
      /* create string-input-stream for this line: */
      if (nullp(value2)) {
        pushSTACK(line); pushSTACK(O(newline_string));
        line = string_concat(2); /* maybe need another Newline */
      }
      pushSTACK(line); funcall(L(make_string_input_stream),1);
      /* make concatenated-stream: */
      pushSTACK(value1); pushSTACK(*inputstream_);
      funcall(L(make_concatenated_stream),2);
      dynamic_bind(S(terminal_read_stream),value1);
      terminal_read_stream_bound = true;
      *inputstream_ = Symbol_value(S(terminal_read_stream));
    } else if (streamp(Symbol_value(S(terminal_read_stream)))) {
      var object stream = Symbol_value(S(terminal_read_stream));
      Symbol_value(S(terminal_read_stream)) = unbound;
      dynamic_bind(S(terminal_read_stream),stream);
      terminal_read_stream_bound = true;
      *inputstream_ = Symbol_value(S(terminal_read_stream));
    }
   #endif  /* !defined(TERMINAL_USES_KEYBOARD) */
    dynamic_bind(S(read_suppress),NIL); /* *READ-SUPPRESS* = NIL */
    /* read object (recursive-p=NIL, whitespace-p=NIL): */
    var object obj = stream_read(inputstream_,NIL,NIL);
    dynamic_unbind(S(read_suppress));
   #if !defined(TERMINAL_USES_KEYBOARD)
    if (terminal_read_stream_bound) {
      var object old_trs = Symbol_value(S(terminal_read_stream));
      dynamic_unbind(S(terminal_read_stream));
      if (streamp(old_trs)) {
        /* maybe need to process something from the first line? */
        var object strm_list = TheStream(old_trs)->strm_concat_list;
        pushSTACK(old_trs); /* save before PEEK-CHAR */
        Symbol_value(S(terminal_read_stream)) =
          (consp(strm_list) && !nullp(Cdr(strm_list))
           /* some input on the first line was not processed ? */
           && (pushSTACK(T), pushSTACK(Car(strm_list)),
               pushSTACK(NIL), pushSTACK(eof_value),
               funcall(L(peek_char),4), !eq(value1,eof_value)))
          ? STACK_0 : (gcv_object_t)unbound;
        skipSTACK(1); /* drop old_trs */
      }
    }
   #endif
    dynamic_unbind(S(key_bindings));
    if (!eq(obj,eof_value)) { /* EOF test (after whitespace) */
      pushSTACK(obj);
      pushSTACK(STACK_(4+1)); pushSTACK(STACK_(0+1+1)); funcall(L(terminal_raw),2);
      /* delete input till EOL */
      if (interactive_stream_p(STACK_(4+1))) {
        while (ls_avail_p(listen_char(STACK_(4+1)))) {
          var object ch = peek_char(&STACK_(4+1));
          if (eq(ch,eof_value))
            break;
          read_char(&STACK_(4+1));
          if (eq(ch,ascii_char(NL)))
            break;
        }
      }
      VALUES2(popSTACK(), NIL); /* (values obj NIL) */
      skipSTACK(4);
     #if STACKCHECKR
      if (STACK != STACKbefore) /* verify if Stack is cleaned up */
        abort(); /* if not --> go to Debugger */
     #endif
      return;
    }
  }
 eof: /* reached EOF */
  pushSTACK(STACK_4); pushSTACK(STACK_(0+1)); funcall(L(terminal_raw),2);
  /* call (CLEAR-INPUT istream) to eat EOF from an interactive stream,
     because a continuable program could misunderstand the EOF: */
  clear_input(STACK_4);
  VALUES2(T,T); /* (values T T) */
  skipSTACK(4);
 #if STACKCHECKR
  if (STACK != STACKbefore) /* verify that STACK is cleaned up */
    abort(); /* if not --> go to Debugger */
 #endif
}

# (SYS::READ-FORM prompt [commandlist])
# liest eine Form (interaktiv) von *standard-input*.
# prompt muss ein String sein.
# Statt einer Form kann auch eine Sondertaste aus commandlist (eine frische
# Aliste) oder SYS::*KEY-BINDINGS* eingegeben werden.
# Werte: form, NIL oder (bei EOF) T, T
LISPFUN(read_form,seclass_default,1,1,norest,nokey,0,NIL)
{ read_form(); skipSTACK(2); }

# (SYS::READ-EVAL-PRINT prompt [commandlist])
# liest eine Form, wertet sie aus und gibt die Werte aus.
# prompt muss ein String sein.
# Statt einer Form kann auch eine Sondertaste aus commandlist (eine frische
# Aliste) oder SYS::*KEY-BINDINGS* eingegeben werden.
# Werte: NIL oder (bei Sondertaste oder EOF) T
LISPFUN(read_eval_print,seclass_default,1,1,norest,nokey,0,NIL)
# (defun read-eval-print (prompt &optional (command-list nil))
#   (multiple-value-bind (form flag)
#       (read-form *standard-output* *standard-input* prompt command-list)
#     (if flag
#       form ; T zurück
#       (progn
#         (setq +++ ++ ++ + + - - form)
#         (let ((vals (multiple-value-list (eval-env form [aktuellesEnvironment]))))
#           (setq /// // // / / vals)
#           (setq *** ** ** * * (first vals))
#           ; primitiv:
#         #|(do ((ostream *standard-output*)
#                (L vals (cdr L)))
#               ((atom L))
#             (write (car L) ostream)
#             (when (consp (cdr L))
#               (write-string " ;" ostream)
#               (terpri ostream)
#           ) )
#         |#; unnötige Leerzeile zwischen Input und Output vermeiden:
#           (let ((ostream *standard-output*))
#             (fresh-line ostream)
#             (when (consp vals)
#               (write (car vals) ostream)
#               (do ((L (cdr vals) (cdr L)))
#                   ((atom L))
#                 (write-string " ;" ostream)
#                 (terpri ostream)
#                 (write (car L) ostream)
#           ) ) )
#         )
#         nil
# ) ) ) )
  {
    read_form(); # Form lesen
    # Stackaufbau: ostream, istream.
    if (!nullp(value2)) { # flag ?
      mv_count=1; skipSTACK(2); return; # T als Wert zurück
    }
    Symbol_value(S(plus3)) = Symbol_value(S(plus2)); # (SETQ +++ ++)
    Symbol_value(S(plus2)) = Symbol_value(S(plus)); # (SETQ ++ +)
    Symbol_value(S(plus)) = Symbol_value(S(minus)); # (SETQ + -)
    Symbol_value(S(minus)) = value1; # (SETQ - form)
    eval(value1); # Form auswerten (im aktuellen Environment)
    pushSTACK(value1); # einen Wert retten
    mv_to_list(); # Werte in Liste packen
    # Stackaufbau: ..., val1, vals.
    Symbol_value(S(durch3)) = Symbol_value(S(durch2)); # (SETQ /// //)
    Symbol_value(S(durch2)) = Symbol_value(S(durch)); # (SETQ // /)
    Symbol_value(S(durch)) = STACK_0; # (SETQ / vals)
    Symbol_value(S(mal3)) = Symbol_value(S(mal2)); # (SETQ *** **)
    Symbol_value(S(mal2)) = Symbol_value(S(mal)); # (SETQ ** *)
    Symbol_value(S(mal)) = STACK_1; # (SETQ * val1)
    # Werte ausgeben:
    STACK_(1+2) = var_stream(S(standard_output),strmflags_wr_ch_B); # ostream := Wert von *STANDARD-OUTPUT*
    #if 0
    if (mconsp(STACK_0)) {
      loop {
        var object valsr = STACK_0;
        STACK_0 = Cdr(valsr);
        terpri(&STACK_(1+2));
        prin1(&STACK_(1+2),Car(valsr)); # nächsten Wert ausgeben
        # ';' als Trennzeichen vorm Zeilenende:
        if (matomp(STACK_0))
          break;
        write_ascii_char(&STACK_(1+2),' ');
        write_ascii_char(&STACK_(1+2),';');
      }
    }
    #else
    # unnötige Leerzeile zwischen Input und Output vermeiden:
    # (Es erscheint immer noch eine unnötige Leerzeile am Bildschirm,
    # wenn stdin vom Terminal kommt und stdout eine Pipe ist, die
    # letztendlich wieder aufs Terminal geht - z.B. via '| tee logfile'.
    # In diesem Fall müssen wir aber - eben wegen 'logfile' - ein NL auf
    # stdout ausgeben, und da stdin am Zeilenende von selbst ein NL aus-
    # gibt, ist diese Leerzeile wirklich unvermeidlich.)
    if (!eq(get_line_position(STACK_(1+2)),Fixnum_0))
      terpri(&STACK_(1+2)); # (fresh-line ostream)
    if (mconsp(STACK_0)) {
      loop {
        var object valsr = STACK_0;
        STACK_0 = Cdr(valsr);
        prin1(&STACK_(1+2),Car(valsr)); # nächsten Wert ausgeben
        # ';' als Trennzeichen vorm Zeilenende:
        if (matomp(STACK_0))
          break;
        write_ascii_char(&STACK_(1+2),' ');
        write_ascii_char(&STACK_(1+2),';');
        terpri(&STACK_(1+2));
      }
    }
    #endif
    skipSTACK(4);
    VALUES1(NIL);
  }

# Startet den normalen Driver (Read-Eval-Print-Loop)
# driver();
  global void driver (void);
  global void driver()
    {
      var p_backtrace_t bt_save = back_trace;
      var const struct backtrace_t bt_here = {NULL, L(driver), STACK , -1};
      back_trace = &bt_here;
      loop {
        var object driverfun = Symbol_value(S(driverstern)); # Wert von *DRIVER*
        if (nullp(driverfun))
          break;
        funcall(driverfun,0); # mit 0 Argumenten aufrufen
      }
      # Default-Driver:
      Symbol_value(S(break_count)) = Fixnum_0; # SYS::*BREAK-COUNT* := 0
      # dann einen Driver-Frame aufbauen:
      {
        var gcv_object_t* top_of_frame = STACK; # Pointer übern Frame
        var sp_jmp_buf returner; # Rücksprungpunkt merken
        finish_entry_frame(DRIVER,&!returner,,;);
        # Hier ist der Einsprungpunkt.
        loop {
          # (SYS::READ-EVAL-PRINT "> ") ausführen:
          pushSTACK(O(prompt_string)); # Prompt "> "
          funcall(L(read_eval_print),1);
          if (eq(value1,T)) # EOF gelesen -> Schleife beenden
            break;
        }
        skipSTACK(2); # Driver-Frame auflösen
      }
      back_trace = bt_save;
    }

/* Starts a secondary driver (Read-Eval-Print-Loop)
 break_driver(continuable_p);
 > continuable_p == can be continued after the driver finishes
 can trigger GC */
global void break_driver (bool continuable_p) {
  var object driverfun = Symbol_value(S(break_driver)); /* *BREAK-DRIVER* */
  if (!nullp(driverfun)) {
    pushSTACK(continuable_p ? T : NIL);
    funcall(driverfun,1); /* call with CONTINUABLE argument */
    if (!continuable_p) /* not continuable? */
      reset(1); /* -> back to the previous REPLoop */
  } else {
    var p_backtrace_t bt_save = back_trace;
    var struct backtrace_t bt_here = {NULL, S(break_driver), STACK , -1};
    back_trace = &bt_here;
    /* Default-Driver: (CLEAR-INPUT *DEBUG-IO*), since whatever has been
       typed so far, was not typed in anticipation of this error */
    Symbol_value(S(terminal_read_stream)) = unbound;
    Symbol_value(S(terminal_read_open_object)) = unbound;
    clear_input(var_stream(S(debug_io),strmflags_rd_ch_B|strmflags_wr_ch_B));
    /* SYS::*BREAK-COUNT* increase: */
    dynamic_bind(S(break_count),fixnum_inc(Symbol_value(S(break_count)),1));
    if (!posfixnump(Symbol_value(S(break_count)))) /* should be Fixnum >=0 */
      Symbol_value(S(break_count)) = Fixnum_0; /* oops - fix it! */
    { /* bind *STANDARD-INPUT* and *STANDARD-OUTPUT* to *DEBUG-IO* */
      var object stream =
        var_stream(S(debug_io),strmflags_rd_ch_B|strmflags_wr_ch_B);
      dynamic_bind(S(standard_input),stream);
      dynamic_bind(S(standard_output),stream);
    }
    dynamic_bind(S(print_escape),T); # bind *PRINT-ESCAPE* to T
    dynamic_bind(S(print_readably),NIL); # bind *PRINT-READABLY* to NIL
    { /* make prompt:
         (format nil "~S. Break> " SYS::*BREAK-COUNT*)
         ==
         (with-output-to-string (s)
           (prin1 SYS::*BREAK-COUNT* s) (write-string ". Break> " s))
         ==
         (let ((s (make-string-output-stream)))
           (prin1 SYS::*BREAK-COUNT* s) (write-string ". Break> " s)
           (get-output-stream-string s)) */
      pushSTACK(make_string_output_stream());
      prin1(&STACK_0,Symbol_value(S(break_count)));
      write_sstring(&STACK_0,O(breakprompt_string));
      STACK_0 = get_output_stream_string(&STACK_0);
    }
    { /* make driver-frame: */
      var gcv_object_t* top_of_frame = STACK; /* pointer over frame */
      var sp_jmp_buf returner; /* return point */
      finish_entry_frame(DRIVER,&!returner,,;);
      /* re-entry point is here */
      loop {
        /* (SYS::READ-EVAL-PRINT Prompt) */
        pushSTACK(STACK_(0+2)); /* Prompt "nnn. Break> " */
        funcall(L(read_eval_print),1);
        if (eq(value1,T)) /* EOF -> finish loop */
          break;
      }
      if (!continuable_p) { /* not continuable? */
        back_trace = bt_save;
        unwind(); reset(1); /* -> back to the previous REPLoop */
      }
      skipSTACK(1+2); /* dissolve driver frame, forget prompt */
      dynamic_unbind(S(print_readably));
      dynamic_unbind(S(print_escape));
      dynamic_unbind(S(standard_output));
      dynamic_unbind(S(standard_input));
      dynamic_unbind(S(break_count));
    }
    back_trace = bt_save;
  }
}

LISPFUNN(load,1)
# (LOAD filename), primitivere Version als in CLTL S. 426
  # Methode:
  # (defun load (filename)
  #   (let ((stream (open filename))
  #         (end-of-file "EOF")) ; einmaliges Objekt
  #     (loop
  #       (let ((obj (read stream nil end-of-file)))
  #         (when (eql obj end-of-file) (return))
  #         (if (compiled-function-p obj) (funcall obj) (eval obj))
  #     ) )
  #     (close stream)
  #     t
  # ) )
  {
    funcall(L(open),1); # (OPEN filename)
    pushSTACK(value1); # stream retten
    loop {
      var object obj = stream_read(&STACK_0,NIL,NIL); # Objekt lesen
      if (eq(obj,eof_value)) # EOF -> fertig
        break;
      if (closurep(obj)) {
        funcall(obj,0); # Closure (vermutlich compilierte Closure) aufrufen
      } else {
        eval_noenv(obj); # sonstige Form evaluieren
      }
    }
    builtin_stream_close(&STACK_0); # stream schließen
    skipSTACK(1); VALUES1(T);
  }

# ---------------------------------------------------------------------------- #
#                   Hilfsfunktionen für Debugger und Stepper

# Die folgenden Funktionen klettern im Stack herum, überschreiten jedoch
# keinen Driver-Frame und auch nicht das obere Stackende.
# Gültige "Stackpointer" sind hierbei Pointer auf Stackelemente oder
# Frames, wo nicht das Stackende und auch kein Driver-Frame ist.
# Modus 1: alle Stackitems
# Modus 2: Frames
# Modus 3: lexikalische Frames: Frame-Info hat FRAME_BIT = 1 und
#          (SKIP2_BIT = 1 oder ENTRYPOINT_BIT = 0 oder BLOCKGO_BIT = 1)
# Modus 4: EVAL- und APPLY-Frames: Frame-Info = [TRAPPED_]EVAL/APPLY_FRAME_INFO
# Modus 5: APPLY-Frames: Frame-Info = [TRAPPED_]APPLY_FRAME_INFO

# Macro: Testet, ob FRAME ein Stackende erreicht hat.
#define stack_upend_p()  \
  (   eq(FRAME_(0),nullobj) # Nullword = oberes Stackende                    \
   || (framecode(FRAME_(0)) == DRIVER_frame_info) # Driver-Frame = Stackende \
   || ((framepointerp(Symbol_value(S(frame_limit2))))                        \
       && (uTheFramepointer(Symbol_value(S(frame_limit2))) cmpSTACKop FRAME) # FRAME > *frame-limit2* ? \
  )   )
#define stack_downend_p()  \
  (   (framecode(FRAME_(0)) == DRIVER_frame_info) # Driver-Frame = Stackende \
   || ((framepointerp(Symbol_value(S(frame_limit1))))                        \
       && (FRAME cmpSTACKop uTheFramepointer(Symbol_value(S(frame_limit1)))) # FRAME < *frame-limit1* ? \
  )   )

# Macro: Testet, ob FRAME auf einen Frame zeigt.
# in erster Näherung:
# #define frame_p()  (!( (as_oint(FRAME_(0)) & wbit(frame_bit_o)) ==0))
# in zweiter Näherung, unter Berücksichtigung der Frames mit Skip2-bit:
  #define frame_p()  framep(FRAME)
  local bool framep (gcv_object_t* FRAME);
  local bool framep(FRAME)
    var gcv_object_t* FRAME;
    {
      # Ein normales Lisp-Objekt ist kein Frame:
      if ((as_oint(FRAME_(0)) & wbit(frame_bit_o)) ==0)
        return false;
      # Beginnt bei FRAME_(-1) ein Frame ohne Skip2-Bit, so ist FRAME_(0)
      # Teil dieses Frames, also nicht selber Beginn eines Frames:
      if (   (!(FRAME==STACK)) # nicht die STACK-Grenzen überschreiten!
          && ((as_oint(FRAME_(-1)) & wbit(skip2_bit_o)) == 0)
          && framep(FRAME STACKop -1)
         )
        return false;
      return true; # Sonst beginnt hier ein Frame.
    }

# Macro: Erniedrigt FRAME bis zum nächsten Frame.
#define next_frame_down()  do { FRAME skipSTACKop -1; } until (frame_p());

# Macro: Testet, ob der Frame bei FRAME ein lexikalischer Frame ist.
#ifdef entrypoint_bit_t
#define lexical_frame_p()  \
  (   (!( (as_oint(FRAME_(0)) & wbit(skip2_bit_o)) ==0))   \
   || ( (as_oint(FRAME_(0)) & wbit(entrypoint_bit_o)) ==0) \
   || (!( (as_oint(FRAME_(0)) & wbit(blockgo_bit_o)) ==0)) \
  )
#else
#define lexical_frame_p()  \
  (/* (!( (as_oint(FRAME_(0)) & wbit(skip2_bit_o)) ==0))   \
   || */ (framecode(FRAME_(0)) >= entrypoint_limit_t)      \
   || (!( (as_oint(FRAME_(0)) & wbit(blockgo_bit_o)) ==0)) \
  )
#endif

# Macro: Testet, ob der Frame bei FRAME ein EVAL/APPLY-Frame ist.
#define evalapply_frame_p()  \
  ((framecode(FRAME_(0)) & ~(bit(eval_bit_t)|bit(trapped_bit_t))) == \
   ((EVAL_frame_info|APPLY_frame_info) & ~(bit(eval_bit_t)|bit(trapped_bit_t))))

# Macro: Testet, ob der Frame bei FRAME ein APPLY-Frame ist.
#define apply_frame_p()  \
  ((framecode(FRAME_(0)) & ~bit(trapped_bit_t)) == (APPLY_frame_info & ~bit(trapped_bit_t)))

# UP: überspringt ein Stackitem nach oben
  local gcv_object_t* frame_up_1 (gcv_object_t* stackptr);
  local gcv_object_t* frame_up_1(stackptr)
    var gcv_object_t* stackptr;
    {
      var gcv_object_t* FRAME = stackptr;
      if (frame_p())
        FRAME = topofframe(FRAME_(0)); # Pointer übern Frame
      else
        FRAME skipSTACKop 1; # Pointer aufs nächste Objekt
      return (stack_upend_p() ? stackptr : FRAME);
    }

# UP: überspringt ein Stackitem nach unten
  local gcv_object_t* frame_down_1 (gcv_object_t* stackptr);
  local gcv_object_t* frame_down_1(stackptr)
    var gcv_object_t* stackptr;
    {
      var gcv_object_t* FRAME = stackptr;
      next_frame_down(); # nächsten Frame drunter suchen
      if (!(topofframe(FRAME_(0)) == stackptr)) # nicht direkt unterhalb stackptr?
        FRAME = stackptr STACKop -1;
      return (stack_downend_p() ? stackptr : FRAME);
    }

# UP: springt zum nächsthöheren Frame
  local gcv_object_t* frame_up_2 (gcv_object_t* stackptr);
  local gcv_object_t* frame_up_2(stackptr)
    var gcv_object_t* stackptr;
    {
      var gcv_object_t* FRAME = stackptr;
      if (frame_p())
        FRAME = topofframe(FRAME_(0)); # Pointer übern Frame
      else
        FRAME skipSTACKop 1; # Pointer aufs nächste Objekt
      loop {
        if (stack_upend_p())
          return stackptr;
        if (as_oint(FRAME_(0)) & wbit(frame_bit_o))
          return FRAME;
        FRAME skipSTACKop 1;
      }
    }

# UP: springt zum nächstniedrigeren Frame
  local gcv_object_t* frame_down_2 (gcv_object_t* stackptr);
  local gcv_object_t* frame_down_2(stackptr)
    var gcv_object_t* stackptr;
    {
      var gcv_object_t* FRAME = stackptr;
      next_frame_down(); # nächsten Frame drunter suchen
      return (stack_downend_p() ? stackptr : FRAME);
    }

# UP: springt zum nächsthöheren lexikalischen Frame
  local gcv_object_t* frame_up_3 (gcv_object_t* stackptr);
  local gcv_object_t* frame_up_3(stackptr)
    var gcv_object_t* stackptr;
    {
      var gcv_object_t* FRAME = stackptr;
      if (frame_p())
        FRAME = topofframe(FRAME_(0)); # Pointer übern Frame
      else
        FRAME skipSTACKop 1; # Pointer aufs nächste Objekt
      loop {
        if (stack_upend_p())
          return stackptr;
        if (frame_p()) {
          if (lexical_frame_p())
            return FRAME;
          FRAME = topofframe(FRAME_(0)); # Pointer übern Frame
        } else {
          FRAME skipSTACKop 1;
        }
      }
    }

# UP: springt zum nächstniedrigeren lexikalischen Frame
  local gcv_object_t* frame_down_3 (gcv_object_t* stackptr);
  local gcv_object_t* frame_down_3(stackptr)
    var gcv_object_t* stackptr;
    {
      var gcv_object_t* FRAME = stackptr;
      loop {
        next_frame_down(); # nächsten Frame drunter suchen
        if (stack_downend_p())
          return stackptr;
        if (lexical_frame_p())
          break;
      }
      return FRAME;
    }

# UP: springt zum nächsthöheren EVAL/APPLY-Frame
  local gcv_object_t* frame_up_4 (gcv_object_t* stackptr);
  local gcv_object_t* frame_up_4(stackptr)
    var gcv_object_t* stackptr;
    {
      var gcv_object_t* FRAME = stackptr;
      if (frame_p())
        FRAME = topofframe(FRAME_(0)); # Pointer übern Frame
      else
        FRAME skipSTACKop 1; # Pointer aufs nächste Objekt
      loop {
        if (stack_upend_p())
          return stackptr;
        if (frame_p()) {
          if (evalapply_frame_p())
            return FRAME;
          FRAME = topofframe(FRAME_(0)); # Pointer übern Frame
        } else {
          FRAME skipSTACKop 1;
        }
      }
    }

# UP: springt zum nächstniedrigeren EVAL/APPLY-Frame
  local gcv_object_t* frame_down_4 (gcv_object_t* stackptr);
  local gcv_object_t* frame_down_4(stackptr)
    var gcv_object_t* stackptr;
    {
      var gcv_object_t* FRAME = stackptr;
      loop {
        next_frame_down(); # nächsten Frame drunter suchen
        if (stack_downend_p())
          return stackptr;
        if (evalapply_frame_p())
          break;
      }
      return FRAME;
    }

# UP: springt zum nächsthöheren APPLY-Frame
  local gcv_object_t* frame_up_5 (gcv_object_t* stackptr);
  local gcv_object_t* frame_up_5(stackptr)
    var gcv_object_t* stackptr;
    {
      var gcv_object_t* FRAME = stackptr;
      if (frame_p())
        FRAME = topofframe(FRAME_(0)); # Pointer übern Frame
      else
        FRAME skipSTACKop 1; # Pointer aufs nächste Objekt
      loop {
        if (stack_upend_p())
          return stackptr;
        if (frame_p()) {
          if (apply_frame_p())
            return FRAME;
          FRAME = topofframe(FRAME_(0)); # Pointer übern Frame
        } else {
          FRAME skipSTACKop 1;
        }
      }
    }

# UP: springt zum nächstniedrigeren APPLY-Frame
  local gcv_object_t* frame_down_5 (gcv_object_t* stackptr);
  local gcv_object_t* frame_down_5(stackptr)
    var gcv_object_t* stackptr;
    {
      var gcv_object_t* FRAME = stackptr;
      loop {
        next_frame_down(); # nächsten Frame drunter suchen
        if (stack_downend_p())
          return stackptr;
        if (apply_frame_p())
          break;
      }
      return FRAME;
    }

# Typ eines Pointers auf eine Hochsteige- bzw. Absteige-Routine:
typedef gcv_object_t* (*climb_fun_t) (gcv_object_t* stackptr);

local const climb_fun_t frame_up_table[] =
  { &frame_up_1, &frame_up_2, &frame_up_3, &frame_up_4, &frame_up_5, };
local const climb_fun_t frame_down_table[] =
  { &frame_down_1, &frame_down_2, &frame_down_3, &frame_down_4, &frame_down_5, };

# UP: Überprüft und decodiert das mode-Argument.
# test_mode_arg(table)
# > STACK_0: mode
# > table: Tabelle der Routinen zum Hochsteigen bzw. zum Absteigen
# < ergebnis: Routine zum Hochsteigen bzw. zum Absteigen
# erhöht STACK um 1
local climb_fun_t test_mode_arg (const climb_fun_t* table) {
  var object arg = popSTACK();
  var uintL mode;
  if (!(posfixnump(arg)
        && ((mode = posfixnum_to_L(arg)) > 0)
        && (mode<=5))) {
    pushSTACK(arg); # TYPE-ERROR slot DATUM
    pushSTACK(O(type_climb_mode)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(arg);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~: bad frame climbing mode ~"));
  }
  return table[mode-1];
}

# UP: Überprüft ein Frame-Pointer-Argument.
# test_framepointer_arg()
# > STACK_0: Lisp-Objekt, sollte ein Frame-Pointer sein
# < ergebnis: Frame-Pointer
# erhöht STACK um 1
  local gcv_object_t* test_framepointer_arg (void);
  local gcv_object_t* test_framepointer_arg()
    {
      var object arg = popSTACK();
      if (!framepointerp(arg)) {
        pushSTACK(arg);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~: ~ is not a stack pointer")
              );
      }
      return uTheFramepointer(arg);
    }

LISPFUNN(frame_up_1,2)
# (SYS::FRAME-UP-1 framepointer mode) liefert den Frame-Pointer 1 höher.
  {
    var climb_fun_t frame_up_x = test_mode_arg(&frame_up_table[0]);
    var gcv_object_t* stackptr = test_framepointer_arg();
    stackptr = (*frame_up_x)(stackptr); # einmal hochsteigen
    VALUES1(make_framepointer(stackptr));
  }

LISPFUNN(frame_up,2)
# (SYS::FRAME-UP framepointer mode) liefert den Frame-Pointer ganz oben.
  {
    var climb_fun_t frame_up_x = test_mode_arg(&frame_up_table[0]);
    var gcv_object_t* stackptr = test_framepointer_arg();
    # hochsteigen, bis es nicht mehr weiter geht:
    loop {
      var gcv_object_t* next_stackptr = (*frame_up_x)(stackptr);
      if (next_stackptr == stackptr)
        break;
      stackptr = next_stackptr;
    }
    VALUES1(make_framepointer(stackptr));
  }

LISPFUNN(frame_down_1,2)
# (SYS::FRAME-DOWN-1 framepointer mode) liefert den Frame-Pointer 1 drunter.
  {
    var climb_fun_t frame_down_x = test_mode_arg(&frame_down_table[0]);
    var gcv_object_t* stackptr = test_framepointer_arg();
    stackptr = (*frame_down_x)(stackptr); # einmal hinabsteigen
    VALUES1(make_framepointer(stackptr));
  }

LISPFUNN(frame_down,2)
# (SYS::FRAME-DOWN framepointer mode) liefert den Frame-Pointer ganz unten.
  {
    var climb_fun_t frame_down_x = test_mode_arg(&frame_down_table[0]);
    var gcv_object_t* stackptr = test_framepointer_arg();
    # hinabsteigen, bis es nicht mehr weiter geht:
    loop {
      var gcv_object_t* next_stackptr = (*frame_down_x)(stackptr);
      if (next_stackptr == stackptr)
        break;
      stackptr = next_stackptr;
    }
    VALUES1(make_framepointer(stackptr));
  }

LISPFUNN(the_frame,0)
# (SYS::THE-FRAME) liefert den aktuellen Stackpointer als Frame-Pointer.
  {
    var gcv_object_t* stackptr = STACK;
    stackptr = frame_up_2(stackptr); # bis zum nächsthöheren Frame hoch
    VALUES1(make_framepointer(stackptr));
  }

# UP: aktiviert dasselbe lexikalische Environment, das beim Framepointer
# STACK_0 aktiv war.
# same_env_as();
# erhöht STACK um 1, baut auf dem STACK einen ENV5-Frame auf
  local void same_env_as (void);
  local void same_env_as()
    {
      var gcv_object_t* FRAME = test_framepointer_arg();
      # 5 Environments noch "leer":
      var object found_var_env = nullobj;
      var object found_fun_env = nullobj;
      var object found_block_env = nullobj;
      var object found_go_env = nullobj;
      var object found_decl_env = nullobj;
      # und füllen:
      loop {
        # ab FRAME abwärts nach ENV-Frames suchen:
        loop {
          FRAME skipSTACKop -1;
          if (FRAME==STACK) # Stack zu Ende?
            goto end;
          if (frame_p()
              && (!( (as_oint(FRAME_(0)) & wbit(skip2_bit_o)) ==0))
              && (!( (as_oint(FRAME_(0)) & wbit(envbind_bit_o)) ==0))
             )
            break;
        }
        # Nächster ENV-Frame gefunden.
        # Sein Inhalt füllt die noch leeren Komponenten von env:
        switch (framecode(FRAME_(0)) & envbind_case_mask_t) {
          case (ENV1V_frame_info & envbind_case_mask_t): # 1 VAR_ENV
            if (eq(found_var_env,nullobj)) { found_var_env = FRAME_(1); }
            break;
          case (ENV1F_frame_info & envbind_case_mask_t): # 1 FUN_ENV
            if (eq(found_fun_env,nullobj)) { found_fun_env = FRAME_(1); }
            break;
          case (ENV1B_frame_info & envbind_case_mask_t): # 1 BLOCK_ENV
            if (eq(found_block_env,nullobj)) { found_block_env = FRAME_(1); }
            break;
          case (ENV1G_frame_info & envbind_case_mask_t): # 1 GO_ENV
            if (eq(found_go_env,nullobj)) { found_go_env = FRAME_(1); }
            break;
          case (ENV1D_frame_info & envbind_case_mask_t): # 1 DECL_ENV
            if (eq(found_decl_env,nullobj)) { found_decl_env = FRAME_(1); }
            break;
          case (ENV2VD_frame_info & envbind_case_mask_t): # 1 VAR_ENV und 1 DECL_ENV
            if (eq(found_var_env,nullobj)) { found_var_env = FRAME_(1); }
            if (eq(found_decl_env,nullobj)) { found_decl_env = FRAME_(2); }
            break;
          case (ENV5_frame_info & envbind_case_mask_t): # alle 5 Environments
            if (eq(found_var_env,nullobj)) { found_var_env = FRAME_(1); }
            if (eq(found_fun_env,nullobj)) { found_fun_env = FRAME_(2); }
            if (eq(found_block_env,nullobj)) { found_block_env = FRAME_(3); }
            if (eq(found_go_env,nullobj)) { found_go_env = FRAME_(4); }
            if (eq(found_decl_env,nullobj)) { found_decl_env = FRAME_(5); }
            break;
          default: NOTREACHED;
        }
        # Falls alle einzelnen Environments von env gefüllt (/=nullobj) sind,
        # ist das Environment fertig:
        if (   (!eq(found_var_env,nullobj))
            && (!eq(found_fun_env,nullobj))
            && (!eq(found_block_env,nullobj))
            && (!eq(found_go_env,nullobj))
            && (!eq(found_decl_env,nullobj))
           )
          goto fertig;
      }
     end: # Stack zu Ende.
      # Hole restliche Environment-Komponenten aus dem aktuellen Environment:
      if (eq(found_var_env,nullobj)) { found_var_env = aktenv.var_env; }
      if (eq(found_fun_env,nullobj)) { found_fun_env = aktenv.fun_env; }
      if (eq(found_block_env,nullobj)) { found_block_env = aktenv.block_env; }
      if (eq(found_go_env,nullobj)) { found_go_env = aktenv.go_env; }
      if (eq(found_decl_env,nullobj)) { found_decl_env = aktenv.decl_env; }
     fertig:
      # Environment-Frame aufbauen:
      make_ENV5_frame();
      # aktuelle Environments setzen:
      aktenv.var_env   = found_var_env  ;
      aktenv.fun_env   = found_fun_env  ;
      aktenv.block_env = found_block_env;
      aktenv.go_env    = found_go_env   ;
      aktenv.decl_env  = found_decl_env ;
    }

LISPFUNN(same_env_as,2)
# (SYS::SAME-ENV-AS framepointer fun) aktiviert dasselbe lexikalische
# Environment, das bei framepointer aktiv war, und ruft dann fun auf.
  {
    var object fun = popSTACK();
    same_env_as(); # Environment von framepointer aktivieren
    funcall(fun,0); # fun aufrufen
    unwind(); # Environment-Frame auflösen
  }

LISPFUNN(eval_at,2)
# (SYS::EVAL-AT framepointer form) aktiviert dasselbe lexikalische
# Environment, das bei framepointer aktiv war, und wertet darin die Form aus.
  {
    var object form = popSTACK();
    same_env_as(); # Environment von framepointer aktivieren
    eval(form); # form auswerten
    unwind(); # Environment-Frame auflösen
  }

LISPFUNN(eval_frame_p,1)
# (SYS::EVAL-FRAME-P framepointer)
# gibt an, ob framepointer auf einen EVAL/APPLY-Frame zeigt.
  {
    var gcv_object_t* FRAME = test_framepointer_arg();
    VALUES_IF(evalapply_frame_p());
  }

LISPFUNN(driver_frame_p,1)
# (SYS::DRIVER-FRAME-P framepointer)
# gibt an, ob framepointer auf einen Driver-Frame zeigt.
  {
    var gcv_object_t* FRAME = test_framepointer_arg();
    VALUES_IF(framecode(FRAME_(0)) == DRIVER_frame_info);
  }

# Fehlermeldung, wenn kein EVAL/APPLY-Frame-Pointer vorliegt.
# fehler_evalframe(obj);
# > obj: kein EVAL/APPLY-Frame-Pointer
  nonreturning_function(local, fehler_evalframe, (object obj)) {
    pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,
           GETTEXT("~: ~ is not a pointer to an EVAL/APPLY frame")
          );
  }

LISPFUNN(trap_eval_frame,2)
# (SYS::TRAP-EVAL-FRAME framepointer flag) schaltet den Breakpoint am
# angegebenen EVAL/APPLY-Frame je nach flag an bzw. aus.
  {
    var object flag = popSTACK();
    var object frame = popSTACK();
    if (!framepointerp(frame))
      fehler_evalframe(frame);
    var gcv_object_t* FRAME = uTheFramepointer(frame);
    if (!evalapply_frame_p())
      fehler_evalframe(frame);
    # FRAME zeigt auf den EVAL/APPLY-Frame.
    if (!nullp(flag)) {
      # Breakpoint einschalten
      *(oint*)(&FRAME_(0)) |= wbit(trapped_bit_o);
    } else {
      # Breakpoint ausschalten
      *(oint*)(&FRAME_(0)) &= ~wbit(trapped_bit_o);
    }
    VALUES1(frame);
  }

LISPFUNN(redo_eval_frame,1)
# (SYS::REDO-EVAL-FRAME framepointer) unwindet bis zum angegebenen
# EVAL/APPLY-Frame und fängt erneut an, diesen abzuarbeiten.
  {
    var object frame = popSTACK();
    if (!framepointerp(frame))
      fehler_evalframe(frame);
    var gcv_object_t* FRAME = uTheFramepointer(frame);
    if (!evalapply_frame_p())
      fehler_evalframe(frame);
    # FRAME zeigt auf den EVAL/APPLY-Frame.
    VALUES0;
    unwind_upto(FRAME); # bis zum EVAL/APPLY-Frame alles auflösen, dorthin springen
  }

LISPFUNN(return_from_eval_frame,2)
# (SYS::RETURN-FROM-EVAL-FRAME framepointer form)
# unwindet bis zum angegebenen EVAL/APPLY-Frame und gibt als dessen Werte die
# Werte der Evaluierung der angegebenen form zurück.
  {
    var object form = popSTACK();
    var object frame = popSTACK();
    if (!framepointerp(frame))
      fehler_evalframe(frame);
    var gcv_object_t* FRAME = uTheFramepointer(frame);
    if (!evalapply_frame_p())
      fehler_evalframe(frame);
    # FRAME zeigt auf den EVAL/APPLY-Frame.
    VALUES1(form);
    unwind_upto(FRAME); # bis zum EVAL/APPLY-Frame alles auflösen, dorthin springen
  }

# ------------------------------------------------------------------------- #
#                                 Debug aux

local void print_back_trace (const gcv_object_t* stream_,
                             const struct backtrace_t *bt, int index) {
  terpri(stream_);
  write_ascii_char(stream_,'<');
  if (index >= 0) prin1(stream_,fixnum(index));
  else write_ascii_char(stream_,'#');
  write_ascii_char(stream_,'>');
  write_ascii_char(stream_,' ');
  prin1(stream_,bt->bt_caller);
  if (bt->bt_num_arg >= 0) {
    write_ascii_char(stream_,' ');
    prin1(stream_,fixnum(bt->bt_num_arg));
  }
}

# UP: Gibt das Stackitem FRAME_(0) detailliert auf den Stream aus
# und liefert den nächsthöheren stackptr.
# print_stackitem(&stream,FRAME)
# can trigger GC
  local gcv_object_t* print_stackitem (const gcv_object_t* stream_, gcv_object_t* FRAME);
  local gcv_object_t* print_stackitem(stream_,FRAME)
    var const gcv_object_t* stream_;
    var gcv_object_t* FRAME;
    {
      if (!frame_p()) {
        # kein Frame, normales LISP-Objekt
        write_sstring(stream_,O(showstack_string_lisp_obj)); # "¿- "
        var object obj = FRAME_(0);
        #if !defined(NO_symbolflags)
        switch (typecode(obj)) { # evtl. Symbol-Flags entfernen
          case_symbolflagged: obj = symbol_without_flags(obj);
          default: break;
        }
        #endif
        prin1(stream_,obj); # LISP-Objekt ausgeben
        return FRAME STACKop 1;
      } else {
        # Frame angetroffen
        var gcv_object_t* FRAME_top = topofframe(FRAME_(0)); # Pointer übern Frame
        switch (framecode(FRAME_(0))) { # je nach Frametyp
          case TRAPPED_APPLY_frame_info:
            # getrapte APPLY-Frames:
            write_sstring(stream_,CLSTEXT(NLstring "APPLY frame with breakpoint for call "));
            goto APPLY_frame;
          case APPLY_frame_info:
            # APPLY-Frames:
            write_sstring(stream_,CLSTEXT(NLstring "APPLY frame for call "));
          APPLY_frame:
            # Funktionsnamen und Argumente ausgeben:
            write_ascii_char(stream_,'('); # '(' ausgeben
            prin1(stream_,TheIclosure(FRAME_(frame_closure))->clos_name); # Namen ausgeben
            {
              var gcv_object_t* argptr = FRAME_top;
              var uintL count = STACK_item_count(FRAME STACKop frame_args,FRAME_top);
              dotimesL(count,count, {
                write_ascii_char(stream_,' '); # ' ' ausgeben
                write_ascii_char(stream_,'\''); # "'" ausgeben
                prin1(stream_,NEXT(argptr)); # nächstes Argument ausgeben
              });
            }
            write_ascii_char(stream_,')'); # ')' ausgeben
            break;
          case TRAPPED_EVAL_frame_info:
            # getrapte EVAL-Frames:
            write_sstring(stream_,CLSTEXT(NLstring "EVAL frame with breakpoint for form "));
            goto EVAL_frame;
          case EVAL_frame_info:
            # EVAL-Frames:
            write_sstring(stream_,CLSTEXT(NLstring "EVAL frame for form "));
          EVAL_frame:
            prin1(stream_,FRAME_(frame_form)); # Form ausgeben
            break;
          case DYNBIND_frame_info:
            # dynamische Variablenbindungsframes:
            write_sstring(stream_,CLSTEXT(NLstring "frame binding variables (~ = dynamically):"));
            # Bindungen ausgeben:
            FRAME skipSTACKop 1;
            until (FRAME==FRAME_top) {
              # Bindung von Symbol FRAME_(0) an Wert FRAME_(1) ausgeben:
              write_sstring(stream_,O(showstack_string_bindung)); # "¿  | "
              write_ascii_char(stream_,'~'); # '~' ausgeben
              write_ascii_char(stream_,' '); # ' ' ausgeben
              prin1(stream_,FRAME_(0)); # Symbol ausgeben
              write_sstring(stream_,O(showstack_string_zuord)); # " <--> "
              prin1(stream_,FRAME_(1)); # Wert ausgeben
              FRAME skipSTACKop 2;
            }
            break;
          #ifdef HAVE_SAVED_REGISTERS
          case CALLBACK_frame_info:
            # Callback-Register-Frames:
            write_sstring(stream_,CLSTEXT(NLstring "CALLBACK frame"));
            break;
          #endif
          # Variablen- und Funktionsbindungsframes:
          case VAR_frame_info:
            write_sstring(stream_,CLSTEXT(NLstring "frame binding variables "));
            #ifdef NO_symbolflags
            prin1(stream_,make_framepointer(FRAME)); # Frame-Pointer ausgeben
            write_sstring(stream_,CLSTEXT(" binds (~ = dynamically):"));
            pushSTACK(FRAME_(frame_next_env)); # weiteres Environment retten
            # Bindungen ausgeben:
            FRAME skipSTACKop frame_bindings;
            until (FRAME==FRAME_top) {
              if (!( (as_oint(FRAME_(varframe_binding_mark)) & wbit(active_bit_o)) ==0)) {
                # Bindung von Symbol FRAME_(1) an Wert FRAME_(2) ausgeben:
                write_sstring(stream_,O(showstack_string_bindung)); # "¿  | "
                if (!( (as_oint(FRAME_(varframe_binding_mark)) & wbit(dynam_bit_o)) ==0)) # Bindung dynamisch?
                  write_ascii_char(stream_,'~'); # ja -> '~' ausgeben
                write_ascii_char(stream_,' '); # ' ' ausgeben
                prin1(stream_,symbol_without_flags(FRAME_(varframe_binding_sym))); # Symbol ausgeben
                write_sstring(stream_,O(showstack_string_zuord)); # " <--> "
                prin1(stream_,FRAME_(varframe_binding_value)); # Wert ausgeben
              }
              FRAME skipSTACKop varframe_binding_size;
            }
            goto VARFUN_frame_next;
            #else
            goto VARFUN_frame;
            #endif
          case FUN_frame_info:
            write_sstring(stream_,CLSTEXT(NLstring "frame binding functions "));
            goto VARFUN_frame;
          VARFUN_frame:
            prin1(stream_,make_framepointer(FRAME)); # Frame-Pointer ausgeben
            write_sstring(stream_,CLSTEXT(" binds (~ = dynamically):"));
            pushSTACK(FRAME_(frame_next_env)); # weiteres Environment retten
            # Bindungen ausgeben:
            FRAME skipSTACKop frame_bindings;
            until (FRAME==FRAME_top) {
              if (!( (as_oint(FRAME_(0)) & wbit(active_bit_o)) ==0)) {
                # Bindung von Symbol FRAME_(0) an Wert FRAME_(1) ausgeben:
                write_sstring(stream_,O(showstack_string_bindung)); # "¿  | "
                if (!( (as_oint(FRAME_(0)) & wbit(dynam_bit_o)) ==0)) # Bindung dynamisch?
                  write_ascii_char(stream_,'~'); # ja -> '~' ausgeben
                write_ascii_char(stream_,' '); # ' ' ausgeben
                prin1(stream_,symbol_without_flags(FRAME_(0))); # Symbol ausgeben
                write_sstring(stream_,O(showstack_string_zuord)); # " <--> "
                prin1(stream_,FRAME_(1)); # Wert ausgeben
              }
              FRAME skipSTACKop 2;
            }
          VARFUN_frame_next:
            # Weiteres Environment ausgeben:
            write_sstring(stream_,CLSTEXT(NLstring "  Next environment: "));
            {
              var object env = popSTACK(); # weiteres Environment
              if (!simple_vector_p(env)) {
                prin1(stream_,env);
              } else {
                # weiteres Environment ist ein Vektor, der Länge 2n+1
                do {
                  pushSTACK(env);
                  var uintL count = floor(Svector_length(env),2); # = n = Bindungszahl
                  var uintL index = 0;
                  dotimesL(count,count, {
                    write_sstring(stream_,O(showstack_string_bindung)); # "¿  | "
                    prin1(stream_,TheSvector(STACK_0)->data[index++]); # Symbol ausgeben
                    write_sstring(stream_,O(showstack_string_zuord)); # " <--> "
                    prin1(stream_,TheSvector(STACK_0)->data[index++]); # Symbol ausgeben
                  });
                  env = TheSvector(popSTACK())->data[index]; # letztes Vektor-Element
                } while (simple_vector_p(env));
              }
            }
            break;
          # Compilierte Block/Tagbody-Frames:
          case CBLOCK_CTAGBODY_frame_info:
            if (simple_vector_p(Car(FRAME_(frame_ctag)))) {
              # compilierte Tagbody-Frames:
              write_sstring(stream_,CLSTEXT(NLstring "compiled tagbody frame for "));
              prin1(stream_,Car(FRAME_(frame_ctag))); # Tag-Vektor
            } else {
              # compilierte Block-Frames:
              write_sstring(stream_,CLSTEXT(NLstring "compiled block frame for "));
              prin1(stream_,Car(FRAME_(frame_ctag))); # Blockname
            }
            break;
          # Interpretierte Block-Frames:
          case IBLOCK_frame_info:
            write_sstring(stream_,CLSTEXT(NLstring "block frame "));
            goto IBLOCK_frame;
          case NESTED_IBLOCK_frame_info:
            write_sstring(stream_,CLSTEXT(NLstring "nested block frame "));
            goto IBLOCK_frame;
          IBLOCK_frame:
            pushSTACK(FRAME_(frame_next_env));
            prin1(stream_,make_framepointer(FRAME)); # Frame-Pointer ausgeben
            write_sstring(stream_,CLSTEXT(" for "));
            prin1(stream_,FRAME_(frame_name)); # Blockname
            goto NEXT_ENV;
          # Interpretierte Tagbody-Frames:
          case ITAGBODY_frame_info:
            write_sstring(stream_,CLSTEXT(NLstring "tagbody frame "));
            goto ITAGBODY_frame;
          case NESTED_ITAGBODY_frame_info:
            write_sstring(stream_,CLSTEXT(NLstring "nested tagbody frame "));
            goto ITAGBODY_frame;
          ITAGBODY_frame:
            pushSTACK(FRAME_(frame_next_env));
            prin1(stream_,make_framepointer(FRAME)); # Frame-Pointer ausgeben
            write_sstring(stream_,CLSTEXT(" for"));
            # Tags/Bodys ausgeben:
            FRAME skipSTACKop frame_bindings;
            until (FRAME==FRAME_top) {
              # Bindung von Tag FRAME_(0) an Body FRAME_(1) ausgeben:
              write_sstring(stream_,O(showstack_string_bindung)); # "¿  | "
              prin1(stream_,FRAME_(0)); # Tag ausgeben
              write_sstring(stream_,O(showstack_string_zuordtag)); # " --> "
              prin1(stream_,FRAME_(1)); # Body ausgeben
              FRAME skipSTACKop 2;
            }
            goto NEXT_ENV;
          NEXT_ENV: # Ausgeben eines Block- oder Tagbody-Environments STACK_0
            write_sstring(stream_,CLSTEXT(NLstring "  Next environment: "));
            {
              var object env = popSTACK();
              if (!consp(env)) {
                prin1(stream_,env);
              } else {
                # weiteres Environment ist eine Aliste
                do {
                  pushSTACK(Cdr(env));
                  env = Car(env);
                  if (atomp(env)) {
                    pushSTACK(S(show_stack));
                    fehler(error,
                           GETTEXT("~: environment is not an alist")
                          );
                  }
                  pushSTACK(Cdr(env));
                  pushSTACK(Car(env));
                  write_sstring(stream_,O(showstack_string_bindung)); # "¿  | "
                  prin1(stream_,popSTACK());
                  write_sstring(stream_,O(showstack_string_zuordtag)); # " --> "
                  prin1(stream_,popSTACK());
                  env = popSTACK();
                } while (consp(env));
              }
            }
            break;
          case CATCH_frame_info:
            # Catch-Frames:
            write_sstring(stream_,CLSTEXT(NLstring "catch frame for tag "));
            prin1(stream_,FRAME_(frame_tag)); # Tag
            break;
          case HANDLER_frame_info:
            # Handler-Frames:
            write_sstring(stream_,CLSTEXT(NLstring "handler frame for conditions"));
            {
              var uintL m2 = Svector_length(Car(FRAME_(frame_handlers))); # 2*m
              var uintL i = 0;
              do {
                write_ascii_char(stream_,' '); # ' ' ausgeben
                prin1(stream_,TheSvector(Car(FRAME_(frame_handlers)))->data[i]); # Typ i ausgeben
                i += 2;
              } while (i < m2);
            }
            break;
          case UNWIND_PROTECT_frame_info:
            # Unwind-Protect-Frames:
            write_sstring(stream_,CLSTEXT(NLstring "unwind-protect frame"));
            break;
          case DRIVER_frame_info:
            # Driver-Frames:
            write_sstring(stream_,CLSTEXT(NLstring NLstring "driver frame"));
            break;
          # Environment-Frames:
          case ENV1V_frame_info:
            write_sstring(stream_,CLSTEXT(NLstring "frame binding environments"));
            write_sstring(stream_,O(showstack_string_VENV_frame)); # "¿  VAR_ENV <--> "
            prin1(stream_,FRAME_(1));
            break;
          case ENV1F_frame_info:
            write_sstring(stream_,CLSTEXT(NLstring "frame binding environments"));
            write_sstring(stream_,O(showstack_string_FENV_frame)); # "¿  FUN_ENV <--> "
            prin1(stream_,FRAME_(1));
            break;
          case ENV1B_frame_info:
            write_sstring(stream_,CLSTEXT(NLstring "frame binding environments"));
            write_sstring(stream_,O(showstack_string_BENV_frame)); # "¿  BLOCK_ENV <--> "
            prin1(stream_,FRAME_(1));
            break;
          case ENV1G_frame_info:
            write_sstring(stream_,CLSTEXT(NLstring "frame binding environments"));
            write_sstring(stream_,O(showstack_string_GENV_frame)); # "¿  GO_ENV <--> "
            prin1(stream_,FRAME_(1));
            break;
          case ENV1D_frame_info:
            write_sstring(stream_,CLSTEXT(NLstring "frame binding environments"));
            write_sstring(stream_,O(showstack_string_DENV_frame)); # "¿  DECL_ENV <--> "
            prin1(stream_,FRAME_(1));
            break;
          case ENV2VD_frame_info:
            write_sstring(stream_,CLSTEXT(NLstring "frame binding environments"));
            write_sstring(stream_,O(showstack_string_VENV_frame)); # "¿  VAR_ENV <--> "
            prin1(stream_,FRAME_(1));
            write_sstring(stream_,O(showstack_string_DENV_frame)); # "¿  DECL_ENV <--> "
            prin1(stream_,FRAME_(2));
            break;
          case ENV5_frame_info:
            write_sstring(stream_,CLSTEXT(NLstring "frame binding environments"));
            write_sstring(stream_,O(showstack_string_VENV_frame)); # "¿  VAR_ENV <--> "
            prin1(stream_,FRAME_(1));
            write_sstring(stream_,O(showstack_string_FENV_frame)); # "¿  FUN_ENV <--> "
            prin1(stream_,FRAME_(2));
            write_sstring(stream_,O(showstack_string_BENV_frame)); # "¿  BLOCK_ENV <--> "
            prin1(stream_,FRAME_(3));
            write_sstring(stream_,O(showstack_string_GENV_frame)); # "¿  GO_ENV <--> "
            prin1(stream_,FRAME_(4));
            write_sstring(stream_,O(showstack_string_DENV_frame)); # "¿  DECL_ENV <--> "
            prin1(stream_,FRAME_(5));
            break;
          default:
            pushSTACK(S(show_stack));
            fehler(serious_condition,
                   GETTEXT("~: unknown frame type")
                  );
        }
        return FRAME_top; # Pointer übern Frame
      }
    }

LISPFUNN(describe_frame,2)
# (SYS::DESCRIBE-FRAME stream framepointer) gibt das Stackitem, auf das der
# Pointer zeigt, detailliert aus.
  {
    var gcv_object_t* FRAME = test_framepointer_arg(); # Pointer in den Stack
    if (!streamp(STACK_0)) fehler_stream(STACK_0);
    { var p_backtrace_t bt = back_trace;
      unwind_back_trace(bt,FRAME);
      if (bt->bt_stack == FRAME) print_back_trace(&STACK_0,bt,0); }
    print_stackitem(&STACK_0,FRAME); # Stack-Item ausgeben
    skipSTACK(1); VALUES0; /* no values */
  }

/* UP: print the stack (up to frame_limit frames, if that is non-0)
 frame by frame (moving using frame_up_x) or all stack items if that is NULL.
 starting with start_frame or STACK if that is NULL
 In debugger, use 'show_stack(0,0,0)'
 can trigger GC */
local inline uintL show_stack (climb_fun_t frame_up_x, uintL frame_limit,
                               gcv_object_t* start_frame)
{ /* run along the stack upwards */
  var gcv_object_t* FRAME = (start_frame == NULL ? STACK : start_frame);
  pushSTACK(var_stream(S(standard_output),strmflags_wr_ch_B));
  var gcv_object_t* stream_ = &STACK_0;
  var uintL count = 0;
  var p_backtrace_t bt = back_trace;
  while (!eq(FRAME_(0),nullobj) /* nullobj = stack end */
         && (frame_limit==0 || count<frame_limit)) {
    while (bt_beyond_stack_p(bt,FRAME)) {
      print_back_trace(stream_,bt,++count);
      bt = bt->bt_next;
    }
    if (frame_up_x != NULL) {
      var gcv_object_t* next_frame = (*frame_up_x)(FRAME);
      if (next_frame == FRAME) break;
      print_stackitem(stream_,FRAME = next_frame);
    } else
      FRAME = print_stackitem(stream_,FRAME);
  }
  skipSTACK(1); /* drop *STANDARD-OUTPUT* */
  return count;
}

LISPFUN(show_stack,seclass_default,0,3,norest,nokey,0,NIL)
{ /* (SHOW-STACK mode limit start-frame) print the stack contents. */
  var gcv_object_t* start_frame = (missingp(STACK_0) ? (skipSTACK(1), &STACK_1)
                                   : test_framepointer_arg());
  var uintL frame_limit = (missingp(STACK_0) ? (skipSTACK(1), 0) :
                           posfixnump(STACK_0) ? posfixnum_to_L(popSTACK())
                           : (fehler_posfixnum(popSTACK()), 0));
  var climb_fun_t frame_up_x = (missingp(STACK_0)
                                ? (skipSTACK(1), (climb_fun_t) NULL)
                                : test_mode_arg(&frame_up_table[0]));
  VALUES1(UL_to_I(show_stack(frame_up_x,frame_limit,start_frame)));
}

LISPFUNN(debug,0)
# (SYSTEM::DEBUG) springt in einen im Hintergrund sitzenden Debugger.
  {
    #if !defined(AMIGAOS)
      abort();
    #else # AMIGAOS
      Debug(0);
    #endif
    VALUES0; # keine Werte
  }

LISPFUNN(proom,0)
# (SYSTEM::%ROOM), liefert 3 Werte:
# - von LISP-Objekten belegter Platz
# - für LISP-Objekte freier Platz
# - von LISP-Objekten statisch belegter Platz
# bei SPVW_PAGES ausführlicher machen??
  {
    var uintL n1 = used_space();
    var uintL n2 = free_space();
    var uintL n3 = static_space();
    pushSTACK(UL_to_I(n1));
    pushSTACK(UL_to_I(n2));
    pushSTACK(UL_to_I(n3));
    STACK_to_mv(3);
  }

LISPFUNN(gc,0)
# (GC) führt eine GC aus
# und liefert den für LISP-Objekte freien Platz (in Bytes)
  {
    gar_col(); # GC ausführen
    VALUES1(fixnum(free_space()));
  }

# read-form neu schreiben, in Zusammenarbeit mit dem Terminal-Stream??

