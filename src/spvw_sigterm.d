/* Handling of terminal signals. */

/* --------------------------- Specification ----------------------------- */

#if defined(HAVE_SIGNALS)
/* Installs the SIGTERM handler. */
local void install_sigterm_handler (void);
#endif

/* --------------------------- Implementation ---------------------------- */

/* print the "exiting" message and quit */
local void quit_on_signal (int sig) {
  pushSTACK(Symbol_value(S(error_output))); fresh_line(&STACK_0);
  pushSTACK(CLSTEXT("Exiting on signal ")); pushSTACK(STACK_1);
  funcall(L(write_string),2);   /* (write-line "exiting" stderr) */
  pushSTACK(sint_to_I(sig)); pushSTACK(STACK_1);
  funcall(L(prin1),2);            /* (prin1 signal stderr) */
  terpri(&STACK_0); skipSTACK(1); /* drop *error-output* */
  quit();
}

/* install error handlers for as many signals as possible */
local void install_sigterm_handler (void) {
#ifdef SIGHUP
  SIGNAL(SIGHUP,&quit_on_signal);
#endif
#ifdef SIGQUIT
  SIGNAL(SIGQUIT,&quit_on_signal);
#endif
#ifdef SIGILL
  SIGNAL(SIGILL,&quit_on_signal);
#endif
#ifdef SIGABRT
  SIGNAL(SIGABRT,&quit_on_signal);
#endif
#ifdef SIGKILL
  SIGNAL(SIGKILL,&quit_on_signal);
#endif
#ifdef SIGTERM
  SIGNAL(SIGTERM,&quit_on_signal);
#endif
}
