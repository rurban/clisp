/*
 * CLISP interface to PARI <http://pari.math.u-bordeaux.fr/>
 * Copyright (C) 1995 Michael Stoll
 * Copyright (C) 2004-2007, 2010, 2017 Sam Steingold
 * This is free software, distributed under the GNU GPL v2+
 */

#include "clisp.h"
#include "config.h"
#undef T
#include <pari/pari.h>
#include <pari/paripriv.h>      /* for GP_DATA */
#include "cpari.h"

/* we could also use DEF-CALL-OUT, but this is faster, smaller,
   and accommodates the "const" arguments better
   (CLISP FFI does not produce "const" argument declarations) */
void clisp_out_putc(char c) {
  pushSTACK(int_char(c)); pushSTACK(Symbol_value(S(standard_output)));
  funcall(L(write_char),2);
}
void clisp_out_puts(const char *s) {
  pushSTACK(asciz_to_string(s,GLO(foreign_encoding)));
  pushSTACK(Symbol_value(S(standard_output)));
  funcall(L(write_string),2);
}
void clisp_out_flush(void) {
  pushSTACK(Symbol_value(S(standard_output)));
  funcall(L(finish_output),1);
}

void clisp_err_putc(char c) {
  pushSTACK(int_char(c)); pushSTACK(Symbol_value(S(error_output)));
  funcall(L(write_char),2);
}
void clisp_err_puts(const char *s) {
  pushSTACK(asciz_to_string(s,GLO(foreign_encoding)));
  pushSTACK(Symbol_value(S(error_output)));
  funcall(L(write_string),2);
}
void clisp_err_flush(void) {
  pushSTACK(Symbol_value(S(error_output)));
  funcall(L(finish_output),1);
}
void clisp_err_recover(long errnum) {
  pushSTACK(asciz_to_string(numerr_name(errnum),GLO(foreign_encoding)));
  error(error_condition,GETTEXT("PARI error: ~S"));
}

PariOUT clisp_out = {clisp_out_putc, clisp_out_puts, clisp_out_flush};
PariOUT clisp_err = {clisp_err_putc, clisp_err_puts, clisp_err_flush};

void init_for_clisp (long parisize, long maxprime)
{
#if defined(HAVE_PARI_INIT_OPTS)
  pari_init_opts(parisize,maxprime,INIT_DFTm);
#elif defined(HAVE_PARI_INIT)
  pari_init(parisize,maxprime);
#else
  #error no pari_init_opts, no pari_init: cannot initialize PARI
#endif
  pari_outfile = stdout;
  pari_logfile = NULL;
  pari_infile = stdin;
  pari_errfile = stderr;
  pariOut = &clisp_out;
  pariErr = &clisp_err;
  cb_pari_err_recover = &clisp_err_recover;
}
