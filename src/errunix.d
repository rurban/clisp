/* Handling of UNIX errors
 OS_error();
 > int errno: error code */
modexp _Noreturn void OS_error (void);
#ifdef UNIX
modexp _Noreturn void OS_error_arg (object etype, object arg);
#endif

#ifdef GNU_GETTEXT
  #define translate(string)  clgettext(string)
#else
  #define translate(string)  string
#endif

local void OS_error_internal (uintC errcode)
{
  /* start error message: */
 #ifdef UNIX
  write_errorstring(GETTEXT("UNIX error "));
 #else
  write_errorstring(GETTEXT("POSIX library error "));
 #endif
  /* output errno: */
  write_errorobject(fixnum(errcode));
  /* output the error-specific message: */
  var object code = ANSIC_error_code_converter(errcode);
  if (symbolp(code)) { /* known name? */
    STACK_7 = code;    /* replace numeric code with the symbolic one */
    write_errorasciz(" (");
    write_errorobject(code);
    write_errorasciz(")");
  }
  var char buffer[BUFSIZ];
  strerror_r(errcode,buffer,BUFSIZ);
  var const char *msg = translate(buffer);
  if (msg && msg[0]) { /* non-empty message? */
    write_errorasciz(": ");
    write_errorasciz(msg);
  }
}
LISPFUNNF(strerror,1) {
  int error_code = I_to_sint(check_sint(popSTACK()));
  char buffer[BUFSIZ];
  begin_system_call();
  strerror_r(error_code,buffer,BUFSIZ);
  end_system_call();
  VALUES1(asciz_to_string(buffer,O(misc_encoding)));
}
global _Noreturn void OS_error (void) {
  var uintC errcode; /* positive error number */
  end_system_call(); /* just in case */
  begin_system_call();
  errcode = errno;
  errno = 0; /* reset for the next error */
  end_system_call();
  clr_break_sem_4(); /* no UNIX operation may be active */
  pushSTACK(fixnum(errcode));
  begin_error(); /* start error message */
  if (!nullp(STACK_3)) /* *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ? */
    STACK_3 = S(os_error);
  OS_error_internal(errcode);
  end_error(args_end_pointer STACKop 7,true); /* finish error */
  NOTREACHED;
}
#ifdef UNIX
global _Noreturn void OS_error_arg (object etype, object arg) {
  var uintC errcode; /* positive error number */
  begin_system_call();
  errcode = errno;
  errno = 0; /* reset for the next error */
  end_system_call();
  clr_break_sem_4(); /* no UNIX operation may be active */
  pushSTACK(arg); /* *-ERROR slot */
  pushSTACK(fixnum(errcode));
  begin_error(); /* start error message */
  if (!nullp(STACK_3)) /* *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ? */
    STACK_3 = etype;
  OS_error_internal(errcode);
  end_error(args_end_pointer STACKop 7,true); /* finish error */
  NOTREACHED;
}
#endif

#undef translate
#ifdef GNU_GETTEXT
  #define translate(string)  clgettextl(string)
#else
  #define translate(string)  string
#endif

#ifdef UNIX
/* print an error
 > int errorcode: error code (errno)
 > FILE: Filename (with quotation marks) as constant ASCIZ-String
 > LINE: line number */
global void errno_out_low (int errorcode, const char* file, uintL line) {
  fprintf(stderr,"\n[%s:%lu] errno = %d", file, (unsigned long)line, errorcode);
  var object code = ANSIC_error_code_converter(errorcode);
  if (symbolp(code)) { /* known name? */
    fprint(stderr," (");
    nobject_out(stderr,code);
    fprint(stderr,")");
  }
  var char buffer[BUFSIZ];
  strerror_r(errorcode,buffer,BUFSIZ);
  var const char *msg = translate(buffer);
  if (msg && msg[0]) { /* non-empty message? */
    fprintf(stderr,": %s",msg);
  }
  fprint(stderr,".\n");
}
#endif
