/* handling of Win32 errors
 OS_error();
 > GetLastError(): error code */
modexp _Noreturn void OS_error (void);
modexp _Noreturn void OS_error_arg (object etype, object arg);
local void OS_error_internal (DWORD errcode) {
  /* print start message: */
  write_errorstring(GETTEXT("Win32 error "));
  /* print error code: */
  write_errorobject(UL_to_I(errcode));
  /* output the error-specific message: */
  var object code = WINDOWS_error_code_converter(errcode);
  if (symbolp(code)) { /* known name? */
    STACK_7 = code;    /* replace numeric code with the symbolic one */
    write_errorasciz(" (");
    write_errorobject(code);
    write_errorasciz(")");
  }
  var char* msg = format_message(errcode);
  if (msg) {
    write_errorasciz(": ");
    write_errorasciz(msg);
    begin_system_call();
    LocalFree(msg);
    end_system_call();
  }
}
modexp _Noreturn void OS_error (void) {
  var DWORD errcode;
  end_system_call();            /* just in case */
  begin_system_call();
  errcode = GetLastError();
  end_system_call();
  clr_break_sem_4();   /* no more active Win32 calls */
  pushSTACK(fixnum(errcode));
  begin_error();
  if (!nullp(STACK_3)) /* *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ? */
    STACK_3 = S(os_error_win32);
  OS_error_internal(errcode);
  end_error(args_end_pointer STACKop 7,true);
  NOTREACHED;
}
modexp _Noreturn void OS_error_arg (object etype, object arg) {
  var DWORD errcode;
  begin_system_call();
  errcode = GetLastError();
  end_system_call();
  clr_break_sem_4();   /* no more active Win32 calls */
  pushSTACK(arg); /* *-ERROR slot */
  pushSTACK(fixnum(errcode));
  begin_error();
  if (!nullp(STACK_3)) /* *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ? */
    STACK_3 = etype;
  OS_error_internal(errcode);
  end_error(args_end_pointer STACKop 7,true);
  NOTREACHED;
}

/* print an error
 > DWORD errorcode: error code (errno)
 > FILE: Filename (with quotation marks) as constant ASCIZ-String
 > LINE: line number */
local void errno_out_body (const char* name, const char* msg) {
  if (name != NULL)
    fprintf(stderr," (%s)",name);
  if (msg != NULL)
    fprintf(stderr,": %s",msg);
  else
    fputc('.',stderr);
}
global void errno_out_low (DWORD errorcode, const char* file, uintL line) {
  fprintf(stderr,"\n[%s:%d] GetLastError() = 0x%x",file,line,errorcode);
  var object code = WINDOWS_error_code_converter(errorcode);
  if (symbolp(code)) { /* known name? */
    fputs(" (",stderr);
    nobject_out(stderr,code);
    fputs(")",stderr);
  }
  var char* msg = format_message(errorcode);
  if (msg)
    fprintf(stderr,": %s.",msg);
  else
    fputc('.',stderr);
  fputc('\n',stderr);
}
