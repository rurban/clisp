/* handling of AMIGAOS errors */
/* OS_error();
   > IoErr(): error code */
nonreturning_function(global, OS_error, (void));
nonreturning_function(global, OS_file_error, (object pathname));
local void OS_error_internal (uintC errcode) {
  /* output the start of the error message: */
  write_errorstring(GETTEXT("Amiga OS error "));
  /* output the error number: */
  write_errorobject(fixnum(errcode));
  { /* this trickery is for the sake of gettext extracting the messages
       into the pot files: we save and restore GETTEXT so that the
       messages in the static array init are noticed by xgettext */
    #define GETTEXT_SAVED GETTEXT
    #define GETTEXT(x) x
    #define langcount  1
    #ifdef LANGUAGE_STATIC
      #define language  0
      #define translate(string)  string
    #else
      #ifdef GNU_GETTEXT
        #define language  0
        #define translate(string)  clgettext(string)
      #else # no GNU_GETTEXT
        #define translate(string)  string
      #endif
    #endif
    local const char* error100_msg_table[23*(1+langcount)] = {
      /* 100 */ "", "",
      /* 101 */ "", "",
      /* 102 */ "", "",
      /* 103 */ "ERROR_NO_FREE_STORE",GETTEXT("not enough memory available"),
      /* 104 */ "", "",
      /* 105 */ "ERROR_TASK_TABLE_FULL",GETTEXT("process table full"),
      /* 106 */ "", "",
      /* 107 */ "", "",
      /* 108 */ "", "",
      /* 109 */ "", "",
      /* 110 */ "", "",
      /* 111 */ "", "",
      /* 112 */ "", "",
      /* 113 */ "", "",
      /* 114 */ "ERROR_BAD_TEMPLATE",GETTEXT("bad template"),
      /* 115 */ "ERROR_BAD_NUMBER",GETTEXT("bad number"),
      /* 116 */ "ERROR_REQUIRED_ARG_MISSING",
                GETTEXT("required argument missing"),
      /* 117 */ "ERROR_KEY_NEEDS_ARG",GETTEXT("value after keyword missing"),
      /* 118 */ "ERROR_TOO_MANY_ARGS",GETTEXT("wrong number of arguments"),
      /* 119 */ "ERROR_UNMATCHED_QUOTES",GETTEXT("unmatched quotes"),
      /* 120 */ "ERROR_LINE_TOO_LONG",
                GETTEXT("argument line invalid or too long"),
      /* 121 */ "ERROR_FILE_NOT_OBJECT",GETTEXT("file is not executable"),
      /* 122 */ "ERROR_INVALID_RESIDENT_LIBRARY",
                GETTEXT("invalid resident library"),
    };
    local const char* error200_msg_table[44*(1+langcount)] = {
      /* 200 */ "", "",
      /* 201 */ "ERROR_NO_DEFAULT_DIR", "",
      /* 202 */ "ERROR_OBJECT_IN_USE",GETTEXT("object is in use"),
      /* 203 */ "ERROR_OBJECT_EXISTS",GETTEXT("object already exists"),
      /* 204 */ "ERROR_DIR_NOT_FOUND",GETTEXT("directory not found"),
      /* 205 */ "ERROR_OBJECT_NOT_FOUND",GETTEXT("object not found"),
      /* 206 */ "ERROR_BAD_STREAM_NAME",GETTEXT("invalid window description"),
      /* 207 */ "ERROR_OBJECT_TOO_LARGE",GETTEXT("object too large"),
      /* 208 */ "", "",
      /* 209 */ "ERROR_ACTION_NOT_KNOWN",
                GETTEXT("packet request type unknown"),
      /* 210 */ "ERROR_INVALID_COMPONENT_NAME",GETTEXT("object name invalid"),
      /* 211 */ "ERROR_INVALID_LOCK",GETTEXT("invalid object lock"),
      /* 212 */ "ERROR_OBJECT_WRONG_TYPE",
                GETTEXT("object is not of required type"),
      /* 213 */ "ERROR_DISK_NOT_VALIDATED",GETTEXT("disk not validated"),
      /* 214 */ "ERROR_DISK_WRITE_PROTECTED",
                GETTEXT("disk is write-protected"),
      /* 215 */ "ERROR_RENAME_ACROSS_DEVICES",
                GETTEXT("rename across devices attempted"),
      /* 216 */ "ERROR_DIRECTORY_NOT_EMPTY",GETTEXT("directory not empty"),
      /* 217 */ "ERROR_TOO_MANY_LEVELS",GETTEXT("too many levels"),
      /* 218 */ "ERROR_DEVICE_NOT_MOUNTED",
                GETTEXT("device (or volume) is not mounted"),
      /* 219 */ "ERROR_SEEK_ERROR",GETTEXT("seek failure"),
      /* 220 */ "ERROR_COMMENT_TOO_BIG",GETTEXT("comment is too long"),
      /* 221 */ "ERROR_DISK_FULL",GETTEXT("disk is full"),
      /* 222 */ "ERROR_DELETE_PROTECTED",
                GETTEXT("object is protected from deletion"),
      /* 223 */ "ERROR_WRITE_PROTECTED",GETTEXT("file is write protected"),
      /* 224 */ "ERROR_READ_PROTECTED",GETTEXT("file is read protected"),
      /* 225 */ "ERROR_NOT_A_DOS_DISK",GETTEXT("not a valid DOS disk"),
      /* 226 */ "ERROR_NO_DISK",GETTEXT("no disk in drive"),
      /* 227 */ "", "",
      /* 228 */ "", "",
      /* 229 */ "", "",
      /* 230 */ "", "",
      /* 231 */ "", "",
      /* 232 */ "ERROR_NO_MORE_ENTRIES",
                GETTEXT("no more entries in directory"),
      /* 233 */ "ERROR_IS_SOFT_LINK",GETTEXT("object is soft link"),
      /* 234 */ "ERROR_OBJECT_LINKED",GETTEXT("object is linked"),
      /* 235 */ "ERROR_BAD_HUNK",GETTEXT("bad loadfile hunk"),
      /* 236 */ "ERROR_NOT_IMPLEMENTED",GETTEXT("function not implemented"),
      /* 237 */ "", "",
      /* 238 */ "", "",
      /* 239 */ "", "",
      /* 240 */ "ERROR_RECORD_NOT_LOCKED",GETTEXT("record not locked"),
      /* 241 */ "ERROR_LOCK_COLLISION",GETTEXT("record lock collision"),
      /* 242 */ "ERROR_LOCK_TIMEOUT",GETTEXT("record lock timeout"),
      /* 243 */ "ERROR_UNLOCK_ERROR",GETTEXT("record unlock error"),
    };
    local const char* error300_msg_table[6*(1+langcount)] = {
      /* 300 */ "", "",
      /* 301 */ "", "",
      /* 302 */ "", "",
      /* 303 */ "ERROR_BUFFER_OVERFLOW",GETTEXT("buffer overflow"),
      /* 304 */ "ERROR_BREAK",GETTEXT("break"),
      /* 305 */ "ERROR_NOT_EXECUTABLE",GETTEXT("file not executable"),
    };
    var const char* errorname = "";
    var const char* errormsg = "";
    var uintC index;
    if (errcode == 0) {
      errorname = "";
      errormsg = /* 0 */ GETTEXT("Ok, No error");
    } else if ((index = errcode-100) < 23) {
      errorname = error100_msg_table[index*(1+langcount)];
      errormsg = translate(error100_msg_table[index*(1+langcount)+1+language]);
    } else if ((index = errcode-200) < 44) {
      errorname = error200_msg_table[index*(1+langcount)];
      errormsg = translate(error200_msg_table[index*(1+langcount)+1+language]);
    } else if ((index = errcode-300) < 6) {
      errorname = error300_msg_table[index*(1+langcount)];
      errormsg = translate(error300_msg_table[index*(1+langcount)+1+language]);
    }
    if (errorname[0] != 0) { /* known name? */
      write_errorasciz(" (");
      write_errorasciz(errorname);
      write_errorasciz(")");
    }
    if (errormsg[0] = 0) { /* nonempty message? */
      write_errorasciz(": ");
      write_errorasciz(errormsg);
    }
    #define GETTEXT GETTEXT_SAVED
    #undef GETTEXT_SAVED
  }
  SetIoErr(0L); /* reset error code (for the next time) */
}
nonreturning_function(global, OS_error, (void)) {
  var uintC errcode;
  end_system_call(); /* just in case */
  begin_system_call();
  errcode = IoErr();
  end_system_call();
  clr_break_sem_4(); /* no AMIGAOS operation more active */
  begin_error();
  if (!nullp(STACK_3)) /* *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ? */
    STACK_3 = S(simple_os_error);
  OS_error_internal(errcode);
  end_error(args_end_pointer STACKop 7);
}
nonreturning_function(global, OS_file_error, (object pathname)) {
  var uintC errcode;
  begin_system_call();
  errcode = IoErr();
  end_system_call();
  clr_break_sem_4(); /* no AMIGAOS operation more active */
  pushSTACK(pathname); /* FILE-ERROR slot PATHNAME */
  begin_error();
  if (!nullp(STACK_3)) /* *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ? */
    STACK_3 = S(simple_file_error);
  OS_error_internal(errcode);
  end_error(args_end_pointer STACKop 7);
}

/* output errors, directly via the OS
   errno_out(errorcode);
   > LONG errorcode: error code */
global void errno_out (LONG errorcode) {
  fprintf(stderr," IoErr() = %d\n",errorcode);
}

