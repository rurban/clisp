# Debugging utilities.

# ------------------------------ Specification ---------------------------------

# Output a constant ASCIZ string, directly by the operating system.
# asciz_out(string);
# > char* asciz: ASCIZ-String
  global void asciz_out (const char * asciz);

# Output an uintL in decimal notation, directly by the operating system.
# dez_out(zahl);
  global void dez_out_ (uintL zahl);

# Output an uintL in hexadecimal notation, directly by the operating system.
# hex_out(zahl);
  global void hex_out_ (unsigned long zahl);

# Output a format string, with one or two embedded %s arguments.
  global void asciz_out_s (const char * asciz, const char * arg1);
  global void asciz_out_ss (const char * asciz, const char * arg1, const char * arg2);

# Output a format string, with up to three embedded %d/%x arguments.
  global void asciz_out_1_ (const char * asciz, unsigned long arg1);
  global void asciz_out_2_ (const char * asciz, unsigned long arg1, unsigned long arg2);
  global void asciz_out_3_ (const char * asciz, unsigned long arg1, unsigned long arg2, unsigned long arg3);

# Output a memory range in hexadecimal notation, directly by the operating
# system.
# mem_hex_out(buf,count);
  global void mem_hex_out (const void* buf, uintL count);

# Output a lisp object in lisp notation to standard output.
# object_out(obj);
# can trigger GC
  global void object_out (object obj);

# ------------------------------ Implementation --------------------------------

  global void asciz_out(asciz)
    var const char * asciz;
    {
      #if defined(MSDOS) || defined(WIN32) || (defined(UNIX) && (O_BINARY != 0)) # if (CRLFstring != NLstring)
        # Translate NLstring to CRLFstring.
        var uintL bufsize = 2*asciz_length(asciz);
        var DYNAMIC_ARRAY(buffer,char,bufsize+1);
        var char* bufptr = buffer;
        loop
          { var char c = *asciz++;
            if (c == '\0')
              break;
            if (c == '\n')
              { *bufptr++ = '\r'; *bufptr++ = '\n'; }
            else
              *bufptr++ = c;
          }
        *bufptr = '\0';
        asciz = buffer;
      #endif
      #ifdef AMIGAOS
        begin_system_call();
        Write(stdout_handle,asciz,asciz_length(asciz));
        end_system_call();
      #endif
      #if (defined(UNIX) && !defined(NEXTAPP)) || defined(MSDOS) || defined(RISCOS)
        begin_system_call();
        full_write(stdout_handle,(RW_BUF_T)asciz,asciz_length(asciz));
        end_system_call();
      #endif
      #ifdef WIN32_NATIVE
        { var DWORD errcode; # save the error code (modified by full_write())
          begin_system_call();
          errcode = GetLastError();
          full_write(stdout_handle,(RW_BUF_T)asciz,asciz_length(asciz));
          SetLastError(errcode);
          end_system_call();
        }
      #endif
      #ifdef NEXTAPP
        begin_system_call();
        nxterminal_write_string(asciz);
        end_system_call();
      #endif
      #if defined(MSDOS) || defined(WIN32) || (defined(UNIX) && (O_BINARY != 0)) # if (CRLFstring != NLstring)
        FREE_DYNAMIC_ARRAY(buffer);
      #endif
    }

  global void dez_out_(zahl)
    var uintL zahl;
    { var struct { uintB contents[10+1]; } buffer;
      # A 10 byte buffer suffices, since zahl < 2^32 <= 10^10 .
      var uintB* bufptr = &buffer.contents[10];
      *bufptr = 0; # end of ASCIZ string
      do { *--bufptr = '0'+(zahl%10); zahl=floor(zahl,10); }
         until (zahl==0);
      asciz_out((char*)bufptr);
    }

  local char hex_table[] = "0123456789ABCDEF";
  global void hex_out_(zahl)
    var unsigned long zahl;
    { var struct { uintB contents[2*sizeof(unsigned long)+1]; } buffer;
      # A 8/16 byte buffer suffices, since zahl < 2^32 <= 16^8
      # or zahl < 2^64 <= 16^16 .
      var uintB* bufptr = &buffer.contents[2*sizeof(unsigned long)];
      *bufptr = 0; # end of ASCIZ string
      do { *--bufptr = hex_table[zahl%16]; zahl=floor(zahl,16); }
         until (zahl==0);
      asciz_out((char*)bufptr);
    }

  global void asciz_out_s(asciz,arg1)
    var const char * asciz;
    var const char * arg1;
    { var uintL bufsize = asciz_length(asciz)+asciz_length(arg1);
      var DYNAMIC_ARRAY(buffer,char,bufsize+1);
      var char* bufptr = buffer;
      loop
        { var char c = *asciz++;
          if (c == '\0')
            break;
          if (c == '%' && *asciz == '%')
            { *bufptr++ = *asciz++; }
          elif (c == '%' && *asciz == 's')
            { asciz++;
              while (*arg1 != '\0') { *bufptr++ = *arg1++; }
            }
          else
            *bufptr++ = c;
        }
      *bufptr = '\0';
      asciz_out(buffer);
      FREE_DYNAMIC_ARRAY(buffer);
    }
  global void asciz_out_ss(asciz,arg1,arg2)
    var const char * asciz;
    var const char * arg1;
    var const char * arg2;
    { var uintL bufsize = asciz_length(asciz)+asciz_length(arg1)+asciz_length(arg2);
      var DYNAMIC_ARRAY(buffer,char,bufsize+1);
      var char* bufptr = buffer;
      loop
        { var char c = *asciz++;
          if (c == '\0')
            break;
          if (c == '%' && *asciz == '%')
            { *bufptr++ = *asciz++; }
          elif (c == '%' && *asciz == 's')
            { asciz++;
              while (*arg1 != '\0') { *bufptr++ = *arg1++; }
              arg1 = arg2;
            }
          else
            *bufptr++ = c;
        }
      *bufptr = '\0';
      asciz_out(buffer);
      FREE_DYNAMIC_ARRAY(buffer);
    }

# We avoid to include <stdarg.h> or <varargs.h> - portability problems.
  local char* asciz_out_aux_hex (char* destptr, unsigned long arg);
  local char* asciz_out_aux_hex(destptr,zahl)
    var char* destptr;
    var unsigned long zahl;
    { var struct { uintB contents[2*sizeof(unsigned long)+1]; } buffer;
      # A 8/16 byte buffer suffices, since zahl < 2^32 <= 16^8
      # or zahl < 2^64 <= 16^16 .
      var uintB* bufptr = &buffer.contents[2*sizeof(unsigned long)];
      *bufptr = 0; # end of ASCIZ string
      do { *--bufptr = hex_table[zahl%16]; zahl=floor(zahl,16); }
         until (zahl==0);
      while (*bufptr) { *destptr++ = *bufptr++; }
      return destptr;
    }
  local char* asciz_out_aux_dez (char* destptr, unsigned long arg);
  local char* asciz_out_aux_dez(destptr,zahl)
    var char* destptr;
    var unsigned long zahl;
    { var struct { uintB contents[20+1]; } buffer;
      # A 20 byte buffer suffices, since zahl < 2^64 <= 10^20 .
      var uintB* bufptr = &buffer.contents[20];
      *bufptr = 0; # end of ASCIZ string
      do { *--bufptr = '0'+(zahl%10); zahl=floor(zahl,10); }
         until (zahl==0);
      while (*bufptr) { *destptr++ = *bufptr++; }
      return destptr;
    }
  global void asciz_out_1_(asciz,arg1)
    var const char * asciz;
    var unsigned long arg1;
    { var uintL bufsize = asciz_length(asciz)+1*20;
      var DYNAMIC_ARRAY(buffer,char,bufsize+1);
      var char* bufptr = buffer;
      loop
        { var char c = *asciz++;
          if (c == '\0')
            break;
          if (c == '%' && *asciz == '%')
            { *bufptr++ = *asciz++; }
          elif (c == '%' && *asciz == 'x')
            { asciz++; bufptr = asciz_out_aux_hex(bufptr,arg1); }
          elif (c == '%' && *asciz == 'd')
            { asciz++; bufptr = asciz_out_aux_dez(bufptr,arg1); }
          else
            *bufptr++ = c;
        }
      *bufptr = '\0';
      asciz_out(buffer);
      FREE_DYNAMIC_ARRAY(buffer);
    }
  global void asciz_out_2_(asciz,arg1,arg2)
    var const char * asciz;
    var unsigned long arg1;
    var unsigned long arg2;
    { var uintL bufsize = asciz_length(asciz)+2*20;
      var DYNAMIC_ARRAY(buffer,char,bufsize+1);
      var char* bufptr = buffer;
      loop
        { var char c = *asciz++;
          if (c == '\0')
            break;
          if (c == '%' && *asciz == '%')
            { *bufptr++ = *asciz++; }
          elif (c == '%' && *asciz == 'x')
            { asciz++; bufptr = asciz_out_aux_hex(bufptr,arg1);
              arg1 = arg2;
            }
          elif (c == '%' && *asciz == 'd')
            { asciz++; bufptr = asciz_out_aux_dez(bufptr,arg1);
              arg1 = arg2;
            }
          else
            *bufptr++ = c;
        }
      *bufptr = '\0';
      asciz_out(buffer);
      FREE_DYNAMIC_ARRAY(buffer);
    }
  global void asciz_out_3_(asciz,arg1,arg2,arg3)
    var const char * asciz;
    var unsigned long arg1;
    var unsigned long arg2;
    var unsigned long arg3;
    { var uintL bufsize = asciz_length(asciz)+3*20;
      var DYNAMIC_ARRAY(buffer,char,bufsize+1);
      var char* bufptr = buffer;
      loop
        { var char c = *asciz++;
          if (c == '\0')
            break;
          if (c == '%' && *asciz == '%')
            { *bufptr++ = *asciz++; }
          elif (c == '%' && *asciz == 'x')
            { asciz++; bufptr = asciz_out_aux_hex(bufptr,arg1);
              arg1 = arg2; arg2 = arg3;
            }
          elif (c == '%' && *asciz == 'd')
            { asciz++; bufptr = asciz_out_aux_dez(bufptr,arg1);
              arg1 = arg2; arg2 = arg3;
            }
          else
            *bufptr++ = c;
        }
      *bufptr = '\0';
      asciz_out(buffer);
      FREE_DYNAMIC_ARRAY(buffer);
    }

  global void mem_hex_out(buf,count)
    var const void* buf;
    var uintL count;
    { if (count > 0)
        { var DYNAMIC_ARRAY(cbuf,char,3*count+1);
          var const uintB* ptr1 = (const uintB*) buf;
          var char* ptr2 = &cbuf[0];
          dotimespL(count,count,
            { *ptr2++ = ' ';
              *ptr2++ = hex_table[floor(*ptr1,16)]; *ptr2++ = hex_table[*ptr1 % 16];
              ptr1++;
            });
          *ptr2 = '\0';
          asciz_out(cbuf);
          FREE_DYNAMIC_ARRAY(cbuf);
    }   }

  global void object_out(obj)
    var object obj;
    { pushSTACK(obj);
      pushSTACK(var_stream(S(terminal_io),strmflags_wr_ch_B)); # stream *TERMINAL-IO*
      prin1(&STACK_0,STACK_1); # output the object
      terpri(&STACK_0); # output a newline
      skipSTACK(2);
    }
