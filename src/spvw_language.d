# Choice of user interface language, and internationalization.

# ------------------------------ Specification ---------------------------------

#ifndef LANGUAGE_STATIC

  # The current user interface language.
    extern uintL language;

  # Initializes the current interface language, according to the given
  # arguments, getting the defaults from environment variables.
    local void init_language (const char* argv_language, const char* argv_localedir);

  # Returns the translation of msgid according to the current interface
  # language.
    global const char * clgettext (const char * msgid);

  # Returns the translation of obj according to the current interface
  # language. obj must be a string. A string is returned.
    global object localized_string (object obj);

  # Returns the translated value of obj. obj is translated, then
  # READ-FROM-STRING is applied to the result.
    global object localized_object (object obj);

#endif

# ------------------------------ Implementation --------------------------------

#ifdef GNU_GETTEXT

  # Modify the environment variables. putenv() is POSIX, but some BSD systems
  # only have setenv(). Therefore (and because it's simpler to use) we
  # implement this interface, but without the third argument.
  # mysetenv(name,value) sets the value of the environment variable `name' to
  # `value' and returns 0. Returns -1 if not enough memory.
  local int mysetenv (const char * name, const char * value);
  local int mysetenv(name,value)
    var const char * name;
    var const char * value;
    { var uintL namelen = asciz_length(name);
      var uintL valuelen = asciz_length(value);
      #if defined(HAVE_PUTENV)
        var char* buffer = malloc(namelen+1+valuelen+1);
        var char* bufptr;
        if (!buffer)
          { return -1; } # no need to set errno = ENOMEM
        bufptr = buffer;
        dotimesL(namelen,namelen, { *bufptr++ = *name++; });
        *bufptr++ = '=';
        dotimesL(valuelen,valuelen, { *bufptr++ = *value++; });
        *bufptr = '\0';
        return putenv(buffer);
      #elif defined(HAVE_SETENV)
        return setenv(name,value,1);
      #else
        # Uh oh, neither putenv() nor setenv(), have to frob the environment
        # ourselves. Routine taken from glibc and fixed in several aspects.
        extern char** environ;
        var char** epp;
        var char* ep;
        var uintL envvar_count = 0;
        for (epp = environ; (ep = *epp) != NULL; epp++)
          { var const char * np = name;
            # Compare *epp and name:
            while (*np != '\0' && *np == *ep) { np++; ep++; }
            if (*np == '\0' && *ep == '=')
              break;
            envvar_count++;
          }
        ep = *epp;
        if (ep == NULL)
          # name not found in environ, add it.
          { # Remember the environ, so that we can free it if we need
            # to reallocate it again next time.
            var static char** last_environ = NULL;
            var char** new_environ = (char**) malloc((envvar_count+2)*sizeof(char*));
            if (!new_environ)
              { return -1; } # no need to set errno = ENOMEM
            { var uintL count;
              epp = environ;
              for (count = 0; count < envvar_count; count++)
                new_environ[count] = epp[count];
            }
            ep = (char*) malloc(namelen+1+valuelen+1);
            if (!ep)
              { free(new_environ); return -1; } # no need to set errno = ENOMEM
            { var char* cp = ep;
              dotimesL(namelen,namelen, { *cp++ = *name++; });
              *cp++ = '=';
              dotimesL(valuelen,valuelen, { *cp++ = *value++; });
              *cp = '\0';
            }
            new_environ[envvar_count] = ep;
            new_environ[envvar_count+1] = NULL;
            environ = new_environ;
            if (last_environ != NULL) { free(last_environ); }
            last_environ = new_environ;
          }
          else
          # name found, replace its value.
          { # We could be tempted to overwrite name's value directly if
            # the new value is not longer than the old value. But that's
            # not a good idea - maybe someone still has a pointer to
            # this area around.
            ep = (char*) malloc(namelen+1+valuelen+1);
            if (!ep)
              { return -1; } # no need to set errno = ENOMEM
            { var char* cp = ep;
              dotimesL(namelen,namelen, { *cp++ = *name++; });
              *cp++ = '=';
              dotimesL(valuelen,valuelen, { *cp++ = *value++; });
              *cp = '\0';
            }
            *epp = ep;
          }
        return 0;
      #endif
    }

#endif

#ifndef LANGUAGE_STATIC

  # Sprache, in der mit dem Benutzer kommuniziert wird:
    global uintL language;

  # Initialisiert die Sprache, gegeben die Sprachbezeichnung.
    local boolean init_language_from (const char* langname);
    #ifdef GNU_GETTEXT
      #define language_deutsch   1
      #define language_francais  2
      #define language_spanish   3
    #endif
    local boolean init_language_from(langname)
      var const char* langname;
      { if (asciz_equal(langname,"ENGLISH") || asciz_equal(langname,"english"))
          { language = language_english; return TRUE; }
        #ifdef GNU_GETTEXT
        if (asciz_equal(langname,"DEUTSCH") || asciz_equal(langname,"deutsch")
            || asciz_equal(langname,"GERMAN") || asciz_equal(langname,"german")
           )
          { language = language_deutsch; return TRUE; }
        if (asciz_equal(langname,"FRANCAIS") || asciz_equal(langname,"francais")
            #ifndef ASCII_CHS
            || asciz_equal(langname,"FRANÇAIS") || asciz_equal(langname,"français")
            #endif
            || asciz_equal(langname,"FRENCH") || asciz_equal(langname,"french")
           )
          { language = language_francais; return TRUE; }
        if (asciz_equal(langname,"ESPANOL") || asciz_equal(langname,"espanol")
            #ifndef ASCII_CHS
            || asciz_equal(langname,"ESPAÑOL") || asciz_equal(langname,"español")
            #endif
            || asciz_equal(langname,"SPANISH") || asciz_equal(langname,"spanish")
           )
          { language = language_spanish; return TRUE; }
        #endif
        return FALSE;
      }

  # Initialisiert die Sprache.
    local void init_language (const char* argv_language, const char* argv_localedir);
    local void init_language(argv_language,argv_localedir)
      var const char* argv_language;
      var const char* argv_localedir;
      { # Sprache wird so festgelegt, mit Prioritäten in dieser Reihenfolge:
        #   1. Fest eingebaut, LANGUAGE_STATIC
        #   2. -L Kommandozeilen-Argument
        #   3. Environment-Variable CLISP_LANGUAGE
        #   4. Environment-Variable LANG
        #   5. Default: Englisch
        if (argv_language)
          { if (init_language_from(argv_language)) goto chosen1; }
        #ifdef HAVE_ENVIRONMENT
        { var const char* langname = getenv("CLISP_LANGUAGE");
          if (langname)
            { if (init_language_from(langname)) goto chosen1; }
          #ifdef AMIGAOS
          langname = getenv("Language"); # since OS 3.0
          if (langname)
            { if (init_language_from(langname)) goto chosen1; }
          #endif
        }
        #endif
        #ifdef GNU_GETTEXT
        # The analysis of getenv("LANG") below will be done - in more detail -
        # by bindtextdomain() and textdomain(). No need to do it ourselves.
        # Do we need to call setlocale(LC_MESSAGES,"") or not??
        goto chosen2;
        #else
        #ifdef HAVE_ENVIRONMENT
        { var const char* lang = getenv("LANG");
          if (lang)
            { # LANG hat i.a. die Syntax Sprache[_Land][.Zeichensatz]
              if (lang[0]=='e' && lang[1]=='n' && !alphanumericp((uintB)lang[2])) # "en"
                { language = language_english; goto chosen2; }
        }   }
        #endif
        # Default: Englisch
        language = language_english; goto chosen2;
        #endif
        chosen1:
          # At this point we have chosen the language based upon the
          # command-line option or the clisp-specific environment variables.
          #ifdef GNU_GETTEXT
            # GNU gettext chooses the message catalog based upon:
            # 1. environment variable LANGUAGE [only if dcgettext.c, not with
            #    cat-compat.c],
            # 2. environment variable LC_ALL,
            # 3. environment variable LC_MESSAGES,
            # 4. environment variable LANG.
            # We clobber LC_MESSAGES and unset the earlier two variables.
            { var const char * locale =
                language == language_english ? "en" :
                language == language_deutsch ? "de" :
                language == language_francais ? "fr" :
                language == language_spanish ? "es" :
                "";
              if (getenv("LANGUAGE")) { mysetenv("LANGUAGE",""); }
              if (getenv("LC_ALL")) { mysetenv("LC_ALL",""); }
              mysetenv("LC_MESSAGES",locale);
              #ifdef LC_MESSAGES # !(UNIX_NEXTSTEP || ...)
              # Given the above, the following line is probably not needed.
              # (Depends on the behaviour of setlocale(LC_MESSAGES,NULL) on
              # your system.) Anyway it doesn't hurt.
              setlocale(LC_MESSAGES,locale);
              #endif
            }
          #endif
        chosen2:
          # At this point we have chosen the language based upon an
          # environment variable GNU gettext knows about.
          #ifdef GNU_GETTEXT
          { var const char * package = "clisp";
            # We apparently don't need to check whether argv_localedir is
            # not NULL and a valid directory. But since we may call chdir()
            # before the gettext library opens the catalog file, we have to
            # convert argv_localedir to be an absolute pathname, if possible.
            #ifdef UNIX
            if (!(argv_localedir == NULL))
              if (argv_localedir[0] != '\0' && argv_localedir[0] != '/')
                { var char currdir[MAXPATHLEN];
                  if (!(getwd(currdir) == NULL))
                    { var uintL currdirlen = asciz_length(currdir);
                      if (currdirlen > 0 && currdir[0] == '/')
                        { var uintL len = currdirlen + 1 + asciz_length(argv_localedir) + 1;
                          var char* abs_localedir = (char*)malloc(len*sizeof(char));
                          if (!(abs_localedir == NULL))
                            { # Append currdir, maybe '/', and argv_localedir into abs_localedir:
                              var char* ptr = abs_localedir;
                              { var const char * srcptr = currdir;
                                var uintL count;
                                dotimespL(count,currdirlen, { *ptr++ = *srcptr++; });
                              }
                              if (ptr[-1] != '/') { *ptr++ = '/'; }
                              { var const char * srcptr = argv_localedir;
                                while ((*ptr++ = *srcptr++) != '\0') continue;
                              }
                              argv_localedir = abs_localedir;
                }   }   }   }
            #endif
            bindtextdomain(package,argv_localedir);
            textdomain(package);
          }
          #endif
          return;
      }

 #ifdef GNU_GETTEXT

  #ifndef ISOLATIN_CHS
    # Our native character set is not ISOLATIN, the one in which the .gmo
    # files are encoded. Therefore we need to convert the messages to
    # ISOLATIN before calling gettext() and to convert the result back
    # to the local character set.
    #ifdef IBMPC_CHS
      # These tables come from utils/charset/cv-{fr,to}-ibmpc.c.
      # Conversion table from local character set to ISOLATIN character set.
      local uintB to_latin_table[256] =
        { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
          0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0xA4,
          0x10, 0x11, 0x12, 0x13, 0xB6, 0xA7, 0x16, 0x17,
          0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
          0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
          0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
          0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
          0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,
          0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
          0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
          0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
          0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,
          0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
          0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
          0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
          0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,
          0xC7, 0xFC, 0xE9, 0xE2, 0xE4, 0xE0, 0xE5, 0xE7,
          0xEA, 0xEB, 0xE8, 0xEF, 0xEE, 0xEC, 0xC4, 0xC5,
          0xC9, 0xE6, 0xC6, 0xF4, 0xF6, 0xF2, 0xFB, 0xF9,
          0xFF, 0xD6, 0xDC, 0xA2, 0xA3, 0xA5, 0x9E, 0x9F,
          0xE1, 0xED, 0xF3, 0xFA, 0xF1, 0xD1, 0xAA, 0xBA,
          0xBF, 0xA9, 0xAC, 0xBD, 0xBC, 0xA1, 0xAB, 0xBB,
          0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7,
          0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF,
          0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,
          0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF,
          0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7,
          0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF,
          0xE0, 0xDF, 0xE2, 0xE3, 0xE4, 0xE5, 0xB5, 0xE7,
          0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF,
          0xF0, 0xB1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF7, 0xF7,
          0xA0, 0xF9, 0xB7, 0xFB, 0xFC, 0xA2, 0xFE, 0xFF
        };
      # Conversion table from ISOLATIN character set to local character set.
      local uintB from_latin_table[256] =
        { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
          0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
          0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
          0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
          0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
          0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
          0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
          0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,
          0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
          0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
          0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
          0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,
          0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
          0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
          0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
          0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,
          0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
          0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F,
          0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
          0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F,
          0x20, 0xAD, 0x9B, 0x9C, 0x0F, 0xBD, 0x7C, 0x15,
          0x22, 0xA9, 0xA6, 0xAE, 0xAA, 0x2D, 0xAE, 0xAF,
          0xF8, 0xF1, 0xFD, 0xB3, 0x27, 0xE6, 0x14, 0xFA,
          0x2C, 0xB9, 0xA7, 0xAF, 0xAC, 0xAB, 0xBE, 0xA8,
          0x41, 0xC1, 0xC2, 0xC3, 0x8E, 0x8F, 0x92, 0x80,
          0xC8, 0x90, 0xCA, 0xCB, 0xCC, 0x49, 0xCE, 0xCF,
          0xD0, 0xA5, 0xD2, 0x4F, 0xD4, 0xD5, 0x9F, 0xD7,
          0xD8, 0xD9, 0xDA, 0xDB, 0x9A, 0xDD, 0xDE, 0xE1,
          0x85, 0xA0, 0x83, 0xE3, 0x84, 0x86, 0x91, 0x87,
          0x8A, 0x82, 0x88, 0x89, 0x8D, 0xA1, 0x8C, 0x8B,
          0xF0, 0xA4, 0x95, 0xA2, 0x93, 0xF5, 0x94, 0xF6,
          0xF8, 0x97, 0xA3, 0x96, 0x81, 0xFD, 0xFE, 0x98
        };
    #endif # IBMPC_CHS
    local const char * cvgettext (const char * msgid);
    local const char * cvgettext(msgid)
      var const char * msgid;
      { local char resultbuf[1024];
        var uintL len = asciz_length(msgid);
        var DYNAMIC_ARRAY(cvmsgid,char,len+1);
        var const uintB* ptr1;
        var uintB* ptr2;
        # Convert msgid argument to Latin1.
        ptr1 = (const uintB*)msgid;
        ptr2 = (uintB*)cvmsgid;
        while (!(*ptr1 == '\0')) { *ptr2++ = to_latin_table[*ptr1++]; }
        *ptr2 = '\0';
        # Lookup message translation.
        ptr1 = (const uintB*)gettext(cvmsgid);
        # Convert translation to local character set.
        ptr2 = resultbuf;
        while (!(*ptr1 == '\0')) { *ptr2++ = from_latin_table[*ptr1++]; }
        *ptr2 = '\0';
        FREE_DYNAMIC_ARRAY(cvmsgid);
        return resultbuf;
      }
  #else # ISOLATIN_CHS
    # No conversion needed.
    #define cvgettext gettext
  #endif

  global const char * clgettext (const char * msgid);
  global const char * clgettext(msgid)
    var const char * msgid;
    { var const char * translated_msg;
      if (msgid[0] == '\0')
        { # If you ask gettext to translate the empty string, it returns
          # the catalog's header (containing meta information)!
          translated_msg = msgid;
        }
        else
        { begin_system_call();
          translated_msg = cvgettext(msgid);
          end_system_call();
        }
      return translated_msg;
    }

  # FIXME: Don't hardwire ISO-8859-1. The catalog's character set is
  # given by the "Content-Type:" line in the meta information.

  global object localized_string (object obj);
  global object localized_string(obj)
    var object obj;
    { ASSERT(stringp(obj));
      with_string_0(obj,Symbol_value(S(iso8859_1)),asciz,
        { obj = asciz_to_string(clgettext(asciz),Symbol_value(S(iso8859_1))); });
      return obj;
    }

  global object localized_object (object obj);
  global object localized_object(obj)
    var object obj;
    { ASSERT(stringp(obj));
      with_string_0(obj,Symbol_value(S(iso8859_1)),asciz,
        { obj = asciz_to_string(clgettext(asciz),Symbol_value(S(iso8859_1))); });
      dynamic_bind(S(packagestern),O(default_package)); # *PACKAGE* binden
      pushSTACK(obj); funcall(L(read_from_string),1); # READ-FROM-STRING ausführen
      dynamic_unbind();
      return value1;
    }

 #endif

#endif
