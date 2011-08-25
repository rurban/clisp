/* Choice of user interface language, and internationalization. */

/* -------------------------- Specification ---------------------------- */

#ifdef GNU_GETTEXT

/* Initializes the current interface language, according to the given
   arguments, getting the defaults from environment variables. */
global void init_language (const char* argv_language, const char* argv_localedir, bool lisp_error_p);

/* Returns the translation of msgid according to the current interface
   language. */
global const char * clgettext (const char * msgid);

#endif

/* Returns the translation of string according to the current interface
 language. A string is returned.
 can trigger GC */
global object CLSTEXT (const char*);

/* Returns the translated value of obj. obj is translated,
 then READ-FROM-STRING is applied to the result.
 can trigger GC */
global object CLOTEXT (const char*);


/* -------------------------- Implementation --------------------------- */

#ifdef GNU_GETTEXT

/* language, that is used for communication with the user: */
static enum {
  language_english,
  language_german,
  language_french,
  language_spanish,
  language_dutch,
  language_russian,
  language_danish
} language;

/* Initializes the language, given the language name. */
local bool init_language_from (const char* langname);

global object current_language_o (void) {
  switch (language) {
    case language_english:  { return S(english); }
    case language_german:   { return S(german); }
    case language_french:   { return S(french); }
    case language_spanish:  { return S(spanish); }
    case language_dutch:    { return S(dutch); }
    case language_russian:  { return S(russian); }
    case language_danish:   { return S(danish); }
    default: NOTREACHED;
  }
}

local bool init_language_from (const char* langname) {
  if (NULL == langname) return false;
  if (asciz_equal(langname,"ENGLISH") || asciz_equal(langname,"english")) {
    language = language_english; return true;
  }
  if (asciz_equal(langname,"DEUTSCH") || asciz_equal(langname,"deutsch")
      || asciz_equal(langname,"GERMAN") || asciz_equal(langname,"german")) {
    language = language_german; return true;
  }
  if (asciz_equal(langname,"FRANCAIS") || asciz_equal(langname,"francais")
     #ifndef ASCII_CHS
      || asciz_equal(langname,"FRAN\307AIS") || asciz_equal(langname,"FRAN\303\207AIS") /* FRENCH */
      || asciz_equal(langname,"fran\347ais") || asciz_equal(langname,"fran\303\247ais") /* french */
     #endif
      || asciz_equal(langname,"FRENCH") || asciz_equal(langname,"french")) {
    language = language_french; return true;
  }
  if (asciz_equal(langname,"ESPANOL") || asciz_equal(langname,"espanol")
     #ifndef ASCII_CHS
      || asciz_equal(langname,"ESPA\321OL") || asciz_equal(langname,"ESPA\303\221OL") /* SPANISH */
      || asciz_equal(langname,"espa\361ol") || asciz_equal(langname,"espa\303\261ol") /* spanish */
     #endif
      || asciz_equal(langname,"SPANISH") || asciz_equal(langname,"spanish")) {
    language = language_spanish; return true;
  }
  if (asciz_equal(langname,"russian") || asciz_equal(langname,"RUSSIAN")
     #ifndef ASCII_CHS
      || asciz_equal(langname,"\320\240\320\243\320\241\320\241\320\232\320\230\320\231")
      || asciz_equal(langname,"\321\200\321\203\321\201\321\201\320\272\320\270\320\271")
      || asciz_equal(langname,"\240\243\241\241\232\230\231")
      || asciz_equal(langname,"\200\203\201\201\272\270\271")
     #endif
      ) {
    language = language_russian; return true;
  }
  if (asciz_equal(langname,"NEDERLANDS") || asciz_equal(langname,"nederlands")
      || asciz_equal(langname,"DUTCH") || asciz_equal(langname,"dutch")) {
    language = language_dutch; return true;
  }
  if (asciz_equal(langname,"DANSK") || asciz_equal(langname,"dansk")
      || asciz_equal(langname,"DANISH") || asciz_equal(langname,"danish")) {
    language = language_danish; return true;
  }
  return false;
}

/* Initializes the language. */
global void init_language
(const char* argv_language, const char* argv_localedir, bool lisp_error_p) {
#define ANSIC_ERROR(f,a)   if (lisp_error_p) ANSIC_error(); else {      \
  fprintf(stderr,GETTEXT("WARNING: %s/%s: %s.\n"),f,a,strerror(errno)); \
  goto init_language_failure;                                           \
 }
#define MY_NOTREACHED if (lisp_error_p) NOTREACHED; else abort()
  /* language is set with priorities in this order:
     1. -L command line argument
     2. environment-variable CLISP_LANGUAGE
     3. environment-variable LANG
     4. default: English */
  if (argv_language == NULL) {
    /* noop */
  } else if (init_language_from(argv_language)
             || init_language_from(getenv("CLISP_LANGUAGE"))) {
  /* At this point we have chosen the language based upon the
   command-line option or the clisp-specific environment variables. */
  /* GNU gettext chooses the message catalog based upon:
   1. environment variable LANGUAGE [only if dcgettext.c, not with
      cat-compat.c],
   2. environment variable LC_ALL,
   3. environment variable LC_MESSAGES,
   4. environment variable LANG.
   We clobber LC_MESSAGES and unset the earlier two variables. */
    var const char *locale1, *locale2;
    switch (language) {
      case language_english: locale1 = "en_US"; locale2 = "en_US.utf8"; break;
      case language_german:  locale1 = "de_DE"; locale2 = "de_DE.utf8"; break;
      case language_french:  locale1 = "fr_FR"; locale2 = "fr_FR.utf8"; break;
      case language_spanish: locale1 = "es_ES"; locale2 = "es_ES.utf8"; break;
      case language_dutch:   locale1 = "nl_NL"; locale2 = "nl_NL.utf8"; break;
      case language_russian: locale1 = "ru_RU"; locale2 = "ru_RU.utf8"; break;
      case language_danish:  locale1 = "da_DK"; locale2 = "da_DK.utf8"; break;
      default:               MY_NOTREACHED;
    }
    if (getenv("LANGUAGE") && unsetenv("LANGUAGE"))
      ANSIC_ERROR("unsetenv","LANGUAGE");
    if (getenv("LC_ALL") && unsetenv("LC_ALL"))
      ANSIC_ERROR("unsetenv","LC_ALL");
    if (NULL == setlocale(LC_MESSAGES,locale2)) {
      if (NULL == setlocale(LC_MESSAGES,locale1)) {
        if (lisp_error_p) {
          pushSTACK(ascii_to_string(locale1));
          pushSTACK(ascii_to_string(locale2));
          pushSTACK(TheSubr(subr_self)->name);
          error(error_condition,GETTEXT("~S: locales ~S and ~S are not installed on this system"));
        } else {
          fprintf(stderr,GETTEXT("locales %s and %s are not installed on this system\n"),locale1,locale2);
          goto init_language_failure;
        }
      } else if (setenv("LC_MESSAGES",locale1,1))
        ANSIC_ERROR("setenv/LC_MESSAGES",locale1);
    } else if (setenv("LC_MESSAGES",locale2,1))
      ANSIC_ERROR("setenv/LC_MESSAGES",locale2);
    { /* Invalidate the gettext internal caches. */
      char *td = textdomain(NULL);
      if (NULL == td) ANSIC_ERROR("textdomain",NULL);
      if (NULL == textdomain(td)) ANSIC_ERROR("textdomain",td);
    }
  } else if (lisp_error_p) {
    pushSTACK(ascii_to_string(argv_language));
    pushSTACK(TheSubr(subr_self)->name);
    error(error_condition,GETTEXT("~S: invalid language ~S"));
  } else {
    fprintf(stderr,"invalid language %s.\n",argv_language);
    goto init_language_failure;
  }
  /* At this point we have chosen the language based upon an
     environment variable GNU gettext knows about.
     argv_localedir=NULL usually means (setq *current-language* ...)
     in which case reusing text domain from the original (or previous)
     call to bindtextdomain() is a wise choice */
  if (argv_localedir) { /* make sure that it exists and is a directory */
    char truename[MAXPATHLEN];
    switch (classify_namestring(argv_localedir,truename,NULL,NULL)) {
      case FILE_KIND_FILE:
        if (lisp_error_p) {
          pushSTACK(ascii_to_string(truename));
          pushSTACK(ascii_to_string(argv_localedir));
          pushSTACK(TheSubr(subr_self)->name);
          error(error_condition,GETTEXT("~S: ~S resolves to ~S which is a file, not a directory"));
        } else {
          fprintf(stderr,GETTEXT("%s resolves o %s which is a file, not a directory\n"),argv_localedir,truename);
          goto init_language_failure;
        }
        MY_NOTREACHED;
      case FILE_KIND_NONE: case FILE_KIND_BAD:
        if (lisp_error_p) {
          pushSTACK(ascii_to_string(argv_localedir));
          pushSTACK(TheSubr(subr_self)->name);
          error(error_condition,GETTEXT("~S: ~S does not exist"));
        } else {
          fprintf(stderr,GETTEXT("%s does not exist\n"),argv_localedir);
          goto init_language_failure;
        }
        MY_NOTREACHED;
      case FILE_KIND_DIR:
        if (NULL == bindtextdomain("clisp",truename))
          ANSIC_ERROR("bindtextdomain/clisp",truename);
        if (NULL == bindtextdomain("clisplow",truename))
          ANSIC_ERROR("bindtextdomain/clisplow",truename);
    }
  }
 #ifdef ENABLE_UNICODE
  if (NULL == bind_textdomain_codeset("clisp","UTF-8"))
    ANSIC_ERROR("bind_textdomain_codeset","UTF-8");
 #endif
  return;
 init_language_failure:
  fprintf(stderr,GETTEXT("WARNING: setting language to %s failed.\n"),argv_language);
#undef ANSIC_ERROR
#undef MY_NOTREACHED
}

local const char * clisp_gettext (const char * domain, const char * msgid) {
  var const char * translated_msg;
  if (msgid[0] == '\0') {
    /* If you ask gettext to translate the empty string, it returns
       the catalog's header (containing meta information)! */
    translated_msg = msgid;
  } else {
    begin_system_call();
    translated_msg = dgettext(domain,msgid);
    end_system_call();
  }
  return translated_msg;
}

/* High-level messages, which are converted to Lisp strings, are
   stored in a separate catalog and returned in the UTF-8 encoding. */
modexp const char * clgettext (const char * msgid)
{ return clisp_gettext("clisp", msgid); }

/* Low-level messages, which are output through fprintf(3), are
   stored in a separate catalog and returned in locale encoding. */
global const char * clgettextl (const char * msgid)
{ return clisp_gettext("clisplow", msgid); }

#else
  #define clgettext(m)  m       /* for CLSTEXT below */
#endif

/* FIXME: Don't hardwire ISO-8859-1. The catalog's character set is
 given by the "Content-Type:" line in the meta information.
 in anticipation of this fix, CLSTEXT is a function, not a macro */
modexp maygc object CLSTEXT (const char* asciz) {
  return asciz_to_string(clgettext(asciz),Symbol_value(S(utf_8)));
}

global maygc object CLOTEXT (const char* asciz) {
  dynamic_bind(S(packagestar),O(default_package)); /* bind *PACKAGE* */
  pushSTACK(CLSTEXT(asciz)); funcall(L(read_from_string),1);
  dynamic_unbind(S(packagestar));
  return value1;
}
