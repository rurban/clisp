# Internationalization for CLISP
# Bruno Haible 1990-2002
# Sam Steingold 1998-2002

#include "lispbibl.c"

#include <locale.h>

#ifdef UNICODE
#define if_UNICODE(statement)  statement
#else
#define if_UNICODE(statement)  /*nothing*/
#endif

# =========== Old-style internationalization, for CLISP itself only ===========

# (SYS::CURRENT-LANGUAGE) returns the current language.
LISPFUNN(current_language,0) {
  VALUES1(O(current_language));
}

# (SYS::SET-CURRENT-LANGUAGE LANG) ==
# (SETF (SYS::CURRENT-LANGUAGE) LANG) ==
# (SETQ *CURRENT-LANGUAGE* LANG)
# LANG is either LANGUAGE or (LANGUAGE . LOCALE-DIRECTORY)
LISPFUNN(set_current_language,1) {
 #ifndef LANGUAGE_STATIC
  if (consp(STACK_0)) {
    var object new_lang = Car(STACK_0);
    var object new_locd = Cdr(STACK_0);
    if (!symbolp(new_lang)) fehler_symbol(new_lang);
    if (!stringp(new_locd)) fehler_string(new_locd);
    with_sstring_0(Symbol_name(new_lang),O(misc_encoding),lang,{
      with_string_0(new_locd,O(misc_encoding),localedir,
                    { init_language(lang,localedir); });
    });
  } else {
    if (!symbolp(STACK_0)) fehler_symbol(STACK_0);
    with_sstring_0(Symbol_name(STACK_0),O(misc_encoding),lang,
                   { init_language(lang,NULL); });
  }
 #else
  if (!eq(STACK_0,O(current_language))) {
    pushSTACK(STACK_0);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~: cannot set language to ~"));
  }
 #endif
  VALUES1(O(current_language)); skipSTACK(1);
}

# (SYS::TEXT english) returns the message in the current language
LISPFUNN(text,1) {
 #ifndef GNU_GETTEXT
  VALUES1(ENGLISH ? STACK_0 : NIL);
 #else
  if (!stringp(STACK_0)) fehler_string(STACK_0);
  with_string_0(STACK_0,Symbol_value(S(ascii)),asciz, {
    VALUES1(CLSTEXT(asciz));
  });
 #endif
  skipSTACK(1);
}

# ============ General internationalization, for Lisp programs too ============

#ifdef GNU_GETTEXT

# Returns the <locale.h> value corresponding to a LC_... constant.
local int check_category (object category)
{
  if (missingp(category) || eq(category,S(Klc_messages)))
    return LC_MESSAGES;
  if (eq(category,S(Klc_ctype)))
    return LC_CTYPE;
  if (eq(category,S(Klc_time)))
    return LC_TIME;
  if (eq(category,S(Klc_collate)))
    return LC_COLLATE;
  if (eq(category,S(Klc_monetary)))
    return LC_MONETARY;
  # Not LC_NUMERIC because setlocale(LC_NUMERIC,"") has unwanted side effects
  # on some libc functions.
  # Not LC_PAPER, LC_NAME, LC_ADDRESS, LC_TELEPHONE, LC_MEASUREMENT,
  # LC_IDENTIFICATION because these categories are not portable.
  pushSTACK(category); # TYPE-ERROR slot DATUM
  pushSTACK(O(type_category)); # TYPE-ERROR slot EXPECTED-TYPE
  pushSTACK(category);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error, GETTEXT("~: invalid locale category ~"));
}

local inline object do_gettext (const char* msgid,
                                const char* domain, int category)
{
  var const char* translated_msg;
  if (msgid[0] == '\0') {
    translated_msg = ""; # Don't return the catalog's header entry.
  } else {
    begin_system_call();
    #ifdef UNICODE
    if (domain != NULL)
      bind_textdomain_codeset(domain,"UTF-8");
    #endif
    translated_msg = dcgettext(domain,msgid,category);
    end_system_call();
  }
  return asciz_to_string(translated_msg,Symbol_value(S(utf_8)));
}

local inline object do_ngettext (const char* msgid, const char* msgid_plural,
                                 const char* domain, uint32 n, int category)
{
  var const char* translated_msg;
  begin_system_call();
  #ifdef UNICODE
  if (domain != NULL)
    bind_textdomain_codeset(domain,"UTF-8");
  #endif
  translated_msg = dcngettext(domain,msgid,msgid_plural,n,category);
  end_system_call();
  return asciz_to_string(translated_msg,Symbol_value(S(utf_8)));
}

#endif

LISPFUN(gettext,1,2,norest,nokey,0,NIL)
# (I18N:GETTEXT msgid [domain [category]]) returns the translation of
# msgid in the given domain, depending on the given category.
{
  var object msgid = STACK_2;
  if (!stringp(msgid))
    fehler_string(msgid);
  #ifdef GNU_GETTEXT
  with_string_0(msgid,Symbol_value(S(ascii)),msgid_asciz, {
    var object domain = STACK_1;
    if (missingp(domain)) {
      var int category = check_category(STACK_0);
      VALUES1(do_gettext(msgid_asciz,NULL,category));
    } else {
      if (!stringp(domain))
        fehler_string(domain);
      with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
        var int category = check_category(STACK_0);
        VALUES1(do_gettext(msgid_asciz,domain_asciz,category));
      });
    }
  });
  #else
  VALUES1(msgid);
  #endif
  skipSTACK(3);
}

LISPFUN(ngettext,3,2,norest,nokey,0,NIL)
# (I18N:NGETTEXT msgid msgid_plural n [domain [category]]) returns the plural
# form of the translation for of msgid and n in the given domain, depending
# on the given category.
{
  var object msgid = STACK_4;
  if (!stringp(msgid))
    fehler_string(msgid);
  var object msgid_plural = STACK_3;
  if (!stringp(msgid_plural))
    fehler_string(msgid_plural);
  var object arg = STACK_2;
  if (!(integerp(arg) && positivep(arg))) {
    pushSTACK(arg); # TYPE-ERROR slot DATUM
    pushSTACK(O(type_posinteger)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(arg); pushSTACK(S(ngettext));
    fehler(type_error, GETTEXT("~: argument should be an integer >=0, not ~"));
  }
  var uint32 n;
  if (posfixnump(arg))
    n = posfixnum_to_L(arg);
  else {
    # arg is a Bignum. Plural form depends only on (mod arg 1000000).
    pushSTACK(arg); pushSTACK(fixnum(1000000)); C_mod();
    n = 1000000 + posfixnum_to_L(value1);
  }
  #ifdef GNU_GETTEXT
  with_string_0(msgid,Symbol_value(S(ascii)),msgid_asciz, {
    with_string_0(msgid_plural,Symbol_value(S(ascii)),msgid_plural_asciz, {
      var object domain = STACK_1;
      if (missingp(domain)) {
        var int category = check_category(STACK_0);
        VALUES1(do_ngettext(msgid_asciz,msgid_plural_asciz,NULL,
                           n,category));
      } else {
        if (!stringp(domain))
          fehler_string(domain);
        with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
          var int category = check_category(STACK_0);
          VALUES1(do_ngettext(msgid_asciz,msgid_plural_asciz,domain_asciz,
                             n,category));
        });
      }
    });
  });
  #else
  VALUES1(n == 1 ? msgid : msgid_plural);
  #endif
  skipSTACK(5);
}

LISPFUNN(textdomain,0)
# (I18N:TEXTDOMAIN) returns the current default domain.
{
  #ifdef GNU_GETTEXT
  var const char* domain;
  begin_system_call();
  domain = textdomain(NULL);
  end_system_call();
  VALUES1(asciz_to_string(domain,Symbol_value(S(ascii))));
  #else
  VALUES1(NIL);
  #endif
}

LISPFUNN(set_textdomain,1)
# (I18N::SET-TEXTDOMAIN domain) sets the default domain.
{
  var object domain = popSTACK();
  if (!stringp(domain))
    fehler_string(domain);
  #ifdef GNU_GETTEXT
  with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
    begin_system_call();
    textdomain(domain_asciz);
    if_UNICODE(bind_textdomain_codeset(domain_asciz,"UTF-8"));
    end_system_call();
  });
  #endif
  VALUES1(domain);
}

LISPFUNN(textdomaindir,1)
# (I18N::TEXTDOMAINDIR domain) returns the message catalog directory
# for the given domain.
{
  var object domain = popSTACK();
  if (!stringp(domain))
    fehler_string(domain);
  #ifdef GNU_GETTEXT
  var const char* dir;
  with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
    begin_system_call();
    dir = bindtextdomain(domain_asciz,NULL);
    end_system_call();
  });
  VALUES1(dir != NULL ? OSdir_to_pathname(dir) : NIL);
  #else
  VALUES1(NIL);
  #endif
}

LISPFUNN(set_textdomaindir,2)
# (I18N::SET-TEXTDOMAINDIR domain directory) sets the message catalog directory
# for the given domain.
{
  var object domain = STACK_1;
  if (!stringp(domain))
    fehler_string(domain);
  #ifdef GNU_GETTEXT
  # Check and use default directory, because the bindtextdomain()
  # documentation recommends that the argument be an absolute pathname,
  # to protect against later chdir() calls.
  var object directory = pathname_to_OSdir(STACK_0,true);
  with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
    begin_system_call();
    bindtextdomain(domain_asciz,TheAsciz(directory));
    end_system_call();
  });
  #endif
  VALUES1(STACK_0);
  skipSTACK(2);
}
