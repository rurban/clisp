/* ======= General internationalization, for Lisp programs too ======= */

#include "clisp.h"
#include "config.h"

#if defined(HAVE_LOCALE_H)
# include <locale.h>
#endif

#ifdef UNICODE
# define if_UNICODE(statement)  statement
#else
# define if_UNICODE(statement)  /*nothing*/
#endif

DEFMODULE(i18n,"I18N")

/* Returns the <locale.h> value corresponding to a LC_... constant. */
DEFCHECKER(check_locale_category, LC_ALL LC_COLLATE LC_CTYPE LC_MESSAGES \
           LC_MONETARY LC_NUMERIC LC_TIME                               \
           LC_PAPER LC_NAME LC_ADDRESS LC_TELEPHONE LC_MEASUREMENT      \
           LC_IDENTIFICATION)

static inline object do_gettext (const char* msgid,
                                 const char* domain, int category)
{
  const char* translated_msg;
  if (msgid[0] == '\0') {
    translated_msg = "";  /* Don't return the catalog's header entry. */
  } else {
    begin_system_call();
#  ifdef UNICODE
    if (domain != NULL)
      bind_textdomain_codeset(domain,"UTF-8");
#  endif
    translated_msg = dcgettext(domain,msgid,category);
    end_system_call();
  }
  return asciz_to_string(translated_msg,Symbol_value(S(utf_8)));
}

static inline object do_ngettext (const char* msgid, const char* msgid_plural,
                                  const char* domain, uint32 n, int category)
{
  const char* translated_msg;
  begin_system_call();
# ifdef UNICODE
  if (domain != NULL)
    bind_textdomain_codeset(domain,"UTF-8");
# endif
  translated_msg = dcngettext(domain,msgid,msgid_plural,n,category);
  end_system_call();
  return asciz_to_string(translated_msg,Symbol_value(S(utf_8)));
}

DEFUNR(I18N:GETTEXT, msgid &optional domain category)
{ /* returns the translation of msgid in the given domain,
     depending on the given category. */
  object msgid = check_string(STACK_2);
# ifdef GNU_GETTEXT
  with_string_0(msgid,Symbol_value(S(ascii)),msgid_asciz, {
    object domain = STACK_1;
    if (missingp(domain)) {
      int category = check_locale_category(STACK_0);
      VALUES1(do_gettext(msgid_asciz,NULL,category));
    } else {
      domain = check_string(domain);
      with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
        int category = check_locale_category(STACK_0);
        VALUES1(do_gettext(msgid_asciz,domain_asciz,category));
      });
    }
  });
# else
  VALUES1(msgid);
# endif
  skipSTACK(3);
}

DEFUNR(I18N:NGETTEXT,msgid msgid_plural n &optional domain category)
{ /* returns the plural form of the translation for of msgid and n in
     the given domain, depending on the given category. */
  object msgid = (STACK_4 = check_string(STACK_4));
  object msgid_plural = (STACK_3 = check_string(STACK_3));
  object arg = (STACK_2 = check_pos_integer(STACK_2));
  uint32 n;
  if (posfixnump(arg))
    n = posfixnum_to_L(arg);
  else {
    /* arg is a Bignum. Plural form depends only on (mod arg 1000000). */
    pushSTACK(arg); pushSTACK(fixnum(1000000)); funcall(L(mod),2);
    n = 1000000 + posfixnum_to_L(value1);
  }
  msgid = STACK_4; msgid_plural = STACK_3;
#ifdef GNU_GETTEXT
  with_string_0(msgid,Symbol_value(S(ascii)),msgid_asciz, {
    with_string_0(msgid_plural,Symbol_value(S(ascii)),msgid_plural_asciz, {
      object domain = STACK_1;
      if (missingp(domain)) {
        int category = check_locale_category(STACK_0);
        VALUES1(do_ngettext(msgid_asciz,msgid_plural_asciz,NULL,
                           n,category));
      } else {
        domain = check_string(domain);
        with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
          int category = check_locale_category(STACK_0);
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

DEFUNR(I18N:TEXTDOMAIN,)
{ /* returns the current default domain. */
#ifdef GNU_GETTEXT
  const char* domain;
  begin_system_call();
  domain = textdomain(NULL);
  end_system_call();
  VALUES1(asciz_to_string(domain,Symbol_value(S(ascii))));
#else
  VALUES1(NIL);
#endif
}

DEFUN(I18N:SET-TEXTDOMAIN, domain)
{ /* sets the default domain. */
  object domain = check_string(popSTACK());
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

DEFUN(I18N:TEXTDOMAINDIR, domain)
{ /* returns the message catalog directory for the given domain. */
  object domain = check_string(popSTACK());
  #ifdef GNU_GETTEXT
  const char* dir;
  with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
    begin_system_call();
    dir = bindtextdomain(domain_asciz,NULL);
    end_system_call();
  });
  VALUES1(dir != NULL ? OSdir_to_pathname(dir) : (object)NIL);
  #else
  VALUES1(NIL);
  #endif
}

DEFUN(I18N:SET-TEXTDOMAINDIR, domain directory)
{ /* sets the message catalog directory for the given domain. */
  object domain = check_string(STACK_1);
#ifdef GNU_GETTEXT
  /* Check and use default directory, because the bindtextdomain()
     documentation recommends that the argument be an absolute pathname,
     to protect against later chdir() calls. */
  object directory = pathname_to_OSdir(STACK_0,true);
  with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
    begin_system_call();
    bindtextdomain(domain_asciz,TheAsciz(directory));
    end_system_call();
  });
#endif
  VALUES1(STACK_0);
  skipSTACK(2);
}


/* ======================== locale ======================== */
DEFUN(I18N:SET-LOCALE, category &optional locale)
{ /* call setlocale(3) */
  int category = check_locale_category(STACK_1);
  object locale = STACK_0;
  char* retval;
  if (missingp(locale)) {
    begin_system_call();
    retval = setlocale(category,NULL);
    end_system_call();
  } else {
    with_string_0(STACK_0 = check_string(locale),Symbol_value(S(ascii)),loc_z,{
        begin_system_call();
        retval = setlocale(category,loc_z);
        end_system_call();
      });
  }
  VALUES1(retval ? asciz_to_string(retval,Symbol_value(S(ascii))) : NIL);
  skipSTACK(2);
}
