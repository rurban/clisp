// -*- mode: c++ -*-
// Copyright (c) 1996-1997 by Gilbert Baumann, distributed under GPL
// --------------------------------------------------------------------------------
//
//    Title: 	C implementation of CLX utilizing the Xlib
//    Created:	Sat Dec  2 18:04:51 1995
//    Author: 	Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
//    Copying:  (c) copyright 1996 by Gilbert Baumann distributed under GPL.
//
// --------------------------------------------------------------------------------
//
// Revision 1.22  1999-06-06  bruno
// - get_font_info_and_display now optionally returns the Lisp font. It also
//   fills in the font's encoding.
// - New function to_XChar2b, used to convert a character sequence to a font
//   index sequence. Works for linear non-iso8859-1 fonts. Still needs work
//   for chinese or japanese fonts.
//
// Revision 1.21  1999-05-30  bruno
// - Add missing begin_callback() in `xlib_io_error_handler'.
// - Save subr_self during some calls in xlib:change-property.
// - Fix some obvious typos in `font_char_info'.
// - Call XFree() when done with the result of XGetAtomName().
// - Improved error handling in `get_font_info_and_display'.
//
// Revision 1.20  1999-04-04  bruno
// - Modifications for UNICODE.
//
// Revision 1.19  1998-10-19  bruno
// - Use the macro `nonreturning_function', not `nonreturning'.
//
// Revision 1.18  1997-06-22  bruno
// - replace first preprocessing with CCMP2C; then only comments need to be
//   stripped before e2d can be applied.
// - compilation in WIDE mode works now (use `eq', not `==' to compare objects)
// - fixed buggy ESLOT4 macro (make_xatom wants a `Display*', not an `object')
// - DYNAMIC_ARRAY has 3 arguments since 1996-07-22
// - allocate_byte_array is now exported from array.d
// - typos and spaces here and there
//
// $Id$
// $Log$
// Revision 1.18  1997/06/12  00:23:35  gilbert
// - nothing special
//
// Revision 1.17  1997/06/02  16:19:27  gilbert
// - Lots of minor tweeks here and there
// - First attempt to get the IMAGE implementation right
// - Found memory leak in font handling code
// - fixed bug in xpm::read-pixmap-from-file occured when shape-mask-p = t
// - (xlib:open-display "localhost") works now
//
// Revision 1.16  1996/10/11  14:18:03  gilbert
// - Some tweakings to get it smoother compiled under 07-22
//
// Revision 1.15  1996/10/11  10:15:52  gilbert
// - removed all GETTEXTs because it does not compile on 07-22
// - error hander now calls the Lisp error handler
// - fixed invokation of DYNAMIC_ARRAY
// - removed all 'reg?' decls
// - lots of bug fixes
// - changed outlook of xlib:%restore-gcontext-components.
// - the clip mask and the dash list are now retrievable from a gcontext
// So the gcontext chapter should be complete now. [sans cache-p]
//
// Revision 1.14  1996/10/09  09:03:42  gilbert
// - lots of bug fixes
// - changed outlook of xlib:%restore-gcontext-components.
// - the clip mask and the dash list are now retrievable from a gcontext
// So the gcontext chapter should be complete now. [sans cache-p]
//
// Revision 1.13  1996/10/05  01:00:50  gilbert
// - begin_call / end_call should be finally added everwhere..
// - fixed some serve bugs in XLIB:CHANGE-PROPERTY.
// - providing {WINDOW,PIXMAP,DRAWABLE,FONT,COLORMAP}-LOOKUP
//
// Revision 1.12  1996/10/04  03:14:53  gilbert
// - Introduced new macro 'STANDARD_XID_OBJECT_LOOK'.
// - The {WINDOW,PIXMAP,DRAWABLE,FONT,COLORMAP}-{DISPLAY,PLIST,PLIST-SETTER,ID}
//   functions now do proper type checking.
//   The corresponding xx-EQUAL functions are based on the XID alone.
//   Same needs to be done for PTR objects.
// - The silly (and ineffient) 'general_p' function vanished.
//
// Revision 1.11  1996/10/03  03:37:12  gilbert
// - all invokations of "TheFpointer()->fp_pointer" are now guarded by "fp_validp".
//
// Revision 1.10  1996/10/03  02:45:00  gilbert
// - made the get_[su]int?? functions do type checking
// - Got rid of get_string and get_stringable.
//
// Revision 1.9  1996/10/02  10:39:45  gilbert
// - eliminated get_xatom_name due to GC vulnerability.
// - got rid of most get_string due to the same reason
// - as always some begin/end_calls added.
//
// Revision 1.8  1996/09/28  21:59:02  gilbert
// - Only lots of code movements to gather stuff which belongs together
//   also to appear together for better maintainability.
//
// Revision 1.7  1996/09/28  20:52:24  gilbert
// - Redone the invoke stuff, because
//   YOU HAVE TO SAVE YOUR FILE BEFORE YOU CHECK IT IN!
//   [Emacs does not check this 8-{ Realy angry! $%^#@#%$@ ]
//
// Revision 1.6  1996/09/28  20:41:23  gilbert
// - added type checking to get_{xid,ptr}_object
// - got rid of display_of all now done with the get_foo_and_display
//   functions, so get_font_info is now called get_font_info_and_display.
// - also get rid of the 'invoke' function, it was considered too
//   unportable.
//
// Revision 1.5  1996/09/28  01:45:06  gilbert
// Converted all enum definitions into the DEF_ENUM macro for code size.
//
// Revision 1.4  1996/09/27  12:48:33  gilbert
// Cleaned up event disassembling code
//
// Revision 1.3  1996/08/02  10:51:40  gilbert
// Only for Bruno.
//
// Revision 1.2  1996/07/27  02:25:31  gilbert
// *** empty log message ***
//
// Revision 1.1  1996/07/08  15:47:43  gilbert
// Initial revision
//   ^^^ That is not true! Coding this started actually in Dec 1995.
//       (Just around a couple of days befor chrismas?)


// --- TODO ---
// - fake the REPORT-ASYNCHRONOUS-ERRORS slot on displays.
//
// - get DISPLAY-ERROR-HANDLER working.
//   define DEFAULT-ERROR-HANDLER in Lisp.
//
// - Garnet seems to loose exposure events from time to time, I do not know if
//   it is my fault or a garnet bug? This thing is hard to trace, since it seems
//   to depend on how fast the window gets mapped, or how fast the garnet code
//   is!
//
// - the get_XXX functions *all* should do type checking
//
// - Most uses of 'fixnum' are wrong, since the actual type is somewhat more
//   precise known. fixnums are only correct as indexes into sequences and
//   such!
//

// -------
//   When passing #<XLIB:GCONTEXT #x00000160>  to FONT-ASCENT it says:
//   "Error: XLIB:FONT-ASCENT: NIL is not of type XLIB:FONT"
//   Why is two way wrong:
//     a. A gcontext should be ok where a font is
//     b. Why is dumped NIL and not the gcontext?
//   O.k. this was due to the fact that the font was actually never set.
//   fix that!
//   
//   (Maybe we just pass the gcontext down to Xlib, since Xlib is supposed
//   to do the same here.)
// -------
//   When a display is closed the host name and such sould not be available any longer
// -------
// 
// - there should be a function called closed-p, which checks wether an given
//   xid or ptr object is closed or not. When re-incarnating CLISP all CLX
//   objects should get closed. get_xxx functions should check on that.  But
//   there a some nasty problems:
//    a.) when I setup the XFree... request it is not yet at the server
//    b.) There may be additional references in the servers queque.
//   Maybe I open a pseudo window, just for sneeking for destruction events?
//
// - XLIB:TEXT-EXTENTS and XLIB:TEXT-WIDTH needs fixing for non simple
//   string arguments.
//
// - Garnet accidentally used :on/:off for save under values
//   [But only sometimes]
//
// - Scan for all funcalls and safe subr_self prior to call.
//
// - Is it right that we make the list of displays public?
//
// - we should  rethink a bit  more  the font  handling  stuff, because  several
//   situations could arise:
//
//    . A  font may not  have an fid, this  implies, that the  font could not be
// 	hashed,  furthermore  we should init  a  finializer to discard  also the
// 	fontinfo when the font object becomes inaccessible.
//    . A font may come without per-character information. (But only when it has
// 	a 0 font id, hence a pseudo font)
//    . If we open a pseudo font, we should then enter it into the hash table.
//    . ensure that the  font-name slot has some  valid value.  (It is even  not
// 	guaranteed to be available.)
//
// - go thru' the whole  source and find all error  messages and check that they
//   are given right. [fehler wants its arguments backwards!]
//
// - since some  make_xxx functions use value1,  I should generally push the
//   return values onto the stack before actually returning.
//
// - since somebody could now create classes on top of  window, check the xid we
//   want to read, if it is unbound emit  some reasonable error message.  [If it
//   is unbound the slot-value function returns an error on its own already.]
//
// - What about the plist slot? (Should we bind it to a useful value?)
//
// - maybe we make the clos-instance representation optional? Since it eats some
//   speed. (Fetching a slot value is not the fastest operation.)
//
// - several X11 functions, which return  lists  may actually  pass NULL as  the
//   empty sequence?!
//
// - some of the enumerating  functions (list XListDepths  etc)  do not set  the
//   count_return on failure, so set it yourself to 0 before  you make the call,
//   so that we do not run into difficulties.
//
// - we should change the names xxx-SETTER to SET-xxx, since this is the way clx
//   names these functions.
//   [But there seems to be an incompatibility between (setf fn) and defsetf?!]
//
// - Maybe we  dont  put all  those symbols in the  XLIB  package  to be able to
//   include the MIT-CLX also.  (Generally CLISP lacks some of the  nice package
//   features  Symbolics  Common LISP  provided.   There were also  anonymous or
//   invisible packages.)
//
// - errors should be reported in a much more meaningful way.
//
// - the big export list in clx.lsp seems to export some superfluous symbols.
//
// - put also ptr objects into the hashtable.
//   Is there any way to get 'em anyhow back?
//
// - the  xlib:free-xxx  routines should remove  that object  from the hashtable.
//   (or shouldn`t they?)  What does the Xserver with free`ed objects?  And also
//   they might be still alive, if some other application uses it.
//   [Well simply take a look at the MIT code.] [What about GC?!]
//
// - should DISPLAY be a class? What with VISUAL-INFO, COLOR an so on ...
//
// - We should really make DISPLAY-AFTER-FUNCTION to live.
//   The is also a XAfterFunction or something like that on the C side of life.
//
// - What exactly is a drawable? a type? a class?
//
// - We should insert a lot more type checks?
//
// - Since type checks  are rather expensive  think about listening to SAFTY and
//   SPEED ...
//


// --- DONE ----
// 
// - some functions emit errors saying #xDDD using decimal notation!
// - upon disconnection we simply get a broken pipe message and exit
//   [This is not the disired way, since we want to handle such an error.]
// - with-gcontext !!!
// - rename the 'font-name' slot of font to 'name'.
// - take  a look  at  the CLUE  code,  what  it does  with the  :xxx  option to
//   create-xxx functions?!
// - DISPLAY-AFTER-FUNCTION setter is needed.
// - make display/window/pixmap clos-instances (see the CLUE patches for that)
// - put xids into the hashtable and do not build the object new on each request.
// - plists (partly done)
// - get_visual
// - how to proceed with visuals? In CLX a visual is just a card29  in CLX it is
//   a pointer to a structure.
// - Together with the CLX implementation should go a wholine implementation.
//   (partly there)


// --- NOTE ---
//
//   This package is not  actually optimized for speed,  since my intention  and
// BTW  the  overall    intension  of  CLISP   is    to make   the   whole beast
// small. (Actually you gain speed due to reduced  paging). Also this code is at
// some places BFI!
//
//   The general idea  behind writing this bunch of  code,  is to provide  a CLX
// implementation for  CLISP,  which is feasible  of   both speed and  space.  I
// actually use  the libX library,  because if you  want to do  graphics on your
// machine, you will already have it in memory, so it adds no extra cost.   [One
// disadvantage is that I am  forced in some places to  go into the internals of
// the libX, since the CLX  specification is more powerful  at some places  than
// the libX implementation. This add another  source of in-portability of CLISP,
// so *please*, if you encounter compilation problems mail me, so I could adjust
// the code ...]
//
//   CLX adds on my machines another ~700k  of memory needs  for CLISP, but this
// implementation add only 70k [*I* know that I am telling  lies here, since the
// libX11 itself has a size of ~500k; But as long as I have no  pure LISP OS but
// need the  UNIX to boot it ...]  and a great  bunch of speed. 
//
//   Also having this implemenation should  gain a big  plus against GCL.  (IMHO
// gcl is very bad code compared to CLISP! [gcl is actually akcl]) flame,flame.
//
// BTW It should be fun to write the graph.d routines on top of CLX!


// --- FUTURE ---
//
//  Xpm Support?
//
// We should also include support for  the xpm library  to have a nice access to
// .xpm files, which is hardly needed, since I do not want to duplicate this non
// trivial code in Lisp.  But  we have to  think about a Lisp representation for
// pixmaps. I could also imagine a `defpixmap`  macro.  Just another question is
// if we want to put the  xpm routines into  another package. (called `x-pixmap`
// or just 'xpm'). I want also to write some documentation for the then Lisp xpm
// routines. But since the xpm library seems to be a changing thing it is also a
// question, how we cope with them.
//
//  Incooperation into the FFI?
//
// Since we use could convert now a  WINDOW object into  a libX11 Window object,
// it may  be worth offer this also   to the FFI.  When I  finished  this work I
// should take a look at the FFI.


// --- IMPLEMENTATION NOTES ----------------------------------------------------
//
//   If there are setf-methods for some functions, I  call `em xxx_setter, later
// I bind  them  into  LISP, with putting  the   right properties on   the right
// symbols.  
//
// The following types are only XID`s:
//   window, drawable, font, pixmap, cursor, colormap, GContext, keysym
//
// No XID, but data-structures:
//   color, display, screen, GC
//
//
//  First I  define some  datatypes   by providing  get_xxx, make_xxx  and xxx_p
//  functions.  Note that this set is not complete, since  not all functions are
//  actually needed.  The consistent  name is curel,  since some macros I define
//  later take a "type" argument, which is  concatenated with 'make_' or 'get_'.
//  This is done to make this file more dense; (Thus save me  a lot of redundand
//  typeing.) 
//
//
//   type     | dpy? | XID-P | hashed-p | Note
//  ----------+------+-------+----------+------------------------------
//   GCONTEXT |  T   | NIL   |   NIL    | Is really a pointer
//   WINDOW   |  T   | T     |   T      |
//   PIXMAP   |  T   | T     |   T      |
//   CURSOR   |  T   | T     |   T      |
//   COLORMAP |  T   | T     |   T      |
//   FONT     |  T   | T/NIL |   T/NIL  |
//   SCREEN   |  T   | NIL   | (should  | Could also been represented as index?
//            |      |       |    be)   |
//   DISPLAY  |  NIL | NIL   |   NIL    | 
//
//   Class Heterarchie
//  --------------------
//     xlib-object --+--> xid-object --+--> DRAWABLE -+--> WINDOW
//                   |                 |              +--> PIXMAP
//                   |                 |--> CURSOR
//                   |                 +--> COLORMAP
//                   |                 +--> FONT
//                   |      
//                   +--> ptr-object --+--> GCONTEXT
//                                     |
//                                     +--> DISPLAY
//
//  Just in case you prefer a textual representation:
//
//    (defclass xlib-object ()                      (plist display))
//    (defclass xid-object  (xlib-object)           (xid))
//    (defclass ptr-object  (xlib-object)           (ptr))
//
//    (defclass drawable    (xid-object)            ())
//    (defclass window      (drawable)              ())
//    (defclass pixmap      (drawable)		    ())
//    (defclass cursor      (xid-object)            ())
//    (defclass colormap    (xid-object)            ())
//    (defclass gcontext    (ptr-object)            ())
//    (defclass display     (ptr-object)            ())
//    (defclass font        (xid-object)            (font-info name))
//


// -- NOTE --
// For further for grepability I use the following tags in comments:
//
//  XXX            - really bad and a major risk to safety/usability.
//  FIXME          - just fix me, something that ought to be done before next release.
//  TODO           - something, which should be done and is already considered useful.
//  FUTURE         - something, which is just an idea  and it is not   yet decided, if I
//                   implement it  ever; It  may  also later  been  considered  silly. I
//                   invite for discussion about those items.
//  OK             - the opposite of XXX. Functions which are considered to be totally
//                   finish and had undergone a test are marked with this.
//  UNDEFINED      - this is thought for undefined functions at whole
//  NOTIMPLEMENTED - is thought for not implemented features in an partly defined function.
//

// enough bla bla, lets start codeing, we have a long long way before us ...


// Wether you want to use general 'slot-value' function or the low
// level function 'slot_up' from record.d
//
#define WITH_SLOT_UP 0

#include "lispbibl.c"
#include "clx.tabs.c"
#include <X11/Xlib.h>
#include <X11/Xutil.h>		// This is for XGetVisualInfo
#include <X11/Xcms.h>           // This if forXcmsCCCOfColormap and XcmsVisualOfCCC.

#define DEBUG_CLX 0

#ifndef FOREIGN
#error FOREIGN is not defined.
#error The clx interface needs a CLISP built with the foreign pointer datatype support.
#error Go into the main CLISP makefile and add a -DFOREIGN=void* to CFLAGS make variable
#error and rebuild CLISP before coming back here.
#endif

// ... But first we provide some prototypes for functions, which maybe worth
// included in libX11 and are defined at the end of this file.
local Visual *XVisualIDToVisual (Display *dpy, VisualID vid);	// might also been called XVisualIDFromVisual**(-1)
local int XScreenNo (Display *dpy, Screen *screen);             // Find the screen index from a Screen*

// Own forward declaration
local Display *pop_display (void);

// Some fix-ups:

#define unless(X) if(!(X))	// I am addicted to it.
#define NOTIMPLEMENTED NOTREACHED
#define UNDEFINED NOTIMPLEMENTED


// debug
#if DEBUG_CLX
#define dprintf(x) do{ printf x; fflush(stdout); }while(0)
#else
#define dprintf(x) do{}while(0)
#endif


// ----------------------------------------------------------------------------------------------------
//  General purpose utilities
// ----------------------------------------------------------------------------------------------------

#if WITH_SLOT_UP
extern object* slot_up (void);	// from record.d
#else
  // Liefert aus einer Slot-Location-Info die Adresse eines existierenden Slots
  // in einer Instanz einer Standard- oder Structure-Klasse.
  #define ptr_to_slot(instance,slotinfo)  \
    (atomp(slotinfo)                                            \
     /* local slot, slotinfo ist Index */                       \
     ? &TheSrecord(instance)->recdata[posfixnum_to_L(slotinfo)] \
     /* shared slot, slotinfo ist (class . index) */            \
     : &TheSvector(TheClass(Car(slotinfo))->shared_slots)       \
                  ->data[posfixnum_to_L(Cdr(slotinfo))]         \
    )

  local object* slot_up (void);
  #ifdef RISCOS_CCBUG
    #pragma -z0
  #endif
  local object* slot_up()
    { pushSTACK(STACK_1); C_class_of(); // (CLASS-OF instance) bestimmen
     {var object slotinfo = // (GETHASH slot-name (class-slot-location-table class))
        gethash(STACK_0,TheClass(value1)->slot_location_table);
      if (!eq(slotinfo,nullobj)) // gefunden?
        { return ptr_to_slot(STACK_1,slotinfo); }
        else
        // missing slot -> (SLOT-MISSING class instance slot-name caller)
        { pushSTACK(value1); pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
          pushSTACK(TheSubr(subr_self)->name);
          funcall(S(slot_missing),4);
          return NULL;
        }
    }}
  #ifdef RISCOS_CCBUG
    #pragma -z1
  #endif
#endif

// So dummer Compiler hier kommt die *Deklaration* :
//local nonreturning void my_standard_type_error (object subrnam);
//       ^^^^^^^^^^ Schau her! _n_o_n_r_e_t_u_r_n_i_n_g_ !
// [hb] Nein, nein, jetzt muss man das so schreiben:
nonreturning_function(local,my_standard_type_error,(object subrnam));
// ... Nun die Definition!
local void my_standard_type_error (object subrnam)
{
  pushSTACK (STACK_0);			// Nochmal EXPECTED-TYPE
  pushSTACK (STACK_2);			// Nochmal DATUM
  pushSTACK (subrnam);			// Funktionsname
  fehler (type_error, ("~: ~ is not of type ~"));
}

// Willst es nich' kappiern, naja dann kann ich auch anners:
#define BLOEDER_COMPILER_VERDAMMT_NOCHMAL for(;;)
// Bei -O6 scheinst'd ja zu raffen, doch brichst dann gerne mal!
// ... soll ich jetzt auch meinen C compiler selbst schreiben?
// Vielleicht sollte man mal 'nen Compiler in Lisp schreiben, der
// wuerde dann wenigstens funktionieren. [Nicht wirklich, denn dieser
// Compiler wuerde sich nicht selbst compilieren koennen.] Nur noch
// ein Beispiel dessen, dass es nicht gut ist Programme in C zu schreiben,
// die aus 500k Zeilen Code bestehen. BTW mein X leckt auch!
// ... Ich geh gleich hin und hol mein CP/M raus, da leckt nichts.
//

local int isa_instance_of_p (object type, object obj)
     // This function tests whether 'obj' is an instance of the class 'type'
     // or some of its subclasses. 'type' should be a symbol.
     //
{
  // BTW -- Is there a better way the check the type of a clos instance?!
  // We have to do:
  //   (clos::subclassp (clos:class-of obj) (clos:find-class type))
  // where
  //   (defun subclassp (class1 class2)
  //     (gethash class2 (class-all-superclasses class1))) ; T oder (Default) NIL
  //
  // class-all-superclasses is a slot of clos::class defined by a defstruct :-(
  //
  // We should modify the calling function to pass the class in instead of the
  // symbol naming the class, this should speed up the type checking a bit.
  // The class is generated by some lisp code, which should pass it down thru'
  // a magic gate or could we use init_function_1 for that?

  pushSTACK (subr_self);				// save subr_self, since we are doing funcalls here.
  pushSTACK (type);					// save is save
  pushSTACK (obj);					// dito
							// 
  if (instancep (STACK_0))				// only instances could be valid here at all
    {							//
      pushSTACK (STACK_1);				// the type (still a symbol)
      funcall (L(find_class), 1);			// find the class [This should not fail].
      if (eq (value1, TheInstance(STACK_1)->inst_class)) // Catch the trivial case for efficiency
	{						// ==> All fine, just return T
	  skipSTACK (2);				// cleanup
	  subr_self = popSTACK ();			// restore subr_self
	  return 1;					// Jup, it isa instance of desired class
	}						// 
      else						// 
	{						// 
	  pushSTACK (value1);				// the <type> class
	  pushSTACK (`CLOS::CLASS-ALL-SUPERCLASSES`);	// 
	  pushSTACK (TheInstance(STACK_2)->inst_class);	// (class-of obj)
	  funcall (L(funcall), 2);			// (class-all-superclasses ..)
	  pushSTACK (value1);				// 
	  funcall (L(gethash), 2);			// (gethash <type> (c.a.s ..))
	  skipSTACK (2);				// clean up
	  subr_self = popSTACK ();			// restore subr_self
	  if (nullp (value1))				// nothing found => class of object is no subclass of the desired type.
	    return 0;					// 
	  else						// 
	    return 1;					// 
	}						// 
    }							// 
  else							// 'obj' is not an instance at all, hence 0
    {							// 
      skipSTACK (2);					// clean up
      subr_self = popSTACK ();				// restore subr_self
      return 0;						// 
    }							// 
}

local int isa_struct_p (object type, object obj)
     // This function is much like the above one, it checks whether 'obj' isa structure of type 'type'.
{
  if (structurep (obj))				        // Is it a structure at all?
    {
      object q;
      for (q = TheStructure(obj)->recdata[0]; consp (q); q = Cdr(q))
	if (eq (Car(q), type))
	  return 1;
    }
  
  // Fall thru'
  return 0;
}

// generic test for unbound values, NIL will be threaten as unbound.
#define gunboundp(X) (eq (X,unbound) || eq (X,NIL))

// with_string_0, n_char_to_string, string_to_asciz now need an encoding.
// Here are some predefined ones (only defined and only needed with UNICODE).
#define pathname_encoding() (C_pathname_encoding(),value1)
  extern void C_pathname_encoding (void);
#define misc_encoding() (C_misc_encoding(),value1)
  extern void C_misc_encoding (void);

// with_stringable_0 is much like with_string_0, but a symbol is also
// allowed as argument. This macro does type checking and may raise an error.
// Furthermore the code generated is wrapped around a do{}while(0). All macros
// from lispbibl.d do not do that. :-(
//
#define with_stringable_0_tc(obj, encoding, cvar, body)		\
  do{								\
    object wsa0_temp = symbolp(obj)?Symbol_name (obj):obj;	\
    if (stringp (wsa0_temp))					\
      {with_string_0 (wsa0_temp, encoding, cvar, body);}	\
    else							\
      {								\
	pushSTACK (obj);					\
	pushSTACK (`(OR STRING SYMBOL)`);			\
	my_standard_type_error (TheSubr (subr_self)->name);	\
	BLOEDER_COMPILER_VERDAMMT_NOCHMAL;			\
      }								\
  }while(0)


// ----------------------------------------------------------------------------------------------------
//  Integer data types
// ----------------------------------------------------------------------------------------------------
//
// Hugh?! Thes function do not check the type?!
//
#define make_uint8(i)    uint8_to_I (i)
#define make_uint16(i)   uint16_to_I (i)
#define make_uint29(ul)  UL_to_I (ul)
#define make_uint32(ul)  UL_to_I (ul)

#define make_sint8(i)    sint8_to_I (i)
#define make_sint16(i)   sint16_to_I (i)
#define make_sint32(i)   L_to_I (i)

#define get_bool(obj)   (!eq(obj,NIL))
#define make_bool(b)    ((b)?(T):(NIL))

#define pixel_p(obj)	 integerp (obj)
#define get_pixel(obj)   get_uint32(obj)
#define make_pixel(obj)  make_uint32(obj)

#define get_fixnum(obj)  fixnum_to_L (obj) /* WARNING: obj should be a variable (evaluated multiple) */

#if 0
#define get_uint8(obj)   I_to_uint8(obj)
#define get_uint16(obj)  I_to_uint16(obj)
#define get_uint29(obj)  I_to_UL (obj)
#define get_uint32(obj)  I_to_UL (obj)

#define get_sint8(obj)   I_to_sint8(obj)
#define get_sint16(obj)  I_to_sint16(obj)
#define get_sint32(obj)  I_to_sint32(obj)
#else

#define uint29_p uint32_p    // XXX the actual type checking code is just too weired!
#define I_to_uint29 I_to_UL  // XXX dito

#define DEFINE_INTEGER_GETTER(type, lspnam)			\
  type get_##type (object obj)					\
   {								\
     if (type##_p (obj))					\
       return I_to_##type (obj);				\
     else							\
       {							\
	 pushSTACK (obj);					\
	 pushSTACK (lspnam);					\
	 my_standard_type_error (TheSubr(subr_self)->name);	\
	 BLOEDER_COMPILER_VERDAMMT_NOCHMAL;                     \
       }							\
   }

DEFINE_INTEGER_GETTER (uint8,  `XLIB::CARD8`)
DEFINE_INTEGER_GETTER (uint16, `XLIB::CARD16`)
DEFINE_INTEGER_GETTER (uint29, `XLIB::CARD29`)
DEFINE_INTEGER_GETTER (uint32, `XLIB::CARD32`)

DEFINE_INTEGER_GETTER (sint8,  `XLIB::INT8`)
DEFINE_INTEGER_GETTER (sint16, `XLIB::INT16`)
DEFINE_INTEGER_GETTER (sint32, `XLIB::INT32`)
#endif

local uint32 get_aint32 (object obj)
     // This is special routine, which accepts either an uint32 or a sint32.
     // However returned is an uint32.
     // Used by XLIB:CHANGE-PROPERTY
{
  if (uint32_p (obj))
    return I_to_uint32 (obj);
  if (sint32_p (obj))
    return (uint32)I_to_sint32 (obj);
  else
    {
      pushSTACK (obj);
      pushSTACK (`(OR XLIB::INT32 XLIB::CARD32)`);
      my_standard_type_error (TheSubr(subr_self)->name);
      BLOEDER_COMPILER_VERDAMMT_NOCHMAL;
    }
}
     

// ----------------------------------------------------------------------------------------------------
//  Displays
// ----------------------------------------------------------------------------------------------------
//
// Objects of type DISPLAY are currently represented as structure; here are the slots:
// The actual defstruct definition in clx.lsp must match. There is a warning in the code.
//
#define slot_DISPLAY_FOREIGN_POINTER 1
#define slot_DISPLAY_HASH_TABLE      2
#define slot_DISPLAY_PLIST           3
#define slot_DISPLAY_AFTER_FUNCTION  4
#define slot_DISPLAY_ERROR_HANDLER   5
//
// The display contains a hash table. All XID objects are entered there, so that
// two XID objects, with equal XID are actually eq.
//

local object make_display (Display *dpy)
     // Given the C representation of a display create the Lisp one and initialize it.
     // The newly created display is added to XLIB:*DISPLAYS*.
{
  pushSTACK (subr_self);
  
  pushSTACK (allocate_structure (6));
  TheStructure (STACK_0)->structure_types = `(XLIB::DISPLAY)`;
  TheStructure (STACK_0)->recdata[slot_DISPLAY_FOREIGN_POINTER] = allocate_fpointer (dpy);
  pushSTACK (`:TEST`);
  pushSTACK (`EQUAL`);
  funcall (L(make_hash_table), 2);
  TheStructure (STACK_0)->recdata[slot_DISPLAY_HASH_TABLE]     = value1;
  TheStructure (STACK_0)->recdata[slot_DISPLAY_PLIST]          = NIL;
  TheStructure (STACK_0)->recdata[slot_DISPLAY_AFTER_FUNCTION] = NIL;
  TheStructure (STACK_0)->recdata[slot_DISPLAY_ERROR_HANDLER]  = NIL;

  // Now enter the display into the list of all displays:
  pushSTACK (`XLIB::*DISPLAYS*`);
  pushSTACK (STACK_1);
  pushSTACK (`XLIB::*DISPLAYS*`);
  funcall (L(symbol_value), 1);
  pushSTACK (value1);
  funcall (L(cons), 2);
  pushSTACK (value1);
  funcall (L(set), 2);

  subr_self = STACK_1;
  value1 = STACK_0;
  skipSTACK (2);
  
  return value1;
}

local object find_display (Display *display)
     // Searched the XLIB:*DISPLAY* variable for `display', return NIL, if display was not found.
     // Used by the error handler, the only callback code here.
{
  pushSTACK (Symbol_value (`XLIB::*DISPLAYS*`));
  for (;consp (STACK_0); STACK_0 = Cdr (STACK_0))
    {
      pushSTACK (Car (STACK_0));
      if (pop_display () == display)
	  return Car (popSTACK ());
    }
  skipSTACK (1);
  return NIL;
}

local void ensure_living_display (object *objf)
     // ensures that the object pointed to by 'objf' is really a display.
     // Also the display must be 'alive', meaning that it does not contain
     // a fptr from an previous incarnation of CLISP. If all that does not
     // hold an error is signaled.
     //
     // 'objf' should point into the stack due to GC.
{
  if (isa_struct_p (`XLIB::DISPLAY`, (*objf)))   // Is it a display at all?
    {
      object fptr = TheStructure(*objf)->recdata[slot_DISPLAY_FOREIGN_POINTER];
      if (fpointerp (fptr))
	{
	  // Now it is a display and has a foreign pointer in the slot, now see if it is a living one.
	  if (fp_validp (TheFpointer (fptr)))
	    {
	      // Everything o.k. just return
	      return;
	    }
	  else
	    {
	      // Raise an error message [Remember fehler wants the arguments backwards!]
	      pushSTACK (TheSubr(subr_self)->name);
	      fehler (error, "~: Wanted to refer to a dead display.");
	      //
	      // FIXME there is closed-display condition defined in CLX. That
	      // would be the obvious thing to signal!
	      //
	    }
	}
    }

  // Fall thru` -- raise type error.
  pushSTACK (*(objf));      			// Value for slot DATUM of TYPE-ERROR
  pushSTACK (`XLIB::DISPLAY`);			// Value for slot EXPECTED-TYPE of TYPE-ERROR
  my_standard_type_error (TheSubr(subr_self)->name);
}

//
// display_hash_table -- return the hashtable of a display object
//
// > STACK_0 the display object
//
// This function is somewhat silly, since it introduces double type checking!
//
local object display_hash_table (void)
{
  ensure_living_display (&(STACK_0));
  return TheStructure (popSTACK ())->recdata[slot_DISPLAY_HASH_TABLE];
}

//
// pop_display --- return the C Display* of an display object
//
// > STACK_0: the Lisp DISPLAY object
//
local Display *pop_display (void)
{
  ensure_living_display (&(STACK_0));
  STACK_0 = TheStructure (STACK_0)->recdata[slot_DISPLAY_FOREIGN_POINTER];
  return TheFpointer (popSTACK ())->fp_pointer;
}


// ----------------------------------------------------------------------------------------------------
//  PTR and XID objects
// ----------------------------------------------------------------------------------------------------
//
//  First the ptr ones.
//
//  ptr_objs are screens and gcontexts, these objects are not hashed. (which is a bad idea btw).
//  But on the other hand for gcontexts it is not too bad, since you get `em only once.
//
//  An other story are the screen, these could be cached. Or  we do not give the
//  actual screen structure, but pass simply the index?
//
local object make_ptr_obj (object type, object dpy, void *ptr)
{
  pushSTACK (subr_self);
  
  /* (make-instance type :display dpy :ptr ptr) */
  pushSTACK (`CLOS::MAKE-INSTANCE`);
  pushSTACK (type);
  pushSTACK (`:DISPLAY`);  pushSTACK (dpy);
  pushSTACK (`:PTR`);      pushSTACK (allocate_fpointer (ptr));
  funcall (L(funcall), 6);
  subr_self = popSTACK ();

  return value1;
}

local void *get_ptr_object_and_display (object type, object obj, Display **dpyf)
     // 'obj'  is the lisp object, whose C representation is returned.
     // When 'dpyf' is non-0, the display of 'obj' is also returned and it is ensured that
     // it lives. [Otherwise an error is signaled.]
     // If 'obj' is not of type 'type', a symbol naming the desired class, an error is issued.
     //
     // Hence this function ensures, that an proper object is returned, or nothing.
{
  pushSTACK (subr_self);					// save subr_self
  pushSTACK (type);
  pushSTACK (obj);

  if (isa_instance_of_p (STACK_1, STACK_0))
    {
      if (dpyf)			// do we want the display?
	{
	  pushSTACK (STACK_0);
	  pushSTACK (`XLIB::DISPLAY`);
#if !WITH_SLOT_UP
	  funcall (L(slot_value), 2);
#else
	  { object *ptr = slot_up (); skipSTACK(2); value1 = *ptr; }
#endif
	  subr_self = STACK_2;
	  pushSTACK (value1);
	  ensure_living_display (&(STACK_0));
	  *dpyf = TheFpointer (TheStructure (popSTACK())->recdata[slot_DISPLAY_FOREIGN_POINTER])->fp_pointer;
	}
	
      pushSTACK (STACK_0);					// 'obj'
      pushSTACK (`XLIB::PTR`);
#if !WITH_SLOT_UP
      funcall (L(slot_value), 2);
#else
      { object *ptr = slot_up (); skipSTACK (2); value1 = *ptr; }
#endif
      ASSERT (fpointerp (value1));				// FIXME raise hash-error?
	
      if (!fp_validp (TheFpointer(value1)))
	{
	  // Raise an error message.
	  pushSTACK (STACK_1);
	  pushSTACK (TheSubr (STACK_2)->name);
	  fehler (error, "~: Wanted to refer to a dead CLX object of type ~.");
	}

      skipSTACK (2);						// clean up
      subr_self = popSTACK ();					// restore subr_self
      return TheFpointer(value1)->fp_pointer;			// all done
    }
  else
    {
      pushSTACK (STACK_0);					// 'obj' -- Slot DATUM of TYPE-ERROR
      pushSTACK (STACK_2);					// 'type' -- Slot EXPECTED-TYPE of TYPE-ERROR
      my_standard_type_error (TheSubr(STACK_4)->name);		// Give the right subr name on error message.
      BLOEDER_COMPILER_VERDAMMT_NOCHMAL;
    }
}

// Now the XID objects

local object make_xid_obj_low (object *prealloc, object *type, object *dpy, XID xid)
{
  if (nullp (*prealloc))
    {
      pushSTACK (subr_self);
      /* (make-instance type :display dpy :id xid) */
      pushSTACK (`CLOS::MAKE-INSTANCE`);
      pushSTACK (*type);
      pushSTACK (`:DISPLAY`);  pushSTACK (*dpy);
      pushSTACK (`:ID`);       pushSTACK (make_uint29 (xid));
      funcall (L(funcall), 6);
      subr_self = popSTACK ();
      return value1;
    }
  else
    {
      //
      // TODO: We should check the type of the preallocated object?!
      //       [Is is a bug or a feature?]
      //
      pushSTACK (subr_self);
      
      pushSTACK (*prealloc);
      pushSTACK (`XLIB::DISPLAY`);
      pushSTACK (*dpy);
      funcall (L(set_slot_value), 3);

      pushSTACK (*prealloc);
      pushSTACK (`XLIB::ID`);
      pushSTACK (make_uint29 (xid));
      funcall (L(set_slot_value), 3);

      subr_self = popSTACK ();
      
      return *prealloc;
    }
}

defvar xlib_a_cons = (NIL . NIL);

local object make_xid_obj_2 (object type, object dpy, XID xid, object prealloc)
{
  // NOTE: - This code is not reentrant :-( But hence it saves consing
  //       - may be we want to check the most significant 3 bits in xid, since
  //         GC inquiry functions return those values, if slot has an unknown
  //         value.
  //       - We could add even more safty here
  //	     1. bark if lookup succeed and prealloc was specified.
  //            [But this  situation  should  not be able  to   occurr,  since a
  //            prealloc is only given upon creation requests.]  However MIT-CLX
  //            does this  check  and raises a hash-error,   if something is not
  //            o.k.  with the  hash  table. [But only if  you  set a debug flag
  //            somewhere].
  //         2. If lookup succeeds we could also check the type.
  //         3. We should check the type of the preallocated object?!
  //            [Compare to make_ptr_obj]
  //
  
  if (xid == 0) // This is trivial, but is it also right?!
    { value1 = NIL; mv_count = 1; }
  else
    {
      pushSTACK (subr_self);					// save that, so that error messages are given right later.
      pushSTACK (prealloc);					// save is save
      pushSTACK (type);						// dito
      pushSTACK (dpy);						// dito

      Car (xlib_a_cons) = make_uint16 (xid & 0xFFFF); 		// lower halfword
      Cdr (xlib_a_cons) = make_uint16 (xid >> 16);		// upper halfword
      
      // Now go with that cons into the hash-table ...
      pushSTACK (xlib_a_cons);					// the key
      pushSTACK (STACK_1);					// the display object
      pushSTACK (display_hash_table ());			// the table [By this call it is ensured that dpy is a display]
      funcall (L(gethash), 2);					// look it up.
      
      if (!eq(value2, NIL))					// something found?
	{
	  mv_count = 1;						// simply return what we found
	}
      else
	{
	  // Nothing found, so create a new object
	  pushSTACK (make_xid_obj_low (&STACK_2, &STACK_1, &STACK_0, xid));

	  // Now enter this into the hashtable
	  pushSTACK (make_uint16 (xid & 0xFFFF));		// lower halfword
	  pushSTACK (make_uint16 (xid >> 16));			// upper halfword
	  funcall (L(cons), 2);					// cons `em
	  pushSTACK (value1);					// key for puthash
	  pushSTACK (STACK_2);					// 
	  pushSTACK (display_hash_table ());			// table " "
	  pushSTACK (STACK_2);					// value " "
	  funcall (L(puthash), 3);				// put it into the hashtable
	  
	  value1 = STACK_0; mv_count = 1;			// return freshly allocated structure
	  skipSTACK (1);					// clean up
	}

      skipSTACK (3);						// remove saved prealloc, type, dpy
      subr_self = popSTACK ();					// restore subr_self and all done		        
    }
  
  return value1;
}

local XID get_xid_object_and_display (object type, object obj, Display **dpyf)
{
  pushSTACK (subr_self);					// save subr_self
  pushSTACK (type);
  pushSTACK (obj);

  if (isa_instance_of_p (STACK_1, STACK_0))
    {
      if (dpyf)							// do we want the display?
	{
	  pushSTACK (STACK_0);
	  pushSTACK (`XLIB::DISPLAY`);
#if !WITH_SLOT_UP
	  funcall (L(slot_value), 2);
#else
	  { object *ptr = slot_up (); skipSTACK(2); value1 = *ptr; }
#endif
	  subr_self = STACK_2;
	  pushSTACK (value1);
	  ensure_living_display (&(STACK_0));
	  *dpyf = TheFpointer (TheStructure (popSTACK())->recdata[slot_DISPLAY_FOREIGN_POINTER])->fp_pointer;
	}
      
      pushSTACK (STACK_0);					// obj already on stack
      pushSTACK (`XLIB::ID`);
#if !WITH_SLOT_UP
      funcall (L(slot_value), 2);
#else
      { object *ptr = slot_up (); skipSTACK(2); value1 = *ptr; }
#endif
      ASSERT (integerp (value1)); 				// FIXME
      skipSTACK (2);						// clean up
      subr_self = popSTACK ();					// restore subr_self
      return (XID)(get_uint29 (value1)); 			// all done
    }
  else
    {
      pushSTACK (STACK_0);					// 'obj' -- Slot DATUM of TYPE-ERROR
      pushSTACK (STACK_2);					// 'type' -- Slot EXPECTED-TYPE of TYPE-ERROR
      my_standard_type_error (TheSubr(STACK_4)->name);		// Give the right subr name on error message.
      BLOEDER_COMPILER_VERDAMMT_NOCHMAL;
    }
}

local object get_display_obj_tc (object type, object obj)
{
  if (isa_instance_of_p (type, obj))
    {
      pushSTACK (subr_self);
  
      pushSTACK (obj);
      pushSTACK (`XLIB::DISPLAY`);
#if !WITH_SLOT_UP
      funcall (L(slot_value), 2);
#else
      { object *ptr = slot_up (); skipSTACK(2); value1 = *ptr; }
#endif
      subr_self = popSTACK ();
      
      return value1;
    }
  else
    {
      pushSTACK (obj);
      pushSTACK (type);
      my_standard_type_error (TheSubr(subr_self)->name);
      BLOEDER_COMPILER_VERDAMMT_NOCHMAL;
    }
}

local object get_display_obj (object obj)
{
  // XXX type checking [Well on the other hand is it really necessary?]
  //     I want to use the combined function above.
  pushSTACK (subr_self);
  
  pushSTACK (obj);
  pushSTACK (`XLIB::DISPLAY`);
#if !WITH_SLOT_UP
  funcall (L(slot_value), 2);
#else
  { object *ptr = slot_up (); skipSTACK(2); value1 = *ptr; }
#endif
  subr_self = popSTACK ();
  
  return value1;
}


// ----------------------------------------------------------------------------------------------------
//  Specializied getters/makers/predicates
// ----------------------------------------------------------------------------------------------------

// Simple Getters
#define get_xid_object(type,obj) get_xid_object_and_display(type,obj,0)
#define get_ptr_object(type,obj) get_ptr_object_and_display(type,obj,0)

#define get_gcontext(obj) ((GC)      get_ptr_object (`XLIB::GCONTEXT`, obj))
#define get_screen(obj)   ((Screen*) get_ptr_object (`XLIB::SCREEN`,   obj))
#define get_image(obj)    ((XImage*) get_ptr_object (`XLIB::IMAGE`,    obj))
#define get_window(obj)   ((Window)  get_xid_object (`XLIB::WINDOW`,   obj))
#define get_pixmap(obj)   ((Pixmap)  get_xid_object (`XLIB::PIXMAP`,   obj))
#define get_cursor(obj)   ((Cursor)  get_xid_object (`XLIB::CURSOR`,   obj))
#define get_colormap(obj) ((Colormap)get_xid_object (`XLIB::COLORMAP`, obj))
#define get_drawable(obj) ((Drawable)get_xid_object (`XLIB::DRAWABLE`, obj))

// Combined getters
#define get_drawable_and_display(obj, dpyf) ((Drawable)get_xid_object_and_display (`XLIB::DRAWABLE`, obj, dpyf))
#define get_window_and_display(obj, dpyf)   ((Window)  get_xid_object_and_display (`XLIB::WINDOW`,   obj, dpyf))
#define get_pixmap_and_display(obj, dpyf)   ((Pixmap)  get_xid_object_and_display (`XLIB::PIXMAP`,   obj, dpyf))
#define get_cursor_and_display(obj, dpyf)   ((Cursor)  get_xid_object_and_display (`XLIB::CURSOR`,   obj, dpyf))
#define get_colormap_and_display(obj, dpyf) ((Colormap)get_xid_object_and_display (`XLIB::COLORMAP`, obj, dpyf))
#define get_gcontext_and_display(obj,dpyf)  ((GC)      get_ptr_object_and_display (`XLIB::GCONTEXT`, obj, dpyf))
#define get_screen_and_display(obj,dpyf)    ((Screen*) get_ptr_object_and_display (`XLIB::SCREEB`,   obj, dpyf))
#define get_font_and_display(obj, dpyf)     ((Font)    get_xid_object_and_display (`XLIB::FONT`,     obj, dpyf))

// Predicates 
#define drawable_p(obj) (isa_instance_of_p (`XLIB::DRAWABLE`, obj))
#define window_p(obj)   (isa_instance_of_p (`XLIB::WINDOW`, obj))
#define pixmap_p(obj)   (isa_instance_of_p (`XLIB::PIXMAP`, obj))
#define cursor_p(obj)   (isa_instance_of_p (`XLIB::CURSOR`, obj))
#define colormap_p(obj) (isa_instance_of_p (`XLIB::COLORMAP`, obj))
#define font_p(obj)     (isa_instance_of_p (`XLIB::FONT`, obj))
#define gcontext_p(obj) (isa_instance_of_p (`XLIB::GCONTEXT`, obj))
#define screen_p(obj)   (isa_instance_of_p (`XLIB::SCREEN`, obj))
#define display_p(obj)  (isa_struct_p (`XLIB::DISPLAY`, obj))
#define color_p(obj)    (isa_struct_p (`XLIB::COLOR`, obj))

// Simple Makers
#define make_xid_obj(a,b,c) make_xid_obj_2(a,b,c,NIL)

#define make_window(dpy,win)   (make_window_2(dpy,win,NIL))
#define make_pixmap(dpy,pix)   (make_pixmap_2(dpy,pix,NIL))
#define make_drawable(dpy,da)  (make_window (dpy, da))
#define make_cursor(dpy,cur)   (make_xid_obj (`XLIB::CURSOR`, dpy, cur))
#define make_colormap(dpy,cm)  (make_xid_obj (`XLIB::COLORMAP`, dpy, cm))
#define make_gcontext(dpy,gc)  (make_ptr_obj (`XLIB::GCONTEXT`, dpy, gc))
#define make_screen(dpy, srcn) (make_ptr_obj (`XLIB::SCREEN`, dpy, srcn))
#define make_font(dpy,fn)      (make_font_with_info (dpy, fn, NIL, NULL))

// Makers with prealloc
#define make_window_2(dpy, win, prealloc) (make_xid_obj_2 (`XLIB::WINDOW`, dpy, win, prealloc))
#define make_pixmap_2(dpy, pm, prealloc)  (make_xid_obj_2 (`XLIB::PIXMAP`, dpy, pm, prealloc))


local object make_font_with_info (object dpy, Font fn, object name, XFontStruct *info)
{
  // This looks much more like assembler, doesn't it?

  pushSTACK (subr_self);				// save original caller
  pushSTACK (name);					// save the name
  pushSTACK (make_xid_obj (`XLIB::FONT`, dpy, fn)); 	// make the xid-object and save it

  // fetch old FONT-INFO slot
  pushSTACK (STACK_0);					// xid-object
  pushSTACK (`XLIB::FONT-INFO`);			// slot
  funcall (L(slot_value), 2);				// (slot-value new-xid-object `font-info)
  
  // do not overwrite any already fetched font info
  unless (fpointerp (value1))
    {
      // o.k allocate a new fpointer
      pushSTACK (STACK_0);				// the new xid-object
      pushSTACK (`XLIB::FONT-INFO`);			// the slot
      pushSTACK (allocate_fpointer (info));		// new value
      funcall (L(set_slot_value), 3);			// update the :font-info slot
    }
  
  if (!nullp (STACK_1))					// name
    {
      pushSTACK (STACK_0);				// the new xid-object
      pushSTACK (`XLIB::NAME`);				// the :name slot
      pushSTACK (STACK_3);				// [name] new value
      funcall (L(set_slot_value), 3);			// update the :name slot
    }

  value1 = STACK_0;					// return value = new xid-object
  skipSTACK (2);					// clean up
  subr_self = popSTACK ();				// restore subr_self
  return value1;					// all done
}

local Font get_font (object obj);
Values C_xlib_gcontext_font (void);

XFontStruct *get_font_info_and_display (object obj, object* fontf, Display **dpyf)
     // Fetches the font information from a font, if it isn't there
     // already, query the server for it.
     // Further more if a gcontext is passed in, fetch its font slot instead.
     // Does type checking and raises error if unappropriate object passed in.
     // If 'fontf' is non-0, also the font as a Lisp object is returned.
     // If 'dpyf' is non-0, also the display of the font is returned and it is
     // ensured that the display actually lives.
{
  XFontStruct *info;
  Display *dpy;
  Font font;
	  
  if (gcontext_p (obj))
    {
      // In all places where a font object is required, a gcontext should be accepted too,
      // so fetch the font slot and go on ...
      pushSTACK (obj);
      pushSTACK (NIL);
      C_xlib_gcontext_font ();				// quick and dirty subr call ...
      obj = value1;					// Now we have the font [or nothing]
    }
  
  if (font_p (obj))
    {
      pushSTACK (obj);

      pushSTACK (subr_self);
      pushSTACK (STACK_1);
      pushSTACK (`XLIB::FONT-INFO`);
      funcall (L(slot_value), 2);			// (slot-value obj `font-info)
      subr_self = popSTACK ();
      
      if (!(fpointerp (value1) && fp_validp (TheFpointer(value1))))
	{
	  // Raise an error message.
	  pushSTACK (STACK_0);
	  pushSTACK (TheSubr (subr_self)->name);
	  fehler (error, "~: Wanted to refer to a dead font: ~");	    
	}

      info = TheFpointer(value1)->fp_pointer;		// 
      if (!info)
	{
	  // We have no font information already, so go and ask the server for it.

	  pushSTACK (value1); 				// but first save what we found.
	  
	  font = get_font_and_display (STACK_1, &dpy);
	  begin_call ();
	  info = XQueryFont (dpy, font);
	  end_call ();

          if (!info)
            {
              pushSTACK (STACK_1);
              fehler (error, "Nonexistent font: ~");
            }

	  if (dpyf) *dpyf = dpy;
	  
          ASSERT (fpointerp (STACK_0));
	  
	  TheFpointer (STACK_0)->fp_pointer = info;	// Store it in the foreign pointer
	  skipSTACK (1);

#ifdef UNICODE
          {
            // Determine the font's encoding, so we can correctly convert
            // characters to indices.
            // Call (XLIB:FONT-PROPERTY font "CHARSET_REGISTRY")
            // and  (XLIB:FONT-PROPERTY font "CHARSET_ENCODING")
            // and translate the resulting pairs to CLISP encodings.
            Atom xatom;
            unsigned long rgstry;
            unsigned long encdng;
            begin_call();
            xatom = XInternAtom (dpy, "CHARSET_REGISTRY", 0);
            if (XGetFontProperty (info, xatom, &rgstry))
              {
                xatom = XInternAtom (dpy, "CHARSET_ENCODING", 0);
                if (XGetFontProperty (info, xatom, &encdng))
                  {
                    Atom xatoms[2];
                    char* names[2];

                    xatoms[0] = rgstry;
                    xatoms[1] = encdng;
                    names[0] = NULL;
                    names[1] = NULL;
                    if (XGetAtomNames (dpy, xatoms, 2, names))
                      {
                        end_call();
                        pushSTACK (asciz_to_string (names[0], misc_encoding ()));
                        pushSTACK (`"-"`);
                        pushSTACK (asciz_to_string (names[1], misc_encoding ()));
                       {var object charset_name = string_concat(3);
                        // On an XFree86-3.3 system (with Emacs intlfonts installed),
                        // I have seen the following encodings:
                        //   adobe-fontspecific
                        //   big5.eten-0
                        //   cns11643.1992-1
                        //   cns11643.1992-2
                        //   cns11643.1992-3
                        //   cns11643.1992-4
                        //   cns11643.1992-5
                        //   cns11643.1992-6
                        //   cns11643.1992-7
                        //   dec-dectech
                        //   ethiopic-unicode
                        //   gb2312.1980-0
                        //   gb2312.80&gb8565.88-0
                        //   gost19768.74-1
                        //   is13194-devanagari
                        //   iso10646-1
                        //   iso646.1991-irv
                        //   iso8859-1
                        //   iso8859-2
                        //   iso8859-3
                        //   iso8859-4
                        //   iso8859-5
                        //   iso8859-7
                        //   iso8859-8
                        //   iso8859-9
                        //   jisc6226.1978-0
                        //   jisx0201.1976-0
                        //   jisx0208.1983-0
                        //   jisx0208.1990-0
                        //   jisx0212.1990-0
                        //   koi8-1
                        //   koi8-r
                        //   ksc5601.1987-0
                        //   misc-fontspecific
                        //   mulearabic-0
                        //   mulearabic-1
                        //   mulearabic-2
                        //   muleindian-1
                        //   muleindian-2
                        //   muleipa-1
                        //   mulelao-1
                        //   muletibetan-0
                        //   muletibetan-1
                        //   omron_udc_zh-0
                        //   sisheng_cwnn-0
                        //   sunolcursor-1
                        //   sunolglyph-1
                        //   tis620.2529-1
                        //   viscii1.1-1
                        var object encoding = NIL;
                        if (string_equal (charset_name, `"iso8859-1"`))
                          encoding = Symbol_value(`CHARSET:ISO-8859-1`);
                        else if (string_equal (charset_name, `"iso8859-2"`))
                          encoding = Symbol_value(`CHARSET:ISO-8859-2`);
                        else if (string_equal (charset_name, `"iso8859-3"`))
                          encoding = Symbol_value(`CHARSET:ISO-8859-3`);
                        else if (string_equal (charset_name, `"iso8859-4"`))
                          encoding = Symbol_value(`CHARSET:ISO-8859-4`);
                        else if (string_equal (charset_name, `"iso8859-5"`))
                          encoding = Symbol_value(`CHARSET:ISO-8859-5`);
                        else if (string_equal (charset_name, `"iso8859-6"`))
                          encoding = Symbol_value(`CHARSET:ISO-8859-6`);
                        else if (string_equal (charset_name, `"iso8859-7"`))
                          encoding = Symbol_value(`CHARSET:ISO-8859-7`);
                        else if (string_equal (charset_name, `"iso8859-8"`))
                          encoding = Symbol_value(`CHARSET:ISO-8859-8`);
                        else if (string_equal (charset_name, `"iso8859-9"`))
                          encoding = Symbol_value(`CHARSET:ISO-8859-9`);
                        else if (string_equal (charset_name, `"iso8859-14"`))
                          encoding = Symbol_value(`CHARSET:ISO-8859-14`);
                        else if (string_equal (charset_name, `"iso8859-15"`))
                          encoding = Symbol_value(`CHARSET:ISO-8859-15`);
                        else if (string_equal (charset_name, `"koi8-r"`))
                          encoding = Symbol_value(`CHARSET:KOI8-R`);
                        //#if defined(HAVE_ICONV) && defined(UNIX_LINUX)
                        //else if (string_equal (charset_name, `"gb2312.1980-0"`))
                        //  encoding = `"GB"`;
                        //#endif
                        if (!nullp(encoding))
                          {
                            pushSTACK (`:CHARSET`); pushSTACK(encoding);
                            pushSTACK (`:OUTPUT-ERROR-ACTION`); pushSTACK(fixnum(info->default_char));
                            funcall (`MAKE-ENCODING`, 4);
                            pushSTACK (STACK_0); // obj
                            pushSTACK (`XLIB::ENCODING`);
                            pushSTACK (value1);
                            funcall (L(set_slot_value), 3);
                          }
                        begin_call ();
                      }}
                    if (names[0])
                      XFree (names[0]);
                    if (names[1])
                      XFree (names[1]);
                  }
              }
            end_call ();
          }
#endif

	}
      else
	{
	  if (dpyf)
	    unused get_font_and_display (STACK_0, dpyf); // caller wants the display, so get it!
	}

      if (fontf)
        *fontf = STACK_0;

      skipSTACK (1);
      return info;					// all done
    }
  else
    {
      // raise error
      pushSTACK (obj); 					// datum
      pushSTACK (`XLIB::FONT`);				// expected type
      my_standard_type_error (TheSubr(subr_self)->name);//
      BLOEDER_COMPILER_VERDAMMT_NOCHMAL;
    }
}

object get_font_name (object obj)
{
  pushSTACK (subr_self);
  pushSTACK (obj);			// the instance
  pushSTACK (`XLIB::NAME`);		// slot
  funcall (L(slot_value), 2);		// lookup the slot
  subr_self = popSTACK ();
  return value1;
}

#define ENSURE_TYPE(datum,booli,type)\
    do{ unless (booli) { pushSTACK (datum); pushSTACK (type); my_standard_type_error (TheSubr(subr_self)->name); }}while(0)

local Font get_font (object self)
     // Does type-checking.
{
  object *fidf, *namef;

  pushSTACK (self);

  ENSURE_TYPE (STACK_0, font_p (STACK_0), `XLIB::FONT`);

  // pushSTACK (self); --- self is already on the stack 
  pushSTACK (`XLIB::ID`);
  fidf = slot_up ();

  // XXX fidf may not be valid as long as we need it, since
  // XXX  the object itself may have been moved by gc!
  
  if (eq (*fidf, unbound))
    {
      // No font id there so, look if we have a name, so that we could open that font? 
      STACK_0 = `XLIB::NAME`;        // We want the :name slot
      namef = slot_up ();	     // look for it
      if (!eq (*namef,unbound))
	{
	  Font font; Display *dpy; 
	  // Ok there is a name ... so try to open the font
	  dpy  = (pushSTACK (STACK_1),pop_display ());
	  // XXX
	  with_string_0 (*namef, misc_encoding (), name,
			 {
			   begin_call ();
			   font = XLoadFont (dpy, name);
			   end_call ();
			 });
	  if (font)
	    {
	      // Hurra! We got a font id, so enter it
	      *fidf = make_uint29 (font);
	      // XXX -- We should enter it also into the hash table!
	      skipSTACK (2);	// clean up
	      return font;	// all done
	    }
	  else
	    {
	      // We could not open the font, so emit an error message
	      skipSTACK (1); 				// raise self to TOS
	      pushSTACK (TheSubr(subr_self)->name);	// function name
	      fehler (error, "~: Cannot not open pseudo font ~");
	    }
	}
      else
	{
	  // We have no name, tell that the luser.
	  skipSTACK (1); 			// raise self to TOS
	  pushSTACK (TheSubr(subr_self)->name);	// function name
	  fehler (error, "~: Cannot not open pseudo font ~, since it has no name associated with it.");
	}
    }
  else
    {
      // We have already a fid, so return it.
      skipSTACK (2);  // clean up
      ASSERT (integerp (*fidf));
      return (XID)(get_uint29 (*fidf));
    }
}

local Atom get_xatom_general (Display *dpy, object obj, int internp)
     // Converts a symbol or a string to an xatom. If 'obj' is no symbol nor a string,
     // an error is raised.
     // if 'internp' is non-0 the atom is interned on the server.
{
  Atom xatom;

  with_stringable_0_tc (obj, misc_encoding (), atom_name,
    {
      begin_call ();	
        xatom = XInternAtom (dpy, atom_name, !internp);
      end_call ();
    });
  
  return xatom;
}

#define get_xatom(dpy,obj)          get_xatom_general (dpy, obj, 1) // interning version
#define get_xatom_nointern(dpy,obj) get_xatom_general (dpy, obj, 0) // non-interning version

local object make_visual (Visual *visual)
{
  XID id;
  begin_call ();
    id = XVisualIDFromVisual (visual);
  end_call ();
  
  return make_uint29 (id);
}

local Visual *get_visual (Display *dpy, object vid)
{
  return XVisualIDToVisual (dpy, get_uint29 (vid));   // no begin/end_call here XVisualIDToVisual is defined by us.
}


// ----------------------------------------------------------------------------------------------------
//  Lots of enums
// ----------------------------------------------------------------------------------------------------

//
// get_enum -- convert a lisp symbol to an integer value i E [0..n).
// 
//  > n: number of elements composing the enum
//  > STACK_n: the object
//  > STACK_n-1 .. STACK_0: the enum keys
//  < result: integer value of enum
//
//  If the object is not of that enum type, a type error is raised.
//
local int get_enum (int n)
{
  // NOTE: This function is mainly used for enums like backingstore etc.
  //       I consider it safe to assume, that i.e. :WHEN-MAPPED == 1, since
  //       these enums are defined in the X Protocol Specification.
  //
  int i;
  
  for (i = 0; i < n; i++)
    if (eq (STACK_(n-i-1), STACK_(n)))
      {
	skipSTACK (n+1);
	return i;
      }

  /* Fall thru` -- raise type error */
  pushSTACK (`MEMBER`);
  pushSTACK (TheSubr(subr_self)->name); 	// incorparate also the subr name -- We have to save it!
  funcall (L(list), n+2);
  pushSTACK (value1);
  funcall (L(nreverse), 1);

  pushSTACK (STACK_0);				// Wert für Slot DATUM von TYPE-ERROR
  pushSTACK (Cdr(value1));			// Wert für Slot EXPECTED-TYPE von TYPE-ERROR
  my_standard_type_error (Car(value1)); 	// Give the right subr name
  BLOEDER_COMPILER_VERDAMMT_NOCHMAL;
}

nonreturning_function(local, enum_error, (char *name, int value, int count));
local void enum_error (char *name, int value, int count)
     // This function should never been called!
{
  funcall (L(list), count);
  pushSTACK (value1);
  pushSTACK (asciz_to_string (name, misc_encoding ()));
  pushSTACK (fixnum (count));
  pushSTACK (fixnum (value));
  fehler (error, "Ouch! ~ is not in [0;~], is it?\n"
	         "Either my enum definition for ~ is wrong, or your X is strange.\n"
                 "List of keywords: ~");
}

#define DEF_ENUM_MAKER(name,count,kws)			\
local object make_##name(int i)				\
{							\
  kws;							\
  unless (i >= 0 && i < count) enum_error (#name, i, count);\
  skipSTACK (count);					\
  return STACK_(-(i+1));				\
}

#define DEF_ENUM_GETTER(name,count,kws)\
local int get_##name(object o)\
{\
   pushSTACK (o);\
   kws;\
   return get_enum (count);\
}

#define DEF_ENUM(a,b,c) DEF_ENUM_MAKER(a,b,c) DEF_ENUM_GETTER(a,b,c)
#define ee(X) pushSTACK(X)

DEF_ENUM_MAKER (map_state,      3, (ee(`:UNMAPPED`), ee(`:UNVIEWABLE`), ee(`:VIEWABLE`)))
DEF_ENUM_GETTER (shape,         3, (ee(`:COMPLEX`), ee(`:CONVEX`), ee(`:NON-CONVEX`)))
DEF_ENUM (W_class,              3, (ee(`:COPY`), ee(`:INPUT-OUTPUT`), ee(`:INPUT-ONLY`)))
DEF_ENUM (stack_mode,           5, (ee(`:ABOVE`), ee(`:BELOW`), ee(`:TOP-IF`), ee(`:BOTTOM-IF`), ee(`:OPPOSITE`)))
DEF_ENUM (arc_mode,             2, (ee(`:CHORD`), ee(`:PIE-SLICE`)))
DEF_ENUM (line_style,           3, (ee(`:SOLID`), ee(`:DASH`), ee(`:DOUBLE-DASH`)))
DEF_ENUM (cap_style,            4, (ee(`:NOT-LAST`), ee(`:BUTT`), ee(`:ROUND`), ee(`:PROJECTING`)))
DEF_ENUM (join_style,           3, (ee(`:MITER`), ee(`:ROUND`), ee(`:BEVEL`)))
DEF_ENUM (fill_style,           4, (ee(`:SOLID`), ee(`:TILED`), ee(`:STIPPLED`), ee(`:OPAQUE-STIPPLED`)))
DEF_ENUM (fill_rule,            2, (ee(`:EVEN-ODD`), ee(`:WINDING`)))
DEF_ENUM (subwindow_mode,       2, (ee(`:CLIP-BY-CHILDREN`), ee(`:INCLUDE-INFERIORS`)))
DEF_ENUM (gravity,             11, (ee(`:FORGET`), ee(`:NORTH-WEST`), ee(`:NORTH`), ee(`:NORTH-EAST`),
			            ee(`:WEST`), ee(`:CENTER`), ee(`:EAST`),
			            ee(`:SOUTH-WEST`), ee(`:SOUTH`), ee(`:SOUTH-EAST`), ee(`:STATIC`)))
                               // NIM: the :static gravity is not mentioned in the CLX manual.
DEF_ENUM (visibility_state,     3, (ee(`:UNOBSCURED`),ee(`:PARTLY-OBSCURED`),ee(`:FULLY-OBSCURED`)) )
DEF_ENUM (top_or_bottom,        2, (ee(`:TOP`),ee(`:BOTTOM`)))
DEF_ENUM (new_value_or_deleted, 2, (ee(`:NEW-VALUE`),ee(`:DELETED`)))
DEF_ENUM (ordering,             4, (ee(`:UNSORTED`),ee(`:Y-SORTED`),ee(`:YX-SORTED`),ee(`:YX-BANDED`)))
DEF_ENUM (mapping_request,      3, (ee(`:MODIFIER`), ee(`:KEYBOARD`), ee(`:POINTER`)))
DEF_ENUM (crossing_mode,        4, (ee(`:NORMAL`), ee(`:GRAB`), ee(`:UNGRAB`), ee(`:WHILE-GRABBED`)))
                               // NIM: :while-grabbed
DEF_ENUM (crossing_kind,        8, (ee(`:ANCESTOR`), ee(`:VIRTUAL`), ee(`:INFERIOR`),
				    ee(`:NONLINEAR`), ee(`:NONLINEAR-VIRTUAL`),
			            ee(`:POINTER`), ee(`:POINTER-ROOT`), ee(`:NONE`)))
                               // NIM: :pointer, :pointer-root, :none
DEF_ENUM (focus_mode,           4, (ee(`:NORMAL`), ee(`:GRAB`), ee(`:UNGRAB`), ee(`:WHILE-GRABBED`)))
                               // This seems to be the same as crossing_mode, but added the :while-grabbed in CLXM
                               // Have to justify that by looking into the source.
                               // I was complaining 'Strange -- the CLX manual says also somthing about :while-grabbed!'
                               // Maybe libX and CLX differ here?
                               //
     
DEF_ENUM (focus_detail,   8, (ee(`:ANCESTOR`), ee(`:VIRTUAL`), ee(`:INFERIOR`), ee(`:NONLINEAR`), ee(`:NONLINEAR-VIRTUAL`),
			      ee(`:POINTER`), ee(`:POINTER-ROOT`), ee(`:NONE`)))
                               // This seems also to be the same as crossing_kind!
     
DEF_ENUM_MAKER (V_class,   6, (ee(`:STATIC-GRAY`),  ee(`:GRAY-SCALE`),
		               ee(`:STATIC-COLOR`), ee(`:PSEUDO-COLOR`),
		               ee(`:TRUE-COLOR`),   ee(`:DIRECT-COLOR`) ))

DEF_ENUM (backing_store,   3, (ee(`:NOT-USEFUL`), ee(`:WHEN-MAPPED`), ee(`:ALWAYS`)))
DEF_ENUM (switch,          2, (ee(`:OFF`), ee(`:ON`)))
DEF_ENUM (close_down_mode, 3, (ee(`:DESTROY`), ee(`:RETAIN-PERMANENT`), ee(`:RETAIN-TEMPORARY`)));
DEF_ENUM (draw_direction,  2, (ee(`:LEFT-TO-RIGHT`), ee(`:RIGHT-TO-LEFT`)))

local Bool get_generic_switch (object o)
{
  if (eq (o, `:NO`) || eq (o, `:OFF`) || nullp (o))
    return 0;
  else
    return 1;
}
#define make_generic_switch make_bool

#define BOOLEEQ(obj,bo) (eq (obj,bo) || eq (obj, Symbol_value (bo)))

local object make_gc_function (int i)
{
  // This Symbol_value thing here is somewhat silly
  switch (i)
    {
    case GXclear: 	 return Symbol_value (`BOOLE-CLR`);   /* 0 */
    case GXand: 	 return Symbol_value (`BOOLE-AND`);   /* src AND dst */
    case GXandReverse: 	 return Symbol_value (`BOOLE-ANDC2`); /* src AND NOT dst */
    case GXcopy: 	 return Symbol_value (`BOOLE-1`);     /* src */
    case GXandInverted:  return Symbol_value (`BOOLE-ANDC1`); /* (NOT src) AND dst */
    case GXnoop: 	 return Symbol_value (`BOOLE-2`);     /* dst */
    case GXxor: 	 return Symbol_value (`BOOLE-XOR`);   /* src XOR dst */
    case GXor: 		 return Symbol_value (`BOOLE-IOR`);   /* src OR dst */
    case GXnor: 	 return Symbol_value (`BOOLE-NOR`);   /* (NOT src) AND (NOT dst) */
    case GXequiv: 	 return Symbol_value (`BOOLE-EQV`);   /* (NOT src) XOR dst */
    case GXinvert: 	 return Symbol_value (`BOOLE-C2`);    /* NOT dst */
    case GXorReverse: 	 return Symbol_value (`BOOLE-ORC2`);  /* src OR (NOT dst) */
    case GXcopyInverted: return Symbol_value (`BOOLE-C1`);    /* NOT src */
    case GXorInverted: 	 return Symbol_value (`BOOLE-ORC1`);  /* (NOT src) OR dst */
    case GXnand: 	 return Symbol_value (`BOOLE-NAND`);  /* (NOT src) OR (NOT dst) */
    case GXset: 	 return Symbol_value (`BOOLE-SET`);   /* 1 */
    default:
      enum_error ("gc_function", i, 0);	// give a wrong error message!
      BLOEDER_COMPILER_VERDAMMT_NOCHMAL;
    }
}

local int get_gc_function (object obj)
{
  // I hope this translations are right -- could somebody please verify?!
  if (BOOLEEQ (obj, `BOOLE-CLR`))   return GXclear;          /* 0 */
  if (BOOLEEQ (obj, `BOOLE-AND`))   return GXand; 	    /* src AND dst */
  if (BOOLEEQ (obj, `BOOLE-ANDC2`)) return GXandReverse;     /* src AND NOT dst */
  if (BOOLEEQ (obj, `BOOLE-1`))     return GXcopy; 	    /* src */
  if (BOOLEEQ (obj, `BOOLE-ANDC1`)) return GXandInverted;    /* (NOT src) AND dst */
  if (BOOLEEQ (obj, `BOOLE-2`))     return GXnoop; 	    /* dst */
  if (BOOLEEQ (obj, `BOOLE-XOR`))   return GXxor; 	    /* src XOR dst */
  if (BOOLEEQ (obj, `BOOLE-IOR`))   return GXor;	            /* src OR dst */
  if (BOOLEEQ (obj, `BOOLE-NOR`))   return GXnor; 	    /* (NOT src) AND (NOT dst) */
  if (BOOLEEQ (obj, `BOOLE-EQV`))   return GXequiv; 	    /* (NOT src) XOR dst */
  if (BOOLEEQ (obj, `BOOLE-C2`))    return GXinvert; 	    /* NOT dst */
  if (BOOLEEQ (obj, `BOOLE-ORC2`))  return GXorReverse;      /* src OR (NOT dst) */
  if (BOOLEEQ (obj, `BOOLE-C1`))    return GXcopyInverted;   /* NOT src */
  if (BOOLEEQ (obj, `BOOLE-ORC1`))  return GXorInverted;     /* (NOT src) OR dst */
  if (BOOLEEQ (obj, `BOOLE-NAND`))  return GXnand; 	    /* (NOT src) OR (NOT dst) */
  if (BOOLEEQ (obj, `BOOLE-SET`))   return GXset; 	    /* 1 */

  pushSTACK (obj);
  pushSTACK (`XLIB::GC-FUNCTION`);
  my_standard_type_error (TheSubr (subr_self)->name); 
  BLOEDER_COMPILER_VERDAMMT_NOCHMAL;

  // Garnet seem to run in that ...
  // ... not any longer
  // return GXcopy;
}

local unsigned long get_gcontext_key (object obj)
{
  if (eq(obj,`:ARC-MODE`)) 		return GCArcMode;
  if (eq(obj,`:BACKGROUND`)) 		return GCBackground;
  if (eq(obj,`:CAP-STYLE`)) 		return GCCapStyle;
  if (eq(obj,`:CLIP-MASK`)) 		return GCClipMask;
  if (eq(obj,`:CLIP-X`)) 		return GCClipXOrigin;
  if (eq(obj,`:CLIP-Y`)) 		return GCClipYOrigin;
  if (eq(obj,`:DASH-OFFSET`)) 		return GCDashOffset;
  if (eq(obj,`:DASHES`)) 		return GCDashList;
  if (eq(obj,`:EXPOSURES`)) 		return GCGraphicsExposures;
  if (eq(obj,`:FILL-RULE`)) 		return GCFillRule;
  if (eq(obj,`:FILL-STYLE`)) 		return GCFillStyle;
  if (eq(obj,`:FONT`)) 			return GCFont;
  if (eq(obj,`:FOREGROUND`)) 		return GCForeground;
  if (eq(obj,`:FUNCTION`)) 		return GCFunction;
  if (eq(obj,`:JOIN-STYLE`)) 		return GCJoinStyle;
  if (eq(obj,`:LINE-STYLE`)) 		return GCLineStyle;
  if (eq(obj,`:LINE-WIDTH`)) 		return GCLineWidth;
  if (eq(obj,`:PLANE-MASK`)) 		return GCPlaneMask;
  if (eq(obj,`:STIPPLE`)) 		return GCStipple;
  if (eq(obj,`:SUBWINDOW-MODE`)) 	return GCSubwindowMode;
  if (eq(obj,`:TILE`)) 			return GCTile;
  if (eq(obj,`:TS-X`)) 			return GCTileStipXOrigin;
  if (eq(obj,`:TS-Y`)) 			return GCTileStipXOrigin;
  NOTIMPLEMENTED;
}


// ----------------------------------------------------------------------------------------------------
//  Masks
// ----------------------------------------------------------------------------------------------------

local unsigned int get_modifier_mask (object obj)
{
  unsigned int mask = 0;
  
  if (eq (obj, `:ANY`)) return AnyModifier;
  if (integerp (obj)) return get_uint16 (obj);
  if (consp (obj))
    {
      pushSTACK (obj);
      while (consp (STACK_0))
	{
	  pushSTACK (Car (obj));
	  pushSTACK (`:SHIFT`);
	  pushSTACK (`:LOCK`);
	  pushSTACK (`:CONTROL`);
	  pushSTACK (`:MOD-1`);
	  pushSTACK (`:MOD-2`);
	  pushSTACK (`:MOD-3`);
	  pushSTACK (`:MOD-4`);
	  pushSTACK (`:MOD-5`);
	  // Should we include here :BUTTON-[12345] ? (Compare to statemask)
	  mask|= 1 << get_enum (8);
	  STACK_0 = Cdr (STACK_0);
	}
      skipSTACK (1);
      return mask;
    }

 error:
  // fall thru' -- raise type error
  pushSTACK (obj);
  pushSTACK (`(OR (MEMBER :ANY) LIST XLIB::MASK16)`);
  // FIXME We chould be more verbose here.
  my_standard_type_error (TheSubr(subr_self)->name); // wrong!
  BLOEDER_COMPILER_VERDAMMT_NOCHMAL;
}


local unsigned long get_event_mask (object obj)
     /* get_event_mask could handle a numerical and symbolic representation of an event mask */
{
  if (uint32_p (obj))
    {
      return get_uint32 (obj);
    }
  else
    if (listp (obj))
      {
	/* We have a list of keys */
	unsigned long mask = 0;
	for (; consp (obj); obj = Cdr (obj))
	  {
	    /* I know this is brute force, but ... */
	         if (eq(Car (obj), `:KEY-PRESS`))		mask |= (1L<<0);
	    else if (eq(Car (obj), `:KEY-RELEASE`))		mask |= (1L<<1);
	    else if (eq(Car (obj), `:BUTTON-PRESS`))		mask |= (1L<<2);
	    else if (eq(Car (obj), `:BUTTON-RELEASE`))		mask |= (1L<<3);
	    else if (eq(Car (obj), `:ENTER-WINDOW`))		mask |= (1L<<4);
	    else if (eq(Car (obj), `:LEAVE-WINDOW`))		mask |= (1L<<5);
	    else if (eq(Car (obj), `:POINTER-MOTION`))		mask |= (1L<<6);
	    else if (eq(Car (obj), `:POINTER-MOTION-HINT`))	mask |= (1L<<7);
	    else if (eq(Car (obj), `:BUTTON-1-MOTION`)) 	mask |= (1L<<8);
	    else if (eq(Car (obj), `:BUTTON-2-MOTION`)) 	mask |= (1L<<9);
	    else if (eq(Car (obj), `:BUTTON-3-MOTION`)) 	mask |= (1L<<10);
	    else if (eq(Car (obj), `:BUTTON-4-MOTION`)) 	mask |= (1L<<11);
	    else if (eq(Car (obj), `:BUTTON-5-MOTION`)) 	mask |= (1L<<12);
	    else if (eq(Car (obj), `:BUTTON-MOTION`))		mask |= (1L<<13);
	    else if (eq(Car (obj), `:KEYMAP-STATE`)) 		mask |= (1L<<14);
	    else if (eq(Car (obj), `:EXPOSURE`)) 		mask |= (1L<<15);
	    else if (eq(Car (obj), `:VISIBILITY-CHANGE`))	mask |= (1L<<16);
	    else if (eq(Car (obj), `:STRUCTURE-NOTIFY`)) 	mask |= (1L<<17);
	    else if (eq(Car (obj), `:RESIZE-REDIRECT`))		mask |= (1L<<18);
	    else if (eq(Car (obj), `:SUBSTRUCTURE-NOTIFY`))	mask |= (1L<<19);
	    else if (eq(Car (obj), `:SUBSTRUCTURE-REDIRECT`))	mask |= (1L<<20);
	    else if (eq(Car (obj), `:FOCUS-CHANGE`))		mask |= (1L<<21);
	    else if (eq(Car (obj), `:PROPERTY-CHANGE`)) 	mask |= (1L<<22);
	    else if (eq(Car (obj), `:COLORMAP-CHANGE`)) 	mask |= (1L<<23);
	    else if (eq(Car (obj), `:OWNER-GRAB-BUTTON`))	mask |= (1L<<24);
	    else goto raise_type_error;
	  }
	if (!eq(obj, NIL))
	  goto raise_type_error;
	
	return mask;
      }
    else
      goto raise_type_error;

 raise_type_error:
    NOTIMPLEMENTED;
  ;
}

local object make_event_mask (unsigned long mask)
{
  return make_uint32 (mask);
}


// ----------------------------------------------------------------------------------------------------
//  Various other types
// ----------------------------------------------------------------------------------------------------

local object make_xatom (Display *dpy, Atom atom)
{
  char *atom_name;
  begin_call ();
  atom_name = XGetAtomName (dpy, atom);
  end_call ();

  pushSTACK (subr_self);
  pushSTACK (asciz_to_string (atom_name, misc_encoding ()));
  pushSTACK (`KEYWORD`);

  begin_call ();
  XFree (atom_name);
  end_call ();

  funcall (L(intern), 2);
  subr_self = popSTACK ();
  return value1;
}

local Time get_timestamp (object obj)
{
  return gunboundp (obj) ? CurrentTime : get_uint32 (obj);
}

local sint32 get_angle (object ang)
  /* translates the CLX angle representation in radian to X represent in sixty-fourth of degree */
{
  sint16 xang;
  pushSTACK (subr_self);
  
  /* calcuate (round (* (/ ang pi) (* 180 64))) */
  pushSTACK (ang);
  pushSTACK (O(FF_pi));
  funcall (L(durch), 2);
  pushSTACK (value1);
  pushSTACK (fixnum (180*64));
  funcall (L(mal), 2);
  pushSTACK (value1);
  funcall (L(round), 1);
  xang = get_sint32 (value1);
  subr_self = popSTACK ();
  return xang;
}

local object make_key_vector (char key_vector[32])
{
  value1= allocate_bit_vector (256);
  memcpy (TheSbvector(value1)->data, key_vector, 32); // may be wrong, may be right?!
  return value1;
}

local void get_key_vector (object obj, char key_vector [32])
{
  NOTIMPLEMENTED;
}

object make_visual_info (Visual *vis)
{
  pushSTACK (allocate_structure (8));
  TheStructure (STACK_0)->structure_types = `(XLIB::VISUAL-INFO)`;
  TheStructure (STACK_0)->recdata[1] = make_uint29 (vis->visualid); 		// id
  TheStructure (STACK_0)->recdata[2] = make_V_class (vis->class);		// class
  TheStructure (STACK_0)->recdata[3] = make_pixel (vis->red_mask);		// red-mask
  TheStructure (STACK_0)->recdata[4] = make_pixel (vis->green_mask);		// green-mask
  TheStructure (STACK_0)->recdata[5] = make_pixel (vis->blue_mask);		// blue-mask
  TheStructure (STACK_0)->recdata[6] = make_uint8 (vis->bits_per_rgb);		// bits-per-rgb
  TheStructure (STACK_0)->recdata[7] = make_uint16 (vis->map_entries);		// colormap-entries
  return popSTACK ();
}

local object make_rgb_val (unsigned short value)
{
  // calculate (/ value 65535.0)
  // FIXME -- should find more clever way to do this ...

  pushSTACK (subr_self);
  pushSTACK (fixnum (value));
  pushSTACK (fixnum (65535));
  funcall (L(durch), 2);
  subr_self = popSTACK();
  return value1;
}

local unsigned short get_rgb_val (object value)
{
  // calculate (round (* value 65535))
  // FIXME -- should really find more clever way to do this ...
  //       -- maybe we check the actual type here?!

  pushSTACK (subr_self);
  pushSTACK (value);
  pushSTACK (fixnum (0xFFFF));
  funcall (L(mal), 2);
  pushSTACK (value1);
  funcall (L(round), 1);
  subr_self = popSTACK();
  return get_uint16 (value1);
}

local void get_color (Display *dpy, object color, XColor *result)
{
  pushSTACK (color);
  ENSURE_TYPE (STACK_0, color_p (STACK_0), `XLIB::COLOR`);
  
  result->pixel = 0;
  result->flags = -1; // Well set all flags .. just in case;
                      // in the .h files is something like do_{red,green,blue} ?!
  result->red   = get_rgb_val (TheStructure (STACK_0)->recdata[1]);
  result->green = get_rgb_val (TheStructure (STACK_0)->recdata[2]);
  result->blue  = get_rgb_val (TheStructure (STACK_0)->recdata[3]);
  skipSTACK (1);
}

local object make_color (XColor *color)
{
  pushSTACK (allocate_structure (4));
  TheStructure (STACK_0)->structure_types = `(XLIB::COLOR)`;
  TheStructure (STACK_0)->recdata[1] = make_rgb_val (color->red);		// red
  TheStructure (STACK_0)->recdata[2] = make_rgb_val (color->green);		// green
  TheStructure (STACK_0)->recdata[3] = make_rgb_val (color->blue);		// blue
  return popSTACK ();
}

//
// general_plist_reader (type) -- used by the various xxx-plist functions
// > type the type the argument should have
// > STACK_0 the object in question
//
local void general_plist_reader (object type)
{
  // the XLIB object in question is already on the stack
  if (isa_instance_of_p (type, STACK_0))
    {
      pushSTACK (`XLIB::PLIST`);
      funcall (L(slot_value), 2);
    }
  else
    {
      pushSTACK (type);
      my_standard_type_error (TheSubr(subr_self)->name);
      BLOEDER_COMPILER_VERDAMMT_NOCHMAL;
    }
}

//
// general_plist_writer (type) -- used by the various xxx-plist functions
//  > type the type the argument should have
//  > STACK_0 the object in question
//  > STACK_1 the new value for plist
//
local void general_plist_writer (object type)
{
  // the XLIB object and the new value are already on the stack
  if (isa_instance_of_p (type, STACK_0))
    {
      pushSTACK (`XLIB::PLIST`);			// the slot
      pushSTACK (STACK_2);				// new value
      funcall (L(set_slot_value), 3);
      skipSTACK (1);
    }
  else
    {
      pushSTACK (type);
      my_standard_type_error (TheSubr(subr_self)->name);
      BLOEDER_COMPILER_VERDAMMT_NOCHMAL;
    }
}

local void general_lookup (object type)
{
  XID xid = get_uint29 (STACK_0);
  ensure_living_display (&(STACK_1));
  value1 = make_xid_obj_2 (type, STACK_1, xid, NIL); mv_count = 1;
  skipSTACK (2);
}

//
// Defines xxx-{DISPLAY,PLIST,PLIST-SETTER,P,EQUAL,ID} and LOOKUP-xxx for xid objects
//
##define STANDARD_XID_OBJECT_LOOK(type)							\
  defun XLIB:##type##-DISPLAY (1)							\
    { value1 = get_display_obj_tc (`XLIB::##type`, popSTACK ()); mv_count = 1; }	\
  defun XLIB:##type##-PLIST (1)								\
    { general_plist_reader (`XLIB::##type##`); }					\
  defun XLIB:##type##-PLIST-SETTER (2)							\
    { general_plist_writer (`XLIB::##type##`); }					\
  defun XLIB:##type##-P (1)								\
    { value1 = make_bool (type##_p (popSTACK ())); mv_count = 1; }			\
  defun XLIB:##type##-ID (1)								\
    { value1 = make_uint29 ((XID)get_##type (popSTACK ())); mv_count = 1; }		\
  defun XLIB:##type##-EQUAL (2)								\
    { value1 = make_bool (get_##type (popSTACK ()) == get_##type (popSTACK ())); }      \
  defun XLIB:LOOKUP-##type (2)								\
    { general_lookup (`XLIB::##type##`); }

//
// Defines xxx-{DISPLAY,PLIST,PLIST-SETTER,P,EQUAL}
// However xxx-ID and LOOKUP-xxx are not defined, since the way to get the xid and
// looking it up differs between ptr objects
//
##define STANDARD_PTR_OBJECT_LOOK(type)							\
  defun XLIB:##type##-DISPLAY (1)							\
    { value1 = get_display_obj_tc (`XLIB::##type`, popSTACK ()); mv_count = 1; }	\
  defun XLIB:##type##-PLIST (1)								\
    { general_plist_reader (`XLIB::##type##`); }					\
  defun XLIB:##type##-PLIST-SETTER (2)							\
    { general_plist_writer (`XLIB::##type##`); }					\
  defun XLIB:##type##-P (1)								\
    { value1 = make_bool (type##_p (popSTACK ())); mv_count = 1; }			\
  defun XLIB:##type##-EQUAL (2)								\
    { value1 = make_bool (get_##type (popSTACK ()) == get_##type (popSTACK ())); }      


// ----------------------------------------------------------------------------------------------------
//  Chapter 1   Data Types
// ----------------------------------------------------------------------------------------------------

STANDARD_XID_OBJECT_LOOK (window)
STANDARD_XID_OBJECT_LOOK (pixmap)
STANDARD_XID_OBJECT_LOOK (drawable)
STANDARD_XID_OBJECT_LOOK (font)
STANDARD_XID_OBJECT_LOOK (colormap)
STANDARD_XID_OBJECT_LOOK (cursor)

STANDARD_PTR_OBJECT_LOOK (gcontext)

defun XLIB:MAKE-EVENT-KEYS (1)
{
  unsigned long mask = get_uint32 (popSTACK ());
  int n = 0;
  if (mask & (1L<<0))  pushSTACK (`:KEY-PRESS`), n++;
  if (mask & (1L<<1))  pushSTACK (`:KEY-RELEASE`), n++;
  if (mask & (1L<<2))  pushSTACK (`:BUTTON-PRESS`), n++;
  if (mask & (1L<<3))  pushSTACK (`:BUTTON-RELEASE`), n++;
  if (mask & (1L<<4))  pushSTACK (`:ENTER-WINDOW`), n++;
  if (mask & (1L<<5))  pushSTACK (`:LEAVE-WINDOW`), n++;
  if (mask & (1L<<6))  pushSTACK (`:POINTER-MOTION`), n++;
  if (mask & (1L<<7))  pushSTACK (`:POINTER-MOTION-HINT`), n++;
  if (mask & (1L<<8))  pushSTACK (`:BUTTON-1-MOTION`), n++;
  if (mask & (1L<<9))  pushSTACK (`:BUTTON-2-MOTION`), n++;
  if (mask & (1L<<10)) pushSTACK (`:BUTTON-3-MOTION`), n++;
  if (mask & (1L<<11)) pushSTACK (`:BUTTON-4-MOTION`), n++;
  if (mask & (1L<<12)) pushSTACK (`:BUTTON-5-MOTION`), n++;
  if (mask & (1L<<13)) pushSTACK (`:BUTTON-MOTION`), n++;
  if (mask & (1L<<14)) pushSTACK (`:KEYMAP-STATE`), n++;
  if (mask & (1L<<15)) pushSTACK (`:EXPOSURE`), n++;
  if (mask & (1L<<16)) pushSTACK (`:VISIBILITY-CHANGE`), n++;
  if (mask & (1L<<17)) pushSTACK (`:STRUCTURE-NOTIFY`), n++;
  if (mask & (1L<<18)) pushSTACK (`:RESIZE-REDIRECT`), n++;
  if (mask & (1L<<19)) pushSTACK (`:SUBSTRUCTURE-NOTIFY`), n++;
  if (mask & (1L<<20)) pushSTACK (`:SUBSTRUCTURE-REDIRECT`), n++;
  if (mask & (1L<<21)) pushSTACK (`:FOCUS-CHANGE`), n++;
  if (mask & (1L<<22)) pushSTACK (`:PROPERTY-CHANGE`), n++;
  if (mask & (1L<<23)) pushSTACK (`:COLORMAP-CHANGE`), n++;
  if (mask & (1L<<24)) pushSTACK (`:OWNER-GRAB-BUTTON`), n++;
  funcall (L(list), n);
}

defun XLIB:MAKE-EVENT-MASK (0, 0, rest, nokey, 0, NIL)
{
  /* First make-up a list of the &rest arguments */
  // FIXME! That is silly! It introduces unnec. consing.
  funcall (L(list), argcount);
  value1 = make_uint32 (get_event_mask (value1)); mv_count = 1;
}

defun XLIB:MAKE-STATE-KEYS (1)
{
  unsigned int mask = get_uint16 (popSTACK ());
  int n = 0;
  if (mask & ShiftMask)       pushSTACK (`:SHIFT`), n++;
  if (mask & LockMask) 	      pushSTACK (`:LOCK`), n++;
  if (mask & ControlMask)     pushSTACK (`:CONTROL`), n++;
  if (mask & Mod1Mask) 	      pushSTACK (`:MOD-1`), n++;
  if (mask & Mod2Mask) 	      pushSTACK (`:MOD-2`), n++;
  if (mask & Mod3Mask) 	      pushSTACK (`:MOD-3`), n++;
  if (mask & Mod4Mask) 	      pushSTACK (`:MOD-4`), n++;
  if (mask & Mod5Mask) 	      pushSTACK (`:MOD-5`), n++;
  if (mask & Button1Mask)     pushSTACK (`:BUTTON-1`), n++;
  if (mask & Button2Mask)     pushSTACK (`:BUTTON-2`), n++;
  if (mask & Button3Mask)     pushSTACK (`:BUTTON-3`), n++;
  if (mask & Button4Mask)     pushSTACK (`:BUTTON-4`), n++;
  if (mask & Button5Mask)     pushSTACK (`:BUTTON-5`), n++;
  funcall (L(list), n);
}

defun XLIB:MAKE-STATE-MASK (0, 0, rest, nokey, 0, NIL)
{
  unsigned int mask = 0;
  while (argcount--)
    {
      pushSTACK (`:SHIFT`);
      pushSTACK (`:LOCK`);
      pushSTACK (`:CONTROL`);
      pushSTACK (`:MOD-1`);
      pushSTACK (`:MOD-2`);
      pushSTACK (`:MOD-3`);
      pushSTACK (`:MOD-4`);
      pushSTACK (`:MOD-5`);
      pushSTACK (`:BUTTON-1`);
      pushSTACK (`:BUTTON-2`);
      pushSTACK (`:BUTTON-3`);
      pushSTACK (`:BUTTON-4`);
      pushSTACK (`:BUTTON-5`);
      mask|= (1L << get_enum (13));
    }
  value1 = make_uint16 (mask); mv_count = 1;
}


// ----------------------------------------------------------------------------------------------------
//  Chapter 2   Displays
// ----------------------------------------------------------------------------------------------------

//
//  XLIB:OPEN-DISPLAY host &key :display &allow-other-keys
//
defun XLIB:OPEN-DISPLAY (0, 0, rest, nokey, 0, NIL)
{
  int xlib_error_handler (Display*, XErrorEvent*);
  int xlib_io_error_handler (Display*);

  char *display_name = NULL;					// the host to connect to
  int  display_number = 0;					// the display number
  Display *dpy;

  unless (argcount % 2 == 1)
    {
      pushSTACK (TheSubr(subr_self)->name);
      fehler (error, ("~: Keyword arguments should occur pairwise."));
    }  

  if (argcount > 0)
    {
      pushSTACK (STACK_(argcount-1));				// the first argument
      unless (nullp (STACK_0))
	{
	  unless (stringp (STACK_0))
	    {
	      pushSTACK (`(OR NULL STRING)`);
	      my_standard_type_error (`XLIB::OPEN-DISPLAY`);
	    }
	  display_name = TheAsciz (string_to_asciz (STACK_0, misc_encoding ()));
	}
      skipSTACK (1);
    }

  // Fetch an optional :DISPLAY argument
  {
    uintC i;
    for (i = 1; i < argcount ; i += 2)
      if (eq (STACK_(i), `:DISPLAY`))
        {
          // keyword found; value is in STACK_(i-1)
          display_number = get_uint8 (STACK_(i-1));
          break;
        }
  }

  // On one hand fetching the DISPLAY variable if in doubt is a
  // nice feature -- on the other hand does it conform to the CLX
  // documentation?

  unless (display_name)
    {
      begin_call ();
      display_name = getenv ("DISPLAY");
      end_call ();
    }
  
  unless (display_name)
    {
      // Which display should we open?!
      pushSTACK (TheSubr(subr_self)->name);							// function name
      fehler (error, ("~: Do not know which display to open."));				// raise error
    }

  {
    int len = asciz_length (display_name);
    DYNAMIC_ARRAY (cname, char, len + 5);

    begin_call ();
    {
      char *s;
      int colon_p = 0;
      for (s = display_name; *s; s++)
	if (*s == ':')
	  colon_p = 1;
      
      if (colon_p)
        strcpy (cname, display_name);
      else
        sprintf (cname, "%s:%d", display_name, display_number);
    }
    end_call ();

    begin_call ();
    dpy = XOpenDisplay (cname);
    end_call ();

    unless (dpy)
      {
        pushSTACK (asciz_to_string (cname, misc_encoding ()));					// display name
        pushSTACK (TheSubr(subr_self)->name);			       				// function name
        fehler (error, ("~: Cannot open display ~."));						// raise error
      }

    FREE_DYNAMIC_ARRAY (cname);
  }

  // Now link in the error handler:
  begin_call ();
  XSetErrorHandler (xlib_error_handler);
  XSetIOErrorHandler (xlib_io_error_handler);
  end_call ();
  
  // If X Server decides to not longer talk to us, the pipe or socket gets broken;
  // If SIGPIPE is not blocked, we receive it and do not handle it, which causes
  // the CLISP to quit. If we block it the system calls return with EPIPE instead,
  // enabling libX to handle it. [Raising the XIOError.]

  {
    signalblock_on (SIGPIPE);
    goto skip_that;
    signalblock_off (SIGPIPE);
  skip_that:;
  }

  // 'signalblock_on' and 'signalblock_off' should be used in pairs.
  // Since we do not want to unblock the SIGPIPE ever; I do this 'skip_that'
  // klugde here. [signalblock_on alone may produces a syntax error.]
  
  value1 = make_display (dpy); mv_count = 1;
  skipSTACK (argcount);
}
 
defun XLIB:DISPLAY-AUTHORIZATION-DATA (1) // OK
{
  skipSTACK (1);
  value1 = allocate_string (0); mv_count = 1;
}

defun XLIB:DISPLAY-AUTHORIZATION-NAME (1) // OK
{
  skipSTACK (1);
  value1 = allocate_string (0); mv_count = 1;
}
 
defun XLIB:DISPLAY-BITMAP-FORMAT (1) // OK
{
  Display *dpy = pop_display ();

  pushSTACK (allocate_structure (4));
  TheStructure (STACK_0)->structure_types = `(XLIB::BITMAP-FORMAT)`;
  TheStructure (STACK_0)->recdata[1] = fixnum (BitmapUnit (dpy));				// unit slot
  TheStructure (STACK_0)->recdata[2] = fixnum (BitmapPad (dpy));				// pad slot
  TheStructure (STACK_0)->recdata[3] = BitmapBitOrder (dpy) == LSBFirst ? T : NIL;		// lsb-first-p slot
  value1 = popSTACK (); mv_count = 1;
}
 
defun XLIB:DISPLAY-BYTE-ORDER (1) // OK
{
  skipSTACK (1);
  // To my knowlegde the libX11 opens the display in the local byte sex ...
  value1 = (BIG_ENDIAN_P) ? `:MSBFIRST` : `:LSBFIRST`;
}

defun XLIB:DISPLAY-DISPLAY (1)
{
  // What should this function return?!
  // From manual: Returns the /display-number/ for the host associated with /display/.
  // Not very informative, is it?
  skipSTACK (1);
  value1 = fixnum (0); mv_count = 1;
}

defun XLIB:DISPLAY-ERROR-HANDLER (1) // OK
{
  ensure_living_display (&(STACK_0));
  value1 = TheStructure (STACK_0)->recdata[slot_DISPLAY_ERROR_HANDLER]; mv_count = 1;
  skipSTACK (1);
}

defun XLIB:DISPLAY-ERROR-HANDLER-SETTER (2)	// OK
{
  ensure_living_display (&(STACK_1));
  value1 = TheStructure (STACK_1)->recdata[slot_DISPLAY_ERROR_HANDLER] = STACK_0; mv_count = 1;
  skipSTACK (2);
}
 
defun XLIB:DISPLAY-IMAGE-LSB-FIRST-P (1) // OK
{
  value1 = ImageByteOrder (pop_display ()) == LSBFirst ? T : NIL;  mv_count = 1;
}
 
defun XLIB:DISPLAY-KEYCODE-RANGE (1) // OK
{
  int max_kc, min_kc;
  Display *dpy = pop_display ();

  begin_call ();
    XDisplayKeycodes (dpy, &min_kc, &max_kc);
  end_call ();
  
  value1 = fixnum (min_kc);
  value2 = fixnum (max_kc);
  mv_count = 2;
}
 
defun XLIB:DISPLAY-MAX-KEYCODE (1) // OK
{
  C_xlib_display_keycode_range ();
  value1 = value2; mv_count = 1;
}
 
defun XLIB:DISPLAY-MAX-REQUEST-LENGTH (1) // OK
{
  Display *dpy = pop_display ();
  long n;

  begin_call ();
    n = XMaxRequestSize (dpy);
  end_call ();
  
  value1 = make_uint32 (n); mv_count = 1;
}
 
defun XLIB:DISPLAY-MIN-KEYCODE (1) // OK
{
  C_xlib_display_keycode_range ();
  mv_count = 1;
}

defun XLIB:DISPLAY-MOTION-BUFFER-SIZE (1) // OK 
{
  Display *dpy = pop_display ();
  unsigned long n;
  
  begin_call ();
    n = XDisplayMotionBufferSize (dpy);
  end_call ();

  value1 = make_uint32 (n); mv_count = 1; // remeber pop_display pops
}
 
defun XLIB:DISPLAY-P (1) // OK
 {
   value1 = make_bool (display_p (popSTACK ())); mv_count = 1;
 }
 
defun XLIB:DISPLAY-PIXMAP-FORMATS (1) // OK 
{
  int cnt = 0;
  int i;
  Display *dpy = pop_display ();
  XPixmapFormatValues *formats;

  begin_call ();
    formats = XListPixmapFormats (dpy, &cnt);
  end_call ();
  
  for (i = 0; i < cnt; i++)
    {
      pushSTACK (allocate_structure (4));
      pushSTACK (allocate_cons ());
      Car (STACK_0) = `XLIB::PIXMAP-FORMAT`;
      Cdr (STACK_0) = NIL;
      TheStructure (STACK_1)->structure_types = popSTACK ();
      TheStructure (STACK_0)->recdata[1] = fixnum (formats[i].depth);
      TheStructure (STACK_0)->recdata[2] = fixnum (formats[i].bits_per_pixel);
      TheStructure (STACK_0)->recdata[3] = fixnum (formats[i].scanline_pad);
    }
  if (formats)
    {
      begin_call ();
      XFree (formats);
      end_call ();
    }
  funcall (L(list), cnt);
}
 
defun XLIB:DISPLAY-PROTOCOL-MAJOR-VERSION (1) // OK 
{
  value1 = fixnum (ProtocolVersion (pop_display ())); mv_count = 1;
}
 
defun xlib:Display-Protocol-Minor-Version (1) // OK 
{
  value1 = fixnum (ProtocolRevision (pop_display ())); mv_count = 1;
}
 
defun XLIB:DISPLAY-PROTOCOL-VERSION (1) // OK
{
  Display *dpy = pop_display ();
  value1 = fixnum (ProtocolVersion (dpy));
  value2 = fixnum (ProtocolRevision (dpy));
  mv_count = 2;
 }
 
defun XLIB:DISPLAY-RESOURCE-ID-BASE (1) 
{UNDEFINED} /* ??? */
defun XLIB:DISPLAY-RESOURCE-ID-MASK (1) 
{UNDEFINED} /* ??? */
 
defun XLIB:DISPLAY-ROOTS (1) // OK
{
  Display *dpy;
  int i;
  int cnt;

  pushSTACK (STACK_0);
  dpy = pop_display ();				// retrieve display pointer
  cnt = ScreenCount (dpy);			// number of screens
  
  for (i = 0; i < cnt; i++)
    // thru` all screens
    pushSTACK (make_screen (STACK_(i), ScreenOfDisplay (dpy, i)));
  
  funcall (L(list), cnt);			// cons`em together
  skipSTACK (1);				// cleanup and all done
 }
 
defun XLIB:DISPLAY-VENDOR (1) // OK
{
  Display *dpy = pop_display ();
  char *s = ServerVendor (dpy);
  pushSTACK (asciz_to_string (s, misc_encoding ()));
  pushSTACK (make_uint32 (VendorRelease (dpy)));
  value2 = popSTACK ();
  value1 = popSTACK ();
  mv_count = 2;
}

defun XLIB:DISPLAY-VENDOR-NAME (1) // OK
{
  C_xlib_display_vendor ();
  mv_count = 1;
}

defun XLIB:DISPLAY-RELEASE-NUMBER (1) // OK
{
  C_xlib_display_vendor ();
  value1 = value2; mv_count = 1;
}

defun XLIB:DISPLAY-XID (1)
     /* This functions seems to return a function to allocate new resource id`s,
      * so have a closer look at (from Xlib.h):
      *	#define XAllocID(dpy) ((*((_XPrivDisplay)dpy)->resource_alloc)((dpy)))
      */
     {UNDEFINED}

defun XLIB:DISPLAY-AFTER-FUNCTION (1) // OK
{
  ensure_living_display (&(STACK_0));
  value1 = TheStructure (STACK_0)->recdata[slot_DISPLAY_AFTER_FUNCTION]; mv_count = 1;
  skipSTACK (1);
}

defun XLIB:DISPLAY-AFTER-FUNCTION-SETTER (2) // OK
{
  // TODO - check for function type
  //        [Not very important since the xlib_after_function should get this error.]
  //
  ensure_living_display (&(STACK_0));
  TheStructure (STACK_0)->recdata[slot_DISPLAY_AFTER_FUNCTION] = STACK_1;
  {
    Display *dpy = pop_display ();
    
    if (nullp (STACK_0))
      {
	begin_call ();
	XSetAfterFunction (dpy, NULL); // Q: Is that right?!
	end_call ();
      }
    else
      {
	int xlib_after_function (Display *display);
	begin_call ();
	XSetAfterFunction (dpy, xlib_after_function);
	end_call ();
      }
  }
  value1 = popSTACK (); mv_count = 1;
}
 
defun XLIB:DISPLAY-FORCE-OUTPUT (1)	// OK
{
  Display *dpy = pop_display ();
  begin_call ();
  XFlush (dpy);
  end_call ();
  value1 = NIL; mv_count = 1;
}

defun XLIB:DISPLAY-FINISH-OUTPUT (1) // OK
{
  Display *dpy = pop_display ();
  begin_call ();
  XSync (dpy, 0);
  end_call ();
  value1 = NIL; mv_count = 1;
}

defun XLIB:CLOSE-DISPLAY (1, 0, norest, key, 1, (:ABORT)) // OK
{
  // We can do nothing meaningful with the :abort option ... or could we?

  // if abort is NIL sync with display and remove the display from the xlib::*displays*
  // list. Destroy the hash table and to make sure that not one single reference to an 
  // X object hinders all other from being garbage collected
  // [syncing is for fetching the errors now]
  //
  // if abort is non-NIL do sync too, but do not report errors, which could occur.

  Display *dpy;
  popSTACK ();					// the :abort option
  pushSTACK (STACK_0);				// the display
  dpy = pop_display ();
  begin_call ();
    XCloseDisplay (dpy);
  end_call ();
  
  // Now remove the display from the XLIB:*DISPLAYS* variable
  // FIXME we should cdr-down the hash table and mark all clx object known as dead.
  pushSTACK (`XLIB::*DISPLAYS*`);
  pushSTACK (STACK_1);				// the display
  pushSTACK (Symbol_value (`XLIB::*DISPLAYS*`));
  funcall (L(delete), 2);
  pushSTACK (value1);
  funcall (L(set), 2);

  skipSTACK (1);
  value1= NIL;					// right?
  mv_count = 1;					// all done
}

defun XLIB:DISPLAY-PLIST (1) // OK
{
  ensure_living_display (&(STACK_0));
  value1 = TheStructure (STACK_0)->recdata[slot_DISPLAY_PLIST]; mv_count = 1;
  skipSTACK (1);
}

defun XLIB:DISPLAY-PLIST-SETTER (2)	// OK
{
  ensure_living_display (&(STACK_0));
  value1 = TheStructure (STACK_0)->recdata[slot_DISPLAY_PLIST] = STACK_1; mv_count = 1;
  skipSTACK (2);
}

defun XLIB:DISPLAY-DEFAULT-SCREEN (1) // NIM // OK
{
  Display *dpy;
  
  pushSTACK (STACK_0); dpy = pop_display ();
  value1 = make_screen (STACK_0, DefaultScreenOfDisplay (dpy));
  skipSTACK (1);
  mv_count = 1;
}

defun XLIB:DISPLAY-NSCREENS (1) // NIM
{
  value1 = fixnum (ScreenCount (pop_display ())); mv_count = 1;
}

defun XLIB:DISPLAY-INVOKE-AFTER-FUNCTION (1)
     // XXX This function does not work at all -- it pushes the hash table instead of the after function, but why?
     // This one seems simply to be a a hook to call the after_function
{
  // (funcall (display-after-function dpy) dpy)
  
  pushSTACK (STACK_0);
  funcall (`XLIB::DISPLAY-AFTER-FUNCTION`, 1);
  pushSTACK (value1);
  pushSTACK (STACK_1);
  funcall (L(funcall), 2);
  skipSTACK (1);
}

defun XLIB:DISPLAY-HOST (1)
{
  char *name = DisplayString (pop_display ());
  char *s;

  // Hunt the ':'
  for (s = name; *s && *s!=':'; s++)
    continue;

  if (s == name)
    value1 = ascii_to_string ("localhost");
  else
    value1 = n_char_to_string (name, s - name, misc_encoding ());

  mv_count = 1;
}

defun XLIB:DISPLAY-REPORT-ASYNCHRONOUS-ERRORS (1)
     // This function is not actually specified in the CLX refman, but the source code says something about it:
     //
     // (def-clx-class (display (:include buffer) ..)
     //  :
     //    (report-asynchronous-errors		; When to report asynchronous errors
     //       `(:immediately) :type list)	; The keywords that can be on this list 
     //	 :					; are :IMMEDIATELY, :BEFORE-EVENT-HANDLING,
     //	  )      				; and :AFTER-FINISH-OUTPUT
     //
{
  skipSTACK (1);
  value1 = `(:IMMEDIATELY)`; mv_count = 1;		// Well, em ... fake it!
}

defun XLIB:DISPLAY-REPORT-ASYNCHRONOUS-ERRORS-SETTER (2)
{
  value1 = STACK_1; mv_count = 1;
  skipSTACK (2);
}
     
defun XLIB:DISPLAY-TRACE (0, 0, rest, nokey, 0, NIL)
     // I do not think I will support this function, since
     //   - traceing seems not to be possible using the libX11
     //   - It may even not be wanted by anybody...?!
     // BTW in the source of MIT-CLX (trace.lsp) I found a mark that display-trace is an obsolete name.
     {UNDEFINED}


// ----------------------------------------------------------------------------------------------------
//  Chapter 3   Screens
// ----------------------------------------------------------------------------------------------------

##define DEF_SCREEN_PROP(lspnam, typ, cnam)\
    defun lspnam (1)						\
    {								\
      value1 = make_##typ (cnam (get_screen (popSTACK ())));	\
      mv_count = 1;						\
    }

DEF_SCREEN_PROP (XLIB:SCREEN-BLACK-PIXEL,           uint32,      BlackPixelOfScreen)
DEF_SCREEN_PROP (XLIB:SCREEN-WHITE-PIXEL,           uint32,      WhitePixelOfScreen)
DEF_SCREEN_PROP (XLIB:SCREEN-EVENT-MASK-AT-OPEN,    uint32,      EventMaskOfScreen)
DEF_SCREEN_PROP (XLIB:SCREEN-HEIGHT,                sint16,      HeightOfScreen)
DEF_SCREEN_PROP (XLIB:SCREEN-HEIGHT-IN-MILLIMETERS, sint16,      HeightMMOfScreen)
DEF_SCREEN_PROP (XLIB:SCREEN-WIDTH, 		    sint16,      WidthOfScreen)
DEF_SCREEN_PROP (XLIB:SCREEN-WIDTH-IN-MILLIMETERS,  sint16,      WidthMMOfScreen)
DEF_SCREEN_PROP (XLIB:SCREEN-MAX-INSTALLED-MAPS,    uint16,      MaxCmapsOfScreen)
DEF_SCREEN_PROP (XLIB:SCREEN-MIN-INSTALLED-MAPS,    uint16,      MinCmapsOfScreen)
DEF_SCREEN_PROP (XLIB:SCREEN-ROOT-DEPTH,	    uint16,      DefaultDepthOfScreen) // right-p?
DEF_SCREEN_PROP (XLIB:SCREEN-ROOT-VISUAL,	    visual,      DefaultVisualOfScreen)
DEF_SCREEN_PROP (XLIB:SCREEN-ROOT-VISUAL-INFO,      visual_info, DefaultVisualOfScreen) // NIM
DEF_SCREEN_PROP (XLIB:SCREEN-SAVE-UNDERS-P,	    bool,        DoesSaveUnders)


defun XLIB:SCREEN-BACKING-STORES (1) // OK
{
  int a = DoesBackingStore (get_screen (popSTACK ()));
  value1 =
    (a == NotUseful) ? `:NEVER` :				// Why here :never but not :not-useful?!
    (a == WhenMapped) ? `:WHEN-MAPPED` :
    `:ALWAYS`;
  mv_count = 1;
}

defun XLIB:SCREEN-DEFAULT-COLORMAP (1) // OK
{
  value1 = make_colormap (get_display_obj (STACK_0), DefaultColormapOfScreen (get_screen (STACK_0))); mv_count = 1;
  skipSTACK (1);
}

defun XLIB:SCREEN-DEPTHS (1)
{
  Display *dpy;
  Screen *scr = get_screen_and_display (STACK_0, &dpy);
  int *depths;
  int ndepths = 0;
  int i;
  int screen_number;
  
  begin_call ();
  screen_number = XScreenNo (dpy, scr);
  depths = XListDepths (dpy, screen_number, &ndepths);
  end_call ();

  for (i = 0; i < ndepths; i++)
    {
      XVisualInfo template, *visual_infos;
      int n_visual_infos, j;

      pushSTACK (make_uint8 (depths[i]));

      // Now enumerate the visual infos ...
      template.depth = depths[i];
      n_visual_infos = 0;
      
      begin_call ();
      visual_infos = XGetVisualInfo (dpy, VisualDepthMask, &template, &n_visual_infos);
      end_call ();

      if (visual_infos)
	{
	  for (j = 0; j < n_visual_infos; j++)
	    pushSTACK (make_visual_info (visual_infos[j].visual));

	  begin_call ();
	  XFree (visual_infos);
	  end_call ();
	}
      
      // Cons `em up
      funcall (L(list), n_visual_infos+1);
    }

  // Final cons
  funcall (L(list), ndepths);
  if (depths)
    {
      begin_call ();
      XFree (depths);
      end_call ();
    }

  skipSTACK (1);		// all done
}
  
defun XLIB:SCREEN-P (1)	// OK
{
  value1 = make_bool (screen_p (popSTACK())); mv_count = 1;
}

defun XLIB:SCREEN-PLIST (1)	// OK
{
  general_plist_reader (`XLIB::SCREEN`);
}

defun XLIB:SCREEN-PLIST-SETTER (2) // OK
{
  general_plist_writer (`XLIB::SCREEN`);
}

defun XLIB:SCREEN-ROOT (1)	// OK
{
  value1 = make_window (get_display_obj (STACK_0), RootWindowOfScreen (get_screen (STACK_0)));
  mv_count = 1;
  skipSTACK (1);
}

//
//  XLIB:VISUAL-INFO display visual-id
//
defun XLIB:VISUAL-INFO (2) // NIM // OK
{
  VisualID vid;
  Display *dpy;
  Visual *visual;

  pushSTACK (STACK_1);
  dpy = pop_display ();
  vid = get_uint29 (STACK_0);
  visual = XVisualIDToVisual (dpy, vid);
  
  if (visual)
    {
      value1 = make_visual_info (visual); mv_count = 1;
      skipSTACK (2);
    }
  else
    {
      pushSTACK (STACK_1);	// display argument
      pushSTACK (STACK_1);	// visual id argument
      fehler (error, ("Visual info not found for id #~ in display ~."));
    }
}

// After all, no SCREEN-EQUAL ?


// ----------------------------------------------------------------------------------------------------
//  Chapter 4   Windows and Pixmaps
// ----------------------------------------------------------------------------------------------------

/* 4.1 Drawables */

/* 4.2 Creating Windows */
defun XLIB:CREATE-WINDOW (0, 0, norest, key, 23,
	                      (:WINDOW :PARENT :X :Y :WIDTH :HEIGHT
			       :DEPTH :BORDER-WIDTH :CLASS :VISUAL :BACKGROUND
			       :BORDER :BIT-GRAVITY :GRAVITY :BACKING-STORE
			       :BACKING-PLANES :BACKING-PIXEL :SAVE-UNDER
			       :EVENT-MASK :DO-NOT-PROPAGATE-MASK :OVERRIDE-REDIRECT
			       :COLORMAP :CURSOR))
{
  XSetWindowAttributes attr;
  unsigned long valuemask = 0;
  Visual *visual = CopyFromParent;
  int class = CopyFromParent;
  int border_width = 0;
  int depth = CopyFromParent;
  Window parent;
  int x,y,width,height;
  Display *dpy;
  Window win;

#define SLOT(ofs, type, cslot, mask)\
    if (!gunboundp (STACK_(ofs))) { attr.cslot = get_##type(STACK_(ofs)); valuemask |= mask; }

#if 0
  SLOT ( 0, cursor,	cursor,			CWCursor);
  SLOT ( 1, colormap,	colormap,		CWColormap);
  SLOT ( 2, switch,	override_redirect,	CWOverrideRedirect);
  SLOT ( 3, uint32,	do_not_propagate_mask,	CWDontPropagate);
  SLOT ( 4, event_mask,	event_mask,		CWEventMask);
  SLOT ( 5, switch,	save_under,		CWSaveUnder);
  SLOT ( 6, uint32,	backing_pixel,		CWBackingPixel);
  SLOT ( 7, uint32,	backing_planes,		CWBackingPlanes);
#endif
  
  if (!gunboundp (STACK_0)) { attr.cursor = get_cursor(STACK_0); valuemask |= CWCursor; }
  if (!gunboundp (STACK_1)) { attr.colormap = get_colormap (STACK_1); valuemask |= CWColormap; }
  
  if (!eq(STACK_2 , unbound) && !eq(STACK_2 , NIL)) { attr.override_redirect     = get_switch (STACK_2);         valuemask |= CWOverrideRedirect; }
  if (!eq(STACK_3 , unbound) && !eq(STACK_3 , NIL)) { attr.do_not_propagate_mask = get_uint32 (STACK_3);         valuemask |= CWDontPropagate; }
  if (!eq(STACK_4 , unbound) && !eq(STACK_4 , NIL)) { attr.event_mask            = get_event_mask (STACK_4);     valuemask |= CWEventMask; }
  if (!eq(STACK_5 , unbound) && !eq(STACK_5 , NIL)) { attr.save_under            = get_generic_switch (STACK_5); valuemask |= CWSaveUnder; }
  if (!eq(STACK_6 , unbound) && !eq(STACK_6 , NIL)) { attr.backing_pixel         = get_uint32 (STACK_6);         valuemask |= CWBackingPixel; }
  if (!eq(STACK_7 , unbound) && !eq(STACK_7 , NIL)) { attr.backing_planes        = get_uint32 (STACK_7);         valuemask |= CWBackingPlanes; }
  if (!eq(STACK_8 , unbound) && !eq(STACK_8 , NIL)) { attr.backing_store         = get_backing_store (STACK_8);  valuemask |= CWBackingStore; }
  if (!eq(STACK_9 , unbound) && !eq(STACK_9 , NIL)) { attr.win_gravity           = get_gravity (STACK_9);        valuemask |= CWWinGravity; }
  if (!eq(STACK_10, unbound) && !eq(STACK_10, NIL)) { attr.bit_gravity           = get_gravity (STACK_10);       valuemask |= CWBitGravity; }

  if (!eq(STACK_(11), unbound) && !eq(STACK_(11), NIL)) /* :border */
    {
      if (eq(STACK_(11), `:COPY`))
	{ attr.border_pixmap = CopyFromParent; valuemask |= CWBorderPixmap; }
      else
      if (pixmap_p (STACK_(11)))
	{ attr.border_pixmap = get_pixmap (STACK_(11)); valuemask |= CWBorderPixmap; }
      else
	{ attr.border_pixel = get_uint32 (STACK_(11)); valuemask |= CWBorderPixel; }
    }
  
  if (!eq(STACK_(12), unbound) && !eq(STACK_(12), NIL)) /* :background */
    {
      if (eq(STACK_(12), `:NONE`))
	{ attr.background_pixmap = None; valuemask |= CWBackPixmap; }
      else
      if (eq(STACK_(12), `:PARENT-RELATIVE`))
	{ attr.background_pixmap = ParentRelative; valuemask |= CWBackPixmap; }
      else
      if (pixmap_p (STACK_(12)))
	{ attr.background_pixmap = get_pixmap (STACK_(12)); valuemask |= CWBackPixmap; }
      else
	{ attr.background_pixel = get_pixel (STACK_(12)); valuemask |= CWBackPixel; }
    }
  
  if (!eq(STACK_(14), unbound) && !eq(STACK_(14), NIL)) /* :class */		class = get_W_class (STACK_(14));
  if (!eq(STACK_(15), unbound) && !eq(STACK_(15), NIL)) /* :border-width */	border_width = get_uint16 (STACK_(15));
  if (!eq(STACK_(16), unbound) && !eq(STACK_(16), NIL)) /* :depth */ 		depth = get_uint16 (STACK_(16));
  
  if (!eq(STACK_(17), unbound) && !eq(STACK_(17), NIL)) /* :height */ //C
    height = get_uint16 (STACK_(17));
  else
    goto required;
  
  if (!eq(STACK_(18), unbound) && !eq(STACK_(18), NIL)) /* :width */ //C
    width = get_uint16 (STACK_(18));
  else
    goto required;
  
  if (!eq(STACK_(19), unbound) && !eq(STACK_(19), NIL)) /* :y */ //C
    y = get_sint16 (STACK_(19));
  else
    goto required;
  
  if (!eq(STACK_(20), unbound) && !eq(STACK_(20), NIL)) /* :x */ //C
    x = get_sint16 (STACK_(20));
  else
    goto required;
  
  if (!eq(STACK_(21), unbound) && !eq(STACK_(21), NIL)) /* :parent */ //C
    {
      parent = get_window_and_display (STACK_(21), &dpy);
      pushSTACK (get_display_obj (STACK_(21)));
    }
  else
    goto required;
  
  if (!eq(STACK_(13+1), unbound) && !eq(STACK_(13+1), NIL)) /* :visual */
    visual = get_visual (dpy, STACK_(13+1));
  
  if (!eq(STACK_(23), unbound) && !eq(STACK_(23), NIL)) /* :window */ //C
    pushSTACK (STACK_(23));
  else
    pushSTACK (NIL);
#undef SLOT

  begin_call ();
  win = XCreateWindow (dpy, parent, x,y, width,height, border_width, depth, class, visual, valuemask, &attr);
  end_call ();
  
  value1 = make_window_2 (STACK_1, win, STACK_0);
  mv_count = 1;
  skipSTACK (23 + 2);
  return;
  
 required:
  pushSTACK (`XLIB::CREATE-WINDOW`);	// Funktionsname
  fehler (error, ("~: At least :X, :Y, :WIDTH, :HEIGHT and :PARENT must be specified"));
}

##define DEF_DRAWABLE_GEOM_GETTER(type, lspnam, attr)				\
	defun XLIB:DRAWABLE-##lspnam (1)	  				\
        {									\
	  Window root;								\
	  int x, y;								\
	  unsigned int width, height;						\
	  unsigned int border_width;						\
	  unsigned int depth;							\
          Display *dpy;								\
	  Drawable da = get_drawable_and_display (STACK_0, &dpy);		\
	  begin_call ();							\
	  XGetGeometry (dpy, da,					 	\
			&root, &x, &y, &width, &height,				\
			&border_width, &depth);					\
	  end_call ();								\
	  value1 = make_##type (attr); mv_count = 1;				\
	  skipSTACK (1);							\
	}

##define DEF_DRAWABLE_GEOM_SETTER(type, lspnam, attr, mask)			\
        defun XLIB:DRAWABLE-##lspnam##-SETTER (2)				\
	{									\
	  XWindowChanges values;						\
	  Window win;								\
	  Display *dpy;								\
          /* Why window here vvvv and not drawable? */				\
	  win = get_window_and_display (STACK_0, &dpy);				\
	  values.attr = get_##type (STACK_1);					\
	  begin_call ();							\
	  XConfigureWindow (dpy, win, mask, &values);				\
	  end_call ();								\
	  value1 = STACK_1; mv_count = 1;					\
	  skipSTACK (2);							\
	}

##define DEF_DRAWABLE_GEOM(type, lspnam, attr, mask)		\
	DEF_DRAWABLE_GEOM_GETTER (type, lspnam, attr)		\
	DEF_DRAWABLE_GEOM_SETTER (type, lspnam, attr, mask)

DEF_DRAWABLE_GEOM (uint16, border-width, border_width, CWBorderWidth)	// OK
DEF_DRAWABLE_GEOM_GETTER (uint8, depth, depth)			// OK
DEF_DRAWABLE_GEOM (uint16, height, height, CWHeight) // OK
DEF_DRAWABLE_GEOM (uint16, width, width, CWWidth)   // OK
DEF_DRAWABLE_GEOM (sint16, x, x, CWX)	     // OK
DEF_DRAWABLE_GEOM (sint16, y, y, CWY)	     // OK

defun XLIB:WINDOW-ALL-EVENT-MASKS (1)
{
  XWindowAttributes attr;
  Display *dpy;
  Window win = get_xid_object_and_display (`XLIB::WINDOW`, STACK_0, &dpy);
  begin_call ();
  XGetWindowAttributes (dpy, win, &attr);
  end_call ();
  value1 = make_event_mask (attr.all_event_masks); mv_count = 1;
  skipSTACK (1);
}

defun XLIB:WINDOW-BACKGROUND-SETTER (2) /*OK*/
{
  XSetWindowAttributes attr;
  unsigned long valuemask = 0;
  
  if (eq (STACK_1, `:NONE`))
    { attr.background_pixmap = None; valuemask |= CWBackPixmap; }
  else if (eq (STACK_1, `:PARENT-RELATIVE`))
    { attr.background_pixmap = ParentRelative; valuemask |= CWBackPixmap; }
  else if (pixmap_p (STACK_1))
    { attr.background_pixmap = get_pixmap (STACK_1); valuemask |= CWBackPixmap; }
  else if (pixel_p (STACK_1))
    { attr.background_pixel = get_pixel (STACK_1); valuemask |= CWBackPixel; }
  else
    {
      // raise type error
      pushSTACK (STACK_1);  		// datum

      // expected type:
      pushSTACK (`(OR XLIB::PIXMAP XLIB::PIXEL (EQL :NONE) (EQL :PARENT-RELATIVE))`);
      my_standard_type_error (`XLIB::WINDOW-BACKGROUND-SETTER`);
    }

  {
    Display *dpy;
    Window win = get_xid_object_and_display (`XLIB::WINDOW`, STACK_0, &dpy);
    begin_call ();
    XChangeWindowAttributes (dpy, win, valuemask, &attr);
    end_call ();
  }
  value1 = STACK_1; mv_count = 1;
  skipSTACK(2);
}

##define DEF_WIN_ATTR_READER(lspnam,typ,slotget)				\
  defun XLIB:WINDOW-##lspnam (1)						\
  {										\
    XWindowAttributes attr;							\
    Display *dpy;								\
    Window win = get_xid_object_and_display (`XLIB::WINDOW`, STACK_0, &dpy);	\
    begin_call ();								\
      XGetWindowAttributes (dpy, win, &attr);					\
    end_call ();								\
    value1 = make_##typ (attr.slotget); mv_count = 1;				\
    skipSTACK (1);								\
  }

##define DEF_WIN_ATTR_READER_2(lspnam,typ,slotget)			\
     defun XLIB:WINDOW-##lspnam (1)					\
     {									\
 	  XWindowAttributes attr;					\
	  Display *dpy;							\
	  Window win = get_window_and_display (STACK_0, &dpy);		\
	  begin_call ();						\
          XGetWindowAttributes (dpy, win, &attr);			\
	  end_call ();							\
          value1 = make_##typ (get_display_obj (STACK_0),attr.slotget);	\
          skipSTACK (1);						\
     }

##define DEF_WIN_ATTR_WRITER(lspnam,typ,slotset,msk)			\
     defun XLIB:WINDOW-##lspnam##-setter (2)			        \
     {									\
          XSetWindowAttributes attr;					\
	  Display *dpy;							\
	  Window win = get_window_and_display (STACK_0, &dpy);		\
          attr.slotset = get_##typ (STACK_1);				\
	  begin_call ();						\
          XChangeWindowAttributes (dpy, win, msk, &attr);		\
	  end_call ();							\
          value1 = STACK_1; mv_count = 1;				\
          skipSTACK (2);						\
     }

##define DEF_WIN_ATTR(lspnam, typ, slotget, slotset, msk)	\
    DEF_WIN_ATTR_READER(lspnam, typ, slotget)			\
    DEF_WIN_ATTR_WRITER(lspnam, typ, slotset, msk)

##define DEF_WIN_ATTR_2(lspnam, typ, slotget, slotset, msk)	\
    DEF_WIN_ATTR_READER_2(lspnam, typ, slotget)			\
    DEF_WIN_ATTR_WRITER(lspnam, typ, slotset, msk)
     
DEF_WIN_ATTR (BACKING-PIXEL,         uint32,        backing_pixel,     backing_pixel,     CWBackingPixel)
DEF_WIN_ATTR (BACKING-PLANES,        uint32,        backing_planes,    backing_planes,    CWBackingPlanes)
DEF_WIN_ATTR (BIT-GRAVITY,           gravity,       bit_gravity,       bit_gravity,       CWBitGravity)
DEF_WIN_ATTR (GRAVITY,               gravity,       win_gravity,       win_gravity,       CWWinGravity)
DEF_WIN_ATTR (EVENT-MASK,            event_mask,    your_event_mask,   event_mask,        CWEventMask)
DEF_WIN_ATTR (OVERRIDE-REDIRECT,     switch,        override_redirect, override_redirect, CWOverrideRedirect)
DEF_WIN_ATTR (BACKING-STORE,         backing_store, backing_store,     backing_store,     CWBackingStore)
DEF_WIN_ATTR (DO-NOT-PROPAGATE-MASK, event_mask,    do_not_propagate_mask, do_not_propagate_mask, CWDontPropagate)
DEF_WIN_ATTR (SAVE-UNDER,            generic_switch,  save_under,        save_under,        CWSaveUnder)

DEF_WIN_ATTR_2 (COLORMAP,                  colormap, colormap, colormap, CWColormap)
DEF_WIN_ATTR_WRITER (CURSOR,               cursor, cursor, CWCursor)
DEF_WIN_ATTR_READER (MAP-STATE,            map_state, map_state)
DEF_WIN_ATTR_READER (CLASS,                W_class, class)
DEF_WIN_ATTR_READER (COLORMAP-INSTALLED-P, bool, map_installed)
DEF_WIN_ATTR_READER (VISUAL,               visual, visual)
DEF_WIN_ATTR_READER (VISUAL-INFO,          visual_info, visual)//NIM

defun XLIB:WINDOW-CURSOR (1)
{
  pushSTACK (`XLIB::WINDOW-CURSOR`);
  fehler (error, ("~ can only be set"));
}

defun XLIB:WINDOW-BORDER-SETTER (2)
{
  Display *dpy;
  Window win   = get_window_and_display (STACK_0, &dpy);
  XSetWindowAttributes attr;
  unsigned long value_mask = 0;
          
  if (eq (STACK_1, `:COPY`))
    { attr.border_pixmap = CopyFromParent; value_mask = CWBorderPixmap; }
  else
    if (pixmap_p (STACK_1))
      { attr.border_pixmap = get_pixmap (STACK_1); value_mask = CWBorderPixmap; }
    else
      if (pixel_p (STACK_1))
	{ attr.border_pixel = get_pixel (STACK_1); value_mask = CWBorderPixel; }
      else
	{
	  // raise type error
	  pushSTACK (STACK_1);  	// datum
	  pushSTACK (`(OR XLIB::PIXMAP XLIB::PIXEL (EQL :COPY))`);
	  my_standard_type_error (`XLIB::WINDOW-BORDER-SETTER`);
	}

  begin_call ();
  XChangeWindowAttributes (dpy, win, value_mask, &attr);
  end_call ();

  value1 = STACK_1; mv_count = 1;
  skipSTACK (2);			// all done
}

//
// XLIB:WINDOW-PRIORITY-SETTER mode window &optional sibling
//
defun XLIB:WINDOW-PRIORITY-SETTER (2, 1)
{
  XWindowChanges changes;
  unsigned int value_mask = 0;
  Display *dpy;
  Window win   = get_window_and_display (STACK_1, &dpy);

  unless (eq (STACK_0, unbound)) { changes.sibling = get_window (STACK_0); value_mask |= CWSibling; }
  changes.stack_mode = get_stack_mode (STACK_2); value_mask |= CWStackMode;
  begin_call ();
  XConfigureWindow (dpy, win, value_mask, &changes);
  end_call ();
  
  value1 = STACK_2; mv_count = 1;
  skipSTACK (3);		// all done
}

/* 4.4  Stacking Order */
defun XLIB:CIRCULATE-WINDOW-DOWN (1)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  begin_call();
  XCirculateSubwindowsDown (dpy, win);
  end_call ();
  skipSTACK (1);
  value1= NIL; mv_count = 1;
}

defun XLIB:CIRCULATE-WINDOW-UP (1)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  begin_call();
  XCirculateSubwindowsUp (dpy, win);
  end_call ();
  skipSTACK (1);
  value1= NIL; mv_count = 1;
}

/* 4.5  Window Hierachy */
defun XLIB:DRAWABLE-ROOT (1)
{ 
  Window root;
  Drawable da;
  Display *dpy;
  int x, y; 
  unsigned int width, height, border_width, depth; 
  da = get_drawable_and_display (STACK_0, &dpy);
  begin_call ();
  XGetGeometry (dpy, da, &root, &x, &y, &width, &height, &border_width, &depth);
  end_call ();
  value1 = make_window (get_display_obj (STACK_0), root); mv_count = 1; 
  skipSTACK (1); 
}

//
//  XLIB:QUERY-TREE window &key (result-type `list)
//
defun XLIB:QUERY-TREE (1, 0, norest, key, 1, (:RESULT-TYPE))
{
  Window win;
  Display *dpy;
  object *dpy_objf;
  Window root;
  Window parent;
  Window *childs;
  unsigned int nchilds, i;

  win = get_window_and_display (STACK_1, &dpy);
  pushSTACK (get_display_obj (STACK_1));
  dpy_objf = &(STACK_0);

  begin_call ();
  if (XQueryTree (dpy, win, &root, &parent, &childs, &nchilds))
    {
      end_call ();
      
      // Now push all childrens
      for (i = 0; i < nchilds; i++)
	pushSTACK (make_window (*dpy_objf, childs[i]));

      if (childs) XFree (childs);

      // Now cons `em together
      funcall (L(list), nchilds);

      // coerce it to the right type if necessary
      if (!eq (STACK_1, unbound))
	{
	  pushSTACK (value1);
	  pushSTACK (STACK_2);
	  funcall (L(coerce), 2);
	}

      // value1 har børns listet
      pushSTACK (value1);
      pushSTACK (make_window (*dpy_objf, parent));
      pushSTACK (make_window (*dpy_objf, root));
      value3 = popSTACK ();
      value2 = popSTACK ();
      value1 = popSTACK ();
      mv_count = 3;
    }
  else
    {
      end_call ();
      // Wat schall wi nu tun?
      value1 = NIL;
      mv_count = 1;		// Dår kan nu mol nichts scheif geihn.
    }

  skipSTACK (3);		// Nu vi er færdig.
}

defun XLIB:REPARENT-WINDOW (4)
{
  Display *dpy;
  Window win  = get_window_and_display (STACK_3, &dpy);
  Window win2 = get_window (STACK_2);
  int x       = get_sint16 (STACK_1);
  int y       = get_sint16 (STACK_0);
  begin_call ();
  XReparentWindow (dpy, win, win2, x, y);
  end_call ();
  skipSTACK (4);
  value1= NIL; mv_count = 1;
}

defun XLIB:TRANSLATE-COORDINATES (4) // (src src-x src-y dst)
{
  int x,y;
  Window child;
  Window src, dest;
  int src_x, src_y;
  Display *dpy;
  int r;

  src   = get_xid_object_and_display (`XLIB::WINDOW`, STACK_3, &dpy);
  dest  = get_window (STACK_0);
  src_x = get_sint16 (STACK_2);
  src_y = get_sint16 (STACK_1);

  begin_call ();
    r = XTranslateCoordinates (dpy, src, dest, src_x, src_y, &x, &y, &child);
  end_call ();

  if (r)
    {
      pushSTACK (make_sint16 (x));
      pushSTACK (make_sint16 (y));
      pushSTACK (make_window (get_display_obj (STACK_5), child));
      value3 = popSTACK ();
      value2 = popSTACK ();
      value1 = popSTACK ();
      mv_count = 3;
    }
  else
    {
      value1 = value2 = value3 = NIL;
      mv_count = 3;
    }
  skipSTACK (4);
}

/* 4.6  Mapping Windows */
defun XLIB:MAP-WINDOW (1)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  begin_call ();
  XMapWindow (dpy, win);
  end_call ();
  skipSTACK (1);
  value1= NIL; mv_count = 1;
}

defun XLIB:MAP-SUBWINDOWS (1)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  begin_call ();
  XMapSubwindows (dpy, win);
  end_call ();
  skipSTACK (1);
  value1= NIL; mv_count = 1;
}

defun XLIB:UNMAP-WINDOW (1)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  begin_call ();
  XUnmapWindow (dpy, win);
  end_call ();
  skipSTACK (1);
  value1= NIL; mv_count = 1;
}

defun XLIB:UNMAP-SUBWINDOWS (1)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  begin_call ();
  XUnmapSubwindows (dpy, win);
  end_call ();
  skipSTACK (1);
  value1= NIL; mv_count = 1;
}

/* 4.7  Destroying Windows */

defun XLIB:DESTROY-WINDOW (1)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  begin_call ();
  XDestroyWindow (dpy, win);
  end_call ();
  skipSTACK (1);
  value1= NIL; mv_count = 1;
}

defun XLIB:DESTROY-SUBWINDOWS (1)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  begin_call ();
  XDestroySubwindows (dpy, win);
  end_call ();
  skipSTACK (1);
  value1= NIL; mv_count = 1;
}

/* 4.8  Pixmaps */
defun XLIB:CREATE-PIXMAP (0, 0, norest, key, 5,
	  (:PIXMAP :WIDTH :HEIGHT :DEPTH :DRAWABLE))
{
  Display *dpy;
  Drawable da;
  Pixmap pm;
  int width,height,depth;
  
  if (eq(STACK_0, unbound) || eq(STACK_1, unbound) || eq(STACK_2, unbound) || eq(STACK_3, unbound))
    {
      NOTIMPLEMENTED;
    }

  da     = get_drawable_and_display (STACK_0, &dpy);
  width  = get_uint16 (STACK_3);	// actually uint15!
  height = get_uint16 (STACK_2);
  depth  = get_uint16 (STACK_1);

  begin_call ();
  pm = XCreatePixmap (dpy, da, width, height, depth);
  end_call ();
  
  value1 = make_pixmap_2 (get_display_obj (STACK_0), pm,
			  ( (!eq(STACK_4, unbound) && !nullp (STACK_4)) ?
			    STACK_4 :
			    NIL) );
  mv_count = 1;
  skipSTACK (5);
  return;
}

defun XLIB:FREE-PIXMAP (1)
{
  Display *dpy;
  Pixmap pix = get_pixmap_and_display (STACK_0, &dpy);
  begin_call();
  XFreePixmap (dpy, pix);
  end_call ();
  skipSTACK (1);
  value1= NIL; mv_count = 1;
}



// ----------------------------------------------------------------------------------------------------
//  Chapter 5   Graphics Contexts
// ----------------------------------------------------------------------------------------------------

// Since libX does not allow to retrieve the clip-mask or the dashes list any
// how, we save the clip-mask and dashes-list in the gcontext instance extra.

// DASHES-LIST is stored in the additional slot xlib::%dashes and is represented
//    as a single uint8 or as a simple vector of uint8's. (This allowes us to
//    pass the vector directly into the C routine if needed.) However this value
//    could be NIL, then the C rep is suffient.

// FLAME -- I find me always fixing the flaws of the narrow-minded C people, not
// capable of defining any clean and consistent interface. Even worse, yesterday
// I spend a couple of hours of debugging just to recognize, that the malloc
// implmentation of the default Linux libc (version 5.3.9 and up [Yes, 5.4.7 is
// even worse]!) is now broken, it messed up *my* memory. (I considered it all
// the time working). I have the strange feeling that the more popular Linux
// becomes the more broken it gets. I want the old days back, where only a
// couple of people messed around with Linux, knowing what they do.
//
// [Ya, back to the 0.96 days (or was it 0.98?), I had only 8MB ram (and ~80MB
// hd) and it was just smooth flying under X; The feeling you get when driving
// an empty 'Autobahn' with a capable car at moderate speed, just smooth,
// effient and relaxing].

// Also the manual says (somewhat foggy):
//     Changing the dash-offset or dash-list overrides any previous XSetDashes
//     request on the context.  The order in which components ... and bla bla
// Maybe have also to save the dash-offset?
//

/* 5.2 Creating Graphics Contexts */

defun XLIB:CREATE-GCONTEXT (0, 0, norest, key, 26,
	  (:DRAWABLE :FUNCTION :PLANE-MASK :FOREGROUND :BACKGROUND
	   :LINE-WIDTH :LINE-STYLE :CAP-STYLE :JOIN-STYLE :FILL-STYLE
	   :FILL-RULE :ARC-MODE :TILE :STIPPLE :TS-X :TS-Y :FONT
	   :SUBWINDOW-MODE :EXPOSURES :CLIP-X :CLIP-Y :CLIP-MASK
	   :CLIP-ORDERING :DASH-OFFSET :DASHES :CACHE-P))
{
  XGCValues values;
  unsigned long valuemask = 0;
  int non_trivial_clip_mask_p = 0; 	// whether user specified a rect-seq
  int non_trivial_dashes_p = 0;	   	// whether user specified a sequence as :dashes argument

#define SLOT(ofs, type, slot, mask)				\
  if (!eq(STACK_(ofs), unbound) && !eq(STACK_(ofs), NIL))	\
    { values.slot = get_##type (STACK_(ofs)); valuemask |= mask; }	

  /* missing: 0=cache-p */
  SLOT (24, gc_function,     function,           GCFunction);
  SLOT (23, uint32,          plane_mask,         GCPlaneMask);
  SLOT (22, pixel,           foreground,         GCForeground);
  SLOT (21, pixel,           background,         GCBackground);
  SLOT (20, sint16,          line_width,         GCLineWidth);
  SLOT (19, line_style,      line_style,         GCLineStyle);
  SLOT (18, cap_style,       cap_style,          GCCapStyle);
  SLOT (17, join_style,      join_style,         GCJoinStyle);
  SLOT (16, fill_style,      fill_style,         GCFillStyle);
  SLOT (15, fill_rule,       fill_rule,          GCFillRule);
  SLOT (14, arc_mode,        arc_mode,           GCArcMode);
  SLOT (13, pixmap,          tile,               GCTile);
  SLOT (12, pixmap,          stipple,            GCStipple);
  SLOT (11, sint16,          ts_x_origin,        GCTileStipXOrigin);
  SLOT (10, sint16,          ts_y_origin,        GCTileStipYOrigin);
  SLOT ( 9, font,            font,               GCFont);
  SLOT ( 8, subwindow_mode,  subwindow_mode,     GCSubwindowMode);
  SLOT ( 7, bool,            graphics_exposures, GCGraphicsExposures);
  SLOT ( 6, sint16,          clip_x_origin,      GCClipXOrigin);
  SLOT ( 5, sint16,          clip_y_origin,      GCClipYOrigin);
  SLOT ( 4, pixmap,          clip_mask,          GCClipMask);
  SLOT ( 2, sint16,          dash_offset,        GCDashOffset);
#undef SLOT

  // Handle the :clip-mask argument, :clipordering is only used if :clip-mask is a rect-seq.
  if (!eq (STACK_4, unbound))	// :clip-mask
    {
      if (pixmap_p (STACK_4))
	{ values.clip_mask = get_pixmap (STACK_4); valuemask |= GCClipMask; }
      else
	if (eq (STACK_4, `:NONE`) || eq (STACK_4, NIL))
	  { values.clip_mask = None; valuemask |= GCClipMask; }
	else
	  non_trivial_clip_mask_p = 1;
    }

  // Now handle the :dashes argument, same procedure as above.
  if (!eq (STACK_1, unbound))
    {
      if (uint8_p (STACK_1))	// simple argument
	{ values.dashes = get_uint8 (STACK_1); valuemask |= GCDashList; }
      else
	non_trivial_dashes_p = 1;
    }
  
  if (!eq(STACK_(25), unbound) && !eq(STACK_(25), NIL)) /* :drawable */
    {
      Display *dpy;
      Drawable da = get_drawable_and_display (STACK_(25), &dpy);
      GC gcon;

      begin_call ();
      gcon = XCreateGC (dpy, da, valuemask, &values);
      end_call ();

      value1 = make_gcontext (get_display_obj STACK_(25), gcon);
      mv_count = 1;

      if (non_trivial_clip_mask_p)
	{
	  // User specified a clip mask, which is a rect-seq.
	  // Use the (SETF GCONTEXT-CLIP-MASK) function to set it up.

	  pushSTACK (value1);	// save gcontext
	  pushSTACK (STACK_5);	// the :clip-mask argument
	  pushSTACK (STACK_1);	// the gcontext again
	  pushSTACK (STACK_6);	// the :clip-ordering argument
	  {
	    Values C_xlib_gcontext_clip_mask_setter (void);
	    C_xlib_gcontext_clip_mask_setter ();
	  }
	  value1 = popSTACK();	// restore gcontext
	}

      if (non_trivial_dashes_p)
	{
	  // Same procedure as above
	  pushSTACK (value1);	// save gcontext
	  pushSTACK (STACK_2);	// the :dashes argument
	  pushSTACK (STACK_1);	// gcontext again
	  {
	    Values C_xlib_gcontext_dashes_setter (void);
	    C_xlib_gcontext_dashes_setter ();
	  }
	  value1 = popSTACK();	// restore gcontext
	}
    }
  else
    {
      pushSTACK (TheSubr (subr_self)->name);
      fehler (error, "~: At least :DRAWABLE should be specifed.");
    }
  skipSTACK (26);
}


/* 5.3 Graphics Context Attributes */

// XGetGCValues (3x11) says:
//   [...]
//   Also note that an invalid resource ID (with one or more of the three
//   most-significant bits set to one) will be returned for GCFont, GCTile, and
//   GCStipple if the component has never been explicitly set by the client.
//   [...]
//
//
// FIXME: What about 64bit (or probably 36bit) architectures?
//        [I have to look into the source code of libX, but I am afraid,
//         that they think every machine is 32bit]
//
#define invalid_xid_p(xid) ((xid) & 0xE0000000)

##define DEF_GCONTEXT_SLOT_GETTER(lspnam, type, slot, mask)			\
	defun XLIB:GCONTEXT-##lspnam (1)					\
	{									\
	  XGCValues values;							\
	  Display *dpy;								\
	  GC gcon = get_gcontext_and_display (STACK_0, &dpy);			\
	  begin_call ();							\
	  XGetGCValues (dpy, gcon, mask, &values);				\
	  end_call ();								\
	  value1 = make_##type (values.slot); mv_count = 1;			\
	  skipSTACK (1);							\
	}

##define DEF_GCONTEXT_SLOT_GETTER2(lspnam, type, slot, mask)			\
	defun XLIB:GCONTEXT-##lspnam (1)					\
	{									\
	  XGCValues values;							\
	  Display *dpy;								\
	  GC gc = get_gcontext_and_display (STACK_0, &dpy);			\
	  begin_call (); XGetGCValues (dpy, gc, mask, &values); end_call ();	\
	  if (invalid_xid_p (values.slot))					\
	    value1 = NIL;							\
	  else									\
	    value1 = make_##type (get_display_obj (STACK_0), values.slot); 	\
          mv_count = 1;								\
	  skipSTACK (1);							\
	}

##define DEF_GCONTEXT_SLOT_SETTER(lspnam, type, slot, mask)			\
	defun XLIB:GCONTEXT-##lspnam##-setter (2)				\
	{									\
	  XGCValues values;							\
	  Display *dpy;								\
	  GC gcon = get_gcontext_and_display (STACK_0, &dpy);			\
	  values.slot = get_##type (STACK_1);					\
	  begin_call ();							\
	  XChangeGC (dpy, gcon, mask, &values);					\
	  end_call ();								\
	  value1 = STACK_1; mv_count = 1;					\
	  skipSTACK (2);							\
	}

##define DEF_GCONTEXT_SLOT(lspnam, type, slot, mask)				\
	DEF_GCONTEXT_SLOT_GETTER (lspnam, type, slot, mask)			\
	DEF_GCONTEXT_SLOT_SETTER (lspnam, type, slot, mask)

##define DEF_GCONTEXT_SLOT2(lspnam, type, slot, mask)				\
	DEF_GCONTEXT_SLOT_GETTER2 (lspnam, type, slot, mask)			\
	DEF_GCONTEXT_SLOT_SETTER (lspnam, type, slot, mask)

/*----------------------------------------------------------------------------------------------
                    lisp name       type            C slot              mask 
 ----------------------------------------------------------------------------------------------*/
DEF_GCONTEXT_SLOT  (arc-mode,       arc_mode,       arc_mode,           GCArcMode)
DEF_GCONTEXT_SLOT  (background,     pixel,          background,         GCBackground)
DEF_GCONTEXT_SLOT  (cap-style,      cap_style,      cap_style,          GCCapStyle)
DEF_GCONTEXT_SLOT  (clip-x,         sint16,         clip_x_origin,      GCClipXOrigin)
DEF_GCONTEXT_SLOT  (clip-y,         sint16,         clip_y_origin,      GCClipYOrigin)
DEF_GCONTEXT_SLOT  (dash-offset,    uint16,         dash_offset,        GCDashOffset)   
DEF_GCONTEXT_SLOT  (exposures,      bool,           graphics_exposures, GCGraphicsExposures)
DEF_GCONTEXT_SLOT  (fill-rule,      fill_rule,      fill_rule,          GCFillRule)
DEF_GCONTEXT_SLOT  (fill-style,     fill_style,     fill_style,         GCFillStyle)
DEF_GCONTEXT_SLOT  (foreground,     pixel,          foreground,         GCForeground)
DEF_GCONTEXT_SLOT  (function,       gc_function,    function,           GCFunction)
DEF_GCONTEXT_SLOT  (join-style,     join_style,     join_style,         GCJoinStyle)
DEF_GCONTEXT_SLOT  (line-style,     line_style,     line_style,         GCLineStyle)
DEF_GCONTEXT_SLOT  (line-width,     sint16,         line_width,         GCLineWidth)
DEF_GCONTEXT_SLOT  (plane-mask,     uint32,         plane_mask,         GCPlaneMask)
DEF_GCONTEXT_SLOT  (subwindow-mode, subwindow_mode, subwindow_mode,     GCSubwindowMode)
DEF_GCONTEXT_SLOT  (ts-x,           sint16,         ts_x_origin,        GCTileStipXOrigin)
DEF_GCONTEXT_SLOT  (ts-y,           sint16,         ts_y_origin,        GCTileStipYOrigin)
DEF_GCONTEXT_SLOT2 (stipple,        pixmap,         stipple,            GCStipple)
DEF_GCONTEXT_SLOT2 (tile,           pixmap,         tile,               GCTile)

// What about getting clip-mask?!

defun XLIB:GCONTEXT-CACHE-P (1)
{
  Display *dpy;
  unused get_gcontext_and_display (STACK_0, &dpy);
  // libX seems to cache all GCs
  value1 = T; mv_count = 1;
  skipSTACK (1);
}
     
defun XLIB:GCONTEXT-CACHE-P-SETTER (2)
{
  Display *dpy;
  unused get_gcontext_and_display (STACK_0, &dpy);
  if (eq (STACK_1, NIL))
    {
      pushSTACK (TheSubr (subr_self)->name);
      fehler (error, "~: This CLX implemenation does not allow uncached graphics contexts.");
    }
  value1 = STACK_1; mv_count = 1;
  skipSTACK (2);
}

//
// xlib:gcontext-dashes gcontext
//
defun XLIB:GCONTEXT-DASHES (1)
{
  unused get_gcontext_and_display (STACK_0, 0);	// only type checking here
  
  // Now see if there is a %dashes slot?
  pushSTACK (STACK_0); 		// the gcontext instance
  pushSTACK (`xlib::%dashes`);	// slot
  funcall (L(slot_boundp), 2);	// is it bound?

  if (nullp (value1))
    {
      // Slot unbound --> oops, not set, so return default value
      value1 = make_uint8 (0); // FIXME: right?
      mv_count = 1;
    }
  else
    {
      pushSTACK (STACK_0); 		// the gcontext instance
      pushSTACK (`xlib::%dashes`);	// slot
      funcall (L(slot_value), 2);	//
      mv_count = 1;

      // Simply return what is there.
      // Or better copy it? Well, if the luser fools around with what he has
      // found, he shoot only himself in the foot, not me. So we need no copy here.
    }
  
  // all done
  skipSTACK (1);
}

//     
// (setf xlib:gcontext-dashes) dashes gcontext
//
defun XLIB:GCONTEXT-DASHES-SETTER (2)
{
  XGCValues values;
  Display *dpy;
  GC gcon = get_gcontext_and_display (STACK_0, &dpy);

  if (uint8_p (STACK_1))
    {
      values.dashes = get_uint8 (STACK_1);
      begin_call ();
        XChangeGC (dpy, gcon, GCDashList, &values);
      end_call ();

      // Now set the %dashes slot.
      pushSTACK (STACK_0); 				// The instance, hence the gcontext
      pushSTACK (`xlib::%dashes`);			// slot
      pushSTACK (make_uint8 ((uint8)values.dashes));	// value
      funcall (L(set_slot_value), 3);
    }
  else
    {
      uintC n;
      
      // Now STACK_1 is required to be a non-empty sequence
      pushSTACK (subr_self);
      pushSTACK (STACK_2);
      funcall (L(length), 1);
      subr_self = popSTACK ();
      n = get_fixnum (value1);

      if (n < 1)
	{
	  pushSTACK (TheSubr(subr_self)->name);
	  fehler (error, "~: The dash list should be non-empty.");
	}

      {
	// FIXME: For efficiency reasons, we should look, if user gave already a byte vector.
	//        [probably via with-gcontext].
	//
	uintC i;
	
	// Allocate a simple [nah, semi-simple] vector of uint8's:
	pushSTACK (allocate_byte_vector (/* elm type: */ Atype_8Bit, /* len: */ n));
	
	// Copy the values from the dash-list argument into the newly created byte-vector representation
	for (i = 0; i < n; i++)
	  {
	    pushSTACK (subr_self); 	       // yes, we are doing funcall here
	    pushSTACK (STACK_3);	       // the dashes-list argument
	    pushSTACK (fixnum (i));	       // index
	    funcall (L(elt), 2);	       // (elt dashes index)
	    subr_self = popSTACK ();	       // restore
	    ((uint8*)TheSbvector (TheIarray (STACK_0)->data)->data)[i] = get_uint8 (value1);
	  }
	
	// The XSetDashes routine requires also the dash_offset, so retrieve it first.
	begin_call ();
	  XGetGCValues (dpy, gcon, GCDashOffset, &values);
	  XSetDashes (dpy, gcon, values.dash_offset, ((char*)(&TheSbvector (TheIarray (STACK_0)->data)->data[0])), n);
	end_call ();

	// Now install the byte-vector into the %dashes slot:
	pushSTACK (STACK_1); 			// The instance, hence the gcontext
	pushSTACK (`xlib::%dashes`);		// slot
	pushSTACK (STACK_2);		        // value, the byte-vector
	funcall (L(set_slot_value), 3);
	skipSTACK (1);				// clean up; pop the byte-vector
      }
    }
  
  value1 = STACK_1; mv_count = 1;
  skipSTACK (2);
  // all done
}

//
// XLIB:GCONTEXT-CLIP-MASK gcontext
//
defun XLIB:GCONTEXT-CLIP-MASK (1)
{
  unused get_gcontext_and_display (STACK_0, 0);	// only type checking here
  
  pushSTACK (STACK_0); 			// the gcontext instance
  pushSTACK (`xlib::%clip-mask`);	// slot
  funcall (L(slot_boundp), 2);		// is it bound?

  if (nullp (value1))
    {
      value1 = `:NONE`;
      skipSTACK (1);
    }
  else
    {
      // the gcontext instance is already on the stack
      pushSTACK (`xlib::%clip-mask`);	// slot
      funcall (L(slot_value), 2);	//
    }
  
  // all done
  mv_count = 1;
}


//
// (SETF XLIB:GCONTEXT-CLIP-MASK) clip-mask gcontext &optional ordering
//
defun XLIB:GCONTEXT-CLIP-MASK-SETTER (2,1)
{
  Display *dpy;
  GC gcontext;

  gcontext = get_gcontext_and_display (STACK_1, &dpy);

  if (eq (STACK_2, `:NONE`) || eq (STACK_2, NIL))
    {
      begin_call ();
        XSetClipMask (dpy, gcontext, None);
      end_call ();
    }
  else
  if (pixmap_p (STACK_2))
    {
      Pixmap pixmap = get_pixmap (STACK_2);
      begin_call ();
        XSetClipMask (dpy, gcontext, pixmap);
      end_call ();
    }
  else
    {
      // FIXME: We could use a more effient representation for the clip-mask in the gcontext.
      //        We should think about the portability of using a halfword-vector and then beam
      //        the data directly into the rectangles vector.
      
      int ordering, n;
      if (eq (STACK_0, unbound) || eq (STACK_0, NIL))
	ordering = Unsorted;
      else
	ordering = get_ordering (STACK_0);

      pushSTACK (subr_self);
      pushSTACK (STACK_3);
      funcall (L(length), 1);
      subr_self = popSTACK();
      n = get_sint32 (value1);
      
      // See if length is a multiple of 4?
      if (n%4)
	{
	  pushSTACK (TheSubr (subr_self)->name);
	  pushSTACK (`XLIB::RECT-SEQ`);
	  pushSTACK (fixnum (n));
	  fehler (error, "~: Argument is no proper ~; Length of sequence, ~, is no multiple of four.");
	}

      {
	int i;

	DYNAMIC_ARRAY (rectangles, XRectangle, n/4);

	// btw all this copying of sequences, could probably be warped into some
	// common function/macro.  This function should also cdr-down lists,
	// since elt is highly unappropriate for lists. [most cases we get
	// lists.]
	//

	for (i = 0; i < n; i += 4)
	  {
	    pushSTACK (STACK_2); pushSTACK (fixnum (i+0)); funcall (L(elt), 2);
	    rectangles[i/4].x = get_sint16(value1);
	    
	    pushSTACK (STACK_2); pushSTACK (fixnum (i+1)); funcall (L(elt), 2);
	    rectangles[i/4].y = get_sint16(value1);
	    
	    pushSTACK (STACK_2); pushSTACK (fixnum (i+2)); funcall (L(elt), 2);
	    rectangles[i/4].width = get_sint16(value1);
	    
	    pushSTACK (STACK_2); pushSTACK (fixnum (i+3)); funcall (L(elt), 2);
	    rectangles[i/4].height = get_sint16(value1);
	  }

	{
	  XGCValues values;
	  begin_call ();
	    XGetGCValues (dpy, gcontext, GCClipXOrigin|GCClipYOrigin, &values);
	    XSetClipRectangles (dpy, gcontext, values.clip_x_origin, values.clip_y_origin, rectangles, n/4, ordering);
	  end_call ();
	}

	// ok. now copy the value given by user, so if he messes around with
	// what he gave as argument, it will not affect the saved value.

	pushSTACK (STACK_2);
	funcall (L(copy_seq), 1);
	STACK_2 = value1;

	FREE_DYNAMIC_ARRAY (rectangles);
      }
    }

  // Now save the value just set in the %clip-mask slot.
  pushSTACK (STACK_1); 				// The instance, hence the gcontext
  pushSTACK (`xlib::%clip-mask`);		// slot
  pushSTACK (STACK_4);				// value
  funcall (L(set_slot_value), 3);

  value1 = STACK_2; mv_count = 1;
  skipSTACK (3);
}

defun XLIB:GCONTEXT-FONT (1, 1)
{
  Display *dpy;
  GC gc;
  int pseudo_font_p;
  XGCValues values;
  
  pseudo_font_p = (!eq(STACK_0, unbound)) ? get_bool (STACK_0) : 0;
  if (pseudo_font_p) NOTIMPLEMENTED;

  gc = get_gcontext_and_display (STACK_1, &dpy);
  
  begin_call ();
  XGetGCValues (dpy, gc, GCFont, &values);
  end_call ();
  
  if (invalid_xid_p (values.font))
    value1 = NIL;
  else
    value1 = make_font (get_display_obj (STACK_1), values.font);
  
  mv_count = 1; 
  skipSTACK (2); 
}

defun XLIB:GCONTEXT-FONT-SETTER (2, 1)
     // want to say: defun (SETF XLIB:GCONTEXT-FONT) (2, 1)
{
  int pseudo_font_p;
  XGCValues values;
  Display *dpy;
  GC gc;
  
  gc = get_gcontext_and_display (STACK_1, &dpy);
  
  pseudo_font_p = (!eq(STACK_0, unbound)) ? get_bool (STACK_0) : 0;
  if (pseudo_font_p) NOTIMPLEMENTED;
  values.font = get_font (STACK_2); 

  begin_call ();
  XChangeGC (dpy, gc, GCFont, &values);
  end_call ();

  value1 = STACK_2; mv_count = 1; 
  skipSTACK (3); 
}

// Standard clx objects look:

defun XLIB:GCONTEXT-ID (1)
{
  value1 = make_uint32 (XGContextFromGC (get_gcontext (popSTACK ()))); mv_count = 1;
}

local void query_best_X (Status (*query) (Display*, Drawable,
					  unsigned int, unsigned int,
					  unsigned int *, unsigned int *))
{
  unsigned int width, height, x, y;
  Display *dpy;
  Drawable da = get_drawable_and_display (STACK_0, &dpy);
  x = get_uint16 (STACK_2);
  y = get_uint16 (STACK_1);

  begin_call ();
  query (dpy, da, x, y, &width, &height);
  end_call ();
  
  pushSTACK (make_uint16 (height));
  pushSTACK (make_uint16 (width));
  value1 = STACK_0;
  value2 = STACK_1;
  mv_count = 2;
  skipSTACK (5);
}

defun XLIB:QUERY-BEST-STIPPLE (3)
{
  query_best_X (XQueryBestStipple);
}

defun XLIB:QUERY-BEST-TILE (3)
{
  query_best_X (XQueryBestTile);
}

/* 5.3  Copying Graphics Contexts */
defun XLIB:COPY-GCONTEXT (2)
{
  Display *dpy;
  GC gcon1 = get_gcontext_and_display (STACK_1, &dpy);
  GC gcon2 = get_gcontext (STACK_0);
  begin_call ();
  XCopyGC (dpy, gcon1, 0x7FFFFFUL, gcon2);
  end_call ();
  value1 = NIL; mv_count = 0;
  skipSTACK (2);
}

defun XLIB:COPY-GCONTEXT-COMPONENTS (2, 0, rest, nokey, 0, NIL)
{
  unsigned i;
  unsigned long mask = 0;
  GC gcon1, gcon2;
  Display *dpy;
  
  for (i = 0; i < argcount-2; i++)
    {
      mask |= get_gcontext_key (STACK_0);
      popSTACK ();
    }

  gcon1 = get_gcontext_and_display (STACK_0, &dpy);
  gcon2 = get_gcontext (STACK_1);

  begin_call ();
  XCopyGC (dpy, gcon2, mask, gcon1);
  end_call ();
  value1 = NIL; mv_count = 0;
  skipSTACK (2);
}

/* 5.4  Destroying Graphics Contexts */
defun XLIB:FREE-GCONTEXT (1)
{
  Display *dpy;
  GC gcon = get_gcontext_and_display (STACK_0, &dpy);
  begin_call();
  XFreeGC (dpy, gcon);
  end_call ();
  skipSTACK (1);
  value1= NIL; mv_count = 1;
}

/* 5.5  Graphics Context Cache */
defun XLIB:FORCE-GCONTEXT-CHANGES (1)
{
  Display *dpy;
  GC gcon = get_gcontext_and_display (STACK_0, &dpy);
  XFlushGC (dpy, gcon);		// This function is actually undocumented
  value1= NIL; mv_count = 1;
}

// ----------------------------------------------------------------------------------------------------
//  WITH-GCONTEXT
//
//  Method: with-gcontext should 'bind' some gcontext slots, so we have to save
//  them and restore them after exiting the body (probably within an
//  unwind-protect). The core of the with-gcontext macro looks like this:
//
//   (let ((saved (%save-gcontext-compoments gcon mask)))
//     (unwind-protect
//           ,body
//        (%restore-gcontext-compoments gcon saved)))
//
//  %save-g.-c. and %restore-g.-c. work by putting the XGCValues structure into
//  a bitvector. Plain and simple.
//
//  clip-mask and the dashes-list are to be saved and restored by the Lisp code.
//
//  Another method would be copy the gcontext and modify the fptr. This would
//  then also work with dash-list and clip-mask on gcontexts modified by some C
//  code. [I plan to incooperate other C libs here.]

//
// xlib:%gcontext-key->mask key
//
//  Finds the libX mask bit for an given symbolic representation of the slot.
//
defun XLIB:%GCONTEXT-KEY->MASK (1)
{
  value1 = make_uint32 (get_gcontext_key (popSTACK ()));
  mv_count = 1;
}

//
// data structure in which the values survive.
//
typedef struct
{
  uint32 mask;			// values mask, specifies which values where saved
  XGCValues values;		// the values itself in native C rep.
} saved_gcontext_values;

//
// xlib:%save-gcontext-components gcontext components
//
//  Returns the gcontext components selected by 'components', a mask32, and returns
//  them in some compact object, which should be considered opaque.
//
defun XLIB:%SAVE-GCONTEXT-COMPONENTS (2) 
{
  saved_gcontext_values values;
  Display *dpy;
  GC gcontext = get_gcontext_and_display (STACK_1, &dpy);
  values.mask = get_uint32 (STACK_0);

  if (values.mask & GCDashList)
    {
      // the dash list itself is saved by Lisp code, but we take care of the dash offset.
      values.mask |= GCDashOffset;
      values.mask &= ~GCDashList;   // do not make any nonsense here.
    }

  if (values.mask & GCClipMask)
    {
      // same story as above.
      values.mask |= GCClipXOrigin|GCClipYOrigin;
      values.mask &= ~GCClipMask;
    }
  
  begin_call ();
  XGetGCValues (dpy, gcontext, values.mask, &values.values);
  //TODO: What to todo on failure of xgetgcvalues?
  end_call ();
  
  // Allocate a new bit vector, which should hold the requested components
  value1 = allocate_bit_vector (8 * sizeof (values));
  begin_call ();
  memcpy (TheSbvector (value1)->data, &values, sizeof (values)); // memcpy considered harmful
  end_call ();
  mv_count = 1;
  skipSTACK (2);
}

//
// xlib:%restore-gcontext-components gcontext values
//
//  Counterpart of xlib:%save-gcontext-components: Installs the saved values.
//  Note that the components mask is not needed, since it is saved together
//  with the values to avoid malformed restores.
//
defun XLIB:%RESTORE-GCONTEXT-COMPONENTS (2)
{
  saved_gcontext_values values;
  Display *dpy;
  GC gcontext = get_gcontext_and_display (STACK_1, &dpy);
  
  memcpy (&values, TheSbvector (STACK_0)->data, sizeof (values));

  // do not attempt to restore invalid resource ids
  // Probably we want to reinvalidate them, but that seems not to be possible.
  if (invalid_xid_p (values.values.font))    values.mask&=~GCFont;
  if (invalid_xid_p (values.values.tile))    values.mask&=~GCTile;
  if (invalid_xid_p (values.values.stipple)) values.mask&=~GCStipple;
  
  begin_call ();
    XChangeGC (dpy, gcontext, values.mask, &values.values);
  end_call ();
  
  skipSTACK (2);
  value1 = NIL; mv_count = 1;
}


// ----------------------------------------------------------------------------------------------------
//  Chapter 6   Graphics Operations
// ----------------------------------------------------------------------------------------------------

/* 6.2  Area and Plane Operations */

defun XLIB:CLEAR-AREA (1, 0, norest, key, 5, (:X :Y :WIDTH :HEIGHT :EXPOSURES-P))
{
  Display *dpy;
  Window win      = get_drawable_and_display (STACK_5, &dpy);
  int x           = gunboundp (STACK_4) ? 0 : get_sint16 (STACK_4);
  int y           = gunboundp (STACK_3) ? 0 : get_sint16 (STACK_3);
  int w           = gunboundp (STACK_1) ? 0 : get_uint16 (STACK_2);
  int h           = gunboundp (STACK_1) ? 0 : get_uint16 (STACK_1);
  int exposures_p = !gunboundp (STACK_0);
  
  begin_call ();
  XClearArea (dpy, win, x,y,w,h, exposures_p);
  end_call ();
  
  skipSTACK (6);
  value1 = NIL; mv_count = 0;
}

//
// XLIB:COPY-AREA source gcontext source-x source-y width height destination destination-x destionation-y
//
defun XLIB:COPY-AREA (9)
{
  int    dest_y = get_sint16 (popSTACK ());
  int    dest_x = get_sint16 (popSTACK ());
  Drawable dest = get_drawable (popSTACK ());
  int    height = get_sint16 (popSTACK ());
  int     width = get_sint16 (popSTACK ());
  int     src_y = get_sint16 (popSTACK ());
  int     src_x = get_sint16 (popSTACK ());
  GC         gc = get_gcontext (popSTACK ());
  Display  *dpy;
  Drawable  src = get_drawable_and_display (popSTACK (), &dpy);
      
  begin_call ();
  XCopyArea (dpy, src, dest, gc, src_x, src_y, width, height, dest_x, dest_y);
  end_call ();
      
  value1 = NIL; mv_count = 1;
}

//
// XLIB:COPY-PLANE source gcontext plane source-x source-y width height destination destionation-x destionation-y
//
defun XLIB:COPY-PLANE (10)
{
  //WAS: invoke (XCopyPlane, 10, 'v', "D1dgiiiiidii");
  int          dest_y = get_sint16 (STACK_0);
  int          dest_x = get_sint16 (STACK_1);
  Drawable       dest = get_drawable (STACK_2);
  int          height = get_sint16 (STACK_3);
  int           width = get_sint16 (STACK_4);
  int           src_y = get_sint16 (STACK_5);
  int           src_x = get_sint16 (STACK_6);
  unsigned long plane = get_uint32 (STACK_7);
  GC               gc = get_gcontext (STACK_8);
  Display        *dpy;
  Drawable        src = get_drawable_and_display (STACK_9, &dpy);

  begin_call ();
  XCopyPlane (dpy, src, dest, gc, src_x, src_y, width, height, dest_x, dest_y, plane);
  end_call ();
      
  skipSTACK (10);
  value1 = NIL; mv_count = 1;
}

/* 6.3  Drawing Points */
//
//  XLIB:DRAW-POINT drawable gcontext x y
//
defun XLIB:DRAW-POINT (4)
{
  //WAS: invoke (XDrawPoint, 4, "D1dgii");
  int        y = get_sint16 (popSTACK ());
  int        x = get_sint16 (popSTACK ());
  GC        gc = get_gcontext (popSTACK ());
  Display *dpy;
  Drawable  da = get_drawable_and_display (popSTACK (), &dpy);

  begin_call ();
  XDrawPoint (dpy, da, gc, x, y);
  end_call ();
  
  value1 = NIL; mv_count = 1;
}

//
//  XLIB:DRAW-POINTS drawable gcontext points &optional relative-p
//
defun XLIB:DRAW-POINTS (3, 1)
{
  Display   *dpy;
  Drawable    da = get_drawable_and_display (STACK_3, &dpy);
  GC          gc = get_gcontext (STACK_2);
  int relative_p = (eq (STACK_0, unbound) ? 0 : !nullp (STACK_0));
  int npts, i;
  
  // Find number of points
  pushSTACK (STACK_1); funcall (L(length), 1); npts = get_uint32 (value1);
  
  {
    DYNAMIC_ARRAY (pts, XPoint, npts);

    for (i = 0; i < npts; i++)
      {
	pushSTACK (STACK_1);	// points argument
	pushSTACK (fixnum (i));	// index
	funcall (L(elt), 2);
	pushSTACK (value1);	// save element
	
	pushSTACK (STACK_0); pushSTACK (fixnum (0)); funcall (L(elt), 2);
	pts[i].x = get_sint16(value1);
	
	pushSTACK (fixnum (1)); funcall (L(elt), 2);
	pts[i].y = get_sint16(value1);
      }

    begin_call ();
    XDrawPoints (dpy, da, gc, pts, npts, relative_p ? CoordModePrevious : CoordModeOrigin);
    end_call ();

    FREE_DYNAMIC_ARRAY (pts);
  }
  
  value1 = NIL; mv_count = 1;
  skipSTACK (4);
}

/* 6.4  Drawing Lines */
//
// XLIB:DRAW-LINE drawable gcontext x0 y0 x1 y1 &optional relative-p
//
defun XLIB:DRAW-LINE (6, 1)
{
  int relative_p, x1,y1,x2,y2;
  GC gc;
  Drawable da;
  Display *dpy;
  
  relative_p = (eq (STACK_0, unbound) || eq (STACK_0,NIL)) ? 0 : 1;
  x1 = get_sint16 (STACK_4); y1 = get_sint16 (STACK_3);
  x2 = get_sint16 (STACK_2); y2 = get_sint16 (STACK_1);
  if (relative_p) { x2 += x1; y2 += y1; }

  da = get_drawable_and_display (STACK_6, &dpy);
  gc = get_gcontext (STACK_5);

  begin_call ();
  XDrawLine (dpy, da, gc, x1, y1, x2, y2);
  end_call ();
  
  skipSTACK (7);
  value1 = NIL; mv_count = 1;
}

// DEUTSCH Kuerzester Mathematikerwitz: epsilon kleiner null.
// ENGLISH Shortest mathematician's joke: epsilon less than zero.

//
// XLIB:DRAW-LINES [5]drawable [4]gcontext [3]points &key [2]:relative-p [1]:fill-p [0](:shape :complex)
//
defun XLIB:DRAW-LINES (3, 0, norest, key, 3, (:RELATIVE-P :FILL-P :SHAPE))
{
  Display   *dpy;
  Drawable    da = get_drawable_and_display (STACK_5, &dpy);
  GC          gc = get_gcontext (STACK_4);
  int relative_p = (eq (STACK_2, unbound) ? 0 : !nullp (STACK_2));
  int     fill_p = (eq (STACK_1, unbound) ? 0 : !nullp (STACK_1));
  int      shape = (eq (STACK_0, unbound) ? Complex : get_shape (STACK_0));
  int npoints,i;
  
  // Find number of points
  pushSTACK (STACK_3); funcall (L(length), 1); npoints = get_uint32 (value1) /2;
  
  {
    DYNAMIC_ARRAY (points, XPoint, npoints);
    
    for (i = 0; i < npoints; i++)
      {
	pushSTACK (STACK_3); pushSTACK (fixnum (2*i + 0)); funcall (L(elt), 2);
	points[i].x = get_sint16(value1);
	
	pushSTACK (STACK_3); pushSTACK (fixnum (2*i + 1)); funcall (L(elt), 2);
	points[i].y = get_sint16(value1);
      }

    begin_call ();
    if (fill_p)
      XFillPolygon (dpy, da, gc, points, npoints, shape, relative_p ? CoordModePrevious : CoordModeOrigin);
    else
      XDrawLines (dpy, da, gc, points, npoints, relative_p ? CoordModePrevious : CoordModeOrigin);
    end_call ();

    FREE_DYNAMIC_ARRAY (points);
  }
  
  value1 = NIL; mv_count = 1;
  skipSTACK (6);
}

//
// XLIB:DRAW-SEGMENTS drawable gcontext segments
//
defun XLIB:DRAW-SEGMENTS (3) 
{
  Display *dpy;
  Drawable da  = get_drawable_and_display (STACK_2, &dpy);
  GC gc        = get_gcontext (STACK_1);
  int nsegments,i;
  
  // Find number of segments
  pushSTACK (STACK_0); funcall (L(length), 1); nsegments = get_uint32 (value1)/4;
  
  {
    DYNAMIC_ARRAY (segments, XSegment, nsegments);
    
    for (i = 0; i < nsegments; i++)
      {
	pushSTACK (STACK_0); pushSTACK (fixnum (4*i + 0)); funcall (L(elt), 2);
	segments[i].x1 = get_sint16(value1);
	
	pushSTACK (STACK_0); pushSTACK (fixnum (4*i + 1)); funcall (L(elt), 2);
	segments[i].y1 = get_sint16(value1);
	
	pushSTACK (STACK_0); pushSTACK (fixnum (4*i + 2)); funcall (L(elt), 2);
	segments[i].x2 = get_sint16(value1);
	
	pushSTACK (STACK_0); pushSTACK (fixnum (4*i + 3)); funcall (L(elt), 2);
	segments[i].y2 = get_sint16(value1);
      }

    begin_call ();
    XDrawSegments (dpy, da, gc, segments, nsegments);
    end_call ();

    FREE_DYNAMIC_ARRAY (segments);
  }
  
  value1 = NIL; mv_count = 1;
  skipSTACK (3);
}

/* 6.5  Drawing Rectangles */
//
//  xlib:draw-rectange drawable gcontext x y width height &optional (fill-p nil)
//
defun XLIB:DRAW-RECTANGLE (6, 1)
{
  Display *dpy;
  int fill_p  = eq (STACK_0, unbound) ? 0 : get_bool (STACK_0);
  int x       = get_sint16 (STACK_4);
  int y       = get_sint16 (STACK_3);
  int w       = get_sint16 (STACK_2);
  int h       = get_sint16 (STACK_1);
  GC gcon     = get_gcontext (STACK_5);
  Drawable da = get_drawable_and_display (STACK_6, &dpy);

  begin_call ();
  (fill_p ? XFillRectangle : XDrawRectangle) (dpy, da, gcon, x, y, w, h);
  end_call ();
  
  skipSTACK (7);
  value1 = NIL; mv_count = 1;
}

//
//  XLIB:DRAW-RECTANGLES drawable gcontext rectangles &optional fill-p
//
defun XLIB:DRAW-RECTANGLES (3, 1)
{
  Display *dpy;
  Drawable da  = get_drawable_and_display (STACK_3, &dpy);
  GC gc        = get_gcontext (STACK_2);
  int fill_p   = (eq (STACK_0, unbound) ? 0 : !nullp (STACK_0));
  int nrectangles,i;
  
  // Find number of rectangles
  pushSTACK (STACK_1); funcall (L(length), 1); nrectangles = get_uint32 (value1) /4;
  
  {
    DYNAMIC_ARRAY (rectangles, XRectangle, nrectangles);
    
    for (i = 0; i < nrectangles; i++)
      {
	pushSTACK (STACK_1); pushSTACK (fixnum (i*4 + 0)); funcall (L(elt), 2);
	rectangles[i].x = get_sint16(value1);
	
	pushSTACK (STACK_1); pushSTACK (fixnum (i*4 + 1)); funcall (L(elt), 2);
	rectangles[i].y = get_sint16(value1);
	
	pushSTACK (STACK_1); pushSTACK (fixnum (i*4 + 2)); funcall (L(elt), 2);
	rectangles[i].width = get_sint16(value1);
	
	pushSTACK (STACK_1); pushSTACK (fixnum (i*4 + 3)); funcall (L(elt), 2);
	rectangles[i].height = get_sint16(value1);
      }

    begin_call ();
    (fill_p ? XFillRectangles : XDrawRectangles) (dpy, da, gc, rectangles, nrectangles);
    end_call ();

    FREE_DYNAMIC_ARRAY (rectangles);
  }
  
  value1 = NIL; mv_count = 1;
  skipSTACK (4);
}

/* 6.6  Drawing Arcs */
//
//  XLIB:DRAW-ARC drawable gcontext x y width height angle1 angle2 &optional fill-p
//
defun XLIB:DRAW-ARC (0, 0, rest, nokey, 0, NIL)
{
  int fill_p, x,y,w,h, ang1, ang2;
  GC gcon;
  Display *dpy;
  Drawable da;
  
  ASSERT ((argcount >= 8) && (argcount <= 9));
  fill_p = (argcount == 9) ? (!nullp (popSTACK ())) : 0;
  x = get_sint16 (STACK_5); y = get_sint16 (STACK_4);
  w = get_sint16 (STACK_3); h = get_sint16 (STACK_2);
  ang1 = get_angle (STACK_1); ang2 = get_angle (STACK_0);

  gcon = get_gcontext (STACK_6);
  da = get_drawable_and_display (STACK_7, &dpy);

  begin_call ();
  (fill_p ? XFillArc : XDrawArc) (dpy, da, gcon, x, y, w, h, ang1, ang2);
  end_call ();
  
  skipSTACK (8);
  value1 = NIL; mv_count = 0;
}

//
// XLIB:DRAW-ARCS drawable gcontext arcs &optional fill-p
//  arcs = ((x y width height angle1 angle2) ...)
//
defun XLIB:DRAW-ARCS (3, 1)
{
  Display *dpy;
  Drawable da  = get_drawable_and_display (STACK_3, &dpy);
  GC gc        = get_gcontext (STACK_2);
  int fill_p   = (eq (STACK_0, unbound) ? 0 : !nullp (STACK_0));
  int narcs,i;
  
  // Find number of arcs
  pushSTACK (STACK_1); funcall (L(length), 1); narcs = get_uint32 (value1) /6;
  
  {
    DYNAMIC_ARRAY (arcs, XArc, narcs);
    
    for (i = 0; i < narcs; i++)
      {
	pushSTACK (STACK_1); pushSTACK (fixnum (i*6 + 0)); funcall (L(elt), 2);
	arcs[i].x = get_sint16(value1);
	
	pushSTACK (STACK_1); pushSTACK (fixnum (i*6 + 1)); funcall (L(elt), 2);
	arcs[i].y = get_sint16(value1);
	
	pushSTACK (STACK_1); pushSTACK (fixnum (i*6 + 2)); funcall (L(elt), 2);
	arcs[i].width = get_sint16(value1);
	
	pushSTACK (STACK_1); pushSTACK (fixnum (i*6 + 3)); funcall (L(elt), 2);
	arcs[i].height = get_sint16(value1);
	
	pushSTACK (STACK_1); pushSTACK (fixnum (i*6 + 4)); funcall (L(elt), 2);
	arcs[i].angle1 = get_angle(value1);
	
	pushSTACK (STACK_1); pushSTACK (fixnum (i*6 + 5)); funcall (L(elt), 2);
	arcs[i].angle2 = get_angle(value1);
      }

    begin_call ();
    (fill_p ? XFillArcs : XDrawArcs) (dpy, da, gc, arcs, narcs);
    end_call ();

    FREE_DYNAMIC_ARRAY (arcs);
  }
  
  value1 = NIL; mv_count = 1;
  skipSTACK (4);
}

/* 6.7  Drawing Text */

#ifdef UNICODE
// Conversion from chart array to XChar2b array.
// Returns 1 if a char array was generated, or 2 if a XChar2b array was generated.
static int to_XChar2b (object font, XFontStruct* font_info, const chart* src, XChar2b* dst, unsigned int count)
{
  object encoding;

  pushSTACK (font);
  pushSTACK (`XLIB::ENCODING`);
#if !WITH_SLOT_UP
  funcall (L(slot_value), 2); encoding = value1;
#else
  { object *ptr = slot_up (); skipSTACK(2); encoding = *ptr; }
#endif

  if (font_info->min_byte1 == 0 && font_info->max_byte1 == 0)
    {
      // Linear addressing
      if (!nullp(encoding) && TheEncoding(encoding)->max_bytes_per_char == 1)
        // Special hack: use the font's encoding
        {
          if (count > 0)
            {
              Encoding_wcstombs(encoding)(encoding,nullobj,&src,src+count,(uintB**)&dst,(uintB*)dst+count);
              return 1;
            }
        }
      else
        while (count > 0)
          {
            unsigned int c = as_cint(*src);
            if (c >= font_info->min_char_or_byte2 && c <= font_info->max_char_or_byte2)
              dst->byte2 = c;
            else
              dst->byte2 = font_info->default_char;
            dst->byte1 = 0;
            src++; dst++; count--;
          }
    }
  else
    {
      // Matrix addressing
      unsigned int d = font_info->max_char_or_byte2 - font_info->min_char_or_byte2 + 1;
      while (count > 0)
        {
          unsigned int c = as_cint(*src);
          dst->byte1 = (c / d) + font_info->min_byte1;
          dst->byte2 = (c % d) + font_info->min_char_or_byte2;
          src++; dst++; count--;
        }
    }
  return 2;
}
#endif

void general_draw_text (int image_p)
     // General text drawing routine to not to have to duplicate code for DRAW-GLYPHS and DRAW-IMAGE-GLYPHS.
{
  int size = 0;		// 8 or 16, 0="have to look into the font"
  
  // First of all fetch the arguments
#if 0
  
  STACK_9= drawable;
  STACK_8= gcontext;
  STACK_7= x;
  STACK_6= y;
  STACK_5= sequence;
  STACK_4= start;
  STACK_3= end;
  STACK_2= translate;
  
  if (!eq (STACK_1, unbound))
    width = get_sint16 (STACK_1);
  else
    width = 17;			// Does not mater we ignore this value either way round.
  
  if (!eq (STACK_0, unbound) && !eq (STACK_0, `:DEFAULT`))
    {
      if (eq (STACK_0, fixnum (8)))  size = 8;
      else if (eq (STACK_0, fixnum (16))) size = 16;
      else
	{
	  // Raise type error
	  pushSTACK (STACK_0); // datum
	  pushSTACK (`MEMBER`);  // (MEMBER 8 16 :DEFAULT)
	  pushSTACK (fixnum (8));
	  pushSTACK (fixnum (16));
	  pushSTACK (`:DEFAULT`);
	  funcall (L(list), 4);
	  pushSTACK (value1);	// type
	  my_standard_type_error (TheSubr(subr_self)->name); // XXX
	}
    }
  
  // invoke the translation function:
  //  XLIB:TRANSLATE-DEFAULT src src-start src-end font dst dst-start
  pushSTACK (source);
  pushSTACK (fixnum (start));
  pushSTACK (fixnum (end));
  pushSTACK (*font);
  pushSTACK (*dest);
  pushSTACK (fixnum (dest_start));
  funcall (*translate, 6);
  // Now:  value1 = first-not-done
  //       value2 = NIL or
  //                a new font or
  //                a delta
  //       value3 = NIL or
  //                current width
  //
  new_start = get_fixnum (value1);
  textitem.nchars = new_start - start;
  if (textitem.nchars <= 0)
    {
      // This should be an error
    }
  
  if (size == 8)
    {
      textitem.chars = (ptr into destination);
    }
  else
    {
      // If we are on a litte-endian machine, we have to convert it ...
      textitem.chars = (copy the appropriate portion);
      // Or we do it at the end of the journey...
      
      // .. otherwise just drop the pointer in:
      textitem.chars = (ptr into destination);
    }
  
  textitem.delta = 0;	// default value for delta
  textitem.font = None; // default value for font
  if (mv_count >= 2 && !nullp (value2))
    {
      if (font_p (value2))   textitem.font = value2;
      if (sint16_p (value2)) textitem.delta = get_fixnum (value2);
      // type-error-p?
    }
  
  if (mv_count >= 3 && !nullp (value3))
    {
      // Currently ignored ...
    }
#else
  
  Display *dpy;
  Drawable da = get_drawable_and_display (STACK_9, &dpy);
  GC gcon = get_gcontext (STACK_8);
  int x = get_sint16 (STACK_7);
  int y = get_sint16 (STACK_6);
  int len = vector_length (STACK_5);
  if (!simple_string_p(STACK_5)) { NOTIMPLEMENTED }

#ifdef UNICODE
  {
    object font;
    XFontStruct* font_info = get_font_info_and_display(STACK_8,&font,0);
    const chart* charptr;
    unpack_sstring_alloca(STACK_5,len,0,charptr=);
    { DYNAMIC_ARRAY(str,XChar2b,len);
      if (to_XChar2b(font,font_info,charptr,str,len) == 1)
        {
          begin_call ();
          (image_p ? XDrawImageString : XDrawString) (dpy, da, gcon, x, y, (char*)str, len);
          end_call ();
        }
      else
        {
          begin_call ();
          (image_p ? XDrawImageString16 : XDrawString16) (dpy, da, gcon, x, y, str, len);
          end_call ();
        }
      FREE_DYNAMIC_ARRAY(str);
    }
  }
#else
  { char* str = (char*) &TheSstring(STACK_5)->data[0];
    begin_call ();
    (image_p ? XDrawImageString : XDrawString) (dpy, da, gcon, x, y, str, len);
    end_call ();
  }
#endif

  value1 = NIL; mv_count = 0;
  skipSTACK (10);
#endif
}

//
//  XLIB:DRAW-GLYPH drawable[6] gcontext[5] x[4] y[3] element[2] &key :translate[1] :width[0]
//
defun XLIB:DRAW-GLYPH (5, 0, norest, key, 3, (:TRANSLATE :WIDTH :SIZE))
{
	NOTIMPLEMENTED;
}

//
//  XLIB:DRAW-GLPYHS drawable[9] gcontext[8] x[7] y[6] sequence[5] &key (:start 0)[4] :end[3]
//                   (:translate #'translate-default)[2] :width[1] (:size :default)[0]
//
defun XLIB:DRAW-GLYPHS (5, 0, norest, key, 5, 
			(:START :END :TRANSLATE :WIDTH :SIZE))
{
  general_draw_text (0);
}

defun XLIB:DRAW-IMAGE-GLYPH (5, 0, norest, key, 3, (:TRANSLATE :WIDTH :SIZE)) 
{UNDEFINED}
//
//  XLIB:DRAW-IMAGE-GLPYHS drawable gcontext x y sequence &key (:start 0) :end
//                         (:translate #'translate-default) :width (:size :default)
//
defun XLIB:DRAW-IMAGE-GLYPHS (5, 0, norest, key, 5,
			      (:START :END :TRANSLATE :WIDTH :SIZE))
{
  general_draw_text (1);
}

//
// XLIB:TRANSLATE-DEFAULT src src-start src-end font dst dst-start
//
//  This function is not actually specified in the manual, so I include here the
//  Lisp code from MIT-CLX for reference:
/*
// (defun translate-default (src src-start src-end font dst dst-start)
     ;; dst is guaranteed to have room for (- src-end src-start) integer elements,
     ;; starting at dst-start; whether dst holds 8-bit or 16-bit elements depends
     ;; on context.  font is the current font, if known.  The function should
     ;; translate as many elements of src as possible into indexes in the current
     ;; font, and store them into dst.
     ;;
     ;; The first return value should be the src index of the first untranslated
     ;; element.  If no further elements need to be translated, the second return
     ;; value should be nil.  If a horizontal motion is required before further
     ;; translation, the second return value should be the delta in x coordinate.
     ;; If a font change is required for further translation, the second return
     ;; value should be the new font.  If known, the pixel width of the translated
     ;; text can be returned as the third value; this can allow for appending of
     ;; subsequent output to the same protocol request, if no overall width has
     ;; been specified at the higher level.
     ;; (returns values: ending-index
     ;;                  (OR null horizontal-motion font)
     ;;                  (OR null translated-width))
     (declare (type sequence src)
	      (type array-index src-start src-end dst-start)
	      (type (or null font) font)
	      (type vector dst)
	      (inline graphic-char-p))
     (declare (values integer (or null integer font) (or null integer)))
     font;;not used
     (if (stringp src)
	 (do ((i src-start (index+ i 1))
	      (j dst-start (index+ j 1))
	      (char))
	     ((index>= i src-end)
	      i)
	     (declare (type array-index i j))
	     (if (graphic-char-p (setq char (char src i)))
		 (setf (aref dst j) (char->card8 char))
	       (return i)))
       (do ((i src-start (index+ i 1))
	    (j dst-start (index+ j 1))
	    (elt))
	   ((index>= i src-end)
	    i)
	   (declare (type array-index i j))
	   (setq elt (elt src i))
	   (cond ((and (characterp elt) (graphic-char-p elt))
		  (setf (aref dst j) (char->card8 elt)))
		 ((integerp elt)
		  (setf (aref dst j) elt))
		 (t
		  (return i))))))
   */

defun XLIB:TRANSLATE-DEFAULT (6) //NIM
{UNDEFINED}

// Together with the above function go the two below listed functions:
//  (Which are indeed trivial, since we are on an ASCII system).
//  [We could move it to the Lisp side?]
defun XLIB:CARD8->CHAR (1)
{
  funcall (L(code_char), 1);
}

defun XLIB:CHAR->CARD8 (1)
{
  funcall (L(char_code), 1);
}


// ----------------------------------------------------------------------------------------------------
//  Chapter 7   Images
// ----------------------------------------------------------------------------------------------------

// Note: As you will probably see within a second, this code is still in development.
// ====  There are still not all image formats supported. But the code should be safe
//       (Thus bailing out, when ever some format comes in, which is not fully understood.)
//
//       If you have urgent need for some particular format please mail me, I'll see that
//       I could implement it then. Unfortunately it is not possible for me to test this code
//       fully, since I do not have an X server, which could understand all the possible formats.
//

DEF_ENUM_GETTER (image_format, 3, (ee(`:BITMAP`), ee(`:XY-PIXMAP`), ee(`:Z-PIXMAP`)))

local uint16 get_image_width (void)
{
  pushSTACK (subr_self);
  funcall (`XLIB::IMAGE-WIDTH`, 1);
  subr_self = popSTACK ();
  return get_uint16 (value1);
}

local uint16 get_image_height (void)
{
  pushSTACK (subr_self);
  funcall (`XLIB::IMAGE-HEIGHT`, 1);
  subr_self = popSTACK ();
  return get_uint16 (value1);
}
     
local uint16 get_image_depth (void)
{
  pushSTACK (subr_self);
  funcall (`XLIB::IMAGE-DEPTH`, 1);
  subr_self = popSTACK ();
  return get_uint16 (value1);
}
     
local void ensure_valid_put_image_args (int src_x, int src_y, int w, int h,
					int width, int height)
  // ensure that the src_{x,y} and w, h arguments are valid,
  // signals error if not.
{
  // I find it much easier to express the valid precondition instead of
  // the error condition;
  if (src_x >= 0 && (src_x + w) <= width &&
      src_y >= 0 && (src_y + h) <= height)
    {
      // everything o.k.
      return;
    }
  else
    {
      // TODO: Be more verbose here.
      fehler (error, ":SRC-X, :SRC-Y, :WIDTH, :HEIGHT are bad");
    }
}

local void handle_image_z (int src_x, int src_y, int x, int y, int w, int h, GC gcontext,
			   Drawable drawable, int bitmap_p, Display *dpy)
  //
  // STACK_0 = the image object; cleans up stack after return
  //
  // Handles images in the Z-format. This functions should be considered local to
  // xlib:put-image.
{
  int width;
  int height;
  int depth;
  char *data;
  int bytes_per_line;
  int ix, iy;
  unsigned long v;
  XImage *im;

  // First fetch the actual image geometry
  pushSTACK (STACK_0); width  = get_image_width ();
  pushSTACK (STACK_0); height = get_image_height ();
  pushSTACK (STACK_0); depth  = get_image_depth ();

  // Now ensure that src_x ... h are valid
  ensure_valid_put_image_args (src_x, src_y, w, h, width, height);
  
  // Calculate the bytes_per_line field
  switch (depth)
    {
    case 1: bytes_per_line = ((width+31)/32)*4; break;
    case 8: bytes_per_line = ((width+3)/4)*4; break;
    default:
      goto sorry;
    }

  // Allocate memory
  begin_call ();
    data = malloc (bytes_per_line * height);
  end_call ();

  if (data == 0)
    {
      pushSTACK (TheSubr(subr_self)->name); // probably wrong subr_self here
      fehler (error, "~: Could not malloc.");
    }

  // Actually create the image
  begin_call ();
    im = XCreateImage (dpy, 0, depth, (bitmap_p && (depth == 1)) ? XYBitmap : ZPixmap, 0,
		       data,
		       width, height,
		       32, bytes_per_line);
  end_call ();

  if (im == 0)
    {
      free (data);
      pushSTACK (TheSubr(subr_self)->name); // probably wrong subr_self here
      fehler (error, "~: XCreateImage call failed.");
    }
  
  // fetch the pixarray
  pushSTACK (STACK_0);
  funcall (`XLIB::IMAGE-Z-PIXARRAY`, 1);
  pushSTACK (value1);
  
  // Now the silly loop
  // This loop is anything but efficient.
  // On the other hand it works reliabable. -- That is more important to me than speed.
  for (iy = 0; iy < height; iy++)
    for (ix = 0; ix < width; ix++)
      {
	pushSTACK (STACK_0);
	pushSTACK (fixnum (iy));
	pushSTACK (fixnum (ix));
	funcall (L(aref), 3);
	v = get_uint32 (value1);
	begin_call ();
	  XPutPixel (im, ix, iy, v);
	end_call ();
      }
  skipSTACK (1);		// pixarray

  begin_call ();
    XPutImage (dpy, drawable, gcontext, im, src_x, src_y, x,y,w,h);
    // Note: XDestroyImage frees 'data' for us
    XDestroyImage (im);
  end_call ();

  skipSTACK (1);		// clean up
  return;			// all done

sorry:
  fehler (error, "Sorry, my implementation of XLIB:PUT-IMAGE is still not complete.");
}

//
// XLIB:PUT-IMAGE drawable gcontext image &key src-x src-y x y width height bitmap-p
//
defun xlib:put-image (3, 0, norest, key, 7, (:SRC-X :SRC-Y :X :Y :WIDTH :HEIGHT :BITMAP-P))
{
  // This is a *VERY* silly implementation.
  // XXX see that the keyword arguments are actually given 
  Display *dpy;
  int src_x   	    = eq(STACK_6,unbound) ? 0 : get_sint32 (STACK_6);
  int src_y   	    = eq(STACK_5,unbound) ? 0 : get_sint32 (STACK_5); 
  int x       	    = get_sint32 (STACK_4);
  int y       	    = get_sint32 (STACK_3);
  int w       	    = get_sint32 (STACK_2);
  int h       	    = get_sint32 (STACK_1);
  GC gcontext       = get_gcontext (STACK_8);
  Drawable drawable = get_drawable_and_display (STACK_9, &dpy);
  int bitmap_p      = get_bool (STACK_0);

  // There seem to be three kinds of images passed in:
  // IMAGE-X, IMAGE-XY, IMAGE-Z
  //
  
  // First see if it is an IMAGE-X?
  pushSTACK (STACK_7); funcall (`XLIB::IMAGE-X-P`, 1);
  if (!nullp (value1))
    {
#if 0
      pushSTACK (STACK_7);
      handle_image_x (src_x, src_y, x, y, w, h, gcontext, drawable, bitmap_p, dpy);
#endif
      // image-x stuff
      // It seems that images of type image-x are already in the format needed by XPutImage.
      //
      int bytes_per_line, bitmap_pad;
      char *data;
      XImage im;

      // Now fill in the XImage structure from the slots 
      pushSTACK (STACK_7); funcall (`XLIB::IMAGE-DEPTH`, 1);    	  im.depth            = get_uint8 (value1);
      pushSTACK (STACK_7); funcall (`XLIB::IMAGE-WIDTH`, 1);    	  im.width            = get_uint16 (value1);
      pushSTACK (STACK_7); funcall (`XLIB::IMAGE-HEIGHT`, 1);   	  im.height           = get_uint16 (value1);
      pushSTACK (STACK_7); funcall (`XLIB::IMAGE-X-FORMAT`, 1);           im.format           = get_image_format (value1);
      pushSTACK (STACK_7); funcall (`XLIB::IMAGE-X-BYTES-PER-LINE`, 1);   im.bytes_per_line   = get_uint16 (value1);
      pushSTACK (STACK_7); funcall (`XLIB::IMAGE-X-PAD`, 1);              im.bitmap_pad       = get_uint8 (value1);
      pushSTACK (STACK_7); funcall (`XLIB::IMAGE-X-BITS-PER-PIXEL`, 1);   im.bits_per_pixel   = get_uint8 (value1);
      pushSTACK (STACK_7); funcall (`XLIB::IMAGE-X-BIT-LSB-FIRST-P`, 1);  im.bitmap_bit_order = eq(value1,NIL) ? MSBFirst : LSBFirst;
      pushSTACK (STACK_7); funcall (`XLIB::IMAGE-X-BYTE-LSB-FIRST-P`, 1); im.byte_order       = eq(value1,NIL) ? MSBFirst : LSBFirst;
      pushSTACK (STACK_7); funcall (`XLIB::IMAGE-X-UNIT`, 1);             im.bitmap_unit      = get_uint8 (value1);
      pushSTACK (STACK_7); funcall (`XLIB::IMAGE-X-LEFT-PAD`, 1);         im.xoffset          = get_uint8 (value1);

      if (bitmap_p && im.depth == 1)
	im.format = XYBitmap;
      
      // Now fetch data it *must* be a vector of card8
      pushSTACK (subr_self); pushSTACK (STACK_8); funcall (`XLIB::IMAGE-X-DATA`, 1); subr_self = popSTACK ();
      if (vectorp (value1) && (Iarray_flags(value1) & arrayflags_atype_mask) == Atype_8Bit)
	{
	  im.data = (char*) &TheSbvector (TheIarray (value1)->data)->data[0];
	}
      else
	{
	  pushSTACK (`(array xlib::card8 (*))`);
	  pushSTACK (STACK_8);
	  pushSTACK (TheSubr (subr_self)->name);
	  fehler (error, "~: Slot :DATA of x-image ~ is not of type ~.");
	}

      dprintf (("\n;; put-image: IMAGE-X  %dx%dx%d, fmt= %s, bpl= %d, pad= %d -> %dx%d+%d+%d",
	      im.width,im.height,im.depth,
	      ((char*[]){"bitmap","xy-pixmap","z-pixmap"})[im.format],
	      im.bytes_per_line, im.bitmap_pad,
	      w,h,x,y));

      begin_call ();
        XPutImage (dpy, drawable, gcontext, &im, src_x, src_y, x,y,w,h);
        XSync (dpy, 0);
      end_call ();

      goto raus;
    }
  else
    {
      // handle_image_z (src_x, src_y, x, y, w, h, gcontext, drawable, bitmap_p, dpy);
      // image-z or image-xy stuff
      XImage *im;
      int width, height, depth, format;
      unsigned long fg,bg;
      
      pushSTACK (`XLIB::IMAGE-WIDTH`); pushSTACK (STACK_8); funcall (L(funcall), 2);
      width = get_sint32 (value1);
      pushSTACK (`XLIB::IMAGE-HEIGHT`); pushSTACK (STACK_8); funcall (L(funcall), 2);
      height = get_sint32 (value1);
      pushSTACK (`XLIB::IMAGE-DEPTH`); pushSTACK (STACK_8); funcall (L(funcall), 2);
      depth = get_sint32 (value1);
      
      {
	XGCValues vals;
	XGetGCValues (dpy, gcontext, GCForeground|GCBackground, &vals);
	fg = vals.foreground;
	bg = vals.background;
	
	dprintf (("\n;; put-image: IMAGE-XY %dx%dx%d", width,height,depth));
	dprintf ((", fg=%.8x, bg=%.8x", fg,bg));
      }
      
      {
	char *data;
	int bytes_per_line;
	int ix, iy;
	unsigned long v;

	switch (depth)
	  {
	  case 1: bytes_per_line = ((width+31)/32)*4; break;
	  case 8: bytes_per_line = ((width+3)/4)*4; break;
	  default:
	    goto fake;
	  }

	begin_call ();
	data = malloc (bytes_per_line * height);
	end_call ();

	if (data == 0)
	  {
	    pushSTACK (TheSubr(subr_self)->name); // probably wrong subr_self here
	    fehler (error, "~: Could not malloc.");
	  }
	
	begin_call ();
	im = XCreateImage (dpy, 0, depth, (bitmap_p && (depth == 1)) ? XYBitmap : ZPixmap, 0,
			   data,
			   width, height,
			   32, bytes_per_line);
	end_call ();
	dprintf (("im.bytes_per_line = %d (vs. %d)", im->bytes_per_line, bytes_per_line));

	if (im == 0)
	  {
	    free (data);
	    pushSTACK (TheSubr(subr_self)->name); // probably wrong subr_self here
	    fehler (error, "~: XCreateImage call failed.");
	  }
	
	dprintf (("\nstill here"));
	
	pushSTACK (`XLIB::IMAGE-XY-P`); pushSTACK (STACK_8); funcall (L(funcall), 2);
	if (!nullp (value1))
	  {
	    pushSTACK (`XLIB::IMAGE-XY-BITMAP-LIST`);
	    pushSTACK (STACK_8);
	    funcall (L(funcall), 2);
	    pushSTACK (value1);
	    funcall (L(car), 1);
	    pushSTACK (value1);
	  }
	else
	  {
	    pushSTACK (`XLIB::IMAGE-Z-PIXARRAY`);
	    pushSTACK (STACK_8);
	    funcall (L(funcall), 2);
	    pushSTACK (value1);
	  }
	dprintf (("\n;im = %.8x",im));
	
	for (ix = 0; ix < width; ix++)
	  for (iy = 0; iy < height; iy++)
	    {
	      pushSTACK (STACK_0);
	      pushSTACK (fixnum (iy));
	      pushSTACK (fixnum (ix));
	      funcall (L(aref), 3);
	      v = get_uint32 (value1);
	      begin_call ();
	      XPutPixel (im, ix, iy, v);
	      end_call ();
	    }
	skipSTACK (1);

	dprintf (("\nwatch out for blitter"));

	dprintf (("\nXPutImage (.., src_x=%d, src_y=%d, x=%d,y=%d,w=%d,h=%d);",
		  src_x, src_y, x,y,w,h));
	
	begin_call ();
	XPutImage (dpy, drawable, gcontext, im, src_x, src_y, x,y,w,h);
	XDestroyImage (im);
	end_call ();
      }
    
    goto raus;
  }
  

 fake:
  dprintf ((" --- FAKED"));

 raus:
  skipSTACK (10);
}



// ----------------------------------------------------------------------------------------------------
//  Chapter 8   Fonts and Characters
// ----------------------------------------------------------------------------------------------------

/* 8.2  Opening Fonts */
defun XLIB:OPEN-FONT (2)
{
  Display *dpy;
  Font font;

  pushSTACK (STACK_1);			// display argument
  dpy = pop_display ();			//

  // XXX Maybe a symbol should be o.k. here too?
  
  if (stringp (STACK_0))
    {
      with_string_0 (STACK_0, misc_encoding (), font_name,
		     {
		       begin_call ();				// 
		       font = XLoadFont (dpy, font_name); 	// Load the font
		       end_call ();				//
		     });
      value1 = make_font_with_info (STACK_1, font, STACK_0, NULL);	// make up the LISP representation
      mv_count = 1;
    }
  else
    {
      pushSTACK (STACK_0);
      pushSTACK (`STRING`);
      my_standard_type_error (TheSubr(subr_self)->name);
    }
  skipSTACK (2);
}

// BTW: Mathematics and alcohol don`t mix -- Don't drink and derive.
// [Found on somebody's signature]
// Put here, because I dislike alcohol. (Err, sometimes ..)

defun XLIB:CLOSE-FONT (1)
{
  // FIXME: The manual says something about that fonts are reference counted..?
  
  Display *dpy;
  Font    font = get_font_and_display (STACK_0, &dpy);

  begin_call ();
  XUnloadFont (dpy, font);
  end_call ();
  
  value1 = NIL; mv_count = 1;
  skipSTACK (1);
}

//
// XLIB:DISCARD-FONT-INFO font
//
defun XLIB:DISCARD-FONT-INFO (1)
{
  XFontStruct *info;

  pushSTACK (subr_self);
  pushSTACK (STACK_1);
  pushSTACK (`XLIB::FONT-INFO`);
  funcall (L(slot_value), 2);	// (slot-value obj `font-info)
  subr_self = popSTACK ();
  
  ASSERT (fpointerp (value1)); 
  if (!fp_validp (TheFpointer(value1)))
    {
      // Raise an error message.
      pushSTACK (STACK_1);
      pushSTACK (TheSubr (STACK_2)->name);
      fehler (error, "~: Wanted to refer to a dead font.");
    }
  info = TheFpointer(value1)->fp_pointer;
  TheFpointer(value1)->fp_pointer = NULL; // No longer valid

  begin_call ();
  if (info)
    XFreeFontInfo (NULL, info, 1);
  end_call ();

  skipSTACK (1);
  value1 = NIL; mv_count = 1;
}

/* 8.3  Listing Fonts */
defun XLIB:FONT-PATH (1, 0, norest, key, 1, (:RESULT-TYPE)) // [OK]
{
  Display *dpy;
  int npathen, i;
  char **pathen;
  
  pushSTACK (STACK_1); dpy = pop_display ();

  begin_call ();
  pathen = XGetFontPath (dpy, &npathen);
  end_call ();
  
  for (i = 0; i < npathen; i++)
    pushSTACK (asciz_to_string (pathen[i], misc_encoding ()));
  funcall (L(list), npathen);

  begin_call ();
  if (pathen) XFreeFontPath (pathen);
  end_call ();
  
  // coerce it to the right type if necessary
  if (!eq (STACK_0, unbound))
    {
      pushSTACK (value1);
      pushSTACK (STACK_1);
      funcall (L(coerce), 2);
    }
  
  skipSTACK (2);		// all done
}

//
//  (SETF (XLIB:FONT-PATH display) new-path) == (XLIB:FONT-PATH-SETTER new-path display)
//
//  NOTE  - The CLX manual says that pathnames are also o.k. as arguments. But I
//          consider  it dirty,  since  the  X  server  may  live  on an  entire
//          different architecture than the client.
//
defun XLIB:FONT-PATH-SETTER (2)
{
  Display *dpy;
  int npathen,i;

  pushSTACK (subr_self);	// save subr_self, since we are doing funcalls here
  
  pushSTACK (STACK_1); dpy = pop_display ();
  
  // Find number of pathen
  pushSTACK (STACK_2);
  funcall (L(length), 1);
  npathen = get_uint32 (value1);
  
  {
    DYNAMIC_ARRAY (pathen, char*, npathen);
    
    for (i = 0; i < npathen; i++)
      {
	pushSTACK (STACK_2);	// pathen
	pushSTACK (fixnum (i));	// index
	funcall (L(elt), 2);
	if (stringp (value1))
	  { with_string_0 (value1, misc_encoding (), frob,
		 	 {
			   uintL j;
			   j = asciz_length (frob)+1;    // das ist bloed, denn laenge ist ja schon bekannt 8-(
			   pathen [i] = malloc (j);      // warum eigendlich kein begin/end_call hier? 8-?
			   while (j--) pathen[i][j] = frob[j];
			 });}
	else
	  {
	    // Raise type error
	    pushSTACK (value1);		// object in question
	    pushSTACK (`STRING`);	// desired type
	    my_standard_type_error (TheSubr(STACK_2)->name);
	  }
      }

    begin_call ();
    XSetFontPath (dpy, pathen, npathen);
    end_call ();

    begin_call ();
    for (i = 0; i < npathen; i++)
      free (pathen [i]);
    end_call ();

    FREE_DYNAMIC_ARRAY (pathen);
  }
  
  value1 = STACK_2; mv_count = 1;
  skipSTACK (3);
}

local void coerce_it (object cl_type);

//
//  XLIB:LIST-FONT-NAMES display pattern &key (:max-fonts 65535) (:result-type `list)
//
//  -> sequence of string.
//
defun XLIB:LIST-FONT-NAMES (2, 0, norest, key, 2, (:MAX-FONTS :RESULT-TYPE)) // OK
{
  Display *dpy  = (pushSTACK (STACK_3), pop_display ());
  int max_fonts = eq (STACK_1, unbound) ? 65535 : get_fixnum (STACK_1);
  int count = 0, i;
  char **names;

  if (stringp (STACK_2))
    {
      with_string_0 (STACK_2, misc_encoding (), pattern,
	{
	  begin_call ();
	  if (names = XListFonts (dpy, pattern, max_fonts, &count))
	    {
	      end_call ();
	      
	      for (i = 0; i < count; i++)
		pushSTACK (asciz_to_string (names[i], misc_encoding ()));
	      
	      begin_call ();
	      XFreeFontNames (names);
	      end_call ();
	      funcall (L(list), count);
	    }
	  else
	    {
	      end_call ();
	      value1 = NIL;
	    }
	  
	  coerce_it (STACK_0);
	  skipSTACK (4);
	});
    }
  else
    {
      pushSTACK (STACK_2);
      pushSTACK (`STRING`);
      my_standard_type_error (TheSubr(subr_self)->name);
    }
}

//
//  XLIB:LIST-FONTS display pattern &key (:max-fonts 65535) (:result-type `list)
//  returns a sequence of pseudo fonts.
//
defun XLIB:LIST-FONTS (2, 0, norest, key, 2, (:MAX-FONTS :RESULT-TYPE))
{
  Display *dpy  = (pushSTACK (STACK_3), pop_display ());
  object *dpyf  = &(STACK_3);
  int max_fonts = eq (STACK_1,unbound) ? 65535 : get_fixnum (STACK_1);
  int count = 0, i;
  char **names;
  XFontStruct *infos;

  if (stringp (STACK_2))
    {
      begin_call ();
      with_string_0 (STACK_2, misc_encoding (), pattern,
	{
	  if (names = XListFontsWithInfo (dpy, pattern, max_fonts, &count, &infos))
	    {
	      end_call ();
      
	      for (i = 0; i < count; i++)
		pushSTACK (make_font_with_info (*dpyf, 0, asciz_to_string (names[i], misc_encoding ()), infos+i));

	      begin_call ();
	      XFreeFontNames (names);
	      end_call ();
      
	      funcall (L(list), count);
	    }
	  else
	    {
	      end_call ();
	      value1 = NIL;
	    }
	});  
      coerce_it (STACK_0);
      skipSTACK (4);
    }  
  else
    {
      pushSTACK (STACK_2);
      pushSTACK (`STRING`);
      my_standard_type_error (TheSubr(subr_self)->name);
    }

  // Hmm ... several question araise here ...
  //  XListFontsWithInfo(display, pattern, maxnames, count_return, info_return)
  // But this function does not return the per character information.
  // Should we introduce a new function get_font_per_char_info ?!
  //
}

/* 8.4  Font Attributes */

##define DEF_FONT_ATTR(lspnam, type, cnam) 				\
defun xlib:##lspnam (1)							\
{									\
  XFontStruct *info = get_font_info_and_display (STACK_0, 0, 0);	\
  value1 = make_##type (info->cnam); mv_count = 1;			\
  skipSTACK (1);							\
}  

//----------------------------------------------------------------------------------------------
//             lisp name                type            C slot
//----------------------------------------------------------------------------------------------
DEF_FONT_ATTR (font-all-chars-exist-p, 	bool, 		all_chars_exist)
DEF_FONT_ATTR (font-ascent, 		sint16, 	ascent)
DEF_FONT_ATTR (font-default-char, 	uint16, 	default_char)
DEF_FONT_ATTR (font-descent, 		sint16, 	descent)
DEF_FONT_ATTR (font-direction, 		draw_direction, direction)
DEF_FONT_ATTR (font-max-byte1, 		uint8, 		max_byte1)
DEF_FONT_ATTR (font-max-byte2, 		uint8, 		max_char_or_byte2)  /* XXX */
DEF_FONT_ATTR (font-max-char, 		uint16, 	max_char_or_byte2)
DEF_FONT_ATTR (font-min-byte1, 		uint8, 		min_byte1)
DEF_FONT_ATTR (font-min-byte2, 		uint8, 		min_char_or_byte2)  /* XXX */
DEF_FONT_ATTR (font-min-char, 		uint16, 	min_char_or_byte2)

DEF_FONT_ATTR (max-char-ascent, 	sint16, 	max_bounds.ascent)
DEF_FONT_ATTR (max-char-attributes, 	uint16, 	max_bounds.attributes)
DEF_FONT_ATTR (max-char-descent, 	sint16, 	max_bounds.descent)
DEF_FONT_ATTR (max-char-left-bearing, 	sint16, 	max_bounds.lbearing)
DEF_FONT_ATTR (max-char-right-bearing, 	sint16, 	max_bounds.rbearing)
DEF_FONT_ATTR (max-char-width, 		sint16, 	max_bounds.width)

DEF_FONT_ATTR (min-char-ascent, 	sint16, 	min_bounds.ascent)
DEF_FONT_ATTR (min-char-attributes, 	uint16, 	min_bounds.attributes)
DEF_FONT_ATTR (min-char-descent, 	sint16, 	min_bounds.descent)
DEF_FONT_ATTR (min-char-left-bearing, 	sint16, 	min_bounds.lbearing)
DEF_FONT_ATTR (min-char-right-bearing, 	sint16, 	min_bounds.rbearing)
DEF_FONT_ATTR (min-char-width, 		sint16, 	min_bounds.width)
//-----------------------------------------------------------------------------------------------

defun XLIB:FONT-NAME (1)
{
  value1 = get_font_name (popSTACK ()); mv_count = 1;
}

defun XLIB:FONT-PROPERTIES (1)
{
  Display *dpy;
  XFontStruct *font_struct = get_font_info_and_display (STACK_0, 0, &dpy);
  int i;

  for (i = 0; i < font_struct->n_properties; i++)
    {
      char *atom_name;
      // Why not make-atom here?
      begin_call ();
      atom_name = XGetAtomName (dpy, font_struct->properties[i].name);
      end_call ();
      
      pushSTACK (asciz_to_string (atom_name, misc_encoding ()));
      pushSTACK (`KEYWORD`);
      funcall (L(intern), 2);
      pushSTACK (value1);
      pushSTACK (make_uint32 (font_struct->properties[i].card32));

      begin_call ();
      XFree (atom_name);
      end_call ();
    }

  funcall (L(list), 2 * font_struct->n_properties);
  skipSTACK (1);		// all done
}

//
// XLIB:FONT-PROPERTY font name
//
defun XLIB:FONT-PROPERTY (2)
{
  Display             *dpy;
  XFontStruct *font_struct = get_font_info_and_display (STACK_1, 0, &dpy);
  Atom atom                = get_xatom (dpy, STACK_0);
  unsigned long value;

  begin_call ();
  if (XGetFontProperty (font_struct, atom, &value))
    {
      end_call ();
      value1 = make_uint32 (value);
    }
  else
    {
      end_call ();
      value1 = NIL;
    }

  mv_count = 1;
  skipSTACK (2);
}


/* 8.5  Character Attributes */

XCharStruct *font_char_info (XFontStruct *fs, unsigned int index)
{
  /* from XLoadFont(3X11):
   *     
   *   If the min_byte1 and max_byte1 members are both zero,
   *   min_char_or_byte2 specifies the linear character index corresponding
   *   to the first element of the per_char array, and max_char_or_byte2
   *   specifies the linear character index of the last element.
   *       If either min_byte1 or max_byte1 are nonzero, both min_char_or_byte2
   *   and max_char_or_byte2 are less than 256, and the 2-byte character
   *   index values corresponding to the per_char array element N (counting
   *   from 0) are:
   *   
   *         byte1 = N/D + min_byte1
   *         byte2 = N\D + min_char_or_byte2
   *   
   *   where:
   *   
   *           D = max_char_or_byte2 - min_char_or_byte2 + 1
   *           / = integer division
   *           \ = integer modulus
   */

  if (fs->min_byte1 == 0 && fs->max_byte1 == 0)
    {
      // Linear indexing ...
      if (index >= fs->min_char_or_byte2 && index <= fs->max_char_or_byte2)
	if (fs->per_char)
	  return fs->per_char+(index-fs->min_char_or_byte2);
	else
	  return &(fs->min_bounds);
    }
  else
    {
      // Nonlinear indexing ..
      unsigned char byte1 = (index >> 8) &0xFF;	// Is this right?!
      unsigned char byte2 = index & 0xFF;
      unsigned int d = fs->max_char_or_byte2 - fs->min_char_or_byte2 + 1;

      if (byte1 >= fs->min_byte1 && byte1 <= fs->max_byte1 &&
	  byte2 >= fs->min_char_or_byte2 && byte2 <= fs->max_char_or_byte2)
	{
	  index = (byte1 - fs->min_byte1)*d + (byte2 - fs->min_char_or_byte2);

	  if (fs->per_char)
	    return fs->per_char+index;
	  else
	    return &(fs->min_bounds);
	}
    }
  // BTW these two cases could be handled in one, but I leave it here for clarity.

  // fall thru'
  return NULL;
}

##define DEF_CHAR_ATTR(lspnam, type, cnam)				\
    defun lspnam (2)			         			\
    {									\
      XFontStruct *font_info = get_font_info_and_display (STACK_1, 0, 0); \
      unsigned int index = get_uint16 (STACK_0);			\
      XCharStruct *char_info = font_char_info (font_info, index);	\
      if (char_info)							\
	if (char_info->lbearing == 0   &&				\
	    char_info->rbearing == 0   &&				\
	    char_info->width == 0      &&				\
	    char_info->attributes == 0 &&				\
	    char_info->ascent == 0     &&				\
	    char_info->descent == 0)					\
	  value1 = NIL;							\
	else								\
	  value1 = make_##type (char_info->cnam);			\
      else								\
	value1 = NIL;							\
      mv_count = 1;							\
      skipSTACK (2);							\
    }

DEF_CHAR_ATTR (xlib:char-left-bearing,  sint16, lbearing)
DEF_CHAR_ATTR (xlib:char-right-bearing, sint16, rbearing)
DEF_CHAR_ATTR (xlib:char-width,		sint16, width)
DEF_CHAR_ATTR (xlib:char-attributes, 	sint16, attributes)
DEF_CHAR_ATTR (xlib:char-ascent,	sint16, ascent)
DEF_CHAR_ATTR (xlib:char-descent,	sint16, descent)

/* 8.6  Querying Text Size */
defun XLIB:TEXT-EXTENTS (2, 0, norest, key, 3, (:START :END :TRANSLATE))
{
  // FIXME: Could font be a graphics context?! -- yes! This is handled by get_font_info_and_display already
  if (simple_string_p (STACK_3))
    {
      object font;
      XFontStruct *font_info = get_font_info_and_display (STACK_4, &font, 0);
      int start = gunboundp (STACK_2) ? 0 : get_uint16 (STACK_2);
      int end   = gunboundp (STACK_1) ? vector_length (STACK_3) : get_uint16 (STACK_1);
      int dir;
      int font_ascent, font_descent;
      XCharStruct overall;

#ifdef UNICODE
      const chart* charptr;
      unpack_sstring_alloca(STACK_3,end-start,start,charptr=);
      { DYNAMIC_ARRAY(str,XChar2b,end-start);
        if (to_XChar2b(font,font_info,charptr,str,end-start) == 1)
          {
            begin_call();
            XTextExtents (font_info, (char*)str, end-start, &dir, &font_ascent, &font_descent, &overall);
            end_call();
          }
        else
          {
            begin_call();
            XTextExtents16 (font_info, str, end-start, &dir, &font_ascent, &font_descent, &overall);
            end_call();
          }
        FREE_DYNAMIC_ARRAY(str);
      }
#else
      { char* string = (char*) &TheSstring(STACK_3)->data[0];
        begin_call();
        XTextExtents (font_info, string + start, end - start, &dir, &font_ascent, &font_descent, &overall);
        end_call();
      }
#endif

      pushSTACK (make_sint32 (overall.width));    // width
      pushSTACK (make_sint16 (overall.ascent));	  // ascent
      pushSTACK (make_sint16 (overall.descent));  // descent
      pushSTACK (make_sint16 (overall.lbearing)); // left
      pushSTACK (make_sint16 (overall.rbearing)); // right
      pushSTACK (make_sint16 (font_ascent));	  // font-ascent
      pushSTACK (make_sint16 (font_descent));	  // font-descent
      pushSTACK (make_draw_direction (dir));	  // direction
      pushSTACK (NIL);				  // first-not-done
      
      value9 = popSTACK ();
      value8 = popSTACK ();
      value7 = popSTACK ();
      value6 = popSTACK ();
      value5 = popSTACK ();
      value4 = popSTACK ();
      value3 = popSTACK ();
      value2 = popSTACK ();
      value1 = popSTACK ();
      mv_count = 9;
      skipSTACK (5);
    }
  else
    {
      NOTIMPLEMENTED;
    }
}

//
//  XLIB:TEXT-WIDTH font sequence &key (:start 0) :end :translate
//   -> width - Type int32
//   -> first-not-done - Type array-index or null.
//
defun XLIB:TEXT-WIDTH (2, 0, norest, key, 3, (:START :END :TRANSLATE))
{
  object font;
  XFontStruct *font_info = get_font_info_and_display (STACK_4, &font, 0);
  
  // First fetch the quite common special case where sequence is a simple string:
  if (simple_string_p (STACK_3))
    {
      int start = gunboundp (STACK_2) ? 0 : get_uint16 (STACK_2);
      int end   = gunboundp (STACK_1) ? vector_length (STACK_3) : get_uint16 (STACK_1);
      int w;

#ifdef UNICODE
      const chart* charptr;
      unpack_sstring_alloca(STACK_3,end-start,start,charptr=);
      { DYNAMIC_ARRAY(str,XChar2b,end-start);
        if (to_XChar2b(font,font_info,charptr,str,end-start) == 1)
          {
            begin_call();
            w = XTextWidth (font_info, (char*)str, end-start);
            end_call();
          }
        else
          {
            begin_call();
            w = XTextWidth16 (font_info, str, end-start);
            end_call();
          }
        FREE_DYNAMIC_ARRAY(str);
      }
#else
      { char* string = (char*) &TheSstring(STACK_3)->data[0];
        begin_call();
        w = XTextWidth (font_info, string+start, end-start);
        end_call();
      }
#endif

      value1 = make_sint32 (w);
      value2 = NIL;
      mv_count = 2;
    }
  else if (listp (STACK_3))
    {
      // Now the generic case for lists
      // XXX -- Fix this also above
      // XXX This is faked, isn't it.
      value1 = make_sint32 (0);
      value2 = NIL;
      mv_count = 2;
    }
  else if (vectorp (STACK_3))
    {
      // Generic case for vectors.
      // XXX faked.
      int start = gunboundp (STACK_2) ? 0 : get_uint16 (STACK_2);
      int end   = gunboundp (STACK_1) ? vector_length (STACK_3) : get_uint16 (STACK_1);
      value1 = make_sint32 (0);
      value2 = NIL;
      mv_count = 2;
    }
  else
    {
      NOTIMPLEMENTED;
    }
   
  skipSTACK (5);
}


// ----------------------------------------------------------------------------------------------------
//  Chapter 9   Colors
// ----------------------------------------------------------------------------------------------------

//  9.2  Color Functions
//
//  These functions moved to LISP.
//  MAKE-COLOR COLOR-BLUE COLOR-GREEN COLOR-P COLOR-RED COLOR-RGB
//

local void coerce_it (object cl_type)
     // coerce value1 to cl_type
     // unbound and NIL are interpreted as a NULL conversation.
{
  if (!eq (cl_type, unbound) && !eq (cl_type, NIL))
    {
      pushSTACK (value1);
      pushSTACK (cl_type);
      funcall (L(coerce), 2);
    }
}

/* 9.3  Colormap Functions */
defun XLIB:CREATE-COLORMAP (2, 1)
{
  int alloc_p  = (eq (STACK_0, unbound) ? 0 : get_bool (STACK_0));
  Display *dpy;
  Window   win = get_window_and_display (STACK_1, &dpy);
  Visual  *vis = get_visual (dpy, STACK_2);
  Colormap map;

  begin_call ();
  map = XCreateColormap (dpy, win, vis, alloc_p);
  end_call ();
      
  value1 = make_colormap (get_display_obj (STACK_1), map);
  mv_count = 1;
  skipSTACK (3);
}

defun XLIB:COPY-COLORMAP-AND-FREE (1)
{
  Display *dpy;
  Colormap  cm = get_colormap_and_display (STACK_0, &dpy);
  Colormap  cm2;

  begin_call ();
  cm2 = XCopyColormapAndFree (dpy, cm);
  end_call ();

  value1 = make_colormap (get_display_obj (STACK_0), cm2);
  mv_count = 1;
  skipSTACK (1);
}

defun XLIB:FREE-COLORMAP (1)
{
  Display *dpy;
  Colormap  cm = get_colormap_and_display (popSTACK (), &dpy);

  begin_call ();
  XFreeColormap (dpy, cm);
  end_call ();
  
  value1 = NIL; mv_count = 1;
}

defun XLIB:INSTALL-COLORMAP (1)
{
  Display *dpy;
  Colormap  cm = get_colormap_and_display (popSTACK (), &dpy);

  begin_call ();
  XInstallColormap (dpy, cm);
  end_call ();
  
  value1 = NIL; mv_count = 1;
}

defun XLIB:INSTALLED-COLORMAPS (1, 0, norest, key, 1, (:RESULT-TYPE))
{
  Display     *dpy;
  Window       win = get_window_and_display (STACK_1, &dpy);
  object *dpy_objf = &(STACK_1);
  int      num_cms = 0;		// paranoia
  int            i;
  Colormap    *cms;

  begin_call ();
  cms = XListInstalledColormaps (dpy, win, &num_cms);
  end_call ();

  // Now push all colormaps ...
  for (i = 0; i < num_cms; i++)
    pushSTACK (make_colormap (*dpy_objf, cms[i]));

  if (cms)
    {
      begin_call ();
      if (cms) XFree (cms);
      end_call ();
    }

  // Now cons 'em together
  funcall (L(list), num_cms);
  coerce_it (STACK_0);

  skipSTACK (2);		// all done
}

defun XLIB:UNINSTALL-COLORMAP (1)
{
  Display *dpy;
  Colormap  cm = get_colormap_and_display (popSTACK (), &dpy);

  begin_call ();
    XUninstallColormap (dpy, cm);
  end_call ();

  value1 = NIL; mv_count = 1;
}

//
//  xlib:colormap-visual-info colormap
//
//  returns the visual-info corresponding to a colormap
//
//  NIM
//
defun XLIB:COLORMAP-VISUAL-INFO (1)
{
  Display *dpy;
  Colormap cm = get_colormap_and_display (STACK_0, &dpy);
  Visual *vis;

  begin_call ();
  {
    XcmsCCC ccc = XcmsCCCOfColormap (dpy, cm);
    vis = ccc ? XcmsVisualOfCCC (ccc) : 0;
    // FIXME: Should we free the XcmsCCC? are they hashed or what?
  }
  end_call ();
  
  value1 = vis ? make_visual_info (vis) : NIL; mv_count = 1;
  skipSTACK (1);
}

defun XLIB:ALLOC-COLOR (2)
{
  Display *dpy;
  Colormap  cm = get_colormap_and_display (STACK_1, &dpy);
  XColor color;

  if (stringp (STACK_0) || symbolp (STACK_0))
    {
      XColor exact_color;

      with_stringable_0_tc (STACK_0, misc_encoding (), name,
        {
	  begin_call ();
	  if (XAllocNamedColor (dpy, cm, name, &color, &exact_color))
	    {
	      end_call ();
	      pushSTACK (make_pixel (color.pixel)); // pixel
	      pushSTACK (make_color (&color));	    // screen color
	      value3 = make_color (&exact_color);   // exact color
	      value2 = popSTACK ();
	      value1 = popSTACK ();
	      mv_count = 3;
	    }
	  else
	    {
	      end_call ();
	      goto failed;
	    }
	});
    }
  else
    if (color_p (STACK_0))
      {
	get_color (dpy, STACK_0, &color);
	begin_call ();
	if (XAllocColor (dpy, cm, &color))
	  {
	    end_call ();
	    
	    pushSTACK (make_pixel (color.pixel));		// pixel
	    value2 = make_color (&color);			// screen color
	    value3 = STACK_1;					// exact color (what the luser gave)
	    value1 = popSTACK ();
	    mv_count = 3;
	  }
	else
	  {
	    end_call ();
	    goto failed;
	  }
      }
    else
      {
	pushSTACK (STACK_0);
	pushSTACK (`(OR STRING SYMBOL XLIB::COLOR)`);
	my_standard_type_error (TheSubr(subr_self)->name);
      }

  skipSTACK (2);
  return;

 failed:
  {
    // I have to see what the MIT-CLX implementation does here ...
    pushSTACK (get_display_obj (STACK_1));	// display argument
    pushSTACK (STACK_1);			// color argument
    fehler (error, ("Color ~ is unknown to display ~."));
  }
}

//
// XLIB:ALLOC-COLOR-CELLS colormap colors &key (:planes 0) :contiguous_p (:result-type `list)
// returns
//   pixels, masks -- Type sequence of pixels
//
defun XLIB:ALLOC-COLOR-CELLS (2, 0, norest, key, 3, (:PLANES :CONTIGUOUS-P :RESULT-TYPE))
{
  Display         *dpy;
  Colormap          cm = get_colormap_and_display (STACK_4, &dpy);
  unsigned int npixels = get_uint32 (STACK_3);
  unsigned int nplanes = eq (STACK_2, unbound) ? 0 : get_uint32 (STACK_2);
  Bool    contiguous_p = eq (STACK_1, unbound) ? False : get_bool (STACK_1);

  // FIXME -- we should introduce some checks here, since the luser gave nonsens
  //          arguments, we might run into real problems.

  {
    DYNAMIC_ARRAY (plane_masks, unsigned long, nplanes);
      {
	DYNAMIC_ARRAY (pixels, unsigned long, npixels);

	begin_call ();
	if (XAllocColorCells (dpy, cm, contiguous_p, plane_masks, nplanes, pixels, npixels))
	  {
	    unsigned i;
	    end_call ();
	    
	    for (i = 0; i < nplanes; i++)
	      pushSTACK (make_uint32 (plane_masks [i]));
	    funcall (L(list), nplanes);

	    coerce_it (STACK_0);
	    pushSTACK (value1);

	    for (i = 0; i < npixels; i++)
	      pushSTACK (make_uint32 (pixels [i]));
	    funcall (L(list), npixels);

	    coerce_it (STACK_0);
	    value2 = popSTACK ();
	    mv_count = 2;
	  }
	else
	  {
	    end_call ();
	    
	    value1 = NIL; mv_count = 1;
	    // Q: Should we raise a x-error-sonstwas condition here?
	  }
	
	FREE_DYNAMIC_ARRAY (pixels);
      }
    FREE_DYNAMIC_ARRAY (plane_masks);
  }
  skipSTACK (5);
}

//
// XLIB:ALLOC-COLOR-PLANES colormap colors &key (:reds 0) (:greens 0) (:blues 0) :contiguous-p (:result-type `list)
//
//  returns: pixels                          -- Type sequence of pixels
//           red-mask, green-mask, blue-mask -- Type pixel.
//

defun XLIB:ALLOC-COLOR-PLANES (2, 0, norest, key, 5, (:REDS :GREENS :BLUES :CONTIGUOUS-P :RESULT-TYPE))
{
  Display         *dpy;
  Colormap          cm = get_colormap_and_display (STACK_6, &dpy);
  unsigned int ncolors = get_uint32 (STACK_5);
  unsigned int   nreds = eq (STACK_4, unbound) ? 0 : get_uint32 (STACK_4);
  unsigned int ngreens = eq (STACK_3, unbound) ? 0 : get_uint32 (STACK_3);
  unsigned int  nblues = eq (STACK_2, unbound) ? 0 : get_uint32 (STACK_2);
  Bool    contiguous_p = eq (STACK_1, unbound) ? False : get_bool (STACK_1);
  unsigned long red_mask, green_mask, blue_mask;

  {
    DYNAMIC_ARRAY (pixels, unsigned long, ncolors);

    begin_call ();
    
    if (XAllocColorPlanes (dpy, cm, contiguous_p, pixels, ncolors, nreds, ngreens, nblues,
			   &red_mask, &green_mask, &blue_mask))
      {
	uintC i;
	end_call ();
	
	for (i = 0; i < ncolors; i++)
	  pushSTACK (make_uint32 (pixels [i]));
	funcall (L(list), ncolors);

	coerce_it (STACK_0);
	pushSTACK (value1);
	pushSTACK (make_uint32 (red_mask));
	pushSTACK (make_uint32 (green_mask));
	pushSTACK (make_uint32 (blue_mask));
	value1 = STACK_3;
	value2 = STACK_2;
	value3 = STACK_1;
	value4 = STACK_0;
	mv_count = 4;
	skipSTACK (4);
      }
    else
      {
	value1 = NIL; mv_count = 1;
      }
    
    FREE_DYNAMIC_ARRAY (pixels);
  }
  skipSTACK (7);
}

//
//  XLIB:FREE-COLORS colormap pixels &optional plane-mask 0
//
defun XLIB:FREE-COLORS (2, 1)
{
  Display *dpy;
  Colormap  cm = get_colormap_and_display (STACK_2, &dpy);
  unsigned long plane_mask = (!eq (STACK_0,unbound) ? get_pixel (STACK_0) : 0);
  unsigned int npixels, i;

  pushSTACK (STACK_1);
  funcall (L(length), 1);
  npixels = get_uint32 (value1);

  {
    DYNAMIC_ARRAY (pixels, unsigned long, npixels);

    for (i = 0; i < npixels; i++)
      {
	pushSTACK (STACK_1);	// pixels
	pushSTACK (fixnum (i));	// index
	funcall (L(elt), 2);
	pixels[i] = get_pixel (value1);
      }

    begin_call ();
    XFreeColors (dpy, cm, pixels, npixels, plane_mask);
    end_call ();

    FREE_DYNAMIC_ARRAY (pixels);
  }

  value1 = NIL; mv_count = 1;
  skipSTACK (3);
}

//
//  XLIB:LOOKUP-COLOR colormap name
//
defun XLIB:LOOKUP-COLOR (2)	// [OK]
{
  Display *dpy;
  Colormap  cm = get_colormap_and_display (STACK_1, &dpy);
  XColor exact_color, screen_color;

  with_stringable_0_tc (STACK_0, misc_encoding (), name,
    {
      begin_call ();
      if (XLookupColor (dpy, cm, name, &exact_color, &screen_color))
	{
	  end_call ();
      
	  pushSTACK (make_color (&screen_color));
	  value2 = make_color (&exact_color);
	  value1 = popSTACK ();
	  mv_count = 2;
	}
      else
	{
	  pushSTACK (get_display_obj (STACK_1)); // display argument
	  pushSTACK (STACK_1);			 // color argument
	  fehler (error, ("Color ~ is unknown to display ~."));
	}
    });
  skipSTACK (2);
}

//
// XLIB:QUERY-COLORS colormap pixels &key (:result-type `list)
//
// returns: colors -- Type sequence of color.
//
defun XLIB:QUERY-COLORS (2, 0, norest, key, 1, (:RESULT-TYPE))
{
  Display *dpy;
  Colormap cm = get_colormap_and_display (STACK_2, &dpy);
  int ncolors, i;

  pushSTACK (STACK_1); funcall (L(length), 1); ncolors = get_uint32 (value1);

  {
    DYNAMIC_ARRAY (colors, XColor, ncolors);

      for (i = 0; i < ncolors; i++)
	{
	  pushSTACK (STACK_1);			 // the colors arguments
	  pushSTACK (fixnum (i));		 // the index
	  funcall (L(elt), 2);			 // fetch it
	  get_color (dpy, value1, &(colors[i])); // and convert
	}

    begin_call ();
    XQueryColors (dpy, cm, colors, ncolors);
    end_call ();
    // FIXME - find what to do with the DoRed, DoGreen, and DoBlue flags?!

    for (i = 0; i < ncolors; i++)
      pushSTACK (make_color (&(colors[i])));
  
    funcall (L(list), i);
    coerce_it (STACK_0);

    FREE_DYNAMIC_ARRAY (colors);
  }
  skipSTACK (3);		// all done
}

//
// XLIB:STORE-COLOR colormap pixel color &key (:red-p t) (:green-p t) (:blue-p t)
//
defun XLIB:STORE-COLOR (3, 0, norest, key, 3, (:RED-P :GREEN-P :BLUE-P))
{
  Display *dpy;
  Colormap cm = get_colormap_and_display (STACK_5, &dpy);
  XColor color;

  get_color (dpy, STACK_3, &color);
  color.pixel = get_uint32 (STACK_4);
  color.flags =
    (!nullp (STACK_2) ? DoRed : 0) |
    (!nullp (STACK_1) ? DoGreen : 0) |
    (!nullp (STACK_0) ? DoBlue : 0);

  begin_call ();
  XStoreColor (dpy, cm, &color);
  end_call ();

  value1 = NIL; mv_count = 0;
  skipSTACK (6);
}

//
// XLIB:STORE-COLORS colormap pixel-colors &key (:red-p t) (:green-p t) (:blue-p t)
//
defun XLIB:STORE-COLORS (2, 0, norest, key, 3, (:RED-P :GREEN-P :BLUE-P))
{
  Display *dpy;
  Colormap cm = get_colormap_and_display (STACK_4, &dpy);
  int ncolors;
  char flags = 
    (!nullp (STACK_2) ? DoRed : 0)   |
    (!nullp (STACK_1) ? DoGreen : 0) |
    (!nullp (STACK_0) ? DoBlue : 0);

  pushSTACK (STACK_3); funcall (L(length), 1); ncolors = get_uint32 (value1);

  if (ncolors%2)
    {
      pushSTACK (STACK_3);
      pushSTACK (TheSubr(subr_self)->name);	// Funktionsname
      fehler (error, ("~: Argument PIXEL-COLORS (~) should an even number of elements"));
    }

  ncolors /= 2;

  {
    DYNAMIC_ARRAY (colors, XColor, ncolors);

    int i;
    for (i = 0; i < ncolors; i++)
      {
	// FIXME: Argument should be a list, so we should rather cdr it down.
	//
	pushSTACK (STACK_3);			// the pixel-colors arguments
	pushSTACK (fixnum (i*2+1));		// the index
	funcall (L(elt), 2);			// fetch it
	get_color (dpy, value1, &(colors[i]));
	
	pushSTACK (STACK_3);			// the pixel-colors arguments
	pushSTACK (fixnum (i*2));		// the index
	funcall (L(elt), 2);			// fetch it
	colors[i].pixel = get_uint32 (value1);
	colors[i].flags = flags;
      }
    
    begin_call ();
    XStoreColors (dpy, cm, colors, ncolors);
    end_call ();

    FREE_DYNAMIC_ARRAY (colors);
  }
  value1 = NIL; mv_count = 1;
  skipSTACK (5);
}



// ----------------------------------------------------------------------------------------------------
//  Chapter 10  Cursors
// ----------------------------------------------------------------------------------------------------  

/* 10.2  Creating Cursors */
//
// XLIB:CREATE-CURSOR &key [5]:source [4]:mask [3]:x [2]:y [1]:foreground [0]:background
//
// FIXME: May also here are color names legal?!
//
defun XLIB:CREATE-CURSOR (0, 0, norest, key, 6, (:SOURCE :MASK :X :Y :FOREGROUND :BACKGROUND))
{
  Display *dpy;
  Pixmap source;
  Pixmap mask = None;
  XColor foreground;
  XColor background;
  unsigned int x, y;
  Cursor cursor;

  if (!eq (STACK_5, unbound))
    {
      source = get_pixmap_and_display (STACK_5, &dpy);
    }
  else
    goto required;
  
  if (!eq (STACK_4, unbound))
    mask = get_pixmap (STACK_4);
  
  if (!eq (STACK_3, unbound))
    x = get_sint16 (STACK_3);
  else
    goto required;

  if (!eq (STACK_2, unbound))
    y = get_sint16 (STACK_2);
  else
    goto required;

  if (!eq (STACK_1, unbound))
    get_color (dpy, STACK_1, &foreground);
  else
    goto required;

  if (!eq (STACK_0, unbound))
    get_color (dpy, STACK_0, &background);
  else
    goto required;

  begin_call ();
  cursor = XCreatePixmapCursor (dpy, source, mask, &foreground, &background, x, y);
  end_call ();

  value1 = make_cursor (get_display_obj (STACK_5),cursor);
  skipSTACK (6);		// All done
  return;
  
 required:
  pushSTACK (TheSubr(subr_self)->name);	// Funktionsname
  fehler (type_error, ("~: At least :SOURCE :X, :Y, :FOREGROUND, and :BACKGROUND must be specified"));
}

//
// XLIB:CREATE-GLYPH-CURSOR &key [5]:source-font [4]:source-char [3]:mask-font [2](:mask-char 0) [1]:foreground [0]:background
//
defun XLIB:CREATE-GLYPH-CURSOR (0, 0, norest, key, 6,
	  (:SOURCE-FONT :SOURCE-CHAR :MASK-FONT  :MASK-CHAR :FOREGROUND :BACKGROUND))

{
  Display *dpy;
  Font source_font;
  unsigned int source_char;
  Font mask_font = None;
  unsigned int mask_char = 0;
  XColor foreground;
  XColor background;
  Cursor cursor;
  
  if (!eq (STACK_5, unbound))		// :source-font
    {
      source_font = get_font_and_display (STACK_5, &dpy);
    }
  else
    goto required;

  if (!eq (STACK_4, unbound))		// :source-char
    source_char = get_uint16 (STACK_4);
  else
    goto required;

  if (!eq (STACK_3, unbound))		// :mask-font
    mask_font = get_font (STACK_3);

  if (!eq (STACK_2, unbound))		// :mask-char
    mask_char = get_uint16 (STACK_2);

  if (!eq (STACK_1, unbound))		// :foreground
    get_color (dpy, STACK_1, &foreground);
  else
    goto required;

  if (!eq (STACK_0, unbound))		// :background
    get_color (dpy, STACK_0, &background);
  else
    goto required;

  begin_call ();
  cursor = XCreateGlyphCursor (dpy, source_font, mask_font, source_char, mask_char, &foreground, &background);
  end_call ();

  value1 = make_cursor (get_display_obj (STACK_5),cursor);
  skipSTACK (6);		// All done
  return;
  
 required:
  pushSTACK (TheSubr(subr_self)->name);	// Funktionsname
  fehler (type_error, ("~: At least :SOURCE-FONT, :SOURCE-CHAR, :FOREGROUND, and :BACKGROUND must be specified"));
}

defun XLIB:FREE-CURSOR (1)
{
  Display *dpy;
  Cursor cur = get_cursor_and_display (STACK_0, &dpy);
  begin_call();
  XFreeCursor (dpy, cur);
  end_call ();
  skipSTACK (1);
  value1= NIL; mv_count = 1;
}

/* 10.3  Cursor Functions */
defun XLIB:QUERY-BEST-CURSOR (3)
{
  query_best_X (XQueryBestCursor);
}

//
//  XLIB:RECOLOR-CURSOR cursor foreground background
//
// FIXME? Are colour names also o.k here?
defun XLIB:RECOLOR-CURSOR (3)
{
  Display *dpy;
  Cursor cursor = get_cursor_and_display (STACK_2, &dpy);
  XColor foreground,background;

  get_color (dpy, STACK_1, &foreground);
  get_color (dpy, STACK_1, &background);

  begin_call ();
  XRecolorCursor (dpy, cursor, &foreground, &background);
  end_call ();

  value1= NIL; mv_count = 1;
  skipSTACK (3);		// all done
}

/* 10.4 Cursor Attributes */


// ----------------------------------------------------------------------------------------------------
//  Chapter 11  Atoms, Properties and Selections
// ----------------------------------------------------------------------------------------------------

/* 11.1  Atoms */
defun XLIB:ATOM-NAME (2) /* OK */
{
  Atom atom;
  Display *dpy;
  char *name;

  atom = get_uint29 (popSTACK ());
  dpy = pop_display ();

  begin_call ();
  name = XGetAtomName (dpy, atom);
  end_call ();
  
  pushSTACK (asciz_to_string (name, misc_encoding ()));
  pushSTACK (`KEYWORD`);

  begin_call ();
  XFree (name);
  end_call ();

  funcall (L (intern), 2);
  mv_count = 1;
}

defun XLIB:FIND-ATOM (2) /* OK */
{
  Display *dpy  = (pushSTACK (STACK_1), pop_display ());
  Atom atom     = get_xatom_nointern (dpy, STACK_0);
  skipSTACK (2);
  value1 = (atom != None) ? make_uint32 (atom) : NIL;
  mv_count = 1;
}

defun XLIB:INTERN-ATOM (2) /* OK */
{
  Display *dpy  = (pushSTACK (STACK_1), pop_display ());
  Atom atom     = get_xatom (dpy, STACK_0);
  value1 = (atom != None) ? make_uint32 (atom) : NIL; mv_count = 1;
  skipSTACK (2);
}

/* 11.2  Properties */
//
//  XLIB:CHANGE-PROPERTY window property data type format &key (:mode :replace) (:start 0) :end :transform
//
defun XLIB:CHANGE-PROPERTY (5, 0, norest, key, 4, (:MODE :START :END :TRANSFORM))
{
  Display  *dpy;
  Window    win = get_window_and_display (STACK_8, &dpy);
  Atom property = get_xatom (dpy, STACK_7);
  Atom     type = get_xatom (dpy, STACK_5);
  int    format = get_uint8 (STACK_4);
  int      mode = PropModeReplace;
  int     start = (eq (STACK_2,unbound) || eq (STACK_2,NIL)) ? 0 : get_uint32 (STACK_2);
  int       end;
  int         i;
  int       len;
  unsigned char *data;

  unless (format == 8 || format == 16 || format == 32)
    {
      pushSTACK (STACK_4);
      pushSTACK (`(MEMBER 8 16 32)`);
      my_standard_type_error (TheSubr (subr_self)->name);
    }

  if (!eq (STACK_3, unbound))
    {
      pushSTACK (STACK_3);
      pushSTACK (`:REPLACE`);
      pushSTACK (`:PREPEND`);
      pushSTACK (`:APPEND`);
      mode = get_enum (3);
    }

  if (eq (STACK_1, unbound) || eq (STACK_1, NIL))
    {
      pushSTACK (subr_self);	// save subr_self
      pushSTACK (STACK_7);	// data argument
      funcall (L(length), 1);
      subr_self = popSTACK ();	// restore subr_self
      end = get_uint32 (value1);
    }
  else
    end = get_uint32 (STACK_1);

  len = (end-start) * (format/8);

  if (len < 0)
    {
      pushSTACK (make_sint32 (len));
      pushSTACK (TheSubr (subr_self)->name);
      fehler (error, "~: How bogus! The effective length (~) is negative.");
    }

  {
    DYNAMIC_ARRAY (data, unsigned char, len ? len : 1);

    for (i = start; i < end; i++)
      {
	pushSTACK (subr_self);		// save subr_self
	pushSTACK (STACK_7);		// data argument
	pushSTACK (fixnum (i));		// index
	funcall (L(elt), 2);		// fetch element
	subr_self = popSTACK ();	// restore subr_self
	
	if (!eq (STACK_0, unbound) && !eq (STACK_0, NIL))
	  {
	    // call the transform function
	    pushSTACK (subr_self);	// save subr_self
	    pushSTACK (value1);
	    funcall (STACK_2, 1);
	    subr_self = popSTACK ();	// restore subr_self
	  }

	switch (format)
	  {
	  case 8:  ((uint8*) data)[i] = get_uint8  (value1); break;
	  case 16: ((uint16*)data)[i] = get_uint16 (value1); break;
	  case 32: ((uint32*)data)[i] = get_aint32 (value1); break;
		   // NOTE: I am using aint32, here not knowing if that is correct,
		   //       the manual does not specify of which type the property data
		   //       should be. [aint16, aint8 also?].
	  default:
	    NOTREACHED;
	  }
      }

    begin_call ();
    XChangeProperty (dpy, win, property, type, format, mode, data, (end-start));
    end_call ();

    FREE_DYNAMIC_ARRAY (data);
  }

  value1 = NIL; mv_count = 1;
  skipSTACK (9);
}


defun XLIB:DELETE-PROPERTY (2)	// OK
{
  Display *dpy;
  Window win = get_window_and_display (STACK_1, &dpy);
  Atom atom  = get_xatom_nointern (dpy, STACK_0);

  if (atom != None)
    {
      begin_call ();
      XDeleteProperty (dpy, win, atom);
      end_call ();
    }

  value1 = NIL; mv_count = 1;
  skipSTACK (2);		// all done
}

//
//  XLIB:GET-PROPERTY window property &key :type (:start 0) :end :delete-p (:result-type `list) :transform
//
//  returns:  data        -- Type sequence
//            type        -- Type xatom
//            format      -- Type (member 8 16 32)
//            bytes-after -- Type card32
//
defun XLIB:GET-PROPERTY (2, 0, norest, key, 6, (:TYPE :START :END :DELETE-P :RESULT-TYPE :TRANSFORM)) //OK
{
  // input:
  Display *display;
  Window w;
  Atom property;
  long long_offset, long_length;
  Bool delete;
  Atom req_type;
  // output:
  Atom actual_type_return;
  int actual_format_return;
  unsigned long nitems_return;
  unsigned long bytes_after_return;
  unsigned char *prop_return = NULL;
  int r;

  w = get_xid_object_and_display (`XLIB::WINDOW`, STACK_7, &display);
  property = get_xatom (display, STACK_6);
  
  // How is :start/:end counted?
  // CLX counts the same way libX counts [This should be documented.]
  long_offset = (eq (STACK_4, unbound) || eq (STACK_4, NIL)) ? 0 : get_uint32 (STACK_4);
  long_length = (eq (STACK_3, unbound) || eq (STACK_3, NIL)) ? 0x7FFFFFFF : (get_uint32(STACK_3) - long_offset);
  delete = (eq (STACK_2, unbound) || eq (STACK_2, NIL)) ? 0 : 1;
  if (eq (STACK_5, unbound) || eq (STACK_5, NIL))
    req_type = AnyPropertyType;
  else
    req_type = get_xatom (display, STACK_5);

  begin_call ();
  r = XGetWindowProperty (display, w, property, long_offset, long_length, delete, req_type,
			  &actual_type_return, &actual_format_return,
			  &nitems_return, &bytes_after_return, &prop_return);
  end_call ();
  
  if (actual_type_return == None)
    {
      pushSTACK (NIL);
      pushSTACK (NIL);
      pushSTACK (fixnum (0));
      pushSTACK (fixnum (0));
    }
  else
    {
      if (req_type != AnyPropertyType && actual_type_return != req_type)
	{
	  pushSTACK (NIL);
	}
      else
	{
	  uintC i;
	  object *transform_f = &(STACK_0);
	  object *result_type_f = &(STACK_1);
      
	  for (i = 0; i < nitems_return; i++)
	    {
	      unless (eq (*transform_f, unbound))
		pushSTACK (*transform_f); // transform function ..
	  
	      switch (actual_format_return)
		{
		case  8: pushSTACK (make_uint8  (prop_return[i])); break;
		case 16: pushSTACK (make_uint16 (((unsigned short*)prop_return)[i])); break;
		case 32: pushSTACK (make_uint32 (((unsigned long*) prop_return)[i])); break;
		default:
		  NOTREACHED;
		}

	      if (eq (*transform_f, unbound))
		{
		}
	      else
		{
		  funcall (L(funcall), 2); // apply the transform function
		  pushSTACK (value1);
		}
	    }
	  if (eq (*result_type_f, `LIST`) || eq (*result_type_f, unbound))
	    {
	      funcall (L(list), nitems_return);
	    }
	  else
	    {
	      funcall (L(vector), nitems_return);
	      coerce_it (*result_type_f);
	    }
      
	  pushSTACK (value1);
	}
      
      if (prop_return)
        {
          begin_call ();
          XFree (prop_return);
          end_call ();
        }
      
	{
	  char *name;
          begin_call ();
          name = XGetAtomName (display, actual_type_return);
          end_call ();
	  pushSTACK (asciz_to_string (name, misc_encoding ()));
	  pushSTACK (`KEYWORD`);
	  funcall (L(intern), 2);
	  pushSTACK (value1);
          begin_call ();
          XFree (name);
          end_call ();
	}
      pushSTACK (make_uint8 (actual_format_return));
      pushSTACK (make_uint32 (bytes_after_return));
    }
  value4 = popSTACK ();
  value3 = popSTACK ();
  value2 = popSTACK ();
  value1 = popSTACK ();
  mv_count = 4;
  skipSTACK (8);
}

//
//  XLIB:LIST-PROPERTIES window &key (:result-type `list)
//	  
defun XLIB:LIST-PROPERTIES (1, 0, norest, key, 1, (:RESULT-TYPE)) //OK
{
  int num_props, i;

  Display *dpy;
  Window win   = get_window_and_display (STACK_1, &dpy);
  Atom *props;

  begin_call ();
    props = XListProperties (dpy, win, &num_props);
  end_call ();

  // Now push all properties ...
  for (i = 0; i < num_props; i++)
    {
      char *name;
      begin_call ();
      name = XGetAtomName (dpy, props[i]);
      end_call ();
      pushSTACK (asciz_to_string (name, misc_encoding ()));
      pushSTACK (`KEYWORD`);
      funcall (L(intern), 2);
      pushSTACK (value1);
      begin_call ();
      XFree (name);
      end_call ();
    }

  begin_call ();
    if (props) XFree (props);
  end_call ();
  
  // Now cons `em together
  funcall (L(list), num_props);

  // coerce it to the right type if necessary
  if (!eq (STACK_0, unbound))
    {
      pushSTACK (value1);
      pushSTACK (STACK_1);
      funcall (L(coerce), 2);
    }

  skipSTACK (2);		// all done
}

//
// XLIB:ROTATE-PROPERTIES window properties &optional (delta 1)
//
defun XLIB:ROTATE-PROPERTIES (2, 1)
{
  Display *dpy;
  Window win   = get_window_and_display (STACK_2, &dpy);
  int delta    = (eq (STACK_0, unbound) ? 1 : get_sint32 (STACK_0));
  int num_props, i;

  pushSTACK (STACK_1);
  funcall (L(length), 1);
  num_props = get_uint32 (value1);

  {
    DYNAMIC_ARRAY (props, Atom, num_props);

    for (i = 0; i < num_props; i++)
      {
	pushSTACK (STACK_1);
	pushSTACK (fixnum (i));
	funcall (L(elt), 2);
	props[i] = get_xatom (dpy, value1);
      }

    begin_call ();
    XRotateWindowProperties (dpy, win, props, num_props, delta);
    end_call ();

    FREE_DYNAMIC_ARRAY (props);
  }
  
  value1 = NIL; mv_count = 1;
  skipSTACK (3);		// all done
}

/* 11.3  Selections */

//
// CONVERT-SELECTION selection type requestor &optional property time
//
defun XLIB:CONVERT-SELECTION (3, 2)
{
  Display *dpy;
  Window requestor = get_window_and_display (STACK_2, &dpy);
  Atom target      = get_xatom (dpy, STACK_3);
  Atom selection   = get_xatom (dpy, STACK_4);
  Atom property    = gunboundp (STACK_1) ? None : get_xatom (dpy, STACK_1);
  Time time        = get_timestamp (STACK_0);
  
  begin_call ();
  XConvertSelection (dpy, selection, target, property, requestor, time);
  end_call ();

  value1 = NIL; mv_count = 1;
}

//
// XLIB:SELECTION-OWNER display selection
//
defun XLIB:SELECTION-OWNER (2)
{
  Display *dpy = (pushSTACK (STACK_1), pop_display ());
  Atom selection = get_xatom (dpy, STACK_0);
  Window owner;

  begin_call ();
  owner = XGetSelectionOwner (dpy, selection);
  end_call ();
  
  value1 = make_window (STACK_1, owner); mv_count = 1;
  skipSTACK (2);
}

//
// (SETF (XLIB:SELECTION-OWNER display selection &optional time) owner)
// == (XLIB:SELECTION-OWNER-SETTER owner display selection &optional time)
//
defun XLIB:SELECTION-OWNER-SETTER (3, 1)
{
  Window owner = get_window (STACK_3);
  Display *dpy = (pushSTACK (STACK_2), pop_display ());
  Atom selection = get_xatom (dpy, STACK_1);
  Time time = get_timestamp (STACK_0);

  begin_call ();
  XSetSelectionOwner (dpy, selection, owner, time);
  end_call ();
  
  value1 = STACK_3; mv_count = 1;
  skipSTACK (4);
}


// ----------------------------------------------------------------------------------------------------
//  Chapter 12  Events and Input
// ----------------------------------------------------------------------------------------------------

/* 12.3  Processing Events */

//
//  First of all, we have to enter all the nasty events, together with all its
//  slots, type information, etc.  It is up to the two different functions for
//  assembling and disassembling event to provide the right macro definitions for
//  DEF_EVENT, ESLOT, and ESLOT2; [I want to do it only *ONCE*, so this klugde.]
// 
//    DEF_EVENT ( <lisp event key>, <C name of event key>, <C type of struct>, <C name of struct in XEvent> )
//        -- start defining an event.
// 
//    ESLOT ( <lisp slot name>, <type>, <C slot> )
//        -- define a slot
// 
//    ESLOT2 ( <lisp slot name>, <type>, <C slot> )
//        -- same as ESLOT, but for objects, which needs the display.
//
//    ESLOT3 is just for the key_vector, since you cannot assign arrays in C
//           usually, but must pass a pointer.
//
//    ESLOT4 is just used for the atom slot, since get_xatom requires an display
//           argument
//
// (If your preprocessor or your compiler can't eat this, hmm... get a new one.)
//

#define COMMON_INPUT_EVENT\
    ESLOT2(`:WINDOW`,           window,                 window)					\
    ESLOT2(`:CHILD`,            window,                 subwindow)				\
    ESLOT2(`:ROOT`,             window,                 root)					\
    ESLOT (`:X`,                sint16,                 x)					\
    ESLOT (`:Y`,                sint16,                 y)					\
    ESLOT (`:ROOT-X`,           sint16,                 x_root)					\
    ESLOT (`:ROOT-Y`,           sint16,                 y_root)					\
    ESLOT (`:STATE`,            uint16,                 state)					\
    ESLOT (`:TIME`,             uint32,                 time)					\
    ESLOT (`:SAME-SCREEN-P`,    bool,                   same_screen)

#define ALL_EVENT_DEFS										\
  DEF_EVENT (`:KEY-PRESS`, KeyPress, XKeyPressedEvent, xkey)					\
    ESLOT (`:CODE`,             uint8,                  keycode)				\
    COMMON_INPUT_EVENT										\
    												\
  DEF_EVENT (`:KEY-RELEASE`, KeyRelease, XKeyReleasedEvent, xkey)				\
    ESLOT (`:CODE`,             uint8,                  keycode)				\
    COMMON_INPUT_EVENT										\
												\
  DEF_EVENT (`:BUTTON-PRESS`, ButtonPress, XButtonPressedEvent, xbutton)			\
    ESLOT (`:CODE`,             uint8,                  button)					\
    COMMON_INPUT_EVENT										\
    												\
  DEF_EVENT (`:BUTTON-RELEASE`, ButtonRelease, XButtonReleasedEvent, xbutton)			\
    ESLOT (`:CODE`,             uint8,                  button)					\
    COMMON_INPUT_EVENT										\
												\
  DEF_EVENT (`:MOTION-NOTIFY`, MotionNotify, XMotionEvent, xmotion)				\
    ESLOT (`:HINT-P`,           bool,                   is_hint)				\
    COMMON_INPUT_EVENT										\
												\
  DEF_EVENT (`:ENTER-NOTIFY`, EnterNotify, XEnterWindowEvent, xcrossing)			\
    ESLOT (`:MODE`,             crossing_mode,          mode)					\
    ESLOT (`:KIND`,             crossing_kind,          detail)					\
    ESLOT (`:FOCUS-P`,          bool,		        focus)					\
    COMMON_INPUT_EVENT										\
    												\
  DEF_EVENT (`:LEAVE-NOTIFY`, LeaveNotify, XLeaveWindowEvent, xcrossing)			\
    ESLOT (`:MODE`,             crossing_mode,          mode)					\
    ESLOT (`:KIND`,             crossing_kind,          detail)					\
    ESLOT (`:FOCUS-P`,          bool,		        focus)					\
    COMMON_INPUT_EVENT										\
    												\
  DEF_EVENT (`:FOCUS-IN`, FocusIn, XFocusChangeEvent, xfocus)					\
    ESLOT2(`:WINDOW`,		window,			window)					\
    ESLOT (`:MODE`,             focus_mode,             mode)					\
    ESLOT (`:KIND`,             focus_detail,           detail)					\
												\
  DEF_EVENT (`:FOCUS-OUT`, FocusOut, XFocusChangeEvent, xfocus)					\
    ESLOT2(`:WINDOW`,		window,			window)					\
    ESLOT (`:MODE`,             focus_mode,             mode)					\
    ESLOT (`:KIND`,             focus_detail,           detail)					\
												\
  DEF_EVENT (`:EXPOSURE`, Expose, XExposeEvent, xexpose)					\
    ESLOT2(`:WINDOW`,           window,                 window)					\
    ESLOT (`:X`,                sint16,                 x)					\
    ESLOT (`:Y`,                sint16,                 y)					\
    ESLOT (`:WIDTH`,            sint16,                 width)					\
    ESLOT (`:HEIGHT`,           sint16,                 height)					\
    ESLOT (`:COUNT`,            uint16,                 count)					\
												\
  DEF_EVENT (`:GRAPHICS-EXPOSURE`, GraphicsExpose, XGraphicsExposeEvent, xgraphicsexpose)	\
    ESLOT2(`:DRAWABLE`,         drawable,               drawable)				\
    ESLOT (`:X`,                sint16,                 x)					\
    ESLOT (`:Y`,                sint16,                 y)					\
    ESLOT (`:WIDTH`,            sint16,                 width)					\
    ESLOT (`:HEIGHT`,           sint16,                 height)					\
    ESLOT (`:COUNT`,            uint16,                 count)					\
    ESLOT (`:MAJOR`,            uint8,                  major_code)				\
    ESLOT (`:MINOR`,            uint16,                 minor_code)				\
												\
  DEF_EVENT (`:KEYMAP-NOTIFY`, KeymapNotify, XKeymapEvent, xkeymap)				\
    ESLOT2(`:WINDOW`,           window,                 window)					\
    ESLOT3(`:KEYMAP`,           key_vector,             key_vector)				\
												\
  DEF_EVENT (`:MAPPING-NOTIFY`, MappingNotify, XMappingEvent, xmapping)				\
    ESLOT (`:COUNT`,            uint8,                  count)					\
    ESLOT (`:START`,            uint8,                  first_keycode)				\
    ESLOT (`:REQUEST`,          mapping_request,        request)				\
												\
  DEF_EVENT (`:NO-EXPOSURE`, NoExpose, XNoExposeEvent, xnoexpose)				\
    ESLOT2(`:DRAWABLE`,         drawable,               drawable)				\
    ESLOT (`:MAJOR`,            uint8,                  major_code)				\
    ESLOT (`:MINOR`,            uint16,                 minor_code)				\
												\
  DEF_EVENT (`:CIRCULATE-NOTIFY`, CirculateNotify, XCirculateEvent, xcirculate)			\
    ESLOT2(`:WINDOW`,           window,                 window)					\
    ESLOT (`:PLACE`,            top_or_bottom,          place)					\
												\
  DEF_EVENT (`:CONFIGURE-NOTIFY`, ConfigureNotify, XConfigureEvent, xconfigure)			\
    ESLOT2(`:WINDOW`,           window,			window)					\
    ESLOT (`:X`,                uint16,			x)					\
    ESLOT (`:Y`,                uint16,			y)					\
    ESLOT (`:WIDTH`,            uint16,			width)					\
    ESLOT (`:HEIGHT`,           uint16,			height)					\
    ESLOT (`:BORDER-WIDTH`,     uint16,			border_width)				\
    ESLOT2(`:ABOVE-SIBLING`,    window,			above)					\
    ESLOT (`:OVERRIDE-REDIRECT-P`,bool,			override_redirect)			\
												\
  DEF_EVENT (`:CREATE-NOTIFY`, CreateNotify, XCreateWindowEvent, xcreatewindow)			\
    ESLOT2(`:PARENT`,		window,			parent)					\
    ESLOT2(`:WINDOW`,		window,			window)					\
    ESLOT (`:X`,		uint16,			x)					\
    ESLOT (`:Y`,		uint16,			y)					\
    ESLOT (`:WIDTH`,            uint16,			width)					\
    ESLOT (`:HEIGHT`,           uint16,			height)					\
    ESLOT (`:BORDER-WIDTH`,     uint16,			border_width)				\
												\
  DEF_EVENT (`:DESTROY-NOTIFY`, DestroyNotify, XDestroyWindowEvent, xdestroywindow)		\
    ESLOT2(`:WINDOW`,		window,			window)					\
												\
  DEF_EVENT (`:GRAVITY-NOTIFY`, GravityNotify, XGravityEvent, xgravity)				\
    ESLOT2(`:WINDOW`, 		window,			window)					\
    ESLOT (`:X`,      		uint16,			x)					\
    ESLOT (`:Y`,      		uint16,			y)					\
												\
  DEF_EVENT (`:MAP-NOTIFY`, MapNotify, XMapEvent, xmap)						\
    ESLOT2(`:WINDOW`,		window,			window)					\
    ESLOT (`:OVERRIDE-REDIRECT-P`,bool,			override_redirect)			\
												\
  DEF_EVENT (`:REPARENT-NOTIFY`, ReparentNotify, XReparentEvent, xreparent)			\
    ESLOT2(`:WINDOW`,		window,			window)					\
    ESLOT2(`:PARENT`,		window,			parent)					\
    ESLOT (`:X`,		uint16,			x)					\
    ESLOT (`:Y`,		uint16,			y)					\
												\
  DEF_EVENT (`:UNMAP-NOTIFY`, UnmapNotify, XUnmapEvent, xunmap)					\
    ESLOT2(`:WINDOW`,		window,			window)					\
    ESLOT (`:CONFIGURE-P`,	bool,			from_configure)				\
												\
  DEF_EVENT (`:VISIBILITY-NOTIFY`, VisibilityNotify, XVisibilityEvent, xvisibility)		\
    ESLOT2(`:WINDOW`,		window,			window)					\
    ESLOT (`:STATE`,		visibility_state,	state)					\
												\
  DEF_EVENT (`:CIRCULATE-REQUEST`, CirculateRequest, XCirculateRequestEvent, xcirculaterequest)	\
    ESLOT2(`:PARENT`,		window,			parent)					\
    ESLOT2(`:WINDOW`,		window,			window)					\
    ESLOT (`:PLACE`,		top_or_bottom,		place)					\
												\
  DEF_EVENT (`:COLORMAP-NOTIFY`, ColormapNotify, XColormapEvent, xcolormap)			\
    ESLOT2(`:WINDOW`,		window,			window)					\
    ESLOT2(`:COLORMAP`,		colormap,		colormap)				\
    ESLOT (`:NEW-P`,		bool,			new)					\
    ESLOT (`:INSTALLED-P`,	bool,			state)					\
												\
  DEF_EVENT (`:CONFIGURE-REQUEST`, ConfigureRequest, XConfigureRequestEvent, xconfigurerequest)	\
    ESLOT2(`:PARENT`,		window,			parent)					\
    ESLOT2(`:WINDOW`,		window,			window)					\
    ESLOT (`:X`,		uint16,			x)					\
    ESLOT (`:Y`,		uint16,			y)					\
    ESLOT (`:WIDTH`,		uint16,			width)					\
    ESLOT (`:HEIGHT`,		uint16,			height)					\
    ESLOT (`:BORDER-WIDTH`,	uint16,			border_width)				\
    ESLOT (`:STACK-MODE`,	stack_mode,		detail)					\
    ESLOT2(`:ABOVE-SIBLING`,	window,			above)					\
    ESLOT (`:VALUE-MASK`,	uint16,			value_mask)				\
												\
  DEF_EVENT (`:MAP-REQUEST`, MapRequest, XMapRequestEvent, xmaprequest)				\
    ESLOT2(`:PARENT`,		window,			parent)					\
    ESLOT2(`:WINDOW`,		window,			window)					\
												\
  DEF_EVENT (`:RESIZE-REQUEST`, ResizeRequest, XResizeRequestEvent, xresizerequest)		\
    ESLOT2(`:WINDOW`,		window,			window)					\
    ESLOT (`:WIDTH`,		uint16,			width)					\
    ESLOT (`:HEIGHT`,		uint16,			height)					\
												\
  DEF_EVENT (`:CLIENT-MESSAGE`, ClientMessage, XClientMessageEvent, xclient)			\
    /* FIXME missing...	*/									\
												\
  DEF_EVENT (`:PROPERTY-NOTIFY`, PropertyNotify, XPropertyEvent, xproperty)			\
    ESLOT2(`:WINDOW`,		window,			window)					\
    ESLOT4(`:ATOM`,		xatom,			atom)					\
    ESLOT (`:STATE`,		new_value_or_deleted,	state)					\
    ESLOT (`:TIME`,		uint32,			time)					\
												\
  DEF_EVENT (`:SELECTION-CLEAR`, SelectionClear, XSelectionClearEvent, xselectionclear)		\
    ESLOT2(`:WINDOW`,		window,			window)					\
    ESLOT4(`:SELECTION`,	xatom,			selection)				\
    ESLOT (`:TIME`,		uint32,			time)					\
												\
  DEF_EVENT (`:SELECTION-NOTIFY`, SelectionNotify, XSelectionEvent, xselection)			\
    ESLOT4(`:SELECTION`,	xatom,			selection)				\
    ESLOT4(`:TARGET`,		xatom,			target)					\
    ESLOT4(`:PROPERTY`,		xatom,			property)				\
    ESLOT (`:TIME`,		uint32,			time)					\
												\
  DEF_EVENT (`:SELECTION-REQUEST`, SelectionRequest, XSelectionRequestEvent, xselectionrequest)	\
    ESLOT2(`:REQUESTOR`,	window,			requestor)				\
    ESLOT4(`:SELECTION`,	xatom,			selection)				\
    ESLOT4(`:TARGET`,		xatom,			target)					\
    ESLOT4(`:PROPERTY`,		xatom,			property)				\
    ESLOT (`:TIME`,		uint32,			time)					\
    

#define EMACS_IS_BROKEN \
}    
//end of nasty zone.

local int disassemble_event_on_stack (XEvent *ev, object *dpy_objf)
     /* Disassembles an X event onto the stack and returns the number of elements
      * push to the stack. [You can then neatly issue a funcall or list call using
      * these stack elements.]
      */
{
#define ESLOT(lispname,type,cslot)					\
	pushSTACK ((lispname));						\
	pushSTACK (make_##type (container->cslot));			\
	cnt += 2;

#define ESLOT2(lispname,type,cslot)					\
	pushSTACK ((lispname));						\
	pushSTACK (make_##type (*dpy_objf, container->cslot));		\
	cnt += 2;

#define ESLOT3 ESLOT

#define ESLOT4(lispname,type,cslot)					\
	pushSTACK ((lispname));						\
	{								\
	  Display *dpy = (pushSTACK(*dpy_objf), pop_display());		\
	  pushSTACK (make_##type (dpy, container->cslot));		\
	}								\
	cnt += 2;

#define DEF_EVENT(lispname, cname, c_container_type, c_container)	\
   }									\
 break;									\
 case cname:								\
   {									\
     c_container_type *container = &(ev->c_container);			\
     pushSTACK (`:EVENT-KEY`);						\
     pushSTACK ((lispname)); cnt += 2;

  int cnt = 0;

  /* These attributes are common to all events (hopefully) */
  pushSTACK (`:DISPLAY`); pushSTACK (STACK_6); cnt += 2;
  pushSTACK (`:EVENT-CODE`); pushSTACK (fixnum (ev->type)); cnt += 2;
  pushSTACK (`:SEND-EVENT-P`); pushSTACK (make_bool (ev->xany.send_event)); cnt += 2;
  pushSTACK (`:EVENT-WINDOW`); pushSTACK (make_window (*dpy_objf, ev->xany.window)); cnt += 2;

  /* BTW I really  hate   it that the   naming convention  for events   is  not
   * consistent, while you have a name for the mask, the event type, the event
   * substructure  and all  may have different  names.  (i.e. you  have to say
   * 'exposure' and sometimes 'expose') This bothers me really .. :-{}^
   */

  switch (ev->type)
    {
    default:
      {
	// Wat nu?
	// Propabably raise some error?!
	
	// THIS LOOKS STRANGE?! 
	// Well, the first ALL_EVENTS gives is '}' + 'break;' the last thing is '{', so ...
	
	ALL_EVENT_DEFS
      }
      break;
      
    }
  return cnt;

#undef DEF_EVENT
#undef ESLOT
#undef ESLOT2
#undef ESLOT3
#undef ESLOT4
}

local void travel_queque (Display *dpy, int peek_p, int discard_p, int force_output_p, int timeout)
     //
     //    peek_p == not remove-processed-p
     // discard_p == remove-unprocessed-p
     // timeout in second or -1 to block.
     // BUGS:
     //  - take care that discard-current-event will work as expected!
     //  - also we need an unwind protect here!
     //  - handler may also be a vector of functions. [How strange?!]
     //  - timeout should be a 'struct timeval'.
     // TODO:
     //  - I want this routine to be interruptible by user in a continueable fashion.
     //    Way to go:
     //    interruptp( { pushSTACK( <subr name> ); tast_break(); goto <continue>; } );
     // Hmm
     //  It may be better to use the appropriate XIf... function.
     //  [What happens if we throw out of `em? Also they also seem to block?! RTFM]
{
  XEvent ev;
  int cnt;
  int r;

travel_queque:

  if (timeout != -1)
    {
      begin_call ();
      XEventsQueued (dpy, force_output_p ? QueuedAfterFlush : QueuedAfterReading);
      r = QLength (dpy);
      end_call ();

      if (r == 0)
	{
	  int conn;
	  struct timeval tv;
	  fd_set ifds;
	  tv.tv_sec = timeout;
	  tv.tv_usec = 0;

	  conn = ConnectionNumber (dpy); // this is the fd.
	  FD_ZERO (&ifds);
	  FD_SET (conn, &ifds);
	  begin_call ();
	  r = select (conn+1, &ifds, NULL, NULL, &tv);
	  end_call ();

	  if ((r > 0) && FD_ISSET (conn, &ifds))
	    {
	      // timeout has to reduce by amount waited here for input; Or what?!
	      timeout = 0;
	    }
	  else
	    {
	      // Nothing there, so just return
	      value1 = NIL; mv_count = 1;
	      return;
	    }
	}
    }

  // .. so there is either now an event in the queue or we should hang:
  begin_call ();
  XPeekEvent (dpy, &ev);
  end_call ();
  
  cnt = disassemble_event_on_stack (&ev, &(STACK_5));
  // Now invoke the handler function
  funcall (STACK_(cnt+4), cnt);	// BUG: This may throw out of our control!
				//      We would need something like an unwind protect here
  				// But only if discard_p == NIL.
  

  // FIXME: We should probably check here, if somebody has already discarded this event?
  begin_call ();
  XNextEvent (dpy, &ev);
  end_call ();

  // Look what we got.
  if (nullp(value1))
    {
      if (discard_p)
	{
	  // travel_queque (dpy, peek_p, discard_p, force_output_p, timeout);
	  goto travel_queque;
	}
      else
	{
	  travel_queque (dpy, peek_p, discard_p, force_output_p, timeout);
	  begin_call ();
	  XPutBackEvent (dpy, &ev);
	  end_call ();
	}
    }
  else
    {
      // Handler successful
      if (peek_p)
	{
	  begin_call ();
	  XPutBackEvent (dpy, &ev);
	  end_call ();
	}
    }
}

local void get_timeout (object o, struct timeval *tv)
{
  // FIXME: should accept also fractions of a second.
  tv->tv_sec = get_uint32 (o);
  tv->tv_usec = 0;
}

//
// XLIB:PROCESS-EVENT display &key handler timout peek-p discard-p force-output-p
//
defun XLIB:PROCESS-EVENT (1, 0, norest, key, 5, (:HANDLER :TIMEOUT :PEEK-P :DISCARD-P :FORCE-OUTPUT-P))
{
  int force_output_p, discard_p, peek_p;
  int timeout;
  Display *dpy;

  // Fetch the arguments 
  pushSTACK (STACK_5); dpy = pop_display ();

  force_output_p = (!eq(STACK_0, unbound)) ? get_bool (STACK_0) : 1;
  discard_p      = (!eq(STACK_1, unbound)) ? get_bool (STACK_1) : 0;
  peek_p         = (!eq(STACK_2, unbound)) ? get_bool (STACK_2) : 0;

  timeout = -1;
  if (!eq(STACK_3, unbound) && integerp (STACK_3))	// XXX
    timeout = get_uint32 (STACK_3);
  
  if (eq(STACK_4, unbound))
    {
      /* error */
      NOTIMPLEMENTED;
    }

  // Now go into the recursive event queque travel routine

  travel_queque (dpy, peek_p, discard_p, force_output_p, timeout);

  // mv_space and mv_count are set by the return of the handler function
  
  skipSTACK (6);
}


/* 12.4  Managing the Event Queue */

local void encode_event (uintC n, object event_key, Display *dpy, XEvent *ev)
     // encodes an event, which lies in the top /n/ stack locations into ev
     // event-key is an optional event key to use, it may also be unbound.
     // But hey! Without an event key we could not assemble an event?!
{
  int ofs;
  int grasp (object slot)
    {
      uintC o;
      for (o = 1 ; o < n; o += 2)
	if (eq (STACK_(o+1), slot))
	  return o;
      return 0;
    }

  pushSTACK (event_key);
  
#define DEF_EVENT(lnam, cnam, ctype, cslot)		\
    }							\
  else							\
  if (eq (STACK_0, lnam))				\
    {							\
      ctype *event = &(ev->cslot);

#define ESLOT(lnam, type, cslot)			\
    {							\
      if ((ofs = grasp (lnam)))				\
	event->cslot = get_##type (STACK_(ofs));	\
      else						\
	event->cslot = 0;				\
    }

#define ESLOT2(lnam, type, cslot) ESLOT(lnam,type,cslot)
#define ESLOT3(lnam, type, cslot)			\
    {							\
      if ((ofs = grasp (lnam)))				\
	get_##type (STACK_(ofs), (event->cslot));	\
      else						\
	{ /* ??? */ }					\
    }
							
#define ESLOT4(lnam, type, cslot)			\
    {							\
      if ((ofs = grasp (lnam)))				\
	event->cslot = get_##type (dpy, STACK_(ofs));	\
      else						\
	event->cslot = 0;				\
    }

  if(0)
    {
      // Same as above in disassemble_event_on_stack this looks strange, but is
      // right, since the first thing DEF_EVENT gives is "} else" the last
      // thing is "if (..) {", so ....

      ALL_EVENT_DEFS
    }
  else
    {
      pushSTACK (STACK_0);		// event_key
      pushSTACK (`XLIB::EVENT-KEY`);	// desired type
      my_standard_type_error (TheSubr (subr_self)->name);
      BLOEDER_COMPILER_VERDAMMT_NOCHMAL;
    }

#undef DEF_EVENT
#undef ESLOT
#undef ESLOT2
#undef ESLOT3
#undef ESLOT4
}

// queue-event display event-key &rest args &key append-p send-event-p &allow-other-keys
//
//   The event is put at the head of the queue if append-p is nil, else the tail.
//   Additional arguments depend on event-key, and are as specified above with
//   declare-event, except that both resource-ids and resource objects are accepted
//   in the event components.
//
defun XLIB:QUEUE-EVENT (0, 0, rest, nokey, 0, NIL)
//defun XLIB:QUEUE-EVENT (2, 0, rest, key_allow, 2, (:APPEND-P :SEND-EVENT-P))
{UNDEFINED}
/* Take a look at XPutBackEvent, but that functions seems only to
 * put events on the head of the queue!
 * Maybe we should go and build our own event queque?
 * Or we fight with the internals of libX?
 * But we could travel the whole event queque until we come to a point, where the
 * queque has ended; XPutBackEvent the event to be added at end and XputBack
 * all other event above that. [Not very fast, but portable]
 * also send-event-p is not in the manual.
 */


//
// XLIB:DISCARD-CURRENT-EVENT display
//  -->
// discarded-p -- Type boolean
//
//   Discard the current event for DISPLAY.
//   Returns NIL when the event queue is empty, else T.
//   To ensure events aren't ignored, application code should only call
//   this when throwing out of event-case or process-next-event, or from
//   inside even-case, event-cond or process-event when :peek-p is T and
//   :discard-p is NIL.
//
defun XLIB:DISCARD-CURRENT-EVENT (1)
     // FIXME -- here the manual is bit unpreciese
     // - Should we hang?
     // - Should we return T/NIL before discarding? properly not.
{
  Display *dpy = pop_display ();

  if (QLength (dpy))		// no begin/end_call here QLength is a macro
    {
      XEvent trash_can;
      begin_call ();
      XNextEvent (dpy, &trash_can);
      end_call ();
      value1 = T;
    }
  else
    value1 = NIL;
  mv_count = 1;
}

//
// XLIB:EVENT-LISTEN display &optional (timeout 0)
//
//  Returns:
//     event-count -- Type (or null integer).
//
//  Returns the number of events queued locally. If the event queue is empty,
//  event-listen waits for an event to arrive. If timeout is non-nil and no event
//  arrives within the specified timeout interval (given in seconds), event-listen
//  returns nil; if timeout is nil, event-listen will not return until an event
//  arrives.
//
defun XLIB:EVENT-LISTEN (1, 1)
{
  Display *dpy;
  struct timeval timeout;
  int r;
  XEvent trashcan;
  
  pushSTACK (STACK_1); dpy = pop_display ();

  if (nullp(STACK_0))
    {
      // Block
      begin_call ();
      until ((r = QLength (dpy))) XPeekEvent (dpy, &trashcan);
      end_call ();
      value1 = make_uint32 (r);
    }
  else
    {
      if (eq(STACK_0, unbound))
	timeout.tv_sec = timeout.tv_usec = 0;
      else
	get_timeout (STACK_0, &timeout);

      r = QLength (dpy);

      if (r)
	{
	  value1 = make_uint32 (r);
	}
      else
	{
	  // Wait
	  int conn;
	  fd_set ifds;

	  conn = ConnectionNumber (dpy); // this is the fd.
	  FD_ZERO (&ifds);
	  FD_SET (conn, &ifds);
	  begin_call ();
	  r = select (conn+1, &ifds, NULL, NULL, &timeout);
	  end_call ();
	  if ((r > 0) && FD_ISSET (conn, &ifds))
	    {
	      begin_call ();
	      r = XEventsQueued (dpy, QueuedAfterReading); // RTFS: To flush or not to flush is here the question!
	      end_call ();
	      value1 = make_uint32 (r);
	    }
	  else
	    {
	      value1 = NIL;
	    }
	}
    }
  skipSTACK (2);
  mv_count = 1;
}

/* 12.5  Sending Events */
//
//  XLIB:SEND-EVENT window event-key event-mask &rest event-slots &key propagate-p &allow-other-keys
//
//  NOTE: The MIT-CLX interface specifies a :display argument here, which is not necessary
//
defun XLIB:SEND-EVENT (0, 0, rest, nokey, 0, NIL)
     //defun XLIB:SEND-EVENT (3, 0, rest, key_allow, 2, (:PROPAGATE-P))
{
  if (argcount < 3) goto too_few;
  {
    XEvent event;
    Display             *dpy;
    Window            window = get_window_and_display (STACK_(argcount-1), &dpy);
    unsigned long event_mask = get_event_mask (STACK_(argcount-3));
    int propagate_p = 0;
    uintC i;
    
    // hunt for the :propagate-p
    for (i = 0; i < argcount; i += 2)
      if (eq (STACK_(i+1), `:PROPAGATE-P`))
	{
	  propagate_p = get_bool (STACK_(i));
	  break;
	}
    
    encode_event (argcount-3, STACK_(argcount-2), dpy, &event);
    begin_call ();
    XSendEvent (dpy, window, propagate_p, event_mask, &event);
    end_call ();
    // XSendEvent returns also some Status, should we interprete it?
    // If yes: How?!
    skipSTACK (argcount);
    value1 = NIL; mv_count = 1;
    return;
  }
  
 too_few:
  NOTIMPLEMENTED;
}

/* 12.6  Pointer Position */

// XLIB:QUERY-POINTER window
//  -> [1] x
//     [2] y
//     [3] same-screen-p
//     [4] child
//     [5] state-mask
//     [6] root-x
//     [8] root-y
//     [9] root
//

defun XLIB:QUERY-POINTER (1)
{
  Display *dpy;
  Window   win = get_window_and_display (STACK_0, &dpy);
  
  Window root, child;
  int root_x, root_y;
  int win_x, win_y;
  unsigned int mask;
  Bool same_screen_p;

  begin_call ();
  same_screen_p = XQueryPointer (dpy, win, &root, &child, &root_x, &root_y, &win_x, &win_y, &mask);
  end_call ();
  
  pushSTACK (get_display_obj (STACK_0));
  pushSTACK (make_window (STACK_0, root));
  pushSTACK (make_window (STACK_1, child));
  
  value1 = make_sint16 (win_x);
  value2 = make_sint16 (win_y);
  value3 = make_bool (same_screen_p);
  value4 = popSTACK ();	// child
  value5 = make_uint16 (mask);
  value6 = make_sint16 (root_x);
  value7 = make_sint16 (root_y);
  value8 = popSTACK ();	// root
  mv_count = 8;
  skipSTACK (2);	// all done
}

//
//  XLIB:GLOBAL-POINTER-POSITION display
//  -> [1] root-x
//     [2] root-y
//     [3] root
//
defun XLIB:GLOBAL-POINTER-POSITION (1)
{
  Display *dpy;
  Window   win;
  
  Window root, child;
  int root_x, root_y;
  int win_x, win_y;
  unsigned int mask;
  Bool same_screen_p;
  
  pushSTACK (STACK_0); dpy = pop_display ();
  begin_call ();
  same_screen_p = XQueryPointer (dpy, DefaultRootWindow (dpy), &root, &child, &root_x, &root_y, &win_x, &win_y, &mask);
  end_call ();
  
  value1 = make_sint16 (root_x);
  value2 = make_sint16 (root_y);
  value3 = make_window (STACK_0, root);
  mv_count = 3;
  skipSTACK (1);
}

//
//  XLIB:POINTER-POSITION window
//  -> [1] x
//     [2] y
//     [3] same-screen-p
//     [4] child
//
defun XLIB:POINTER-POSITION (1)
{
  C_xlib_query_pointer ();
  mv_count = 4;
}

//
//  XLIB:MOTION-EVENTS window &key :start :stop (:result-type `list)
//
//  -> (repeat-seq (int16 x) (int16 y) (timestamp time))
//
defun XLIB:MOTION-EVENTS (1, 0, norest, key, 3, (:START :STOP :RESULT-TYPE))
{
  Display *dpy;
  Window win = get_window_and_display (STACK_3, &dpy);
  Time start = get_timestamp (STACK_2);
  Time stop = get_timestamp (STACK_1);
  XTimeCoord *events = 0;
  int nevents = 0;

  begin_call ();
  events = XGetMotionEvents (dpy, win, start, stop, &nevents);
  end_call ();
  
  if (events)
    {
      int i;
      for (i = 0; i < nevents; i++)
	{
	  pushSTACK (make_sint16 (events[i].x));
	  pushSTACK (make_sint16 (events[i].y));
	  pushSTACK (make_uint32 (events[i].time));
	}
      funcall (L(list), 3*i);
      // XXX RTFS: Should I XFree on events?
    }
  else
    value1 = NIL;
  
  coerce_it (STACK_0);
  mv_count = 1;
  skipSTACK (4);
}

//
// XLIB:WARP-POINTER destination x y
//
defun XLIB:WARP-POINTER (3)
{
  int y = get_sint32 (popSTACK ());
  int x = get_sint32 (popSTACK ());
  Display *dpy;
  Window dest = get_window_and_display (popSTACK (), &dpy);

  begin_call ();
  XWarpPointer (dpy, None, dest, 0, 0, 0, 0, x, y);
  end_call ();
  
  value1 = NIL; mv_count = 1;
}

//
// XLIB:WARP-POINTER-RELATIVE display delta-x delta-y
//
defun XLIB:WARP-POINTER-RELATIVE (3)
{
  int dy = get_sint32 (popSTACK ());
  int dx = get_sint32 (popSTACK ());
  Display *dpy = pop_display ();

  begin_call ();
  XWarpPointer (dpy, None, None, 0, 0, 0, 0, dx, dy);
  end_call ();
  
  value1 = NIL; mv_count = 1;
}

//
// XLIB:WARP-POINTER-IF-INSIDE destination destination-x destination-y source source-x source-y
//                             &optional (source-width 0) (source-height 0)
//
defun XLIB:WARP-POINTER-IF-INSIDE (6, 2)
{
  Display *dpy;
  Window dest = get_window_and_display (STACK_7, &dpy);
  int dest_x = get_sint16 (STACK_6);
  int dest_y = get_sint16 (STACK_5);
  Window src = get_window (STACK_4);
  int src_x = get_sint16 (STACK_3);
  int src_y = get_sint16 (STACK_2);
  int src_w = (eq (STACK_1,unbound) ? 0 : get_sint16 (STACK_1));
  int src_h = (eq (STACK_0,unbound) ? 0 : get_sint16 (STACK_0));

  begin_call ();
  XWarpPointer (dpy, src, dest, src_x, src_y, src_w, src_h, dest_x, dest_y);
  end_call ();
  
  value1 = NIL; mv_count = 1;
  skipSTACK (8);
  return;
}

//
// XLIB:WARP-POINTER-RELATIVE-IF-INSIDE x-offset y-offset source source-x source-y &optional (source-width 0) (source-height 0)
//
defun XLIB:WARP-POINTER-RELATIVE-IF-INSIDE (5, 2)
{
  int x_off = get_sint16 (STACK_6);
  int y_off = get_sint16 (STACK_5);
  Display *dpy;
  Window src = get_window_and_display (STACK_4, &dpy);
  int src_x = get_sint16 (STACK_3);
  int src_y = get_sint16 (STACK_2);
  int src_w = eq (STACK_1,unbound) ? 0 : get_sint16 (STACK_1);
  int src_h = eq (STACK_0,unbound) ? 0 : get_sint16 (STACK_0);

  begin_call ();
  XWarpPointer (dpy, src, None, src_x, src_y, src_w, src_h, x_off, y_off);
  end_call ();
    
  value1 = NIL; mv_count = 1;
  skipSTACK (7);
  return;
}

/* 12.7  Managing Input Focus */

//
// XLIB:SET-INPUT-FOCUS dpy focus revert-to &optional time
//
// btw. why not (SETF INPUT-FOCUS) ?
//
// FIXME (RTFS): focus and revert-to are actually swapped in manual.
//
defun XLIB:SET-INPUT-FOCUS (3, 1)
{
  Time time;
  int revert_to;
  Display *dpy;
  Window focus;
  
  time = get_timestamp (popSTACK ());
  pushSTACK (`:NONE`);  
  pushSTACK (`:POINTER-ROOT`);
  pushSTACK (`:PARENT`);
  revert_to = get_enum (3);
  focus = get_window (popSTACK ());
  dpy = pop_display ();

  begin_call ();
  XSetInputFocus (dpy, focus, revert_to, time);
  end_call ();
  
  value1 = NIL; mv_count = 1;
}

//
//  XLIB:INPUT-FOCUS display
//  -->
//  focus     -- Type (or window (member :none :pointer-root))
//  revert-to -- Type (or window (member :none :pointer-root :parent))
//
// In the manual is said, that the revert-to could be a window, but
// the libX11 function just returns a state ?!  
//
defun XLIB:INPUT-FOCUS (1)
{
  Display *dpy;
  Window focus;
  int revert;
  
  pushSTACK (STACK_0);
  dpy = pop_display ();

  begin_call ();
  XGetInputFocus (dpy, &focus, &revert);
  end_call ();
  
  /* value1 (= focus) */
  if (focus == PointerRoot) pushSTACK (`:POINTER-ROOT`);
  else if (focus == None) pushSTACK (`:NONE`);
  else pushSTACK (make_window (STACK_0, focus));
  
  /* value2 (= revert) */
  if (revert == RevertToPointerRoot) pushSTACK (`:POINTER-ROOT`);
  else if (revert == RevertToParent) pushSTACK (`:PARENT`);
  else if (revert == RevertToNone) pushSTACK (`:NONE`);
  else pushSTACK (NIL);		// safty ...
  
  value2 = popSTACK ();
  value1 = popSTACK ();
  mv_count = 2;
}

local void ungrab_X (int (*X)(Display *dpy, Time time))
{
  Time    time = get_timestamp (popSTACK ());
  Display *dpy = pop_display ();

  begin_call ();
  X (dpy, time);
  end_call ();
  
  value1 = NIL; mv_count = 1;
  skipSTACK (2);
}

//
//  XLIB:GRAB-POINTER window event-mask &key :owner-p :sync-pointer-p :sync-keyboard-p :confine-to :cursor :time
//
defun XLIB:GRAB-POINTER (2, 0, norest, key, 6, (:OWNER-P :SYNC-POINTER-P :SYNC-KEYBOARD-P :CONFINE-TO :CURSOR :TIME))
{
  Display             *dpy;
  Window               win = get_window_and_display (STACK_7, &dpy);
  unsigned long event_mask = get_event_mask (STACK_6);
  Bool             owner_p = !(eq (STACK_5, unbound) || eq (STACK_5, NIL));
  Bool        sync_pointer = (eq (STACK_4, unbound) || eq (STACK_4, NIL));
  Bool       sync_keyboard = (eq (STACK_3, unbound) || eq (STACK_3, NIL));
  Window        confine_to = eq (STACK_2,unbound) ? None : get_window (STACK_2);
  Cursor            cursor = eq (STACK_1,unbound) ? None : get_cursor (STACK_1);
  Time                time = get_timestamp (STACK_0);
  int r;

  begin_call ();
  r = XGrabPointer (dpy, win, owner_p, event_mask, sync_pointer, sync_keyboard, confine_to, cursor, time);
  end_call ();
  
       if (r == AlreadyGrabbed)  value1 = `:ALREADY-GRABBED`;
  else if (r == GrabFrozen)      value1 = `:FROZEN`;
  else if (r == GrabInvalidTime) value1 = `:INVALID-TIME`;
  else if (r == GrabNotViewable) value1 = `:NOT-VIEWABLE`; // NIM
  else value1 = `:SUCCESS`;
  
  mv_count = 1;
  skipSTACK (8);
}

defun XLIB:UNGRAB-POINTER (1, 0, norest, key, 1, (:TIME))
{
  ungrab_X (XUngrabPointer);
}

//
// XLIB:CHANGE-ACTIVE-POINTER-GRAB display event-mask &optional cursor time
//
defun XLIB:CHANGE-ACTIVE-POINTER-GRAB (2, 2)
{
  Display *dpy = (pushSTACK (STACK_3), pop_display ());
  unsigned long event_mask = get_event_mask (STACK_2);
  Cursor            cursor = eq (STACK_1,unbound) ? None : get_cursor (STACK_1);
  Time                time = get_timestamp (STACK_0);

  begin_call ();
  XChangeActivePointerGrab (dpy, event_mask, cursor, time);
  end_call ();
  
  skipSTACK (4);
  value1 = NIL; mv_count = 1;
}

/* 12.9  Grabbing a Button */
//
//  XLIB:GRAB-BUTTON window button event-mask &key (:modifiers 0) :owner-p :sync-pointer-p :sync-keyboard-p :confine-to :cursor
//
defun XLIB:GRAB-BUTTON (3, 0, norest, key, 6, (:MODIFIERS :OWNER-P :SYNC-POINTER-P :SYNC-KEYBOARD-P :CONFINE-TO :CURSOR))
{
  Display             *dpy;
  Window               win = get_window_and_display (STACK_8, &dpy);
  int               button = !(eq (STACK_7, `:ANY`) ? AnyButton : get_uint8 (STACK_7));
  unsigned long event_mask = get_event_mask (STACK_6);
  unsigned int   modifiers = eq (STACK_0, unbound) ? 0 : get_modifier_mask (STACK_5);
  Bool             owner_p = !(eq (STACK_4, unbound) || eq (STACK_4, NIL));
  Bool        sync_pointer = (eq (STACK_3, unbound) || eq (STACK_3, NIL));
  Bool       sync_keyboard = (eq (STACK_2, unbound) || eq (STACK_2, NIL));
  Window        confine_to = eq (STACK_1,unbound) ? None : get_window (STACK_1);
  Cursor            cursor = eq (STACK_0,unbound) ? None : get_cursor (STACK_0);

  begin_call ();
  XGrabButton (dpy, button, modifiers, win, owner_p, event_mask, sync_pointer, sync_keyboard, confine_to, cursor);
  end_call ();
  
  value1 = NIL; mv_count = 1;
  skipSTACK (9);
}

defun XLIB:UNGRAB-BUTTON (2, 0, norest, key, 1, (:MODIFIERS))
{
  Display           *dpy;
  Window             win = get_window_and_display (STACK_2, &dpy);
  int               code = (eq (STACK_1, `:ANY`) ? AnyKey : get_uint8(STACK_1));
  unsigned int modifiers = (eq (STACK_0, unbound) ? 0 : get_modifier_mask (STACK_0));

  begin_call ();
  XUngrabButton (dpy, code, modifiers, win);
  end_call ();
  
  value1 = NIL; mv_count = 1;
  skipSTACK (3);
}

/* 12.10  Grabbing the Keyboard */
defun XLIB:GRAB-KEYBOARD (1, 0, norest, key, 4,
			  (:OWNER-P :SYNC-POINTER-P :SYNC-KEYBOARD-P :TIME))
{
  Display         *dpy;
  Window           win = get_window_and_display (STACK_4, &dpy);
  Bool         owner_p = (eq (STACK_3, unbound) ? 0 : get_bool (STACK_3));
  Bool  sync_pointer_p = (eq (STACK_2, unbound) ? 0 : get_bool (STACK_2)) ? GrabModeSync : GrabModeAsync;
  Bool sync_keyboard_p = (eq (STACK_1, unbound) ? 0 : get_bool (STACK_1)) ? GrabModeSync : GrabModeAsync;
  Time            time = get_timestamp (STACK_0);
  int r;

  begin_call ();
  r = XGrabKeyboard (dpy, win, owner_p, sync_pointer_p, sync_keyboard_p, time);
  end_call ();
  
       if (r == AlreadyGrabbed)  value1 = `:ALREADY-GRABBED`;
  else if (r == GrabFrozen)      value1 = `:FROZEN`;
  else if (r == GrabInvalidTime) value1 = `:INVALID-TIME`;
  else if (r == GrabNotViewable) value1 = `:NOT-VIEWABLE`;
  else value1 = `:SUCCESS`;
  
  mv_count = 1;
  skipSTACK (5);
}

defun XLIB:UNGRAB-KEYBOARD (1, 0, norest, key, 1, (:TIME))
{
  ungrab_X (XUngrabKeyboard);
}

/* 12.11  Grabbing a Key */
//
//  XLIB:GRAB-KEY window key &key (:modifiers 0) :owner-p :sync-pointer-p :sync-keyboard-p
//
defun XLIB:GRAB-KEY (2, 0, norest, key, 4,
		     (:MODIFIERS :OWNER-P :SYNC-POINTER-P :SYNC-KEYBOARD-P))
{
  Display           *dpy;
  Window             win = get_window_and_display (STACK_5, &dpy);
  int            keycode = get_uint8 (STACK_4);
  unsigned int modifiers = (eq (STACK_3, unbound) ? 0 : get_modifier_mask (STACK_3));
  Bool           owner_p = (eq (STACK_2, unbound) ? 0 : get_bool (STACK_2));
  Bool    sync_pointer_p = (eq (STACK_1, unbound) ? 0 : get_bool (STACK_1)) ? GrabModeSync : GrabModeAsync;
  Bool   sync_keyboard_p = (eq (STACK_0, unbound) ? 0 : get_bool (STACK_0)) ? GrabModeSync : GrabModeAsync;

  begin_call ();
  XGrabKey (dpy, keycode, modifiers, win, owner_p, sync_keyboard_p, sync_keyboard_p);
  end_call ();
  
  value1 = NIL; mv_count = 1;
  skipSTACK (6);
}

defun XLIB:UNGRAB-KEY (2, 0, norest, key, 1, (:MODIFIERS))
{
  Display           *dpy;
  Window             win = get_window_and_display (STACK_2, &dpy);
  int               code = (eq (STACK_1, `:ANY`) ? AnyKey : get_uint8(STACK_1));
  unsigned int modifiers = (eq (STACK_0, unbound) ? 0 : get_modifier_mask (STACK_0));

  begin_call ();
  XUngrabKey (dpy, code, modifiers, win);
  end_call ();
  
  value1 = NIL; mv_count = 1;
  skipSTACK (3);
}

/* 12.13  Releasing Queued Events */
defun XLIB:ALLOW-EVENTS (2, 1)
{
  Time timestamp = get_timestamp (popSTACK ());
  int mode;
  Display *dpy;
  
  pushSTACK (`:ASYNC-POINTER`);
  pushSTACK (`:SYNC-POINTER`);
  pushSTACK (`:REPLAY-POINTER`);
  pushSTACK (`:ASYNC-KEYBOARD`);
  pushSTACK (`:SYNC-KEYBOARD`);
  pushSTACK (`:REPLAY-KEYBOARD`);
  pushSTACK (`:ASYNC-BOTH`);
  pushSTACK (`:SYNC-BOTH`);
  mode = get_enum (8);
  dpy = pop_display ();

  begin_call ();
  XAllowEvents (dpy, mode, timestamp);
  end_call ();
  
  value1 = NIL; mv_count = 1;
}


// ----------------------------------------------------------------------------------------------------
//  Chapter 13  Resources
// ----------------------------------------------------------------------------------------------------

// Maybe we want simply to drop in the LISP code here?

/* 13.3  Basic Resource Database Functions */
##if 0
defun XLIB:MAKE-RESOURCE-DATABASE (0) {UN DEFINED}
defun XLIB:ADD-RESOURCE (3) {UN DEFINED}
defun XLIB:DELETE-RESOURCE (2) {UN DEFINED}
defun XLIB:MAP-RESOURCE (2, 0, rest, nokey, 0, NIL) {UN DEFINED}
defun XLIB:MERGE-RESOURCES (2) {UN DEFINED}

/* 13.4  Accessing Resource Values */
defun XLIB:GET-RESOURCE (5) {UN DEFINED}
defun XLIB:GET-SEARCH-TABLE (3) {UN DEFINED}
defun XLIB:GET-SEARCH-RESOURCE (3) {UN DEFINED}

/* 13.5  Resource Database Files */
defun XLIB:READ-RESOURCES (2, 0, norest, key, 3, (:KEY :TEST :TEST-NOT))
{UN DEFINED}
defun XLIB:WRITE-RESOURCES (2, 0, norest, key, 3, (:WRITE :TEST :TEST-NOT))
{UN DEFINED}
##endif


// ----------------------------------------------------------------------------------------------------
//  Chapter 14  Control Functions
// ----------------------------------------------------------------------------------------------------

/* 14.1  Grabbing the Server */
defun XLIB:GRAB-SERVER (1)
{
  Display *dpy = pop_display ();

  begin_call ();
  XGrabServer (dpy);
  end_call ();
  
  value1 = NIL; mv_count = 1;
}

defun XLIB:UNGRAB-SERVER (1)
{
  Display *dpy = pop_display ();

  begin_call ();
  XUngrabServer (dpy);
  end_call ();
  
  value1 = NIL; mv_count = 1;
}

/* 14.2  Pointer Control */
defun XLIB:CHANGE-POINTER-CONTROL (1, 0, norest, key, 2, (:ACCELERATION :THRESHOLD))
{
  Bool do_accel = False;
  Bool do_threshold = False;
  int accel_numerator = -1;
  int accel_denominator = -1;
  int threshold = -1;
  Display *dpy;
  
  if (!eq (STACK_0, unbound) && !eq (STACK_0,NIL))
    {
      do_threshold = True;
      threshold = eq (STACK_1, `:DEFAULT`) ? -1 : get_sint16 (STACK_0);
    }
  
  if (!eq (STACK_1, unbound) && !eq (STACK_1,NIL))
    {
      do_accel = True;
      if (eq (STACK_1, `:DEFAULT`))
	accel_numerator = -1;
      else
	{
	  // This is basically a translation from this lisp code:
	  //
	  // (do* ((rational (rationalize number))
	  //       (numerator (numerator rational) (ash numerator -1))
	  //       (denominator (denominator rational) (ash denominator -1)))
	  //      ((or (= numerator 1)
	  //           (and (< (abs numerator) #x8000)
	  //                (< denominator #x8000)))
	  //       (values
	  //         numerator (min denominator #x7fff))))
	  //
	  pushSTACK (STACK_1); 	       // 0     (LOAD&PUSH 1)                       ; argument
	  funcall (L(rationalize), 1); // 1     (CALLS2&PUSH 177)                   ; RATIONALIZE
	  pushSTACK (value1);	       // 
	  pushSTACK (STACK_0);	       // 3     (LOAD&PUSH 0)
	  funcall (L(numerator), 1);   // 4     (CALLS2&PUSH 178)                   ; NUMERATOR
	  pushSTACK (value1);	       //
	  pushSTACK (STACK_1);	       // 6     (LOAD&PUSH 1)
	  funcall (L(denominator), 1); // 7     (CALLS2&PUSH 179)                   ; DENOMINATOR
	  pushSTACK (value1);	       //
	  goto L21;		       // 9     (JMP L21)
	L11:			       // 11    L11
	  pushSTACK (STACK_1);	       // 11    (LOAD&PUSH 1)
	  pushSTACK (fixnum (-1));     // 12    (CONST&PUSH 2)                      ; -1
	  funcall (L(ash), 2);	       // 13    (CALLS2&STORE 210 1)                ; ASH
	  STACK_1 = value1;	       //
	  pushSTACK (STACK_0);	       // 16    (LOAD&PUSH 0)
	  pushSTACK (fixnum (-1));     // 17    (CONST&PUSH 2)                      ; -1
	  funcall (L(ash), 2);	       // 18    (CALLS2&STORE 210 0)                ; ASH
	  STACK_0 = value1;	       // 18    
	L21:			       // 21    L21
	  pushSTACK (STACK_1);	       // 21    (LOAD&PUSH 1)
	  pushSTACK (fixnum (1));      // 22    (CONST&PUSH 0)                      ; 1
	  funcall (L(gleich), 2);      // 23    (CALLSR&JMPIF 1 45 L41)             ; =
	  if(!nullp(value1)) goto L41; // 
	  pushSTACK (STACK_1);	       // 27    (LOAD&PUSH 1)
	  funcall (L(abs), 1);	       // 28    (CALLS2&PUSH 159)                   ; ABS
	  pushSTACK (value1);	       //
	  pushSTACK (fixnum (0x8000)); // 30    (CONST&PUSH 1)                      ; 32768
	  funcall (L(kleiner), 2);     // 31    (CALLSR&JMPIFNOT 1 47 L11)          ; <
	  if(nullp(value1)) goto L11;  //
	  pushSTACK (STACK_0);	       // 35    (LOAD&PUSH 0)
	  pushSTACK (fixnum (0x8000)); // 36    (CONST&PUSH 1)                      ; 32768
	  funcall (L(kleiner), 2);     // 37    (CALLSR&JMPIFNOT 1 47 L11)          ; <
	  if(nullp(value1)) goto L11;  // 
	L41:			       // 41    L41
	  // rest done in C ...
	  accel_denominator = get_sint16 (popSTACK ());
	  accel_numerator = get_sint16 (popSTACK ());
	  if (accel_denominator > 0x7FFF)
	    accel_denominator = 0x7FFF;
	  
	  skipSTACK (1);	// right?!
	  
	  // 41    (LOAD&PUSH 1)
	  // 42    (LOAD&PUSH 1)
	  // 43    (CONST&PUSH 3)                      ; 32767
	  // 44    (CALLSR&PUSH 1 52)                  ; MIN
	  // 47    (STACK-TO-MV 2)
          // 49    (SKIP&RET 5)
	  
	  // Bruno: Why could not a compiler create this?! :-)
	}
    }
  
  pushSTACK (STACK_2); dpy = pop_display ();

  begin_call ();
  XChangePointerControl (dpy, do_accel, do_threshold, accel_numerator, accel_denominator, threshold);
  end_call ();
  
  skipSTACK (3);
  value1 = NIL; mv_count = 1;
}

defun XLIB:POINTER-CONTROL (1)
{
  Display *display = pop_display ();
  int accel_numerator = 0;
  int accel_denominator = 1;
  int threshold = 0;

  begin_call ();
  XGetPointerControl (display, &accel_numerator, &accel_denominator, &threshold);
  end_call ();
  
  pushSTACK (make_sint32 (threshold));
  pushSTACK (make_sint32 (accel_numerator));
  pushSTACK (make_sint32 (accel_denominator));
  funcall (L(durch), 2);
  value2 = popSTACK ();
  mv_count = 2;
  skipSTACK (1);
}

//
// XLIB:POINTER-MAPPING display &key (:result-type `list)
//
defun XLIB:POINTER-MAPPING (1, 0, norest, key, 1, (:RESULT-TYPE))
{
  unsigned char map [5]; // Does the Protocol say anything about the maximum number
                         // of button?! Or the other way round: Are there any
                         // pointing devices with more than five buttons?
  unsigned int nmap, i;
  Display *dpy;
  pushSTACK (STACK_1); dpy = pop_display ();

  begin_call ();
  nmap = XGetPointerMapping (dpy, map, sizeof (map)/sizeof (map[0]));
  end_call ();
  
  for (i = 0; i < nmap; i++)
    pushSTACK (make_uint8 (map[i]));
  funcall (L(list), nmap);
  
  if (!eq (STACK_0, unbound))
    {
      pushSTACK (value1);
      pushSTACK (STACK_0);
      funcall (L(coerce), 2);
    }
  
  skipSTACK (2);	// all done
}

//
// (SETF (XLIB:POINTER-MAPPING display) mapping)
// == (XLIB:POINTER-MAPPING-SETTER mapping display)
//
defun XLIB:POINTER-MAPPING-SETTER (2)
{
  Display *dpy;
  int nmap, i;
  int result;
  
  pushSTACK (STACK_0); dpy = pop_display ();
  pushSTACK (STACK_1); funcall (L(length), 1); nmap = get_uint32 (value1);
  
  {
    DYNAMIC_ARRAY (map, unsigned char, nmap);
    
    for (i = 0; i < nmap; i++)
      {
	pushSTACK (STACK_1);	// mapping argument
	pushSTACK (fixnum (i)); // index
	funcall (L(elt), 2);
	map [i] = get_uint8 (value1);
      }

    begin_call ();
    result = XSetPointerMapping (dpy, map, nmap);
    end_call ();
    
    // From XSetPointerMapping(3X11):
    //
    // If any of  the buttons to be altered  are logically in the down
    // state, XSetPointerMapping  returns MappingBusy, and the mapping
    // is not changed.
    //
    // What should we do with that?!
    //

    FREE_DYNAMIC_ARRAY (map);
  }
  
  value1 = STACK_1; mv_count = 1;
  skipSTACK (2);	// all done
  
  // Isn't that all a overdoze for this functions?! (But since my mouse thinks
  // from time to time the left button has only been invented just to make
  // noise, I need it now and then)
}

/* 14.3  Keyboard Control */
defun XLIB:BELL (1, 1)
{
  int percent;
  Display *dpy;
  percent = (eq (STACK_0,unbound) ? 0 : get_sint16 (STACK_0));
  popSTACK ();
  dpy = pop_display ();

  begin_call ();
  XBell (dpy, percent);
  end_call ();
  
  value1 = NIL; mv_count = 1;
}

defun XLIB:CHANGE-KEYBOARD-CONTROL (1, 0, norest, key, 8,
				    (:KEY-CLICK-PERCENT :BELL-PERCENT :BELL-PITCH :BELL-DURATION :LED :LED-MODE
				     :KEY :AUTO-REPEAT-MODE)) 
{UNDEFINED}

defun XLIB:KEYBOARD-CONTROL (1)
{
  Display *dpy = pop_display ();
  XKeyboardState coffee;
  
  begin_call ();
  XGetKeyboardControl (dpy, &coffee);
  end_call ();
  
  pushSTACK (make_uint32 (coffee.led_mask));
  value7 = allocate_bit_vector (256);
  memcpy (TheSbvector(value7)->data, coffee.auto_repeats, 32); // may be wrong, may be right?!
  value1 = make_uint8 (coffee.key_click_percent);
  value2 = make_uint8 (coffee.bell_percent);
  value3 = make_uint16 (coffee.bell_pitch);
  value4 = make_uint16 (coffee.bell_duration);
  value5 = popSTACK ();
  value6 = (coffee.global_auto_repeat == AutoRepeatModeOn) ? `:ON` : `:OFF`;
  mv_count = 7;
}

defun XLIB:MODIFIER-MAPPING (1)
{
  Display *dpy = pop_display ();
  XModifierKeymap *more_coffee;
  int i;

  begin_call ();
  more_coffee = XGetModifierMapping (dpy);
  end_call ();
  
  if (more_coffee)
    {
      for (i = 1; i <= 8*more_coffee->max_keypermod; i++)
	{
	  pushSTACK (fixnum (more_coffee->modifiermap[i]));
	  if (i%more_coffee->max_keypermod == 0)
	    {
	      funcall (L(list), more_coffee->max_keypermod);
	      pushSTACK (value1);
	    }
	}
      begin_call ();
      XFreeModifiermap (more_coffee);
      end_call ();
      
      value8 = popSTACK ();
      value7 = popSTACK ();
      value6 = popSTACK ();
      value5 = popSTACK ();
      value4 = popSTACK ();
      value3 = popSTACK ();
      value2 = popSTACK ();
      value1 = popSTACK ();
      mv_count = 8;
    }
  else
    {
      value1 = NIL; mv_count = 1;
    }
}

//
// XLIB:QUERY-KEYMAP display &optional bit-vector
//
// NOTE: Also this function is different to the manual. The manual does not
//       specify the optional argument.
//
defun XLIB:QUERY-KEYMAP (1, 1)
{
  Display *dpy;
  
  pushSTACK (STACK_1); dpy = pop_display ();
  
  if (!eq (STACK_0, unbound))
    {
      unless (simple_bit_vector_p (STACK_0) && Sbvector_length (STACK_0) == 256)
	{
	  // raise type error
	  pushSTACK (STACK_0);
	  pushSTACK (`(SIMPLE-BIT-VECTOR 256)`);
	  my_standard_type_error (`XLIB::QUERY-KEYMAP`);
	}
    }
  else
    STACK_0 = allocate_bit_vector (256);

  {
    unsigned char *ptr = TheSbvector(STACK_0)->data;
    begin_call ();
    XQueryKeymap (dpy, ptr); // beam it right into the bit-vector!
    end_call ();
  }
  
  value1 = STACK_0; mv_count = 1;
  skipSTACK (2);	// all done
}

defun XLIB:SET-MODIFIER-MAPPING (1, 0, norest, key, 8, (:SHIFT :LOCK :CONTROL :MOD1 :MOD2 :MOD3 :MOD4 :MOD5))
{UNDEFINED}

/* 14.4  Keyboard Encodings */
defun XLIB:CHANGE-KEYBOARD-MAPPING (2, 0, norest, key, 3, (:START :END :FIRST-KEYCODE)) 
{UNDEFINED}

defun XLIB:KEYBOARD-MAPPING (1, 0, norest, key, 4, (:FIRST-KEYCODE :START :END :DATA))
{
  UNDEFINED
    //  XGetKeyboardMapping
}

//
// XLIB:KEYCODE->KEYSYM display keycode keysym-index
//
// NOTE: In the Manual this function is called "keycode-keysym"
//

defun XLIB:KEYCODE->KEYSYM (3)
{
  int       index = get_uint8 (popSTACK ());
  KeyCode keycode = get_uint8 (popSTACK ());
  Display    *dpy = pop_display ();
  KeySym keysym;

  begin_call ();
  keysym = XKeycodeToKeysym (dpy, keycode, index);
  end_call ();

  // There is a comment in MIT-CLX, translate.lsp, which I want to quote here:
  //
  //    The keysym-mapping is brain dammaged.
  //    Mappings for both-case alphabetic characters have the
  //    entry for keysym-index zero set to the uppercase keysym
  //    (this is normally where the lowercase keysym goes), and the
  //    entry for keysym-index one is zero.
  //
  // Then code continues:
  //   (cond ((zerop keysym-index)                  ; Lowercase alphabetic keysyms
  //          (keysym-downcase keysym))
  //
  // That [above] is already implemented in libX, but not this [below] klugde:
  //         ((and (zerop keysym) (plusp keysym-index)) ; Get the uppercase keysym
  //          (aref mapping keycode 0))
  //
  // .. so
  if (keysym == NoSymbol && index > 0)
    {
      begin_call ();
      keysym = XKeycodeToKeysym (dpy, keycode, 0);
      end_call ();
    }
  // I wanted to say "value1 = (keysym == NoSymbol) ? NIL : make_uint32 (keysym);",
  // but seeing the MIT-CLX code, I better say simply:
  //
  value1 = make_uint32 (keysym == NoSymbol ? 0 : keysym);
  mv_count = 1;
}

//
// XLIB:KEYCODE->CHARACTER display keysym &optional (state 0)
//
// NOTE: The manual calls this function "keycode-keysym"
//
// This functions is somewhat wired:
//   - It should be called KEYSYM->SONSTWAS
//   - We could also get a string instead of a single character
//   - The modifier bits  (the state argument)  mentioned  in the manual  are no
//     longer in the Common Lisp standard.
//
// BTW: How  does a LISP  program determinate in  a readable  way  the name of a
//      keysym, I think I should add  that. (For an  idea inspect e.g the libX11
//      functions XStringToKeysym and XKeysymToString).
//
// Well with normal CLX this goes just like this:
//
//  (xlib:keysym->character dpy 97)     --> #\a
//  (xlib:keysym->character dpy 97 4)   --> #\CONTROL-\a        ; 4 is <ctrl>
//  (xlib:keysym->character dpy 97 8)   --> #\META-\a		; 8 is <meta>
//  (xlib:keysym->character dpy 65)     --> #\A
//  (xlib:keysym->character dpy 65 4)   --> #\CONTROL-A
//  (xlib:keysym->character dpy 65 8)   --> #\META-\A
//  (xlib:keysym->character dpy #xFF52) --> NIL			; #xFF52 is <up>
//
// Had we unicode characters, this function would become more interesting.
// Yeah is there any correspondes between the various latin-n maps in X11 and unicode?!
// I really want unicode support.

defun XLIB:KEYSYM->CHARACTER (2, 1)
{
  Display *dpy;
  KeySym keysym;
  
  // FIXME for now we ignore the state argument:
  popSTACK ();
  
  keysym = get_uint32 (popSTACK ());
  dpy = pop_display ();
  
  NOTIMPLEMENTED;
  // Too wired -- I have to browse some more in the manuals ... Back soon.
}

//
// XLIB:KEYSYM->KEYCODES display keysym
//    Return keycodes for keysym, as multiple values
//
// Hmm. It goes like this:
// (xlib:keysym->keycode dpy 65) --> 38
// (xlib:keysym->keycode dpy #xFF52) --> 148 ; 98 ; 80		; #xFF52 keysym for <up>
//

defun XLIB:KEYSYM->KEYCODES (2) //NIM
{
  UNDEFINED
}

// And there also the undocumented function:
// defun XLIB:KEYCODE->CHARACTER (3, 0, norest, key, 2, (:KEYSYM-INDEX :KEYSYM-INDEX-FUNCTION)) {UNDEFINED}

/* 14.5  Client Termination */
defun XLIB:ADD-TO-SAVE-SET (1)
{
  // WAS: invoke (XAddToSaveSet, 1, "D1w");
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  
  begin_call ();
  XAddToSaveSet (dpy, win);
  end_call ();
  
  value1 = NIL; mv_count = 1;
  skipSTACK (1);
}

defun XLIB:CLOSE-DOWN-MODE (1)
{
  //FIXME: This is wrong -- The close down mode could not been asked from the
  //       server, but you could store it in the display structure. (Like
  //       MIT-CLX does it.)
  pushSTACK (`XLIB::CLOSE-DOWN-MODE`);
  fehler (error, ("~ can only be set"));
}

defun XLIB:CLOSE-DOWN-MODE-SETTER (2)
{
  Display *dpy = pop_display ();
  int mode = get_close_down_mode (STACK_0);

  begin_call ();
  XSetCloseDownMode (dpy, mode);
  end_call ();

  value1 = popSTACK (); mv_count = 1;
}

defun XLIB:KILL-CLIENT (2)
{
  XID resource_id = get_uint29 (popSTACK ());
  Display *dpy = pop_display ();
  
  begin_call ();
  XKillClient (dpy, resource_id);
  end_call ();
  
  value1 = NIL; mv_count = 1;
}

defun XLIB:KILL-TEMPORARY-CLIENTS (1)
{
  Display *dpy = pop_display ();
  
  begin_call ();
  XKillClient (dpy, AllTemporary);
  end_call ();
  
  value1 = NIL; mv_count = 1;
}

defun XLIB:REMOVE-FROM-SAVE-SET (1)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  
  begin_call ();
  XRemoveFromSaveSet (dpy, win);
  end_call ();
  
  value1 = NIL; mv_count = 1;
  skipSTACK (1);
}

/* 14.6  Managing Host Access */
defun XLIB:ACCESS-CONTROL (1)
{
  Display *dpy = pop_display ();
  XHostAddress *hosts;
  Bool state;
  int nhosts;

  begin_call ();
  hosts = XListHosts (dpy, &nhosts, &state);
  if (hosts) XFree (hosts);
  end_call ();
  
  value1= make_bool (state); mv_count = 1;
}

defun XLIB:ACCESS-CONTROL-SETTER (2)
     // alias
     // defun (SETF XLIB:ACCESS-CONTROL) (2)
     //
{
  Display *dpy = pop_display ();
  Bool state = get_bool (STACK_0);

  begin_call ();
  XSetAccessControl (dpy, state);
  end_call ();
  
  value1 = popSTACK (); mv_count = 1;
}

// Maybe I drop code from xhost(1) in here ...
defun XLIB:ACCESS-HOSTS (1, 1) 
{UNDEFINED}
defun XLIB:ADD-ACCESS-HOST (2, 1) 
{UNDEFINED}
defun XLIB:REMOVE-ACCESS-HOST (2, 1) 
{UNDEFINED}

/* 14.7  Screen Saver */
defun XLIB:ACTIVATE-SCREEN-SAVER (1)
{
  begin_call ();
  XActivateScreenSaver (pop_display ());
  end_call ();
  
  value1 = NIL; mv_count = 1;
}

defun XLIB:RESET-SCREEN-SAVER (1)
{
  begin_call ();
  XResetScreenSaver (pop_display ());
  end_call ();
  
  value1 = NIL; mv_count = 1;
}

//
// Lots of mixing with :on/:off, :yes/:no, why not T and NIL, the natural way?!
// [Was that written by Pascal programmers?] @*~#&%§"
//

defun XLIB:SCREEN-SAVER (1)
{
  Display *dpy = pop_display ();
  int timeout;
  int interval;
  int prefer_blanking;
  int allow_exposures;
  
  begin_call ();
  XGetScreenSaver (dpy, &timeout, &interval, &prefer_blanking, &allow_exposures);
  end_call ();
  
  value1 = make_sint16 (timeout);
  value2 = make_sint16 (interval);
  value3 = prefer_blanking ? `:YES` : `:NO`;
  value4 = allow_exposures ? `:YES` : `:NO`;
  // Hey?! Manual says :YES/:NO but actual implementation does :ON/:OFF! &$#"&!
    mv_count = 4;
}

//
//  XLIB:SET-SCREEN-SAVER display timeout period blanking exposures
//
defun XLIB:SET-SCREEN-SAVER (5)
{
  int exposures, blanking, period, timeout;
  Display *dpy;
  
  pushSTACK (STACK_4); dpy = pop_display ();
  
  timeout = eq (STACK_3, `:DEFAULT`) ? -1 : get_sint32 (STACK_3);
  period = get_uint32 (STACK_2);
  
  pushSTACK (STACK_1);
  pushSTACK (`:NO`);
  pushSTACK (`:YES`);
  pushSTACK (`:DEFAULT`);
  blanking = get_enum (3);
  
  pushSTACK (STACK_0);
  pushSTACK (`:NO`);
  pushSTACK (`:YES`);
  pushSTACK (`:DEFAULT`);
  exposures = get_enum (3);
  
  begin_call ();
  XSetScreenSaver (dpy, timeout, period, blanking, exposures);
  end_call ();
  
  value1 = NIL; mv_count = 1;
  skipSTACK (5);
}


// ----------------------------------------------------------------------------------------------------
//  Chapter 15  Extentsions
// ----------------------------------------------------------------------------------------------------

/* 15.1  Extentions */
defun XLIB:LIST-EXTENSIONS (1, 0, norest, key, 1, (:RESULT-TYPE))
{
  int n = 0;
  char **extlist;
  Display *dpy;
  
  pushSTACK (STACK_1); dpy = pop_display ();

  begin_call ();
  extlist = XListExtensions (dpy, &n);
  end_call ();

  if (extlist)
    {
      int i;
      for (i = 0; i < n; i++)
	pushSTACK (asciz_to_string (extlist[i], misc_encoding ()));
      funcall (L(list), n);
      coerce_it (STACK_0);
      
      begin_call ();
      XFreeExtensionList (extlist);
      end_call ();
    }
  else
    {
      value1 = NIL; mv_count = 1;
    }
  skipSTACK (2);
}

defun XLIB:QUERY-EXTENSION (2)
{
  int opcode, event, error;
  Display *dpy;
  Status r;
  
  pushSTACK (STACK_1); dpy = pop_display ();

  with_stringable_0_tc (STACK_0, misc_encoding (), name,
    {
      begin_call ();
      r = XQueryExtension (dpy, name, &opcode, &event, &error);
      end_call ();
    });
  
  if (r)
    {
      value1 = make_uint8 (opcode);
      value2 = make_uint8 (event);
      value3 = make_uint8 (error);
      mv_count = 3;
    }
  else
    {
      value1 = NIL; mv_count = 1;
    }
  
  skipSTACK (2);
}


// ----------------------------------------------------------------------------------------------------
//  Chapter 16  Errors
// ----------------------------------------------------------------------------------------------------

//These pages are missing :-(
//Not any more but not rather informative
//

#if 0
man XErrorEvent says:
 :
 :
       The XErrorEvent structure contains:

       typedef struct {
	     int type;
	     Display *display;	/* Display the event was read from */
	     unsigned long serial;    /* serial number of failed request */
	     unsigned char error_code;/* error code of failed request */
	     unsigned char request_code;/* Major op-code of failed request */
	     unsigned char minor_code;/* Minor op-code of failed request */
	     XID resourceid;	  /* resource id */
       } XErrorEvent;

       When you receive this event, the structure members are set as follows.

       The serial member is the number of requests, starting from one, sent over
       the network connection since it was opened.  It is the number that was the
       value of NextRequest immediately before the failing call was made.  The
       request_code member is a protocol request of the procedure that failed, as
       defined in <X11/Xproto.h>.
#endif

// Error handler for errors occured on the display.
// This error handler is installed on all open displays, we simply call up the
// Lisp error handler here found in the ERROR-HANDLER slot in the display.
//
int xlib_error_handler (Display *display, XErrorEvent *event)
{
  int f = 0;
  
  begin_callback ();

  // find the display.
  pushSTACK (find_display (display));
  if (nullp (STACK_0))
    NOTREACHED;			// hmm?
  
  // find the error handler
  pushSTACK (TheStructure (STACK_0)->recdata[slot_DISPLAY_ERROR_HANDLER]);
  
  if (nullp (STACK_0))
    STACK_0 = `xlib::default-error-handler`;
  else
    if (listp (STACK_0) || vectorp (STACK_0))  // sequencep
      {
	pushSTACK (fixnum (event->error_code));
	funcall (L(aref), 2);
	pushSTACK (value1);
      }
  
  // Build the argument list for the error handler:
  pushSTACK (STACK_1);	  // display
    pushSTACK (`#(xlib::unknown-error xlib::request-error xlib::value-error xlib::window-error xlib::pixmap-error xlib::atom-error xlib::cursor-error xlib::font-error xlib::match-error xlib::drawable-error xlib::access-error xlib::alloc-error xlib::colormap-error xlib::gcontext-error xlib::id-choice-error xlib::name-error xlib::length-error xlib::implementation-error)`);
    pushSTACK (fixnum (event->error_code));
    funcall (L(aref), 2);
  pushSTACK (value1);		// error code

  pushSTACK (`:current-sequence`); pushSTACK (make_uint16 (NextRequest (display)));
  pushSTACK (`:sequence`);         pushSTACK (make_uint16 (event->serial));
  pushSTACK (`:major`);            pushSTACK (make_uint8  (event->request_code));
  pushSTACK (`:minor`);            pushSTACK (make_uint16 (event->minor_code));

  if (event->error_code == BadColor    ||  	// colormap-error
      event->error_code == BadCursor   ||  	// cursor-error
      event->error_code == BadDrawable ||  	// drawable-error
      event->error_code == BadFont     ||  	// font-error
      event->error_code == BadGC       ||  	// gcontext-error
      event->error_code == BadIDChoice ||  	// id-choice-error
      event->error_code == BadPixmap   ||  	// pixmap-error
      event->error_code == BadWindow)      	// window-error
    {
      pushSTACK (`:resource-id`);
      pushSTACK (make_uint32 (event->resourceid));
      f = 1;
    }

  if (event->error_code == BadAtom)  		// atom-error
    {
      pushSTACK (`:atom-id`);
      pushSTACK (make_uint32 (event->resourceid));
      f = 1;
    }

  if (event->error_code == BadValue)  		// value-error
    {
      pushSTACK (`:value`);
      pushSTACK (make_uint32 (event->resourceid));
      f = 1;
    }

  // Now call the handler:
  funcall (L(funcall), f ? 13 : 11);

  skipSTACK (1);				// clean up
  
  end_callback ();

  return 0;					// anything done with this?
}

int xlib_io_error_handler (Display *display)
{
  begin_callback ();

  pushSTACK (find_display (display));
  fehler (error, "IO Error on display ~.");
}

int xlib_after_function (Display *display)
{
  begin_callback ();

  pushSTACK (find_display (display));
  ensure_living_display (&(STACK_0)); // somewhat bogus?!
  pushSTACK (TheStructure (STACK_0)->recdata[slot_DISPLAY_AFTER_FUNCTION]);
  pushSTACK (STACK_1);
  funcall (L(funcall), 2);
  skipSTACK (1);
  
  end_callback ();
  return 0;
}


// ----------------------------------------------------------------------------------------------------
//  The Shape Extension 
// ----------------------------------------------------------------------------------------------------

##if WANT_XSHAPE
#include <X11/extensions/shape.h>

//NOTE: The functions in here are my own invents ...

// First three little enums (three? I can only see two!)

DEF_ENUM (shape_kind,      2, (ee(`:BOUNDING`),ee(`:CLIP`)))
DEF_ENUM (shape_operation, 5, (ee(`:SET`),ee(`:UNION`),ee(`:INTERSECT`),ee(`:SUBTRACT`),ee(`:INVERT`)))
     
local Bool ensure_shape_extension (Display *dpy, object dpy_obj, int error_p)
     // Ensures that the SHAPE extension is initialized. If it is not available
     // and error_p is set raise an appropriate error message.
{
  int event_base, error_base;
  if (XShapeQueryExtension (dpy, &event_base, &error_base))
    {
      // Everything is ok just proceed
      return True; 
    }
  else
    {
      if (error_p)
	{
	  // raise an error
	  pushSTACK (dpy_obj); 			// the display
	  pushSTACK (TheSubr(subr_self)->name);	// function name
	  fehler (error, ("~: Shape extension is not available on display ~."));
	}
      else
	return False;
    }
}

//
//  XLIB:SHAPE-VERSION display
//
//  =>  major ;
//      minor
//
defun XLIB:SHAPE-VERSION (1)
{
  Display *dpy;
  int major_version, minor_version;
  pushSTACK (STACK_0); dpy = pop_display ();
  if (ensure_shape_extension (dpy, STACK_0, 0))	// Is it there?
    {
      if (XShapeQueryVersion (dpy, &major_version, &minor_version))
	{
	  value1 = make_uint16 (major_version);
	  value2 = make_uint16 (minor_version);
	  mv_count = 2;
	  skipSTACK (1);
	  return;				// all done
	}
    }

  // Just return NIL here
  value1 = NIL; mv_count = 1;
  skipSTACK (1);
}

//
//  XLIB:SHAPE-COMBINE destination source &key (:kind :bounding) (:x-offset 0) (:y-offset 0)
//                                             (:operation :set) (:ordering :unsorted)
//
defun XLIB:SHAPE-COMBINE (2, 0, norest, key, 5, (:KIND :X-OFFSET :Y-OFFSET :OPERATION :ORDERING))
{
  Display *dpy;
  Window  dest = get_window_and_display (STACK_6, &dpy);
  int     kind = eq (STACK_4,unbound) ? ShapeBounding : get_shape_kind (STACK_4);
  int    x_off = eq (STACK_3,unbound) ? 0 : get_sint16 (STACK_3);
  int    y_off = eq (STACK_2,unbound) ? 0 : get_sint16 (STACK_2);
  int       op = eq (STACK_1,unbound) ? ShapeSet : get_shape_operation (STACK_1);
  int ordering = eq (STACK_0,unbound) ? Unsorted : get_ordering (STACK_0);
  
  (void)ensure_shape_extension (dpy, get_display_obj (STACK_6), 1);
  
  // Now we have to select on the second arg, which operation is actually wanted:
  // pixmap -> XShapeCombineMask
  // window -> XShapeCombineShape
  // sequence of rectangles -> XShapeCombineRectangles
  //
  // FIXME: Should we emit an error message if we get keywords, which are not applicable?
    
  if (pixmap_p (STACK_5))
    {
      Pixmap src = get_pixmap (STACK_5);
      XShapeCombineMask (dpy, dest, kind, x_off, y_off, src, op);
    }
  else if (window_p (STACK_5))
    {
      // FIXME -- a :source-kind keyword is missing here.
      Pixmap src = get_window (STACK_5);
      XShapeCombineShape (dpy, dest, kind, x_off, y_off, src, kind/*src_kind*/, op);
    }
  else if (listp (STACK_5) || vectorp (STACK_5))
    {
      int i, nrectangles;
      
      // Find number of rectangles
      pushSTACK (STACK_5); funcall (L(length), 1); nrectangles = get_uint32 (value1);
      
      {
	DYNAMIC_ARRAY (rectangles, XRectangle, nrectangles);

	for (i = 0; i < nrectangles; i++)
	  {
	    pushSTACK (STACK_5);	// rectangles
	    pushSTACK (fixnum (i));	// index
	    funcall (L(elt), 2);
	    pushSTACK (value1);	// save element
	    
	    pushSTACK (STACK_0); pushSTACK (fixnum (0)); funcall (L(elt), 2);
	    rectangles[i].x = get_sint16(value1);
	    
	    pushSTACK (STACK_0); pushSTACK (fixnum (1)); funcall (L(elt), 2);
	    rectangles[i].y = get_sint16(value1);
	    
	    pushSTACK (STACK_0); pushSTACK (fixnum (2)); funcall (L(elt), 2);
	    rectangles[i].width = get_sint16(value1);
	    
	    pushSTACK (fixnum (3)); funcall (L(elt), 2);
	    rectangles[i].height = get_sint16(value1);
	  }
	
	XShapeCombineRectangles (dpy, dest, kind, x_off, y_off, rectangles, nrectangles,
				 op, ordering);

	FREE_DYNAMIC_ARRAY (rectangles);
      }
    }
  
  value1 = NIL; mv_count = 1;
  skipSTACK (7);				// all done
}

//
//  XLIB:SHAPE-OFFSET destination kind x-offset y-offset
//
defun XLIB:SHAPE-OFFSET (4)
{
  Display *dpy;
  Window  dest = get_window_and_display (STACK_3, &dpy);
  int     kind = get_shape_kind (STACK_2);
  int x_offset = get_sint16 (STACK_1);
  int y_offset = get_sint16 (STACK_0);

  (void)ensure_shape_extension (dpy, get_display_obj (STACK_3), 1);

  XShapeOffsetShape (dpy, dest, kind, x_offset, y_offset);
  
  value1 = NIL; mv_count = 1;
  skipSTACK (4);
}

//
//  XLIB:SHAPE-EXTENTS window
//   -> bounding-shaped-p
//      clip-shaped-p
//      x-bounding, y-bounding, x-clip, y-clip
//      w-bounding, h-bounding, w-clip, h-clip
   
defun XLIB:SHAPE-EXTENTS (1) 
{UNDEFINED}

//
//  XLIB:SHAPE-RECTANGLES window kind
//
//    -> rectangles - (rep-seq (sint16 sint16 sint16 sint16))
//       ordering   - (member :unsorted :y-sorted :yx-sorted :yx-banded)
defun XLIB:SHAPE-RECTANGLES (2)
{UNDEFINED}
##endif


// ----------------------------------------------------------------------------------------------------
//  Not explicitly specified functions
// ----------------------------------------------------------------------------------------------------

/* I think I will  not actually support these  functions, until there are needed
 * by some application.
 *
 * Since  they   are not in  the   CLX Manual,  they   are actually undocumented
 * functions   of CLX,  which  should  either  way round  not   be  used by  CLX
 * programs. (But  it is  strange, that the  corresponding symbols  are exported
 * from the CLX package!)
 *
 * I may be wrong due to the WM functions, since these seems to be actually used
 * by a couple of applications.
 */
     
//
//  XLIB:ICONIFY-WINDOW window screen
//
defun XLIB:ICONIFY-WINDOW (2)
{
  Screen  *scr = get_screen (popSTACK ());
  Display *dpy;
  Window   win = get_window_and_display (popSTACK (), &dpy);
  XIconifyWindow (dpy, win, XScreenNumberOfScreen (scr));
  value1 = NIL; mv_count = 1;
}

//
//  XLIB:WITHDRAW-WINDOW window screen
//
defun XLIB:WITHDRAW-WINDOW (2)
{
  Screen  *scr = get_screen (popSTACK ());
  Display *dpy;
  Window   win = get_window_and_display (popSTACK (), &dpy);
  XWithdrawWindow (dpy, win, XScreenNumberOfScreen (scr));
  value1 = NIL; mv_count = 1;
}

defun XLIB:DEFAULT-KEYSYM-INDEX (3)
     // XLIB:DEFAULT-KEYSYM-INDEX display keycode state
     //  Returns a keysym-index for use with keycode->character
{
  // ????
  UNDEFINED;
}

##if 0
// ???
defun XLIB:DESCRIBE-ERROR (2) {UNDEFINED}
defun XLIB:DESCRIBE-EVENT (3, 1) {UNDEFINED}
defun XLIB:DESCRIBE-REPLY (2) {UNDEFINED}
defun XLIB:DESCRIBE-REQUEST (2) {UNDEFINED}
defun XLIB:DESCRIBE-TRACE (1, 1) {UNDEFINED}
defun XLIB:EVENT-HANDLER (2) {UNDEFINED}
defun XLIB:GET-EXTERNAL-EVENT-CODE (2) {UNDEFINED}
defun XLIB:MAKE-EVENT-HANDLERS (0, 0, norest, key, 2, (:TYPE :DEFAULT)) {UNDEFINED}
defun XLIB:DECODE-CORE-ERROR (2, 1) {UNDEFINED}

// Digging with resources
defun XLIB:ROOT-RESOURCES (1, 0, norest, key, 4, (:DATABASE :KEY :TEST :TEST-NOT)) {UNDEFINED}
defun XLIB:RESOURCE-DATABASE-TIMESTAMP (1) {UNDEFINED}
defun XLIB:RESOURCE-KEY (1) {UNDEFINED}

// This is actually specified and will be implemented in Lisp
defun XLIB:DEFAULT-ERROR-HANDLER (2, 0, rest, key_allow, 1, (:ASYNCHRONOUS))

// These seem to handle keysym translations
defun XLIB:KEYSYM-IN-MAP-P (3) {UNDEFINED}
defun XLIB:KEYSYM-SET (1) {UNDEFINED}
defun XLIB:CHARACTER->KEYSYMS (1, 1) {UNDEFINED}
defun XLIB:CHARACTER-IN-MAP-P (3) {UNDEFINED}
defun XLIB:DEFAULT-KEYSYM-TRANSLATE (3) {UNDEFINED}
defun XLIB:DEFINE-KEYSYM (2, 0, norest, key, 5, (:LOWERCASE :TRANSLATE :MODIFIERS :MASK :DISPLAY)) {UNDEFINED}
defun XLIB:DEFINE-KEYSYM-SET (3) {UNDEFINED}
defun XLIB:MAPPING-NOTIFY (4) {UNDEFINED}
defun XLIB:UNDEFINE-KEYSYM (2, 0, norest, key_allow, 2, (:DISPLAY :MODIFIERS))

// These seem to be some tracing feature
defun XLIB:UNTRACE-DISPLAY (1) {UNDEFINED}
defun XLIB:SUSPEND-DISPLAY-TRACING (1) {UNDEFINED}
defun XLIB:RESUME-DISPLAY-TRACING (1) {UNDEFINED}
defun XLIB:SHOW-TRACE (1, 0, norest, key, 2, (:LENGTH :SHOW-PROCESS)) {UNDEFINED}
defun XLIB:TRACE-DISPLAY (1) {UNDEFINED}

//  Somewhat bogus ...
defun XLIB:SET-WM-RESOURCES (2, 0, norest, key, 3, (:WRITE :TEST :TEST-NOT)) {UNDEFINED}
defun XLIB:NO-OPERATION (1) {UNDEFINED}

// [ MOVED TO LISP
##if 0
// All these are defined in manager.lsp and are simply droped in ...
defun XLIB:ICON-SIZES (1) {UNDEFINED}
defun XLIB:SET-WM-CLASS (3) {UNDEFINED}
defun XLIB:SET-WM-PROPERTIES (0, 0, rest, nokey, 0, NIL) {UNDEFINED}//LISPFUN  (xlib_set_wm_properties, 1, 0, rest, key, 36, (:NAME :ICON-NAME :RESOURCE-NAME :RESOURCE-CLASS :COMMAND :CLIENT-MACHINE :HINTS :NORMAL-HINTS :ZOOM-HINTS :USER-SPECIFIED-POSITION-P :USER-SPECIFIED-SIZE-P :PROGRAM-SPECIFIED-POSITION-P :PROGRAM-SPECIFIED-SIZE-P :X :Y :WIDTH :HEIGHT :MIN-WIDTH :MIN-HEIGHT :MAX-WIDTH :MAX-HEIGHT :WIDTH-INC :HEIGHT-INC :MIN-ASPECT :MAX-ASPECT :BASE-WIDTH :BASE-HEIGHT :WIN-GRAVITY :INPUT :INITIAL-STATE :ICON-PIXMAP :ICON-WINDOW :ICON-X :ICON-Y :ICON-MASK :WINDOW-GROUP))
defun XLIB:MAKE-WM-HINTS (0, 0, norest, key, 9, (:INPUT :INITIAL-STATE :ICON-PIXMAP :ICON-WINDOW :ICON-X :ICON-Y :ICON-MASK :WINDOW-GROUP :FLAGS)) {UNDEFINED}
defun XLIB:MAKE-WM-SIZE-HINTS (0, 0, norest, key, 19, (:USER-SPECIFIED-POSITION-P :USER-SPECIFIED-SIZE-P :X :Y :WIDTH :HEIGHT :MIN-WIDTH :MIN-HEIGHT :MAX-WIDTH :MAX-HEIGHT :WIDTH-INC :HEIGHT-INC :MIN-ASPECT :MAX-ASPECT :BASE-WIDTH :BASE-HEIGHT :WIN-GRAVITY :PROGRAM-SPECIFIED-POSITION-P :PROGRAM-SPECIFIED-SIZE-P)) {UNDEFINED}
defun XLIB:GET-WM-CLASS (1) {UNDEFINED}
defun XLIB:TRANSIENT-FOR (1) {UNDEFINED}
defun XLIB:WM-CLIENT-MACHINE (1) {UNDEFINED}
defun XLIB:WM-COLORMAP-WINDOWS (1) {UNDEFINED}
defun XLIB:WM-COMMAND (1) {UNDEFINED}
defun XLIB:WM-HINTS (1) {UNDEFINED}
defun XLIB:WM-HINTS-FLAGS (1) {UNDEFINED}
defun XLIB:WM-HINTS-ICON-MASK (1) {UNDEFINED}
defun XLIB:WM-HINTS-ICON-PIXMAP (1) {UNDEFINED}
defun XLIB:WM-HINTS-ICON-WINDOW (1) {UNDEFINED}
defun XLIB:WM-HINTS-ICON-X (1) {UNDEFINED}
defun XLIB:WM-HINTS-ICON-Y (1) {UNDEFINED}
defun XLIB:WM-HINTS-INITIAL-STATE (1) {UNDEFINED}
defun XLIB:WM-HINTS-INPUT (1) {UNDEFINED}
defun XLIB:WM-HINTS-P (1) {UNDEFINED}
defun XLIB:WM-HINTS-WINDOW-GROUP (1) {UNDEFINED}
defun XLIB:WM-ICON-NAME (1) {UNDEFINED}
defun XLIB:WM-NORMAL-HINTS (1) {UNDEFINED}
defun XLIB:WM-PROTOCOLS (1) {UNDEFINED}
defun XLIB:WM-RESOURCES (2, 0, norest, key, 3, (:KEY :TEST :TEST-NOT)) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-BASE-HEIGHT (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-BASE-WIDTH (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-HEIGHT (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-HEIGHT-INC (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-MAX-ASPECT (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-MAX-HEIGHT (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-MAX-WIDTH (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-MIN-ASPECT (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-MIN-HEIGHT (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-MIN-WIDTH (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-P (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-USER-SPECIFIED-POSITION-P (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-USER-SPECIFIED-SIZE-P (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-WIDTH (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-WIDTH-INC (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-WIN-GRAVITY (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-X (1) {UNDEFINED}
defun XLIB:WM-SIZE-HINTS-Y (1) {UNDEFINED}
defun XLIB:RGB-COLORMAPS (2) {UNDEFINED}
defun XLIB:WM-NAME (1) {UNDEFINED}
// ... and properly some more ...

// These are simply defstruct generated functions -- moved to Lisp
defun XLIB:VISUAL-INFO-BITS-PER-RGB (1) 
defun XLIB:VISUAL-INFO-BLUE-MASK (1) 
defun XLIB:VISUAL-INFO-CLASS (1) 
defun XLIB:VISUAL-INFO-COLORMAP-ENTRIES (1) 
defun XLIB:VISUAL-INFO-DISPLAY (1) 
defun XLIB:VISUAL-INFO-GREEN-MASK (1) 
defun XLIB:VISUAL-INFO-ID (1) 
defun XLIB:VISUAL-INFO-P (1) 
defun XLIB:VISUAL-INFO-PLIST (1) 
defun XLIB:VISUAL-INFO-RED-MASK (1)

// These here are defined in Lisp:
defun XLIB:CUT-BUFFER (1, 0, norest, key, 6, (:BUFFER :TYPE :RESULT-TYPE :TRANSFORM :START :END)) {UNDEFINED}
defun XLIB:ROTATE-CUT-BUFFERS (1, 2) {UNDEFINED}
defun xlib:bitmap-image (0, 1, rest)
##endif
// ]

// [ CONSIDERED OBSOLETE
##if 0
defun XLIB:WM-ZOOM-HINTS (1) {UNDEFINED}
defun XLIB:SET-STANDARD-PROPERTIES (1, 0, rest, nokey, 0, NIL) {UNDEFINED}
defun XLIB:GET-STANDARD-COLORMAP (2) {UNDEFINED}
defun XLIB:SET-STANDARD-COLORMAP (6) {UNDEFINED}
##endif
// ]
##endif
// Puh! That is really lots of typeing ...
//    ... But what don`t I do to get (hopyfully) GARNET working?

// But we not yet finished, we yet to finish the libX11 :-)


// ----------------------------------------------------------------------------------------------------
//  Fixups of libX
// ----------------------------------------------------------------------------------------------------

local Visual *XVisualIDToVisual (Display *dpy, VisualID vid)
{
  /*PORTABLE-P?*/
  XVisualInfo template, *r;
  Visual *result;
  int n;
  
  template.visualid = vid;
  begin_call ();
    r = XGetVisualInfo (dpy, VisualIDMask, &template, &n);
  end_call ();
  if (n == 1)
    {
      result = r->visual;
      begin_call ();
        XFree (r);
      end_call ();
      return result;
    }
  else
    {
      begin_call ();
        if (r) XFree (r);
      end_call ();
      
      // Maybe we emerge a x-bad-SONSTWAS condition here, since the 0 value _is_ meaningful to the libX11;
      // It is CopyFromParent.
      return 0;
    }
}

local int XScreenNo (Display *dpy, Screen *screen)
     // Find the screen number of an screen
{
  int i;
  for (i = 0; ScreenCount (dpy); i++)
    if (ScreenOfDisplay (dpy,i) == screen)
      return i;

  // not found, what should we return?!
  return 0; // Hier kann nichs schief gehen.
            // Und wenn Gilbert sagt, nichs, dann meint er, nix!
}

// So, now we could expose this to the compiler.

//
// Now the somewhat standard tail of my files, which wander out of my small loved five-years old box.
//        [Cheers! Long live the ISA bus :-]
//
//    Sorry, if you do not have a wide display or a small font and eyes as good as mine.
//
//    Most lines are written between two o`clock and five o`clock in the morning.
//

#define SILLY 1
#if SILLY
int this_is_a_test_for_the_linker_and_the_debugger_and_the_nm_utility__lets_have_a_look_if_they_could_cope_with_this_indeed_very_long_identifer__still_reading_this__if_not_in_the_editor___CONGRATULATIONS;
#endif


// ----------------------------------------------------------------------------------------------------
//  Xpm Interface
// ----------------------------------------------------------------------------------------------------
// Need this for my small sokoban port ...

##if WANT_XPM
#include <X11/xpm.h>

//
// XPM:READ-FILE-TO-PIXMAP drawable filename &key :shape-mask-p (:pixmap-p nil)
//
// -> pixmap
//    shape
//    error code
//
defun XPM:READ-FILE-TO-PIXMAP (2, 0, norest, key, 2, (:SHAPE-MASK-P :PIXMAP-P))
{
  Display     *dpy;
  Drawable      da = get_drawable_and_display (STACK_3, &dpy);
  int shape_mask_p = eq (STACK_1, unbound) ? 0 : get_bool (STACK_1);
  int     pixmap_p = eq (STACK_0, unbound) ? 1 : get_bool (STACK_0);
  int r;
  Pixmap pixmap = 0;
  Pixmap shape_mask = 0; 

  pushSTACK (get_display_obj (STACK_3));

  pushSTACK (STACK_3);
  funcall (L(namestring), 1);
  STACK_3 = value1;
  
  with_string_0 (STACK_3, pathname_encoding (), filename,
    {
      begin_call();
      r = XpmReadFileToPixmap (dpy, da, filename,
			       pixmap_p?&pixmap:NULL,
			       shape_mask_p?&shape_mask:NULL, NULL);
      end_call ();
    });
  
  if (pixmap)     pushSTACK (make_pixmap (STACK_0, pixmap));     else pushSTACK (NIL);
  if (shape_mask) pushSTACK (make_pixmap (STACK_1, shape_mask)); else pushSTACK (NIL);

  switch (r)
    {
    case XpmColorError:
      pushSTACK (`:COLOR-ERROR`); break;

    case XpmSuccess:
      pushSTACK (`:SUCCESS`); break;

    case XpmOpenFailed:
      pushSTACK (`:OPEN-FAILED`); break;

    case XpmFileInvalid:
      pushSTACK (`:FILE-INVALID`); break;
      
    case XpmNoMemory:
      pushSTACK (`:NO-MEMORY`); break;
      
    case XpmColorFailed:
      pushSTACK (`:COLOR-FAILED`); break;

    default:
      pushSTACK (NIL);
    }

  value3 = popSTACK ();
  value2 = popSTACK ();
  value1 = popSTACK ();
  mv_count = 3;
  skipSTACK (5);
}
##endif

void module__clx__init_function_2 (module_ *module)
{
  // setze doch `XLIB::*DISPLAYS*` auf NIL !
#if 0
  uintC i;

  for (i = 0 ; i < module__clx__object_tab_size; i++)
    {
      dprintf (("\n;; otab[%d] = '%s' -->",i,module__clx__object_tab_initdata[i]));
      pushSTACK (((object * )( & module__clx__object_tab))[i]);
      funcall (L(princ),1);
    }
#endif  
}

// Local variables:
// truncate-lines: t
// fill-column: 80
// end:


