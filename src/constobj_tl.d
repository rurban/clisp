/* Thread-local objects
 * Sam Steingold 1998-2008
 * The macro LISPOBJ declares a LISP object.
 LISPOBJ_TL(name)
 > name: object is addressable as TLO(name) */

/* internal variables of the reader: */
LISPOBJ_TL(token_buff_1)
LISPOBJ_TL(token_buff_2)
LISPOBJ_TL(displaced_string)
