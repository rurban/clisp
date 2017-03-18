/* Thread-local objects
 * Sam Steingold 1998-2008
 * The macro LISPOBJ declares a LISP object.
 LISPOBJ_TL(name)
 > name: object is addressable as TLO(name) */

/* internal variables of the reader: */
LISPOBJ_TL(token_buff_1,".")
LISPOBJ_TL(token_buff_2,".")
LISPOBJ_TL(displaced_string,".")

/* for STREAM.D */
#if !(defined(CAN_ALLOCATE_8BIT_VECTORS_ON_C_STACK) || defined(CAN_ALLOCATE_STRINGS_ON_C_STACK))
LISPOBJ_TL(dynamic_8bit_vector,"NIL") /* cache for macro DYNAMIC_8BIT_VECTOR */
LISPOBJ_TL(dynamic_string,"NIL") /* cache for macro DYNAMIC_STRING */
#endif
