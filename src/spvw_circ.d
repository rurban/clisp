# Detection of circularities. Used by the printer and reader.

# ------------------------------ Specification ---------------------------------

# get_circularities(obj,pr_array,pr_closure)
# Returns a table of all circularities of an object.
# A circularity is a sub-object, which can be reached from the given object
# through more than one path.
# > object obj: object
# > boolean pr_array: if true, elements of arrays are considered sub-objects
#                     during the recursive traversal.
# > boolean pr_closure: if true, elements of closures are considered sub-objects
#                       during the recursive traversal.
# < result: T on stack overflow,
#           NIL if there are no circularities,
#           #(0 ...) a vector of length (n+1), containing the integer 0 and
#                    the n circularities, n>0.
# can trigger GC
  global object get_circularities (object obj, boolean pr_array, boolean pr_closure);

# subst_circ(ptr,alist)
# Resolves #n# references in the object *ptr, using the alist as a replacement
# table.
# > *ptr : object
# > alist : alist (read-label --> object to be substituted)
# < *ptr : object with resolved references
# < result : first invalid reference found, or nullobj if everything is OK
  global object subst_circ (object* ptr, object alist);
# Note: This substitution must respect circularities, so that it can be
# applied to circular structures, such as values of #. (in particular
# #.(FIND-CLASS 'FOO)).

# ------------------------------ Implementation --------------------------------

# Common subroutines.

#ifdef MULTITHREAD

# Multi-level bit map, used as a hash set. This is a (slower, but reentrant)
# alternative to using the GC's mark bit.

# Subdividing an address into bit packets.
# Try to reduce the number of bit packets, thus reducing the number of
# indirections. But the first one can bit large (because at the end of the
# indirection chain, we have a single bit per sizeof(object), not a big
# pointer. The last ones (>= 22) can be big, because it's not a problem
# if the size of the bitmap grows linearly with process_size/4MB.
#if (oint_addr_len <= 32)
  #define mlb0 0
  #define mlb1 10
  #define mlb2 14
  #define mlb3 18
  #define mlb4 22
  #define mlb5 32
  #define mlb_levels 5
#else
  #define mlb0 0
  #define mlb1 10
  #define mlb2 14
  #define mlb3 18
  #define mlb4 22
  #define mlb5 33
  #define mlb6 64
  #define mlb_levels 6
#endif
#if (mlb_levels >= 5)
  #define mlbs0  (mlb1-mlb0)  # = 10
  #define mlbs1  (mlb2-mlb1)  # = 4
  #define mlbs2  (mlb3-mlb2)  # = 4
  #define mlbs3  (mlb4-mlb3)  # = 4
  #define mlbs4  (mlb5-mlb4)  # >= 10
  #if (mlb_levels >= 6)
    #define mlbs5  (mlb6-mlb5)  # >= 10
  #endif
#endif

# A multi-level bit map.
# It is a hash set providing one bit for every possible object. The index into
# the table is actually an aint which we begin by dividing by sizeof(object).
# (Since any object on heap has a size >= sizeof(object), distinct objects
# have addresses that differ by at least sizeof(object), this will be
# represented by different bits.)
  #if (mlb_levels == 5)
    typedef uintL***** mlbitmap_base_t;
  #endif
  #if (mlb_levels == 6)
    typedef uintL****** mlbitmap_base_t;
  #endif
  typedef struct {
          mlbitmap_base_t base; # start pointer = address of malloc()ed area
          uintL alloc_size;     # size of malloc()ed area
          uintL used_size;      # size of used part; the remainder of the area is zeroed
          jmp_buf oom_context;  # context to jump to in case of malloc/realloc failure
  } mlbitmap;

# Create a multi-level bit map.
# The caller must initialize bitmap->oom_context himself.
  local void mlb_alloc (mlbitmap* bitmap);

# Add an object to a bitmap.
# Returns TRUE if the object was already present, else FALSE.
  local boolean mlb_add (mlbitmap* bitmap, object obj);

# Free a multi-level bit map.
  local void mlb_free (mlbitmap* bitmap);

  local void mlb_alloc(bitmap)
    var mlbitmap* bitmap;
    { bitmap->base = NULL;
      bitmap->alloc_size = 0;
      bitmap->used_size = 0;
    }

  # Expand a bitmap so that its alloc_size becomes >= newsize.
  local uintP mlb_expand (mlbitmap* bitmap, uintL newsize);
  local uintP mlb_expand(bitmap,newsize)
    var mlbitmap* bitmap;
    var uintL newsize;
    {  if (newsize < 2*bitmap->alloc_size) { newsize = 2*bitmap->alloc_size; }
       begin_system_call();
     { var char* newbase = (bitmap->base==NULL ? malloc(newsize) : realloc((char*)bitmap->base,newsize));
       end_system_call();
       if (newbase==NULL) { longjmp(bitmap->oom_context,TRUE); }
      {var uintP delta = (uintP)newbase - (uintP)bitmap->base;
       bzero(newbase+bitmap->alloc_size,newsize-bitmap->alloc_size);
       if (bitmap->base)
         # Relocate the pointers inside the bitmap. We know that they form a tree,
         # therefore a recursive descent reaches every pointer exactly once.
         if (!(delta == 0))
           {
            #if (mlb_levels >= 6)
             var uintL****** p5 = (mlbitmap_base_t)newbase;
            {var uintC count5 = bit(mlbs5);
             for (; count5 > 0; p5++, count5--)
               { var uintL***** p4 = *p5;
                 if (p4)
                   { *p5 = p4 = (uintL*****)((uintP)p4 + delta);
            #else
                     var uintL***** p4 = (mlbitmap_base_t)newbase;
            #endif
                    {var uintC count4 = bit(mlbs4);
                     for (; count4 > 0; p4++, count4--)
                       { var uintL**** p3 = *p4;
                         if (p3)
                           { *p4 = p3 = (uintL****)((uintP)p3 + delta);
                            {var uintC count3 = bit(mlbs3);
                             for (; count3 > 0; p3++, count3--)
                               { var uintL*** p2 = *p3;
                                 if (p2)
                                   { *p3 = p2 = (uintL***)((uintP)p2 + delta);
                                    {var uintC count2 = bit(mlbs2);
                                     for (; count2 > 0; p2++, count2--)
                                       { var uintL** p1 = *p2;
                                         if (p1)
                                           { *p2 = p1 = (uintL**)((uintP)p1 + delta);
                                            {var uintC count1 = bit(mlbs1);
                                             for (; count1 > 0; p1++, count1--)
                                               { var uintL* p0 = *p1;
                                                 if (p0)
                                                   { *p1 = p0 = (uintL*)((uintP)p0 + delta); }
                    }  }   }}  }   }}  }   }}  }
            #if (mlb_levels >= 6)
            }  }   }
            #endif
           }
       bitmap->base = (mlbitmap_base_t)newbase;
       bitmap->alloc_size = newsize;
       return delta;
    }}}

  local boolean mlb_add(bitmap,obj)
    var mlbitmap* bitmap;
    var object obj;
    { var aint addr = (aint)ThePointer(obj);
      #if (mlb_levels >= 6)
      var uintL******* p6 = &bitmap->base;
      if (*p6)
        { var uintL****** p5 = &(*p6)[(addr >> mlb5) & (bit(mlbs5)-1)];
      #else
          var uintL****** p5 = &bitmap->base;
      #endif
          if (*p5)
            { var uintL***** p4 = &(*p5)[(addr >> mlb4) & (bit(mlbs4)-1)];
              if (*p4)
                { var uintL**** p3 = &(*p4)[(addr >> mlb3) & (bit(mlbs3)-1)];
                  if (*p3)
                    { var uintL*** p2 = &(*p3)[(addr >> mlb2) & (bit(mlbs2)-1)];
                      if (*p2)
                        { var uintL** p1 = &(*p2)[(addr >> mlb1) & (bit(mlbs1)-1)];
                          if (*p1)
                            { var uintL* p0 = &(*p1)[((addr >> mlb0) / (32*sizeof(object)))
                                                     & ((bit(mlbs0)-1) / (32*sizeof(object)))];
                              var uintL i0 = ((addr >> mlb0) / sizeof(object)) % 32;
                              if (*p0 & bit(i0)) return TRUE;
                              *p0 |= bit(i0); return FALSE;
                            }
                         {var const uintL need = bit(mlbs0)/(32*sizeof(object))*sizeof(uintL);
                          if (bitmap->used_size + need > bitmap->alloc_size)
                            { var uintP delta = mlb_expand(bitmap,bitmap->used_size+need);
                              p1 = (uintL**)((char*)p1 + delta);
                            }
                          {var char* room = (char*)bitmap->base+bitmap->used_size;
                           *p1 = (uintL*)room;
                           {var uintL* p0 = &(*p1)[((addr >> mlb0) / (32*sizeof(object)))
                                                   & ((bit(mlbs0)-1) / (32*sizeof(object)))];
                            var uintL i0 = ((addr >> mlb0) / sizeof(object)) % 32;
                            *p0 = bit(i0);
                           }
                           bitmap->used_size += need;
                           return FALSE;
                        }}}
                     {var const uintL need = bit(mlbs1)*sizeof(uintL*)
                                             + bit(mlbs0)/(32*sizeof(object))*sizeof(uintL);
                      if (bitmap->used_size + need > bitmap->alloc_size)
                        { var uintP delta = mlb_expand(bitmap,bitmap->used_size+need);
                          p2 = (uintL***)((char*)p2 + delta);
                        }
                      {var char* room = (char*)bitmap->base+bitmap->used_size;
                       *p2 = (uintL**)room; room += bit(mlbs1)*sizeof(uintL*);
                       {var uintL** p1 = &(*p2)[(addr >> mlb1) & (bit(mlbs1)-1)];
                        *p1 = (uintL*)room;
                        {var uintL* p0 = &(*p1)[((addr >> mlb0) / (32*sizeof(object)))
                                                & ((bit(mlbs0)-1) / (32*sizeof(object)))];
                         var uintL i0 = ((addr >> mlb0) / sizeof(object)) % 32;
                         *p0 = bit(i0);
                       }}
                       bitmap->used_size += need;
                       return FALSE;
                    }}}
                 {var const uintL need = bit(mlbs2)*sizeof(uintL**)
                                         + bit(mlbs1)*sizeof(uintL*)
                                         + bit(mlbs0)/(32*sizeof(object))*sizeof(uintL);
                  if (bitmap->used_size + need > bitmap->alloc_size)
                    { var uintP delta = mlb_expand(bitmap,bitmap->used_size+need);
                      p3 = (uintL****)((char*)p3 + delta);
                    }
                  {var char* room = (char*)bitmap->base+bitmap->used_size;
                   *p3 = (uintL***)room; room += bit(mlbs2)*sizeof(uintL**);
                   {var uintL*** p2 = &(*p3)[(addr >> mlb2) & (bit(mlbs2)-1)];
                    *p2 = (uintL**)room; room += bit(mlbs1)*sizeof(uintL*);
                    {var uintL** p1 = &(*p2)[(addr >> mlb1) & (bit(mlbs1)-1)];
                     *p1 = (uintL*)room;
                     {var uintL* p0 = &(*p1)[((addr >> mlb0) / (32*sizeof(object)))
                                             & ((bit(mlbs0)-1) / (32*sizeof(object)))];
                      var uintL i0 = ((addr >> mlb0) / sizeof(object)) % 32;
                      *p0 = bit(i0);
                   }}}
                   bitmap->used_size += need;
                   return FALSE;
                }}}
             {var const uintL need = bit(mlbs3)*sizeof(uintL***)
                                     + bit(mlbs2)*sizeof(uintL**)
                                     + bit(mlbs1)*sizeof(uintL*)
                                     + bit(mlbs0)/(32*sizeof(object))*sizeof(uintL);
              if (bitmap->used_size + need > bitmap->alloc_size)
                { var uintP delta = mlb_expand(bitmap,bitmap->used_size+need);
                  p4 = (uintL*****)((char*)p4 + delta);
                }
              {var char* room = (char*)bitmap->base+bitmap->used_size;
               *p4 = (uintL****)room; room += bit(mlbs3)*sizeof(uintL***);
               {var uintL**** p3 = &(*p4)[(addr >> mlb3) & (bit(mlbs3)-1)];
                *p3 = (uintL***)room; room += bit(mlbs2)*sizeof(uintL**);
                {var uintL*** p2 = &(*p3)[(addr >> mlb2) & (bit(mlbs2)-1)];
                 *p2 = (uintL**)room; room += bit(mlbs1)*sizeof(uintL*);
                 {var uintL** p1 = &(*p2)[(addr >> mlb1) & (bit(mlbs1)-1)];
                  *p1 = (uintL*)room;
                  {var uintL* p0 = &(*p1)[((addr >> mlb0) / (32*sizeof(object)))
                                          & ((bit(mlbs0)-1) / (32*sizeof(object)))];
                   var uintL i0 = ((addr >> mlb0) / sizeof(object)) % 32;
                   *p0 = bit(i0);
               }}}}
               bitmap->used_size += need;
               return FALSE;
            }}}
         {var const uintL need = bit(mlbs4)*sizeof(uintL****)
                                 + bit(mlbs3)*sizeof(uintL***)
                                 + bit(mlbs2)*sizeof(uintL**)
                                 + bit(mlbs1)*sizeof(uintL*)
                                 + bit(mlbs0)/(32*sizeof(object))*sizeof(uintL);
          if (bitmap->used_size + need > bitmap->alloc_size)
            { var uintP delta = mlb_expand(bitmap,bitmap->used_size+need);
              #if (mlb_levels > 5)
              p5 = (uintL******)((char*)p5 + delta);
              #endif
            }
          {var char* room = (char*)bitmap->base+bitmap->used_size;
           *p5 = (uintL*****)room; room += bit(mlbs4)*sizeof(uintL****);
           {var uintL***** p4 = &(*p5)[(addr >> mlb4) & (bit(mlbs4)-1)];
            *p4 = (uintL****)room; room += bit(mlbs3)*sizeof(uintL***);
            {var uintL**** p3 = &(*p4)[(addr >> mlb3) & (bit(mlbs3)-1)];
             *p3 = (uintL***)room; room += bit(mlbs2)*sizeof(uintL**);
             {var uintL*** p2 = &(*p3)[(addr >> mlb2) & (bit(mlbs2)-1)];
              *p2 = (uintL**)room; room += bit(mlbs1)*sizeof(uintL*);
              {var uintL** p1 = &(*p2)[(addr >> mlb1) & (bit(mlbs1)-1)];
               *p1 = (uintL*)room;
               {var uintL* p0 = &(*p1)[((addr >> mlb0) / (32*sizeof(object)))
                                       & ((bit(mlbs0)-1) / (32*sizeof(object)))];
                var uintL i0 = ((addr >> mlb0) / sizeof(object)) % 32;
                *p0 = bit(i0);
           }}}}}
           bitmap->used_size += need;
           return FALSE;
         }}
     #if (mlb_levels >= 6)
        }
     {var const uintL need = bit(mlbs5)*sizeof(uintL*****)
                             + bit(mlbs4)*sizeof(uintL****)
                             + bit(mlbs3)*sizeof(uintL***)
                             + bit(mlbs2)*sizeof(uintL**)
                             + bit(mlbs1)*sizeof(uintL*)
                             + bit(mlbs0)/(32*sizeof(object))*sizeof(uintL);
      if (bitmap->used_size + need > bitmap->alloc_size)
        { var uintP delta = mlb_expand(bitmap,bitmap->used_size+need);
          #if (mlb_levels > 6)
          p6 = (uintL*******)((char*)p6 + delta);
          #endif
        }
      {var char* room = (char*)bitmap->base+bitmap->used_size;
       *p6 = (uintL******)room; room += bit(mlbs5)*sizeof(uintL*****);
       {var uintL****** p5 = &(*p6)[(addr >> mlb5) & (bit(mlbs5)-1)];
        *p5 = (uintL*****)room; room += bit(mlbs4)*sizeof(uintL****);
        {var uintL***** p4 = &(*p5)[(addr >> mlb4) & (bit(mlbs4)-1)];
         *p4 = (uintL****)room; room += bit(mlbs3)*sizeof(uintL***);
         {var uintL**** p3 = &(*p4)[(addr >> mlb3) & (bit(mlbs3)-1)];
          *p3 = (uintL***)room; room += bit(mlbs2)*sizeof(uintL**);
          {var uintL*** p2 = &(*p3)[(addr >> mlb2) & (bit(mlbs2)-1)];
           *p2 = (uintL**)room; room += bit(mlbs1)*sizeof(uintL*);
           {var uintL** p1 = &(*p2)[(addr >> mlb1) & (bit(mlbs1)-1)];
            *p1 = (uintL*)room;
            {var uintL* p0 = &(*p1)[((addr >> mlb0) / (32*sizeof(object)))
                                    & ((bit(mlbs0)-1) / (32*sizeof(object)))];
             var uintL i0 = ((addr >> mlb0) / sizeof(object)) % 32;
             *p0 = bit(i0);
       }}}}}}
       bitmap->used_size += need;
       return FALSE;
     }}
     #endif
    }

  local void mlb_free(bitmap)
    var mlbitmap* bitmap;
    { if (bitmap->base)
        { begin_system_call();
          free((char*)bitmap->base);
          end_system_call();
    }   }

#endif


# Implementation of get_circularities.

#ifdef MULTITHREAD

# get_circularities(obj,pr_array,pr_closure)
# Method:
# Traverse the object recursively, noting in a hash set (a multi-level bit map)
# the sub-objects traversed. While doing this, push the circularities onto the
# STACK. Then release the bitmap.
# Allocate a vector for the circularities (this kann GC auslösen!), move the
# circularities from the STACK into the vector.

# Global variables during get_circularities.
  typedef struct { mlbitmap bitmap;
                   boolean pr_array;
                   boolean pr_closure;
                   uintL counter;
                   jmp_buf abbruch_context;
                   object* abbruch_STACK;
                 }
          get_circ_global;

# UP: markiert das Objekt obj, legt auftretende Zirkularitäten auf den STACK
# und zählt sie in env->counter mit.
  local void get_circ_mark(obj,env)
    var object obj;
    var get_circ_global* env;
    { entry:
      #ifdef TYPECODES
      switch (typecode(obj)) # je nach Typ
      #else
      if (orecordp(obj)) { goto case_orecord; }
      elif (consp(obj)) { goto case_cons; }
      else { goto m_end; }
      switch (0)
      #endif
        { case_cons:
            if (mlb_add(&env->bitmap,obj)) goto m_schon_da; # markiert?
            { var object obj_cdr = Cdr(obj); # Komponenten
              var object obj_car = Car(obj);
              if (SP_overflow()) # SP-Tiefe überprüfen
                longjmp(env->abbruch_context,TRUE); # Abbruch
              get_circ_mark(obj_car,env); # CAR markieren (rekursiv)
              obj = obj_cdr; goto entry; # CDR markieren (tail-end-rekursiv)
            }
          case_symbol:
            if (mlb_add(&env->bitmap,obj)) # markiert?
              if (eq(Symbol_package(obj),NIL)) # uninterniertes Symbol?
                goto m_schon_da; # ja -> war schon da, merken
                else
                goto m_end; # nein -> war zwar schon da, aber unberücksichtigt lassen
            goto m_end;
          case_bvector: # Bit-Vector
          case_string: # String
          case_bignum: # Bignum
          #ifndef WIDE
          case_ffloat: # Single-Float
          #endif
          case_dfloat: # Double-Float
          case_lfloat: # Long-Float
          case_ratio: # Ratio
          case_complex: # Complex
            # Objekt ohne Komponenten, die ausgegeben werden:
            if (mlb_add(&env->bitmap,obj)) goto m_schon_da; # markiert?
            goto m_end;
          case_svector: # Simple-Vector
            if (mlb_add(&env->bitmap,obj)) goto m_schon_da; # markiert?
            # bisher unmarkiert
            if (env->pr_array) # Komponenten weiterzuverfolgen?
              { var uintL count = Svector_length(obj);
                if (!(count==0))
                  # markiere count>0 Komponenten
                  { var object* ptr = &TheSvector(obj)->data[0];
                    if (SP_overflow()) # SP-Tiefe überprüfen
                      longjmp(env->abbruch_context,TRUE); # Abbruch
                    dotimespL(count,count, { get_circ_mark(*ptr++,env); } ); # markiere Komponenten (rekursiv)
              }   }
            goto m_end;
          case_mdarray: case_ovector:
            # Nicht-simpler Array mit Komponenten, die Objekte sind:
            if (mlb_add(&env->bitmap,obj)) goto m_schon_da; # markiert?
            # bisher unmarkiert
            if (env->pr_array) # Komponenten weiterzuverfolgen?
              { obj=TheIarray(obj)->data; goto entry; } # Datenvektor (tail-end-rekursiv) markieren
              else
              goto m_end;
          case_closure: # Closure
            if (mlb_add(&env->bitmap,obj)) goto m_schon_da; # markiert?
            # bisher unmarkiert
            if (env->pr_closure) # Komponenten weiterzuverfolgen?
              goto m_record_components; # alle Komponenten werden ausgeben (s. unten)
              else # nur den Namen (tail-end-rekursiv) markieren
              { obj=TheClosure(obj)->clos_name; goto entry; }
          case_structure: # Structure
            if (mlb_add(&env->bitmap,obj)) goto m_schon_da; # markiert?
            # bisher unmarkiert
            goto m_record_components;
          case_stream: # Stream
            if (mlb_add(&env->bitmap,obj)) goto m_schon_da; # markiert?
            # bisher unmarkiert
            switch (TheStream(obj)->strmtype)
              { case strmtype_broad:
                case strmtype_concat:
                  goto m_record_components;
                default:
                  goto m_end;
              }
          case_instance: # CLOS-Instanz
            if (mlb_add(&env->bitmap,obj)) goto m_schon_da; # markiert?
            # bisher unmarkiert
            goto m_record_components;
          case_orecord: # sonstigen Record markieren:
            switch (Record_type(obj))
              {
                #ifndef TYPECODES
                case_Rectype_Symbol_above;
                case_Rectype_bvector_above;
                case_Rectype_string_above;
                case_Rectype_Bignum_above;
                case_Rectype_Ffloat_above;
                case_Rectype_Dfloat_above;
                case_Rectype_Lfloat_above;
                case_Rectype_Ratio_above;
                case_Rectype_Complex_above;
                case_Rectype_Svector_above;
                case_Rectype_mdarray_above;
                case_Rectype_ovector_above;
                #endif
                case_Rectype_Closure_above;
                case_Rectype_Structure_above;
                case_Rectype_Stream_above;
                case_Rectype_Instance_above;
                default: ;
              }
            if (mlb_add(&env->bitmap,obj)) goto m_schon_da; # markiert?
            # bisher unmarkiert
            switch (Record_type(obj))
              { case Rectype_Hashtable:
                  # Hash-Table: je nach Array-Ausgabe-Flag
                  if (env->pr_array) break; else goto m_end;
                case Rectype_Package:
                  # Packages werden nicht komponentenweise ausgegeben
                  goto m_end;
                case Rectype_Readtable:
                  # Readtables werden nicht komponentenweise ausgegeben
                  goto m_end;
                default: break;
              }
            # Pathnames, Random-States, Bytes, Fsubrs, Loadtimeevals,
            # Symbol-Macros und evtl. Hash-Tables werden evtl.
            # komponentenweise ausgegeben.
            m_record_components: # Komponenten eines Records markieren:
              { var uintC count = Record_length(obj);
                if (!(count==0))
                  # markiere count>0 Komponenten
                  { var object* ptr = &TheRecord(obj)->recdata[0];
                    if (SP_overflow()) # SP-Tiefe überprüfen
                      longjmp(env->abbruch_context,TRUE); # Abbruch
                    dotimespC(count,count, { get_circ_mark(*ptr++,env); } ); # markiere Komponenten (rekursiv)
              }   }
            goto m_end;
          m_schon_da:
            # Objekt wurde markiert, war aber schon markiert.
            # Es ist eine Zirkularität.
            if (STACK_overflow()) # STACK-Tiefe überprüfen
              longjmp(env->abbruch_context,TRUE); # Abbruch
            # Objekt im STACK ablegen:
            pushSTACK(obj);
            env->counter++; # und mitzählen
            goto m_end;
          #ifdef TYPECODES
          case_machine: # Maschinenpointer
          case_char: # Character
          case_subr: # Subr
          case_system: # Frame-pointer, Read-label, system
          case_fixnum: # Fixnum
          case_sfloat: # Short-Float
          #ifdef WIDE
          case_ffloat: # Single-Float
          #endif
          #endif
          default:
            # Objekt kann nicht markiert werden -> fertig
            goto m_end;
          m_end: ; # fertig
    }   }

  global object get_circularities(obj,pr_array,pr_closure)
    var object obj;
    var boolean pr_array;
    var boolean pr_closure;
    { var get_circ_global my_global; # Zähler und Kontext (incl. STACK-Wert)
                                     # für den Fall eines Abbruchs
      set_break_sem_1(); # Break unmöglich machen
      if (!setjmp(my_global.abbruch_context)) # Kontext abspeichern
        { bcopy(my_global.abbruch_context,my_global.bitmap.oom_context,sizeof(jmp_buf));
          mlb_alloc(&my_global.bitmap); # Bitmap allozieren
          my_global.pr_array = pr_array;
          my_global.pr_closure = pr_closure;
          my_global.counter = 0; # Zähler := 0
          my_global.abbruch_STACK = STACK;
          # Die Kontext-Konserve my_global ist jetzt fertig.
          get_circ_mark(obj,&my_global); # Objekt markieren, mehrfache
                                         # Strukturen auf dem STACK ablegen
                                         # in my_global.counter zählen
          mlb_free(&my_global.bitmap); # Bitmap freigeben
          clr_break_sem_1(); # Break wieder möglich
          { var uintL n = my_global.counter; # Anzahl der Objekte auf dem STACK
            if (n==0)
              return(NIL); # keine da -> NIL zurück und fertig
              else
              { var object vector = allocate_vector(n+1); # Vektor mit n+1 Elementen
                # füllen:
                var object* ptr = &TheSvector(vector)->data[0];
                *ptr++ = Fixnum_0; # erstes Element = Fixnum 0
                # restliche Elemente eintragen (mindestens eins):
                dotimespL(n,n, { *ptr++ = popSTACK(); } );
                return(vector); # Vektor als Ergebnis
        } }   }
        else
        # nach Abbruch wegen SP- oder STACK-Überlauf
        { setSTACK(STACK = my_global.abbruch_STACK); # STACK wieder zurücksetzen
          # Der Kontext ist jetzt wiederhergestellt.
          mlb_free(&my_global.bitmap); # Bitmap freigeben
          clr_break_sem_1(); # Break wieder möglich
          return(T); # T als Ergebnis
        }
    }

#else # !MULTITHREAD

# get_circularities(obj,pr_array,pr_closure)
# Methode:
# Markiere rekursiv das Objekt, lege dabei die Zirkularitäten auf den STACK,
# demarkiere rekursiv das Objekt,
# alloziere Vektor für die Zirkularitäten (kann GC auslösen!),
# fülle die Zirkularitäten vom STACK in den Vektor um.
  typedef struct { boolean pr_array;
                   boolean pr_closure;
                   uintL counter;
                   jmp_buf abbruch_context;
                   object* abbruch_STACK;
                 }
          get_circ_global;
  # Darauf muss man aus den zwei lokalen Routinen heraus zugreifen.
  local void get_circ_mark (object obj, get_circ_global* env);
  local void get_circ_unmark (object obj, get_circ_global* env);
  global object get_circularities(obj,pr_array,pr_closure)
    var object obj;
    var boolean pr_array;
    var boolean pr_closure;
    { var get_circ_global my_global; # Zähler und Kontext (incl. STACK-Wert)
                                     # für den Fall eines Abbruchs
      set_break_sem_1(); # Break unmöglich machen
      if (!setjmp(my_global.abbruch_context)) # Kontext abspeichern
        { my_global.pr_array = pr_array;
          my_global.pr_closure = pr_closure;
          my_global.counter = 0; # Zähler := 0
          my_global.abbruch_STACK = STACK;
          # Die Kontext-Konserve my_global ist jetzt fertig.
          get_circ_mark(obj,&my_global); # Objekt markieren, mehrfache
                                         # Strukturen auf dem STACK ablegen
                                         # in my_global.counter zählen
          get_circ_unmark(obj,&my_global); # Markierungen wieder löschen
          clr_break_sem_1(); # Break wieder möglich
          { var uintL n = my_global.counter; # Anzahl der Objekte auf dem STACK
            if (n==0)
              return(NIL); # keine da -> NIL zurück und fertig
              else
              { var object vector = allocate_vector(n+1); # Vektor mit n+1 Elementen
                # füllen:
                var object* ptr = &TheSvector(vector)->data[0];
                *ptr++ = Fixnum_0; # erstes Element = Fixnum 0
                # restliche Elemente eintragen (mindestens eins):
                dotimespL(n,n, { *ptr++ = popSTACK(); } );
                return(vector); # Vektor als Ergebnis
        } }   }
        else
        # nach Abbruch wegen SP- oder STACK-Überlauf
        { setSTACK(STACK = my_global.abbruch_STACK); # STACK wieder zurücksetzen
          # Der Kontext ist jetzt wiederhergestellt.
          get_circ_unmark(obj,&my_global); # Markierungen wieder löschen
          clr_break_sem_1(); # Break wieder möglich
          return(T); # T als Ergebnis
        }
    }
# UP: markiert das Objekt obj, legt auftretende Zirkularitäten auf den STACK
# und zählt sie in env->counter mit.
  local void get_circ_mark(obj,env)
    var object obj;
    var get_circ_global* env;
    { entry:
      #ifdef TYPECODES
      switch (typecode(obj)) # je nach Typ
      #else
      if (orecordp(obj)) { goto case_orecord; }
      elif (consp(obj)) { goto case_cons; }
      else { goto m_end; }
      switch (0)
      #endif
        { case_cons:
            if (marked(TheCons(obj))) goto m_schon_da; # markiert?
            { var object obj_cdr = Cdr(obj); # Komponenten (ohne Markierungsbit)
              var object obj_car = Car(obj);
              mark(TheCons(obj)); # markieren
              if (SP_overflow()) # SP-Tiefe überprüfen
                longjmp(env->abbruch_context,TRUE); # Abbruch
              get_circ_mark(obj_car,env); # CAR markieren (rekursiv)
              obj = obj_cdr; goto entry; # CDR markieren (tail-end-rekursiv)
            }
          case_symbol:
            if (marked(TheSymbol(obj))) # markiert?
              { if (eq(Symbol_package(obj),NIL)) # uninterniertes Symbol?
                  goto m_schon_da; # ja -> war schon da, merken
                  else
                  goto m_end; # nein -> war zwar schon da, aber unberücksichtigt lassen
              }
            # bisher unmarkiertes Symbol
            mark(TheSymbol(obj)); # markieren
            goto m_end;
          case_bvector: # Bit-Vector
          case_string: # String
          case_bignum: # Bignum
          #ifndef WIDE
          case_ffloat: # Single-Float
          #endif
          case_dfloat: # Double-Float
          case_lfloat: # Long-Float
          case_ratio: # Ratio
          case_complex: # Complex
            # Objekt ohne Komponenten, die ausgegeben werden:
            if (marked(ThePointer(obj))) goto m_schon_da; # markiert?
            # bisher unmarkiert
            mark(ThePointer(obj)); # markieren
            goto m_end;
          case_svector: # Simple-Vector
            if (marked(TheSvector(obj))) goto m_schon_da; # markiert?
            # bisher unmarkiert
            mark(TheSvector(obj)); # markieren
            m_svector:
            if (env->pr_array) # Komponenten weiterzuverfolgen?
              { var uintL count = Svector_length(obj);
                if (!(count==0))
                  # markiere count>0 Komponenten
                  { var object* ptr = &TheSvector(obj)->data[0];
                    if (SP_overflow()) # SP-Tiefe überprüfen
                      longjmp(env->abbruch_context,TRUE); # Abbruch
                    dotimespL(count,count, { get_circ_mark(*ptr++,env); } ); # markiere Komponenten (rekursiv)
              }   }
            goto m_end;
          case_mdarray: case_ovector:
            # Nicht-simpler Array mit Komponenten, die Objekte sind:
            if (marked(TheIarray(obj))) goto m_schon_da; # markiert?
            # bisher unmarkiert
            mark(TheIarray(obj)); # markieren
            m_array:
            if (env->pr_array) # Komponenten weiterzuverfolgen?
              { obj=TheIarray(obj)->data; goto entry; } # Datenvektor (tail-end-rekursiv) markieren
              else
              goto m_end;
          case_closure: # Closure
            if (marked(TheClosure(obj))) goto m_schon_da; # markiert?
            # bisher unmarkiert
            mark(TheClosure(obj)); # markieren
            if (env->pr_closure) # Komponenten weiterzuverfolgen?
              goto m_record_components; # alle Komponenten werden ausgeben (s. unten)
              else # nur den Namen (tail-end-rekursiv) markieren
              { obj=TheClosure(obj)->clos_name; goto entry; }
          case_structure: # Structure
            if (marked(TheStructure(obj))) goto m_schon_da; # markiert?
            # bisher unmarkiert
            mark(TheStructure(obj)); # markieren
            goto m_record_components;
          case_stream: # Stream
            if (marked(TheStream(obj))) goto m_schon_da; # markiert?
            # bisher unmarkiert
            mark(TheStream(obj));
            switch (TheStream(obj)->strmtype)
              { case strmtype_broad:
                case strmtype_concat:
                  goto m_record_components;
                default:
                  goto m_end;
              }
          case_instance: # CLOS-Instanz
            if (marked(TheInstance(obj))) goto m_schon_da; # markiert?
            # bisher unmarkiert
            mark(TheInstance(obj)); # markieren
            goto m_record_components;
          case_orecord: # sonstigen Record markieren:
            switch (Record_type(obj))
              {
                #ifndef TYPECODES
                case_Rectype_Symbol_above;
                case_Rectype_bvector_above;
                case_Rectype_string_above;
                case_Rectype_Bignum_above;
                case_Rectype_Ffloat_above;
                case_Rectype_Dfloat_above;
                case_Rectype_Lfloat_above;
                case_Rectype_Ratio_above;
                case_Rectype_Complex_above;
                case_Rectype_Svector_above;
                case_Rectype_mdarray_above;
                case_Rectype_ovector_above;
                #endif
                case_Rectype_Closure_above;
                case_Rectype_Structure_above;
                case_Rectype_Stream_above;
                case_Rectype_Instance_above;
                default: ;
              }
            if (marked(TheRecord(obj))) goto m_schon_da; # markiert?
            # bisher unmarkiert
            mark(TheRecord(obj)); # markieren
            switch (Record_type(obj))
              { case Rectype_Hashtable:
                  # Hash-Table: je nach Array-Ausgabe-Flag
                  if (env->pr_array) break; else goto m_end;
                case Rectype_Package:
                  # Packages werden nicht komponentenweise ausgegeben
                  goto m_end;
                case Rectype_Readtable:
                  # Readtables werden nicht komponentenweise ausgegeben
                  goto m_end;
                default: break;
              }
            # Pathnames, Random-States, Bytes, Fsubrs, Loadtimeevals,
            # Symbol-Macros und evtl. Hash-Tables werden evtl.
            # komponentenweise ausgegeben.
            m_record_components: # Komponenten eines Records markieren:
              { var uintC count = Record_length(obj);
                if (!(count==0))
                  # markiere count>0 Komponenten
                  { var object* ptr = &TheRecord(obj)->recdata[0];
                    if (SP_overflow()) # SP-Tiefe überprüfen
                      longjmp(env->abbruch_context,TRUE); # Abbruch
                    dotimespC(count,count, { get_circ_mark(*ptr++,env); } ); # markiere Komponenten (rekursiv)
              }   }
            goto m_end;
          m_schon_da:
            # Objekt wurde markiert, war aber schon markiert.
            # Es ist eine Zirkularität.
            if (STACK_overflow()) # STACK-Tiefe überprüfen
              longjmp(env->abbruch_context,TRUE); # Abbruch
            # Objekt mit gelöschtem garcol_bit im STACK ablegen:
            pushSTACK(without_mark_bit(obj));
            env->counter++; # und mitzählen
            goto m_end;
          #ifdef TYPECODES
          case_machine: # Maschinenpointer
          case_char: # Character
          case_subr: # Subr
          case_system: # Frame-pointer, Read-label, system
          case_fixnum: # Fixnum
          case_sfloat: # Short-Float
          #ifdef WIDE
          case_ffloat: # Single-Float
          #endif
          #endif
          default:
            # Objekt kann nicht markiert werden -> fertig
            goto m_end;
          m_end: ; # fertig
    }   }
# UP: Demarkiert Objekt obj.
  local void get_circ_unmark(obj,env)
    var object obj;
    var get_circ_global* env;
    { entry:
      #ifdef TYPECODES
      switch (typecode(obj) & ~bit(garcol_bit_t)) # je nach Typinfo ohne garcol_bit
      #else
      if (orecordp(obj)) { goto case_orecord; }
      elif (consp(obj)) { goto case_cons; }
      else { goto u_end; }
      switch (0)
      #endif
        { case_cons:
            if (!marked(TheCons(obj))) goto u_end; # schon demarkiert?
            unmark(TheCons(obj)); # demarkieren
            get_circ_unmark(Car(obj),env); # CAR demarkieren (rekursiv)
            obj=Cdr(obj); goto entry; # CDR demarkieren (tail-end-rekursiv)
          case_bvector: # Bit-Vector
          case_string: # String
          case_symbol:
            # Symbol demarkieren. Wertzelle etc. für PRINT unwesentlich.
          case_bignum: # Bignum
          #ifndef WIDE
          case_ffloat: # Single-Float
          #endif
          case_dfloat: # Double-Float
          case_lfloat: # Long-Float
          case_ratio: # Ratio
          case_complex: # Complex
            # Objekt demarkieren, das keine markierten Komponenten hat:
            unmark(ThePointer(obj)); # demarkieren
            goto u_end;
          case_svector:
            # Simple-Vector demarkieren, seine Komponenten ebenfalls:
            if (!marked(TheSvector(obj))) goto u_end; # schon demarkiert?
            unmark(TheSvector(obj)); # demarkieren
            u_svector:
            if (env->pr_array) # wurden die Komponenten weiterverfolgt?
              { var uintL count = Svector_length(obj);
                if (!(count==0))
                  # demarkiere count>0 Komponenten
                  { var object* ptr = &TheSvector(obj)->data[0];
                    dotimespL(count,count, { get_circ_unmark(*ptr++,env); } ); # demarkiere Komponenten (rekursiv)
              }   }
            goto u_end;
          case_mdarray: case_ovector:
            # Nicht-simpler Array mit Komponenten, die Objekte sind:
            if (!marked(TheIarray(obj))) goto u_end; # schon demarkiert?
            unmark(TheIarray(obj)); # demarkieren
            u_array:
            if (env->pr_array) # wurden die Komponenten weiterverfolgt?
              { obj=TheIarray(obj)->data; goto entry; } # Datenvektor (tail-end-rekursiv) demarkieren
              else
              goto u_end;
          case_closure: # Closure demarkieren
            if (!marked(TheClosure(obj))) goto u_end; # schon demarkiert?
            unmark(TheClosure(obj)); # demarkieren
            if (env->pr_closure) # wurden Komponenten weiterverfolgt?
              goto u_record_components; # alle Komponenten werden ausgeben (s. unten)
              else # nur den Namen (tail-end-rekursiv) demarkieren
              { obj=TheClosure(obj)->clos_name; goto entry; }
          case_structure: # Structure demarkieren:
            if (!marked(TheStructure(obj))) goto u_end; # schon demarkiert?
            unmark(TheStructure(obj)); # demarkieren
            goto u_record_components;
          case_stream: # Stream demarkieren:
            if (!marked(TheStream(obj))) goto u_end; # schon demarkiert?
            unmark(TheStream(obj)); # demarkieren
            switch (TheStream(obj)->strmtype)
              { case strmtype_broad:
                case strmtype_concat:
                  goto u_record_components;
                default:
                  goto u_end;
              }
          case_instance: # CLOS-Instanz demarkieren:
            if (!marked(TheInstance(obj))) goto u_end; # schon demarkiert?
            unmark(TheInstance(obj)); # demarkieren
            goto u_record_components;
          case_orecord: # sonstigen Record demarkieren:
            switch (Record_type(obj))
              {
                #ifndef TYPECODES
                case_Rectype_bvector_above;
                case_Rectype_string_above;
                case_Rectype_Symbol_above;
                case_Rectype_Bignum_above;
                case_Rectype_Ffloat_above;
                case_Rectype_Dfloat_above;
                case_Rectype_Lfloat_above;
                case_Rectype_Ratio_above;
                case_Rectype_Complex_above;
                case_Rectype_Svector_above;
                case_Rectype_mdarray_above;
                case_Rectype_ovector_above;
                #endif
                case_Rectype_Closure_above;
                case_Rectype_Structure_above;
                case_Rectype_Stream_above;
                case_Rectype_Instance_above;
                default: ;
              }
            if (!marked(TheRecord(obj))) goto u_end; # schon demarkiert?
            unmark(TheRecord(obj)); # demarkieren
            switch (Record_type(obj))
              { case Rectype_Hashtable:
                  # Hash-Table: je nach Array-Ausgabe-Flag
                  if (env->pr_array) break; else goto u_end;
                case Rectype_Package:
                  # Packages werden nicht komponentenweise ausgegeben
                  goto u_end;
                case Rectype_Readtable:
                  # Readtables werden nicht komponentenweise ausgegeben
                  goto u_end;
                default: break;
              }
            # Pathnames, Random-States, Bytes, Fsubrs, Loadtimeevals,
            # Symbol-Macros und evtl. Hash-Tables werden evtl.
            # komponentenweise ausgegeben.
            u_record_components: # Komponenten eines Records demarkieren:
              { var uintC count = Record_length(obj);
                if (!(count==0))
                  # demarkiere count>0 Komponenten
                  { var object* ptr = &TheRecord(obj)->recdata[0];
                    dotimespC(count,count, { get_circ_unmark(*ptr++,env); } ); # demarkiere Komponenten (rekursiv)
              }   }
            goto u_end;
          #ifdef TYPECODES
          case_machine: # Maschinenpointer
          case_char: # Character
          case_subr: # Subr
          case_system: # Frame-pointer, Read-label, system
          case_fixnum: # Fixnum
          case_sfloat: # Short-Float
          #ifdef WIDE
          case_ffloat: # Single-Float
          #endif
          #endif
          default:
            # Objekt demarkieren, das gar keine Markierung haben kann:
            goto u_end;
          u_end: ; # fertig
    }   }

#endif


# Implementation of subst_circ.

#ifdef MULTITHREAD

# Global variables during subst_circ.
  typedef struct { mlbitmap bitmap;
                   object alist;
                   jmp_buf abbruch_context;
                   object bad;
                 }
          subst_circ_global;

  local void subst_circ_mark (object* ptr, subst_circ_global* env);
  local void subst_circ_mark(ptr,env)
    var object* ptr;
    var subst_circ_global* env;
    {
      #if !(defined(NO_SP_CHECK) || defined(NOCOST_SP_CHECK))
      if (SP_overflow()) # SP-Tiefe überprüfen
        { env->bad = nullobj; longjmp(env->abbruch_context,TRUE); } # Abbruch
      #endif
      enter_subst:
     {var object obj = *ptr;
      # Fallunterscheidung nach Typ:
      # Objekte ohne Teilobjekte (Maschinenpointer, Bit-Vektoren,
      # Strings, Characters, SUBRs, Integers, Floats) enthalten
      # keine Referenzen. Ebenso Symbole und rationale Zahlen (bei ihnen
      # können die Teilobjekte nicht in #n= - Syntax eingegeben worden
      # sein) und komplexe Zahlen (für ihre Komponenten sind nur
      # Integers, Floats, rationale Zahlen zugelassen, also Objekte,
      # die ihrerseits keine Referenzen enthalten können).
      #ifdef TYPECODES
      switch (typecode(obj))
      #else
      if (orecordp(obj)) { goto case_orecord; }
      elif (consp(obj)) { goto case_cons; }
      elif (immediate_number_p(obj)) { goto case_number; }
      elif (charp(obj)) { goto case_char; }
      elif (subrp(obj)) { goto case_subr; }
      elif (machinep(obj)) { goto case_machine; }
      elif (read_label_p(obj)) { goto case_read_label; }
      elif (systemp(obj)) { return; }
      else switch (0)
      #endif
        { case_svector: # Simple-Vector
            if (mlb_add(&env->bitmap,obj)) return; # Objekt schon markiert?
            # alle Elemente durchlaufen:
            { var uintL len = Svector_length(obj);
              if (!(len==0))
                { var object* objptr = &TheSvector(obj)->data[0];
                  dotimespL(len,len, { subst_circ_mark(&(*objptr++),env); } );
            }   }
            return;
          case_mdarray:
          case_ovector:
            # nicht-simpler Array, kein String oder Bit-Vektor
            if (mlb_add(&env->bitmap,obj)) return; # Objekt schon markiert?
            # Datenvektor durchlaufen: endrekursiv subst_circ_mark(Datenvektor)
            ptr = &TheIarray(obj)->data; goto enter_subst;
          case_closure: _case_structure _case_stream case_orecord: case_instance: # Record
            #ifndef TYPECODES
            switch (Record_type(obj))
              { case_Rectype_Svector_above;
                case_Rectype_mdarray_above;
                case_Rectype_ovector_above;
                case_Rectype_bvector_above;
                case_Rectype_string_above;
                case_Rectype_number_above;
                case_Rectype_Symbol_above;
                default: ;
              }
            #endif
            if (mlb_add(&env->bitmap,obj)) return; # Objekt schon markiert?
            # Beim Ersetzen von Read-Labels in Hash-Tables verliert deren
            # Aufbau seinen Gültigkeit (denn die Hashfunktion der in ihr
            # gespeicherten Objekte verändert sich).
            if (Record_type(obj) == Rectype_Hashtable) # eine Hash-Table ?
              { mark_ht_invalid(TheHashtable(obj)); } # ja -> für Reorganisation vormerken
            # alle Elemente durchlaufen:
            { var uintC len = Record_length(obj);
              if (!(len==0))
                { var object* objptr = &TheRecord(obj)->recdata[0];
                  dotimespC(len,len, { subst_circ_mark(&(*objptr++),env); } );
            }   }
            return;
          #ifdef TYPECODES
          case_system: # Frame-Pointer oder Read-Label oder System
            if (!(as_oint(obj) & wbit(0+oint_addr_shift)))
              # Frame-Pointer
              {}
              else
              # Read-Label oder System
              if (as_oint(obj) & wbit(oint_data_len-1+oint_addr_shift))
                {} # System
                else
          #endif
                case_read_label:
                # Read-Label
                { # Read-Label obj in der Aliste suchen:
                  var object alist = env->alist;
                  while (consp(alist))
                    { var object acons = Car(alist);
                      if (eq(Car(acons),obj))
                        # gefunden
                        { # *ptr = obj = (car acons) durch (cdr acons) ersetzen,
                          # dabei aber das Markierungsbit unverändert lassen:
                          *ptr = (marked(ptr) ? with_mark_bit(Cdr(acons)) : Cdr(acons));
                          return;
                        }
                      alist = Cdr(alist);
                    }
                  # nicht gefunden -> Abbruch
                  env->bad = obj;
                  longjmp(env->abbruch_context,TRUE);
                }
            return;
          case_cons: # Cons
            if (mlb_add(&env->bitmap,obj)) return; # Objekt schon markiert?
            # rekursiv: subst_circ_mark(&Car(obj))
            subst_circ_mark(&TheCons(obj)->car,env);
            # endrekursiv: subst_circ_mark(&Cdr(obj))
            ptr = &TheCons(obj)->cdr; goto enter_subst;
          case_machine: # Maschinenpointer
          case_bvector: # Bit-Vektor
          case_string: # String
          case_char: # Character
          case_subr: # SUBR
          case_number: # Zahl
          case_symbol: # Symbol
            # Objekt enthält keine Referenzen -> nichts zu tun
            return;
          default: NOTREACHED
    }}  }

  global object subst_circ(ptr,alist)
    var object* ptr;
    var object alist;
    { var subst_circ_global my_global;
      my_global.alist = alist;
      set_break_sem_1(); # Break unmöglich machen
      if (!setjmp(my_global.abbruch_context))
        { bcopy(my_global.abbruch_context,my_global.bitmap.oom_context,sizeof(jmp_buf));
          mlb_alloc(&my_global.bitmap);
          subst_circ_mark(ptr,&my_global); # markieren und substituieren
          mlb_free(&my_global.bitmap);
          clr_break_sem_1(); # Break wieder möglich
          return nullobj;
        }
        else
        # Abbruch aus subst_circ_mark() heraus
        { mlb_free(&my_global.bitmap);
          clr_break_sem_1(); # Break wieder möglich
          #if !(defined(NO_SP_CHECK) || defined(NOCOST_SP_CHECK))
          if (eq(my_global.bad,nullobj))
            { SP_ueber(); }
          #endif
          return my_global.bad; # wegen fehlerhafter Referenz
    }   }

#else # !MULTITHREAD

#if 0 # ohne Zirkularitätenberücksichtigung

  local void subst (object* ptr);
  local object subst_circ_alist;
  local jmp_buf subst_circ_jmpbuf;
  local object subst_circ_bad;
  global object subst_circ(ptr,alist)
    var object* ptr;
    var object alist;
    { subst_circ_alist = alist;
      begin_setjmp_call();
      if (!setjmp(subst_circ_jmpbuf))
        { end_setjmp_call(); subst(ptr); return nullobj; }
        else
        # Abbruch wegen fehlerhafter Referenz
        { end_longjmp_call(); return subst_circ_bad; }
    }
  local void subst(ptr)
    var object ptr;
    { check_SP();
      enter_subst:
     {var object obj = *ptr;
      # Fallunterscheidung nach Typ:
      # Objekte ohne Teilobjekte (Maschinenpointer, Bit-Vektoren,
      # Strings, Characters, SUBRs, Integers, Floats) enthalten
      # keine Referenzen. Ebenso Symbole und rationale Zahlen (bei ihnen
      # können die Teilobjekte nicht in #n= - Syntax eingegeben worden
      # sein) und komplexe Zahlen (für ihre Komponenten sind nur
      # Integers, Floats, rationale Zahlen zugelassen, also Objekte,
      # die ihrerseits keine Referenzen enthalten können).
      #ifdef TYPECODES
      switch (mtypecode(*ptr))
      #else
      if (orecordp(obj)) { goto case_orecord; }
      elif (consp(obj)) { goto case_cons; }
      elif (immediate_number_p(obj)) { goto case_number; }
      elif (charp(obj)) { goto case_char; }
      elif (subrp(obj)) { goto case_subr; }
      elif (machinep(obj)) { goto case_machine; }
      elif (read_label_p(obj)) { goto case_read_label; }
      elif (systemp(obj)) { return; }
      else switch (0)
      #endif
        { case_svector: # Simple-Vector
            # alle Elemente durchlaufen:
            { var uintL len = Svector_length(obj);
              if (!(len==0))
                { var object* objptr = &TheSvector(obj)->data[0];
                  dotimespL(len,len, { subst(&(*objptr++)); } );
            }   }
            break;
          case_mdarray:
          case_ovector:
            # nicht-simpler Array, kein String oder Bit-Vektor
            # Datenvektor durchlaufen: endrekursiv subst(Datenvektor)
            ptr = &TheIarray(obj)->data; goto enter_subst;
          case_closure: _case_structure _case_stream case_orecord: case_instance: # Record
            #ifndef TYPECODES
            switch (Record_type(obj))
              { case_Rectype_Svector_above;
                case_Rectype_mdarray_above;
                case_Rectype_ovector_above;
                case_Rectype_bvector_above;
                case_Rectype_string_above;
                case_Rectype_number_above;
                case_Rectype_Symbol_above;
                default: ;
              }
            #endif
            # alle Elemente durchlaufen:
            { var uintC len = Record_length(obj);
              if (!(len==0))
                { var object* objptr = &TheRecord(obj)->recdata[0];
                  dotimespC(len,len, { subst(&(*objptr++)); } );
            }   }
            break;
          #ifdef TYPECODES
          case_system: # Frame-Pointer oder Read-Label oder System
            if (!(as_oint(obj) & wbit(0+oint_addr_shift)))
              # Frame-Pointer
              {}
              else
              # Read-Label oder System
              if (as_oint(obj) & wbit(oint_data_len-1+oint_addr_shift))
                {} # System
                else
          #endif
                case_read_label:
                # Read-Label
                { # Read-Label obj in der Aliste suchen:
                  var object alist = subst_circ_alist;
                  while (consp(alist))
                    { var object acons = Car(alist);
                      if (eq(Car(acons),obj))
                        # gefunden
                        { # *ptr = obj = (car acons) durch (cdr acons) ersetzen:
                          *ptr = Cdr(acons);
                          return;
                        }
                      alist = Cdr(alist);
                    }
                  # nicht gefunden -> Abbruch
                  subst_circ_bad = obj;
                  begin_longjmp_call();
                  longjmp(subst_circ_jmpbuf,TRUE);
                }
            break;
          case_cons: # Cons
            # rekursiv: subst(&Car(obj))
            subst(&TheCons(obj)->car);
            # endrekursiv: subst(&Cdr(obj))
            ptr = &TheCons(obj)->cdr; goto enter_subst;
          case_machine: # Maschinenpointer
          case_bvector: # Bit-Vektor
          case_string: # String
          case_char: # Character
          case_subr: # SUBR
          case_number: # Zahl
          case_symbol: # Symbol
            # Objekt enthält keine Referenzen -> nichts zu tun
            break;
          default: NOTREACHED
    }}  }

#else # mit Zirkularitätenberücksichtigung

# Methode:
# Markiere rekursiv die Objekte, in denen die Substitution gerade durchgeführt
# wird/wurde. Danach demarkiere rekursiv das Objekt.

  local void subst_circ_mark (object* ptr);
  local void subst_circ_unmark (object* ptr);
  local object subst_circ_alist;
  local jmp_buf subst_circ_jmpbuf;
  local object subst_circ_bad;
  global object subst_circ(ptr,alist)
    var object* ptr;
    var object alist;
    { subst_circ_alist = alist;
      set_break_sem_1(); # Break unmöglich machen
      if (!setjmp(subst_circ_jmpbuf))
        { subst_circ_mark(ptr); # markieren und substituieren
          subst_circ_unmark(ptr); # Markierungen wieder löschen
          clr_break_sem_1(); # Break wieder möglich
          return nullobj;
        }
        else
        # Abbruch aus subst_circ_mark() heraus
        { subst_circ_unmark(ptr); # erst alles demarkieren
          clr_break_sem_1(); # Break wieder möglich
          #if !(defined(NO_SP_CHECK) || defined(NOCOST_SP_CHECK))
          if (eq(subst_circ_bad,nullobj))
            { SP_ueber(); }
          #endif
          return subst_circ_bad; # wegen fehlerhafter Referenz
    }   }
  local void subst_circ_mark(ptr)
    var object* ptr;
    {
      #if !(defined(NO_SP_CHECK) || defined(NOCOST_SP_CHECK))
      if (SP_overflow()) # SP-Tiefe überprüfen
        { subst_circ_bad = nullobj; longjmp(subst_circ_jmpbuf,TRUE); } # Abbruch
      #endif
      enter_subst:
     {var object obj = without_mark_bit(*ptr);
      # Fallunterscheidung nach Typ:
      # Objekte ohne Teilobjekte (Maschinenpointer, Bit-Vektoren,
      # Strings, Characters, SUBRs, Integers, Floats) enthalten
      # keine Referenzen. Ebenso Symbole und rationale Zahlen (bei ihnen
      # können die Teilobjekte nicht in #n= - Syntax eingegeben worden
      # sein) und komplexe Zahlen (für ihre Komponenten sind nur
      # Integers, Floats, rationale Zahlen zugelassen, also Objekte,
      # die ihrerseits keine Referenzen enthalten können).
      #ifdef TYPECODES
      switch (typecode(obj))
      #else
      if (orecordp(obj)) { goto case_orecord; }
      elif (consp(obj)) { goto case_cons; }
      elif (immediate_number_p(obj)) { goto case_number; }
      elif (charp(obj)) { goto case_char; }
      elif (subrp(obj)) { goto case_subr; }
      elif (machinep(obj)) { goto case_machine; }
      elif (read_label_p(obj)) { goto case_read_label; }
      elif (systemp(obj)) { return; }
      else switch (0)
      #endif
        { case_svector: # Simple-Vector
            if (marked(TheSvector(obj))) return; # Objekt schon markiert?
            mark(TheSvector(obj)); # markieren
            # alle Elemente durchlaufen:
            { var uintL len = Svector_length(obj);
              if (!(len==0))
                { var object* objptr = &TheSvector(obj)->data[0];
                  dotimespL(len,len, { subst_circ_mark(&(*objptr++)); } );
            }   }
            return;
          case_mdarray:
          case_ovector:
            # nicht-simpler Array, kein String oder Bit-Vektor
            if (marked(TheIarray(obj))) return; # Objekt schon markiert?
            mark(TheIarray(obj)); # markieren
            # Datenvektor durchlaufen: endrekursiv subst_circ_mark(Datenvektor)
            ptr = &TheIarray(obj)->data; goto enter_subst;
          case_closure: _case_structure _case_stream case_orecord: case_instance: # Record
            #ifndef TYPECODES
            switch (Record_type(obj))
              { case_Rectype_Svector_above;
                case_Rectype_mdarray_above;
                case_Rectype_ovector_above;
                case_Rectype_bvector_above;
                case_Rectype_string_above;
                case_Rectype_number_above;
                case_Rectype_Symbol_above;
                default: ;
              }
            #endif
            if (marked(TheRecord(obj))) return; # Objekt schon markiert?
            mark(TheRecord(obj)); # markieren
            # Beim Ersetzen von Read-Labels in Hash-Tables verliert deren
            # Aufbau seinen Gültigkeit (denn die Hashfunktion der in ihr
            # gespeicherten Objekte verändert sich).
            if (Record_type(obj) == Rectype_Hashtable) # eine Hash-Table ?
              { mark_ht_invalid(TheHashtable(obj)); } # ja -> für Reorganisation vormerken
            # alle Elemente durchlaufen:
            { var uintC len = Record_length(obj);
              if (!(len==0))
                { var object* objptr = &TheRecord(obj)->recdata[0];
                  dotimespC(len,len, { subst_circ_mark(&(*objptr++)); } );
            }   }
            return;
          #ifdef TYPECODES
          case_system: # Frame-Pointer oder Read-Label oder System
            if (!(as_oint(obj) & wbit(0+oint_addr_shift)))
              # Frame-Pointer
              {}
              else
              # Read-Label oder System
              if (as_oint(obj) & wbit(oint_data_len-1+oint_addr_shift))
                {} # System
                else
          #endif
                case_read_label:
                # Read-Label
                { # Read-Label obj in der Aliste suchen:
                  var object alist = subst_circ_alist;
                  while (consp(alist))
                    { var object acons = Car(alist);
                      if (eq(Car(acons),obj))
                        # gefunden
                        { # *ptr = obj = (car acons) durch (cdr acons) ersetzen,
                          # dabei aber das Markierungsbit unverändert lassen:
                          *ptr = (marked(ptr) ? with_mark_bit(Cdr(acons)) : Cdr(acons));
                          return;
                        }
                      alist = Cdr(alist);
                    }
                  # nicht gefunden -> Abbruch
                  subst_circ_bad = obj;
                  longjmp(subst_circ_jmpbuf,TRUE);
                }
            return;
          case_cons: # Cons
            if (marked(TheCons(obj))) return; # Objekt schon markiert?
            mark(TheCons(obj)); # markieren
            # rekursiv: subst_circ_mark(&Car(obj))
            subst_circ_mark(&TheCons(obj)->car);
            # endrekursiv: subst_circ_mark(&Cdr(obj))
            ptr = &TheCons(obj)->cdr; goto enter_subst;
          case_machine: # Maschinenpointer
          case_bvector: # Bit-Vektor
          case_string: # String
          case_char: # Character
          case_subr: # SUBR
          case_number: # Zahl
          case_symbol: # Symbol
            # Objekt enthält keine Referenzen -> nichts zu tun
            return;
          default: NOTREACHED
    }}  }
  local void subst_circ_unmark(ptr)
    var object* ptr;
    { enter_subst:
     {var object obj = *ptr;
      # Fallunterscheidung nach Typ, wie oben:
      #ifdef TYPECODES
      switch (typecode(obj))
      #else
      if (orecordp(obj)) { goto case_orecord; }
      elif (consp(obj)) { goto case_cons; }
      elif (immediate_number_p(obj)) { goto case_number; }
      elif (charp(obj)) { goto case_char; }
      elif (subrp(obj)) { goto case_subr; }
      elif (machinep(obj)) { goto case_machine; }
      elif (read_label_p(obj) || systemp(obj)) { goto case_system; }
      else switch (0)
      #endif
        { case_svector: # Simple-Vector
            if (!marked(TheSvector(obj))) return; # schon demarkiert?
            unmark(TheSvector(obj)); # demarkieren
            # alle Elemente durchlaufen:
            { var uintL len = Svector_length(obj);
              if (!(len==0))
                { var object* objptr = &TheSvector(obj)->data[0];
                  dotimespL(len,len, { subst_circ_unmark(&(*objptr++)); } );
            }   }
            return;
          case_mdarray:
          case_ovector:
            # nicht-simpler Array, kein String oder Bit-Vektor
            if (!marked(TheIarray(obj))) return; # schon demarkiert?
            unmark(TheIarray(obj)); # demarkieren
            # Datenvektor durchlaufen: endrekursiv subst_circ_unmark(Datenvektor)
            ptr = &TheIarray(obj)->data; goto enter_subst;
          case_closure: _case_structure _case_stream case_orecord: case_instance: # Record
            #ifndef TYPECODES
            switch (Record_type(obj))
              { case_Rectype_Svector_above;
                case_Rectype_mdarray_above;
                case_Rectype_ovector_above;
                case_Rectype_bvector_above;
                case_Rectype_string_above;
                case_Rectype_number_above;
                case_Rectype_Symbol_above;
                default: ;
              }
            #endif
            if (!marked(TheRecord(obj))) return; # schon demarkiert?
            unmark(TheRecord(obj)); # demarkieren
            # alle Elemente durchlaufen:
            { var uintC len = Record_length(obj);
              if (!(len==0))
                { var object* objptr = &TheRecord(obj)->recdata[0];
                  dotimespC(len,len, { subst_circ_unmark(&(*objptr++)); } );
            }   }
            return;
          case_cons: # Cons
            if (!marked(TheCons(obj))) return; # schon demarkiert?
            unmark(TheCons(obj)); # demarkieren
            # rekursiv: subst_circ_unmark(&Car(obj))
            subst_circ_unmark(&TheCons(obj)->car);
            # endrekursiv: subst_circ_unmark(&Cdr(obj))
            ptr = &TheCons(obj)->cdr; goto enter_subst;
          case_system: # Frame-Pointer oder Read-Label oder System
          case_machine: # Maschinenpointer
          case_bvector: # Bit-Vektor
          case_string: # String
          case_char: # Character
          case_subr: # SUBR
          case_number: # Zahl
          case_symbol: # Symbol
            # Objekt enthält keine Referenzen -> nichts zu tun
            return;
          default: NOTREACHED
    }}  }

#endif

#endif
