/*
 * Export CLISP internals for modules
 * Bruno Haible 1994-2004
 * Sam Steingold 1998-2004
 */

#include "lispbibl.c"

# Ausgabe von Strings mit eingebetteten Zahlen, wie printf().
# Nur dass die Zahlen auch vom Typ `unsigned long long' sein können.
# Wir vermeiden es, <stdarg.h> oder <varargs.h> vorauszusetzen.

typedef struct {
  char base; /* 'd' for decimal, 'x' for hexadecimal */
  int size;
  union {
    uint8 val8;
    uint16 val16;
    uint32 val32;
    #ifdef HAVE_LONGLONG
    uint64 val64;
    #endif
  } value;
} printf_arg;

#ifdef HAVE_LONGLONG
  #define fill_printf_arg(where,expr)  \
    where.size = sizeof(expr); \
    if (sizeof(expr) == sizeof(uint8)) { where.value.val8 = (uint8)(expr); } \
    else if (sizeof(expr) == sizeof(uint16)) { where.value.val16 = (uint16)(expr); } \
    else if (sizeof(expr) == sizeof(uint32)) { where.value.val32 = (uint32)(expr); } \
    else if (sizeof(expr) == sizeof(uint64)) { where.value.val64 = (uint64)(expr); } \
    else abort();
#else
  #define fill_printf_arg(where,expr)  \
    where.size = sizeof(expr); \
    if (sizeof(expr) == sizeof(uint8)) { where.value.val8 = (uint8)(expr); } \
    else if (sizeof(expr) == sizeof(uint16)) { where.value.val16 = (uint16)(expr); } \
    else if (sizeof(expr) == sizeof(uint32)) { where.value.val32 = (uint32)(expr); } \
    else abort();
#endif

local const char* Lsuffix = "L";
local const char* ULsuffix = "UL";
#ifdef HAVE_LONGLONG
local const char* ULLsuffix = "ULL";
#endif

void print_printf_arg (const printf_arg* arg)
  {
    switch (arg->size) {
      case sizeof(uint8):
        printf(arg->base=='d' ? "%u" : "0x%X", (unsigned int)(arg->value.val8));
        break;
      case sizeof(uint16):
        printf(arg->base=='d' ? "%u" : "0x%X", (unsigned int)(arg->value.val16));
        break;
      case sizeof(uint32):
        printf(arg->base=='d' ? "%lu%s" : "0x%lX%s", (unsigned long)(arg->value.val32), ULsuffix);
        break;
      #ifdef HAVE_LONGLONG
      case sizeof(uint64):
        #if (long_bitsize == 64)
          if (!(sizeof(uint64) == sizeof(unsigned long))) abort();
          printf("0x%lX%s", (unsigned long)(arg->value.val64), ULsuffix);
        #else
          if (!(sizeof(uint32) == sizeof(unsigned long))) abort();
          printf("0x%lX%08lX%s",
                 (unsigned long)(arg->value.val64 >> 32),
                 (unsigned long)(arg->value.val64 & 0xFFFFFFFFUL),
                 ULLsuffix
                );
        #endif
        break;
      #endif
      default:
        abort();
    }
  }

void printf_with_args (const char* string, int argcount, printf_arg* args)
{
  while (*string) {
    if (string[0]=='%') {
      if (!(string[1]=='d' || string[1]=='x')) abort();
      if (!(argcount > 0)) abort();
      args->base = string[1]; print_printf_arg(args);
      string+=2; args++; argcount--;
    } else {
      putchar(*string); string++;
    }
  }
}

#define printf0(string)  printf(string)
#define printf1(string,arg0)  \
  { var printf_arg args[1]; \
    fill_printf_arg(args[0],arg0); \
    printf_with_args(string,1,args); \
  }
#define printf2(string,arg0,arg1)  \
  { var printf_arg args[2]; \
    fill_printf_arg(args[0],arg0); \
    fill_printf_arg(args[1],arg1); \
    printf_with_args(string,2,args); \
  }
#define printf3(string,arg0,arg1,arg2)  \
  { var printf_arg args[3]; \
    fill_printf_arg(args[0],arg0); \
    fill_printf_arg(args[1],arg1); \
    fill_printf_arg(args[2],arg2); \
    printf_with_args(string,3,args); \
  }
#define printf4(string,arg0,arg1,arg2,arg3)  \
  { var printf_arg args[4]; \
    fill_printf_arg(args[0],arg0); \
    fill_printf_arg(args[1],arg1); \
    fill_printf_arg(args[2],arg2); \
    fill_printf_arg(args[3],arg3); \
    printf_with_args(string,4,args); \
  }
#define printf5(string,arg0,arg1,arg2,arg3,arg4)  \
  { var printf_arg args[5]; \
    fill_printf_arg(args[0],arg0); \
    fill_printf_arg(args[1],arg1); \
    fill_printf_arg(args[2],arg2); \
    fill_printf_arg(args[3],arg3); \
    fill_printf_arg(args[4],arg4); \
    printf_with_args(string,5,args); \
  }
#define printf6(string,arg0,arg1,arg2,arg3,arg4,arg5)  \
  { var printf_arg args[6]; \
    fill_printf_arg(args[0],arg0); \
    fill_printf_arg(args[1],arg1); \
    fill_printf_arg(args[2],arg2); \
    fill_printf_arg(args[3],arg3); \
    fill_printf_arg(args[4],arg4); \
    fill_printf_arg(args[5],arg5); \
    printf_with_args(string,6,args); \
  }
#define printf7(string,arg0,arg1,arg2,arg3,arg4,arg5,arg6)  \
  { var printf_arg args[7]; \
    fill_printf_arg(args[0],arg0); \
    fill_printf_arg(args[1],arg1); \
    fill_printf_arg(args[2],arg2); \
    fill_printf_arg(args[3],arg3); \
    fill_printf_arg(args[4],arg4); \
    fill_printf_arg(args[5],arg5); \
    fill_printf_arg(args[6],arg6); \
    printf_with_args(string,7,args); \
  }

void print_file (const char* fname) {
  char buf[BUFSIZ];
  FILE* includefile = fopen(fname,"r");
  char* line;
  if (includefile == NULL) { perror(fname); exit(1); }
  while ((line = fgets(buf,BUFSIZ,includefile)) != NULL)
    fputs(line,stdout);
  if (ferror(includefile) || fclose(includefile)) { perror(fname); exit(1); }
}

static FILE *header_f = NULL, *test_f = NULL;
static unsigned int test_count = 0, typedef_count = 0;

void emit_typedef_test (const char *new_type) {
  fprintf(test_f,"  printf(\"sizeof(%s)=%%d\\n\",sizeof(%s));\n",
          new_type,new_type);
  test_count++;
}
void emit_typedef (const char* def, const char* new_type) {
  fprintf(header_f,"typedef %s %s;\n",def,new_type);
  typedef_count++;
  if (test_f) emit_typedef_test(new_type);
}

void emit_typedef_f (const char* format, const char* new_type) {
  fputs("typedef ",header_f);
  fprintf(header_f,format,new_type);
  fputs(";\n",header_f);
  test_count++;
  if (test_f) emit_typedef_test(new_type);
}

int main(int argc, char* argv[])
{
  char buf[BUFSIZ];

  header_f = stdout;
  if (argc == 2) {              /* open the test file and start it */
    test_f = fopen(argv[1],"w");
    if (test_f == NULL) { perror(argv[1]); exit(1); }
    fprintf(stderr,"writing test file %s\n",argv[1]);
    fprintf(test_f,"/* generated by %s on %s %s */\n"
            "#if USE_CLISP_H\n#include \"clisp.h\"\n#else\n"
            "#include \"lispbibl.c\"\n#endif\n#include <stdio.h>\n\n"
            "int main () {\n",
            __FILE__,__DATE__,__TIME__);
  }

  printf("#define SAFETY %d\n",SAFETY);
 #if defined(UNICODE)
  printf("#define CLISP_UNICODE 1\n");
 #else
  printf("#define CLISP_UNICODE 0\n");
 #endif
 #ifdef WIN32_NATIVE
  printf("#define WIN32_NATIVE\n");
 #endif
 #ifdef UNIX
  printf("#define UNIX\n");
 #endif
  /* assume unixconf.h and intparam.h are already included */
  printf("#define BIG_ENDIAN_P  %d%s\n",BIG_ENDIAN_P,ULsuffix);
#ifdef HAVE_SAVED_REGISTERS
  printf("#ifndef IN_MODULE_CC\n");
 #ifdef STACK_register
  printf("register long STACK_reg __asm__(\"%s\");\n",STACK_register);
 #endif
 #ifdef mv_count_register
  printf("register long mv_count_reg __asm__(\"%s\");\n",mv_count_register);
 #endif
 #ifdef value1_register
  printf("register long value1_reg __asm__(\"%s\");\n",value1_register);
 #endif
 #ifdef back_trace_register
  printf("register long back_trace_reg __asm__(\"%s\");\n",back_trace_register);
 #endif
  printf("struct registers { ");
 #ifdef STACK_register
  printf("long STACK_register_contents; ");
 #endif
 #ifdef mv_count_register
  printf("long mv_count_register_contents; ");
 #endif
 #ifdef value1_register
  printf("long value1_register_contents; ");
 #endif
 #ifdef back_trace_register
  printf("long back_trace_register_contents; ");
 #endif
  printf("};\n");
  printf("extern struct registers * callback_saved_registers;\n");
  printf("#endif\n");
#endif
  printf("#if !defined(__GNUC__) && !defined(inline)\n");
  printf("#define inline\n");
  printf("#endif\n");
  printf("#ifdef __cplusplus\n");
  printf("#define BEGIN_DECLS  extern \"C\" {\n");
  printf("#define END_DECLS    }\n");
  printf("#else\n");
  printf("#define BEGIN_DECLS\n");
  printf("#define END_DECLS\n");
  printf("#endif\n");
  printf("#define CONCAT_(xxx,yyy)  xxx##yyy\n");
  printf("#define CONCAT3_(aaa,bbb,ccc)  aaa##bbb##ccc\n");
#if notused
  printf("#define CONCAT4_(aaa,bbb,ccc,ddd)  aaa##bbb##ccc##ddd\n");
  printf("#define CONCAT5_(aaa,bbb,ccc,ddd,eee)  aaa##bbb##ccc##ddd##eee\n");
#endif
  printf("#define CONCAT(xxx,yyy)  CONCAT_(xxx,yyy)\n");
  printf("#define CONCAT3(aaa,bbb,ccc)  CONCAT3_(aaa,bbb,ccc)\n");
#if notused
  printf("#define CONCAT4(aaa,bbb,ccc,ddd)  CONCAT4_(aaa,bbb,ccc,ddd)\n");
  printf("#define CONCAT5(aaa,bbb,ccc,ddd,eee)  CONCAT5_(aaa,bbb,ccc,ddd,eee)\n");
#endif
  printf("#define STRING(token) #token\n");
  printf("#define STRINGIFY(token) STRING(token)\n");
#if defined(GNU) && !defined(__APPLE_CC__)
 #if (__GNUC__ >= 3) || ((__GNUC__ == 2) && (__GNUC_MINOR__ >= 7))
  printf("#define nonreturning_function(storclass,funname,arguments)  \\\n");
  printf("  storclass void __attribute__((__noreturn__)) funname arguments\n");
 #else
  printf("#define nonreturning_function(storclass,funname,arguments)  \\\n");
  printf("  storclass void funname arguments\n");
 #endif
#elif defined(MICROSOFT)
  printf("#define nonreturning_function(storclass,funname,arguments)  \\\n");
  printf("  __declspec(noreturn) storclass void funname arguments\n");
#else
  printf("#define nonreturning_function(storclass,funname,arguments)  \\\n");
  printf("  storclass void funname arguments\n");
#endif
  printf("#define var\n");
  printf("#define NOTREACHED  fehler_notreached(__FILE__,__LINE__)\n");
  printf("#define ASSERT(expr)  do { if (!(expr)) NOTREACHED; } while(0)\n");
#ifdef GNU
  printf("#define alloca  __builtin_alloca\n");
#elif defined(MICROSOFT)
  printf("#include <malloc.h>\n");
  printf("#define alloca _alloca\n");
#elif defined(HAVE_ALLOCA_H)
  printf("#include <alloca.h>\n");
 #ifndef alloca
  #if !(defined(UNIX_OSF) || defined(UNIX_DEC_ULTRIX))
   printf("extern void* alloca (int size);\n");
  #endif
 #endif
#elif defined(_AIX)
  printf("#pragma alloca\n");
#elif !defined(NO_ALLOCA)
  printf("extern void* alloca (int size);\n");
#endif
#ifdef __CHAR_UNSIGNED__
  emit_typedef("signed char","SBYTE");
#else
  emit_typedef("char","SBYTE");
#endif
  emit_typedef("unsigned char","UBYTE");
  emit_typedef("short","SWORD");
  emit_typedef("unsigned short","UWORD");
#if (long_bitsize==32)
  emit_typedef("long","SLONG");
  emit_typedef("unsigned long","ULONG");
#elif (int_bitsize==32)
  emit_typedef("int","SLONG");
  emit_typedef("unsigned int","ULONG");
#endif
#if (long_bitsize==64)
  emit_typedef("long","SLONGLONG");
  emit_typedef("unsigned long","ULONGLONG");
#elif defined(MICROSOFT)
  /* lispbibl.d defines HAVE_LONGLONG for MICROSOFT,
     so MICROSOFT has to come before HAVE_LONGLONG here */
  emit_typedef("__int64","SLONGLONG");
  emit_typedef("unsigned __int64","ULONGLONG");
#elif defined(HAVE_LONGLONG)
  emit_typedef("long long","SLONGLONG");
  emit_typedef("unsigned long long","ULONGLONG");
#endif
#ifdef HAVE_STDBOOL_H
  printf("#include <stdbool.h>\n");
#else
  print_file("stdbool.h");
#endif
  printf("#undef NULL\n");
#ifdef __cplusplus
  printf("#define NULL  0\n");
#else
  printf("#define NULL  ((void*) 0L)\n");
#endif
#if defined(GNU)
  printf("#define unspecified 0\n");
#else
  printf("#define unspecified 1\n");
#endif
#if !(defined(GNU) || (pointer_bitsize > 32))
  printf("#define pointerplus(pointer,offset)  ((void*)((ULONG)(pointer)+(offset)))\n");
#else
  printf("#define pointerplus(pointer,offset)  ((UBYTE*)(pointer)+(offset))\n");
#endif
  printf("#define bit(n)  (1%s<<(n))\n",Lsuffix);
#if notused
  printf("#define bitm(n)  (2%s<<((n)-1))\n",Lsuffix);
#endif
#if !defined(SPARC)
  printf("#define bit_test(x,n)  ((x) & bit(n))\n");
#else
 #if !defined(GNU)
  printf("#define bit_test(x,n)  ((n)<12 ? ((x) & bit(n)) : ((sint32)((uint32)(x) << (31-(n))) < 0))\n");
 #else
  printf("#define bit_test(x,n)  ((((n)<12) && ((x) & bit(n))) || (((n)>=12) && ((sint32)((uint32)(x) << (31-(n))) < 0)))\n");
 #endif
#endif
  printf("#define minus_bit(n)  (-1%s<<(n))\n",Lsuffix);
#if notused
  printf("#define minus_bitm(n)  (-2%s<<((n)-1))\n",Lsuffix);
  printf("#define floor(a_from_floor,b_from_floor)  ((a_from_floor) / (b_from_floor))\n");
  printf("#define ceiling(a_from_ceiling,b_from_ceiling)  (((a_from_ceiling) + (b_from_ceiling) - 1) / (b_from_ceiling))\n");
  printf("#define round_down(a_from_round,b_from_round)  (floor(a_from_round,b_from_round)*(b_from_round))\n");
  printf("#define round_up(a_from_round,b_from_round)  (ceiling(a_from_round,b_from_round)*(b_from_round))\n");
#endif
 #if defined(GNU)
   #ifdef DECALPHA
     printf("#define DYNAMIC_ARRAY(arrayvar,arrayeltype,arraysize)  arrayeltype arrayvar[(arraysize)+1]\n");
   #else
     printf("#define DYNAMIC_ARRAY(arrayvar,arrayeltype,arraysize)  arrayeltype arrayvar[arraysize]\n");
   #endif
   printf("#define FREE_DYNAMIC_ARRAY(arrayvar)\n");
 #elif (defined(UNIX) && (defined(HAVE_ALLOCA_H) || defined(_AIX) || !defined(NO_ALLOCA))) || defined(MICROSOFT)
   printf("#define DYNAMIC_ARRAY(arrayvar,arrayeltype,arraysize)  arrayeltype* arrayvar = (arrayeltype*)alloca((arraysize)*sizeof(arrayeltype))\n");
   printf("#define FREE_DYNAMIC_ARRAY(arrayvar)\n");
 #else
   printf("#include <stdlib.h>\n");
   printf("extern void* malloca (size_t size);\n");
   printf("extern void freea (void* ptr);\n");
   printf("#define DYNAMIC_ARRAY(arrayvar,arrayeltype,arraysize)  arrayeltype* arrayvar = (arrayeltype*)malloca((arraysize)*sizeof(arrayeltype))\n");
   printf("#define FREE_DYNAMIC_ARRAY(arrayvar)  freea(arrayvar)\n");
 #endif
  { int i;
    for (i=1; i<=8; i++) {
      sprintf(buf,"uint%d",i); emit_typedef("UBYTE",buf);
      sprintf(buf,"sint%d",i); emit_typedef("SBYTE",buf);
    }
    for (i=9; i<=16; i++) {
      sprintf(buf,"uint%d",i); emit_typedef("UWORD",buf);
      sprintf(buf,"sint%d",i); emit_typedef("SWORD",buf);
    }
    for (i=17; i<=32; i++) {
      sprintf(buf,"uint%d",i); emit_typedef("ULONG",buf);
      sprintf(buf,"sint%d",i); emit_typedef("SLONG",buf);
    }
   #ifdef HAVE_LONGLONG
    for (i=33; i<=64; i++)
      if ((i==33) || (i==48) || (i==64)) {
        sprintf(buf,"uint%d",i); emit_typedef("ULONGLONG",buf);
        sprintf(buf,"sint%d",i); emit_typedef("SLONGLONG",buf);
      }
   #endif
  }
  sprintf(buf,"sint%d",intBsize); emit_typedef(buf,"sintB");
  sprintf(buf,"uint%d",intBsize); emit_typedef(buf,"uintB");
#if notused
  sprintf(buf,"sint%d",intWsize); emit_typedef(buf,"sintW");
#endif
  sprintf(buf,"uint%d",intWsize); emit_typedef(buf,"uintW");
  sprintf(buf,"sint%d",intLsize); emit_typedef(buf,"sintL");
  sprintf(buf,"uint%d",intLsize); emit_typedef(buf,"uintL");
#if notused
  #ifdef intQsize
  sprintf(buf,"sint%d",intQsize); emit_typedef(buf,"sintQ");
  sprintf(buf,"uint%d",intQsize); emit_typedef(buf,"uintQ");
  #else
  emit_typedef("struct { sintL hi; uintL lo; }","sintL2");
  emit_typedef("struct { uintL hi; uintL lo; }","uintL2");
  #endif
#endif
  sprintf(buf,"sint%d",pointer_bitsize); emit_typedef(buf,"sintP");
  sprintf(buf,"uint%d",pointer_bitsize); emit_typedef(buf,"uintP");
#if notused
  sprintf(buf,"sint%d",intBWsize); emit_typedef(buf,"sintBW");
  sprintf(buf,"uint%d",intBWsize); emit_typedef(buf,"uintBW");
  sprintf(buf,"sint%d",intWLsize); emit_typedef(buf,"sintWL");
#endif
  sprintf(buf,"uint%d",intWLsize); emit_typedef(buf,"uintWL");
#if notused
  sprintf(buf,"sint%d",intBWLsize); emit_typedef(buf,"sintBWL");
#endif
  sprintf(buf,"uint%d",intBWLsize); emit_typedef(buf,"uintBWL");
  printf("#define uintC uintWL\n");
#if notused
  printf("#define sintC sintWL\n");
  sprintf(buf,"sint%d",intDsize); emit_typedef(buf,"sintD");
#endif
  sprintf(buf,"uint%d",intDsize); emit_typedef(buf,"uintD");
  printf("#include <stdlib.h>\n");
#if notused
 #ifdef WIDE_HARD
   printf("#define WIDE_HARD\n");
 #endif
 #ifdef WIDE_SOFT
   printf("#define WIDE_SOFT\n");
 #endif
 #ifdef WIDE_AUXI
   printf("#define WIDE_AUXI\n");
 #endif
 #ifdef WIDE
   printf("#define WIDE\n");
 #endif
#endif
  var const char* attribute_aligned_object = "";
#if defined(WIDE_AUXI) || defined(OBJECT_STRUCT) || defined(WIDE_STRUCT)
 #if defined(WIDE) && !defined(WIDE_HARD)
  #ifdef GENERATIONAL_GC
  attribute_aligned_object = " __attribute__ ((aligned(8)))";
  #endif
 #endif
#endif
#if !defined(WIDE_SOFT)
 #if defined(WIDE_AUXI)
  strcpy(buf,"struct { union { struct { ");
  #if BIG_ENDIAN_P
  strcat(buf,"uintP auxi_ob; uintP one_ob;");
  #else
  strcat(buf,"uintP one_ob; uintP auxi_ob;");
  #endif
  strcat(buf," } both; oint align_o");
  strcat(buf,attribute_aligned_object);
  strcat(buf,"; } u");
  strcat(buf,attribute_aligned_object);
  strcat(buf,"; }");
  emit_typedef(buf,"gcv_object_t");
  printf("#define one_o  u.both.one_ob\n");
  printf("#define auxi_o  u.both.auxi_ob\n");
 #elif defined(OBJECT_STRUCT)
  #ifdef DEBUG_GCSAFETY
  printf("struct object { uintP one_o; uintL allocstamp; };\n");
  printf("struct gcv_object_t { uintP one_o; operator object () const; gcv_object_t (object obj); gcv_object_t (struct fake_gcv_object obj); gcv_object_t (); };\n");
  #else
  emit_typedef("struct { uintP one_o; }","gcv_object_t");
  #endif
 #else
  emit_typedef("void *","gcv_object_t");
 #endif
 #ifdef WIDE_AUXI
  emit_typedef("uint64","oint");
  emit_typedef("sint64","soint");
 #else
  emit_typedef("uintP","oint");
  emit_typedef("sintP","soint");
 #endif
#else
  emit_typedef("uint64","oint");
  emit_typedef("sint64","soint");
 #ifdef WIDE_STRUCT
  strcpy(buf,"struct { union {\n");
  #if BIG_ENDIAN_P==WIDE_ENDIANNESS
  strcat(buf,"  struct { /*tint*/ uintL type; /*aint*/ uintL addr; } both;\n");
  #else
  strcat(buf,"  struct { /*aint*/ uintL addr; /*tint*/ uintL type; } both;\n");
  #endif
  strcat(buf,"  oint one_u");
  strcat(buf,attribute_aligned_object);
  strcat(buf,"; } u");
  strcat(buf,attribute_aligned_object);
  strcat(buf,"; }");
  emit_typedef(buf,"gcv_object_t");
  printf("#define one_o  u.one_u\n");
 #else
  emit_typedef("oint","gcv_object_t");
 #endif
#endif
#if defined(WIDE_STRUCT) || defined(OBJECT_STRUCT)
  printf("#define as_oint(expr)  ((expr).one_o)\n");
 #if defined(WIDE_STRUCT)
  printf("#define as_object(o)  ((object){u:{one_u:(o)}");
  #ifdef DEBUG_GCSAFETY
  printf(",allocstamp:alloccount");
  #endif
  printf("})\n");
 #elif defined(OBJECT_STRUCT)
  printf("#define as_object(o)  ((object){one_o:(o)");
  #ifdef DEBUG_GCSAFETY
  printf(",allocstamp:alloccount");
  #endif
  printf("})\n");
 #endif
#elif defined(WIDE_AUXI)
  printf("#define as_oint(expr)  ((expr).u.align_o)\n");
  printf("#define as_object_with_auxi(o)  ((object){u:{both:{ one_ob: (o), auxi_ob: 0 }}");
 #ifdef DEBUG_GCSAFETY
  printf(",allocstamp:alloccount");
 #endif
  printf("})\n");
  printf("#define as_object(o)  ((object){u:{align_o:(o)}");
 #ifdef DEBUG_GCSAFETY
  printf(",allocstamp:alloccount");
 #endif
  printf("})\n");
#else
  printf("#define as_oint(expr)  (oint)(expr)\n");
  printf("#define as_object(o)  (gcv_object_t)(o)\n");
#endif
#if notused
  printf1("#define addressbus_mask  %x\n",(oint)addressbus_mask);
  printf("#define oint_type_shift  %d\n",oint_type_shift);
  printf("#define oint_type_len  %d\n",oint_type_len);
  printf1("#define oint_type_mask  %x\n",(oint)oint_type_mask);
  printf("#define oint_addr_shift  %d\n",oint_addr_shift);
  printf("#define oint_addr_len  %d\n",oint_addr_len);
  printf1("#define oint_addr_mask  %x\n",(oint)oint_addr_mask);
  printf("#define oint_data_shift  %d\n",oint_data_shift);
  printf("#define oint_data_len  %d\n",oint_data_len);
  printf1("#define oint_data_mask  %x\n",(oint)oint_data_mask);
  printf("#define addr_shift  %d\n",addr_shift);
#endif
  sprintf(buf,"uint%d",oint_type_len); emit_typedef(buf,"tint");
  sprintf(buf,"uint%d",oint_addr_len); emit_typedef(buf,"aint");
#if notused
  sprintf(buf,"sint%d",oint_addr_len); emit_typedef(buf,"saint");
#endif
#ifdef DEBUG_GCSAFETY
  printf("extern uintL alloccount;\n");
#else
  emit_typedef("gcv_object_t","object");
#endif
#if notused
  printf1("#define tint_type_mask  %x\n",(tint)tint_type_mask);
#endif
#if !(defined(WIDE_SOFT) || defined(WIDE_AUXI) || defined(OBJECT_STRUCT))
  printf("#define objectplus(obj,offset)  ((object)pointerplus(obj,offset))\n");
#elif defined(WIDE_AUXI)
  printf("static inline object objectplus (object obj, saint offset) { return (object){u:{both:{ one_ob: obj.one_o+offset, auxi_ob: obj.auxi_o }}}; }\n");
#else
  printf("#define objectplus(obj,offset)  as_object(as_oint(obj)+(soint)(offset))\n");
#endif
#if !(defined(WIDE_SOFT) || defined(WIDE_AUXI))
#if notused
  printf("#define wbit  bit\n");
  printf("#define wbitm  bitm\n");
#endif
  printf("#define wbit_test  bit_test\n");
  printf("#define minus_wbit  minus_bit\n");
#else
  printf("#define wbit(n)  (1LL<<(n))\n");
#if notused
  printf("#define wbitm(n)  (2LL<<((n)-1))\n");
#endif
  printf("#define wbit_test(x,n)  ((x) & wbit(n))\n");
  printf("#define minus_wbit(n)  (-1LL<<(n))\n");
#endif
#ifdef TYPECODES
 #if !(exact_uint_size_p(oint_type_len) && (tint_type_mask == bit(oint_type_len)-1))
  printf2("#define typecode(expr)  ((tint)(as_oint(expr) >> %d) & %x)\n",oint_type_shift,(oint)(oint_type_mask >> oint_type_shift));
  printf("#define mtypecode(expr)  typecode(expr)\n");
 #else
 #if defined(MC68000) && defined(GNU) && !defined(NO_ASM) && (oint_type_shift==24) && (oint_type_len==8)
  printf("#define typecode(expr)  ({var tint __typecode; __asm__ (\"roll #8,%%0\" : \"=d\" (__typecode) : \"0\" (as_oint(expr)) ); __typecode; })\n");
 #elif defined(SPARC) && !defined(WIDE)
  printf("#define typecode(expr)  ((as_oint(expr) << %d) >> %d)\n",
         32-oint_type_len-oint_type_shift,32-oint_type_len);
 #elif defined(WIDE) && defined(WIDE_STRUCT)
  printf("#define typecode(expr)  ((expr).u.both.type)\n");
 #else
  printf("#define typecode(expr)  ((tint)(as_oint(expr) >> %d))\n",
         oint_type_shift);
 #endif
 #ifdef fast_mtypecode
  #ifndef WIDE
  printf("#define mtypecode(expr)  (*(tint*)&(expr)+%d)\n",
         3*((oint_type_shift==0)==BIG_ENDIAN_P));
  #endif
  #ifdef WIDE
   #ifdef WIDE_STRUCT
  printf("#define mtypecode(expr)  ((expr).u.both.type)\n");
   #elif (oint_type_len==16)
  printf("#define mtypecode(expr)  (*((tint*)&(expr)+%d))\n",
         3*((oint_type_shift==0)==BIG_ENDIAN_P));
   #elif (oint_type_len==32)
  printf("#define mtypecode(expr)  (*((tint*)&(expr)+%d))\n",
         ((oint_type_shift==0)==BIG_ENDIAN_P));
   #endif
  #endif
 #else
  printf("#define mtypecode(expr)  typecode(expr)\n");
 #endif
#endif
#if notused
 #if defined(WIDE) && defined(WIDE_STRUCT)
  printf("#define untype(expr)  ((expr).u.both.addr)\n");
 #elif !(defined(SPARC) && (oint_addr_len+oint_addr_shift<32))
  printf2("#define untype(expr)  ((aint)(as_oint(expr) >> %d) & %x)\n",
          oint_addr_shift,(oint)(oint_addr_mask >> oint_addr_shift));
 #else
  printf("#define untype(expr)  ((aint)((as_oint(expr) << %d) >> %d))\n",
         32-oint_addr_len-oint_addr_shift,32-oint_addr_len);
 #endif
#endif
#if defined(WIDE) && defined(WIDE_STRUCT)
 #if BIG_ENDIAN_P==WIDE_ENDIANNESS
  printf("#define type_untype_object(type,address)  ((object){{(tint)(type),(aint)(address)}");
 #else
  printf("#define type_untype_object(type,address)  ((object){{(aint)(address),(tint)(type)}");
 #endif
 #ifdef DEBUG_GCSAFETY
  printf(",allocstamp:alloccount");
 #endif
  printf("})\n");
#elif !(oint_addr_shift==0)
  printf("#define type_untype_object(type,address)  (as_object(((oint)(tint)(type) << %d) + ((oint)(aint)(address) << %d)))\n",oint_type_shift,oint_addr_shift);
#else
 #if defined(WIDE_SOFT)
  printf("#define type_untype_object(type,address)  objectplus((oint)(aint)(address),(oint)(tint)(type)<<%d)\n",oint_type_shift);
 #elif defined(WIDE_AUXI)
  printf("#define type_untype_object(type,address)  as_object_with_auxi((aint)pointerplus((address),(aint)(tint)(type)<<%d))\n",oint_type_shift);
 #elif defined(OBJECT_STRUCT)
  printf("#define type_untype_object(type,address)  as_object((oint)pointerplus((address),(oint)(tint)(type)<<%d))\n",oint_type_shift);
 #else
  printf("#define type_untype_object(type,address)  as_object(pointerplus((address),(oint)(tint)(type)<<%d))\n",oint_type_shift);
 #endif
#endif
#if defined(WIDE) && defined(WIDE_STRUCT)
 #if BIG_ENDIAN_P==WIDE_ENDIANNESS
  printf("#define type_data_object(type,address)  ((object){{(tint)(type),(aint)(address)}");
 #else
  printf("#define type_data_object(type,address)  ((object){{(aint)(address),(tint)(type)}");
 #endif
 #ifdef DEBUG_GCSAFETY
  printf(",allocstamp:alloccount");
 #endif
  printf("})\n");
#elif !(oint_addr_shift==0)
  printf("#define type_data_object(type,data)  (as_object(((oint)(tint)(type) << %d) + ((oint)(aint)(data) << %d)))\n",oint_type_shift,oint_addr_shift);
#else
  printf("#define type_data_object(type,data)  (as_object(((oint)(tint)(type) << %d) + (oint)(aint)(data)))\n",oint_type_shift);
#endif
#if (addr_shift==0)
  printf("#define upointer  untype\n");
#else
  printf("#define optimized_upointer(obj)  ((aint)((as_oint(obj) << %d) >> %d))\n",32-oint_addr_len-oint_addr_shift,32-oint_addr_len-addr_shift);
  printf("#define upointer(obj)  (untype(obj)<<%d)\n",addr_shift);
#endif
#if (addr_shift==0)
  printf("#define type_pointer_object(type,address)  type_untype_object(type,address)\n");
#elif defined(WIDE_SOFT) && !defined(WIDE_STRUCT)
  printf("#define type_pointer_object(type,address)    type_untype_object(type,(aint)(address)>>%d)\n",addr_shift);
#else
  printf("#define type_pointer_object(type,address)  (as_object(((oint)(tint)(type) << %d) + ((oint)(aint)(address) << %d)))\n",oint_type_shift,oint_addr_shift-addr_shift);
#endif
  printf("#define type_constpointer_object(type,address)  type_pointer_object(type,address)\n");
#if defined(WIDE_SOFT) && defined(WIDE_STRUCT)
  printf("#define type_zero_oint(type)  as_oint(type_untype_object(type,0))\n");
#else
  printf("#define type_zero_oint(type)  ((oint)(tint)(type) << %d)\n",oint_type_shift);
#endif
#else
  printf("#define type_data_object(type,data)  (as_object(((oint)(tint)(type) << %d) + ((oint)(aint)(data) << %d)))\n",oint_type_shift,oint_data_shift);
  printf("#define type_zero_oint(type)  ((oint)(tint)(type) << %d)\n",oint_type_shift);
  printf("#define immediate_object_p(obj)  ((7 & ~as_oint(obj)) == 0)\n");
  printf("#define gcinvariant_object_p(obj)  (((as_oint(obj) & 1) == 0) || immediate_object_p(obj))\n");
  printf("#define gcinvariant_bias_p(bias)  ((((bias) & 1) == 0) || ((7 & ~(bias)) == 0))\n");
#endif
#ifdef DEBUG_GCSAFETY
  printf("static inline bool gcinvariant_symbol_p (object obj);\n");
  printf("inline gcv_object_t::operator object () const { return (object){ one_o: one_o, allocstamp: alloccount }; }\n");
  printf("inline gcv_object_t::gcv_object_t (object obj) { if (!(gcinvariant_object_p(obj) || gcinvariant_symbol_p(obj) || obj.allocstamp == alloccount)) abort(); one_o = as_oint(obj); }\n");
  printf("inline gcv_object_t::gcv_object_t () {}\n");
#endif
#ifdef TYPECODES
  printf("#define VAROBJECT_HEADER  object GCself;\n");
#else
  printf("#define VAROBJECT_HEADER  object GCself; uintL tfl;\n");
#endif
#ifndef TYPECODES
  printf("#define varobject_type(ptr) ((sintB)((ptr)->tfl & 0xFF))\n");
#endif
#ifdef TYPECODES
  sprintf(buf,"struct { VAROBJECT_HEADER uintB recflags; sintB rectype; uintW recfiller; gcv_object_t recdata[unspecified]%s; }",attribute_aligned_object);
#else
  sprintf(buf,"struct { VAROBJECT_HEADER gcv_object_t recdata[unspecified]%s; }",attribute_aligned_object);
#endif
  emit_typedef(buf,"record_");
  emit_typedef("record_ *","Record");
#ifdef TYPECODES
  printf("#define record_type(ptr)  ((ptr)->rectype)\n");
#else
  printf("#define record_type(ptr)  varobject_type(ptr)\n");
#endif
  printf("#define Record_type(obj)  record_type(TheRecord(obj))\n");
#ifdef TYPECODES
  printf("#define record_flags(ptr)  ((ptr)->recflags)\n");
#else
  printf("#define record_flags(ptr)  (((ptr)->tfl >> 8) & 0xFF)\n");
#endif
  printf("#define Record_flags(obj)  record_flags(TheRecord(obj))\n");
#ifdef TYPECODES
  printf("#define LRECORD_HEADER  VAROBJECT_HEADER uintL length;\n");
#else
  printf("#define LRECORD_HEADER  VAROBJECT_HEADER\n");
#endif
  emit_typedef("struct { LRECORD_HEADER }","lrecord_");
  emit_typedef("lrecord_ *","Lrecord");
#ifdef TYPECODES
  printf("#define SRECORD_HEADER  VAROBJECT_HEADER uintB recflags; sintB rectype; uintW reclength;\n");
#else
  printf("#define SRECORD_HEADER  VAROBJECT_HEADER\n");
#endif
  sprintf(buf,"struct { SRECORD_HEADER object recdata[unspecified]%s; }",attribute_aligned_object);
  emit_typedef(buf,"srecord_");
  emit_typedef("srecord_ *","Srecord");
#ifdef TYPECODES
  printf("#define srecord_length(ptr)  ((ptr)->reclength)\n");
#else
  printf("#define srecord_length(ptr)  ((ptr)->tfl >> 16)\n");
#endif
#ifdef TYPECODES
  printf("#define XRECORD_HEADER  VAROBJECT_HEADER uintB recflags; sintB rectype; uintB reclength; uintB recxlength;\n");
#else
  printf("#define XRECORD_HEADER  VAROBJECT_HEADER\n");
#endif
#if notused
  sprintf(buf,"struct { XRECORD_HEADER gcv_object_t recdata[unspecified]%s; }",attribute_aligned_object);
  emit_typedef(buf,"xrecord_");
  emit_typedef("xrecord_ *","Xrecord");
#endif
  sprintf(buf,"struct { gcv_object_t cdr%s; gcv_object_t car%s; }",attribute_aligned_object,attribute_aligned_object);
  emit_typedef(buf,"cons_");
  emit_typedef("cons_ *","Cons");
#if notused
  #ifdef SPVW_MIXED
    sprintf(buf,"struct { XRECORD_HEADER gcv_object_t rt_num%s; gcv_object_t rt_den%s; }",attribute_aligned_object,attribute_aligned_object);
  #else
    sprintf(buf,"struct { gcv_object_t rt_num%s; gcv_object_t rt_den%s; }",attribute_aligned_object,attribute_aligned_object);
  #endif
  emit_typedef(buf,"ratio_");
  emit_typedef("ratio_ *","Ratio");
  #ifdef SPVW_MIXED
    sprintf(buf,"struct { XRECORD_HEADER gcv_object_t c_real%s; gcv_object_t c_imag%s; }",attribute_aligned_object,attribute_aligned_object);
  #else
    sprintf(buf,"struct { gcv_object_t c_real%s; gcv_object_t c_imag%s; }",attribute_aligned_object,attribute_aligned_object);
  #endif
  emit_typedef(buf,"complex_");
  emit_typedef("complex_ *","Complex");
#endif
  sprintf(buf,"struct { VAROBJECT_HEADER gcv_object_t symvalue%s; gcv_object_t symfunction%s; gcv_object_t proplist%s; gcv_object_t pname%s; gcv_object_t homepackage%s; }",attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object);
  emit_typedef(buf,"symbol_");
  emit_typedef("symbol_ *","Symbol");
  sprintf(buf,"uint%d",char_int_len); emit_typedef(buf,"cint");
  printf1("#define int_char(int_from_int_char)  type_data_object(%d,(aint)(cint)(int_from_int_char))\n",(tint)char_type);
#if !((oint_data_shift==0) && (char_int_len<=oint_data_len) && (exact_uint_size_p(char_int_len)))
 #ifdef TYPECODES
  printf("#define char_int(char_from_char_int)  ((cint)(untype(char_from_char_int)))\n");
 #else
  printf1("#define char_int(char_from_char_int)  ((cint)(as_oint(char_from_char_int)>>%d))\n",oint_data_shift);
 #endif
#else
  printf("#define char_int(char_from_char_int)  ((cint)as_oint(char_from_char_int))\n");
#endif
#ifdef CHART_STRUCT
  emit_typedef("struct { cint one; }","chart");
  printf("#define as_cint(ch)  ((ch).one)\n");
  printf("#define as_chart(c)  ((chart){one:(c)})\n");
#else
  emit_typedef("cint","chart");
  printf("#define as_cint(ch)  (ch)\n");
  printf("#define as_chart(c)  (c)\n");
#endif
  printf("#define code_char(ch)  int_char(as_cint(ch))\n");
  printf("#define char_code(obj)  as_chart(char_int(obj))\n");
  printf("#define fixnum(x)  type_data_object(%d,x)\n",fixnum_type);
  printf("#define Fixnum_0  fixnum(0)\n");
  printf("#define Fixnum_1  fixnum(1)\n");
  printf2("#define Fixnum_minus1  type_data_object(%d,%x)\n",(tint)(fixnum_type | bit(sign_bit_t)),(aint)(bitm(oint_data_len)-1));
#if !(defined(SPARC) && (oint_data_len+oint_data_shift<32))
  printf2("#define posfixnum_to_L(obj)  ((uintL)((as_oint(obj)&%x)>>%d))\n",(oint)wbitm(oint_data_len+oint_data_shift)-1,oint_data_shift);
#else
  printf("#define posfixnum_to_L(obj)  ((uintL)((as_oint(obj) << %d) >> %d))\n",32-oint_data_len-oint_data_shift,32-oint_data_len);
#endif
#if notused
  printf1("#define negfixnum_to_L(obj)  (posfixnum_to_L(obj) | %x)\n",(uintL)(-bitm(oint_data_len)));
#endif
#if (oint_data_len>=intLsize)
  printf("#define fixnum_to_L(obj)  (sintL)posfixnum_to_L(obj)\n");
#elif (sign_bit_o == oint_data_len+oint_data_shift)
  printf("#define fixnum_to_L(obj)  (((sintL)as_oint(obj) << %d) >> %d)\n",intLsize-1-sign_bit_o,intLsize-1-sign_bit_o+oint_data_shift);
#else
 #if !defined(SPARC)
  printf5("#define fixnum_to_L(obj)  (sintL)( ((((sintL)as_oint(obj) >> %d) << %d) >> %d) | ((uintL)((as_oint(obj) & %x) >> %d)) )\n",sign_bit_o,intLsize-1,intLsize-1-oint_data_len,(oint)wbitm(oint_data_len+oint_data_shift)-1,oint_data_shift);
 #else
  printf("#define fixnum_to_L(obj)  (sintL)( ((((sintL)as_oint(obj) >> %d) << %d) >> %d) | (((uintL)as_oint(obj) << %d) >> %d) )\n",sign_bit_o,intLsize-1,intLsize-1-oint_data_len,intLsize-oint_data_len-oint_data_shift,intLsize-oint_data_len);
 #endif
#endif
  printf("#define fixnum_inc(obj,delta)  objectplus(obj, (soint)(delta) << %d)\n",oint_data_shift);
  printf("#define posfixnum(x)  fixnum_inc(Fixnum_0,x)\n");
  printf("#define negfixnum(x)  fixnum_inc(fixnum_inc(Fixnum_minus1,1),x)\n");
  printf("#define sfixnum(x) ((x)>=0 ? posfixnum(x) : negfixnum(x))\n");
#ifdef TYPECODES
  emit_typedef("struct { VAROBJECT_HEADER uintC length; uintD data[unspecified]; }","bignum_");
#else
  emit_typedef("struct { VAROBJECT_HEADER uintD data[unspecified]; }","bignum_");
#endif
  emit_typedef("bignum_ *","Bignum");
#ifdef TYPECODES
  printf("#define bignum_length(ptr)  ((ptr)->length)\n");
#else
  printf("#define bignum_length(ptr)  srecord_length(ptr)\n");
#endif
  printf("#define Bignum_length(obj)  bignum_length(TheBignum(obj))\n");
  emit_typedef("uint32","ffloat");
  emit_typedef("union { ffloat eksplicit; }","ffloatjanus");
#ifdef intQsize
  emit_typedef("uint64","dfloat");
#else
 #if BIG_ENDIAN_P
  emit_typedef("struct {uint32 semhi,mlo;}","dfloat");
 #else
  emit_typedef("struct {uint32 mlo,semhi;}","dfloat");
 #endif
#endif
  emit_typedef("union { dfloat eksplicit; }","dfloatjanus");
#if notused
  emit_typedef("struct { LRECORD_HEADER }","sarray_");
  emit_typedef("sarray_ *","Sarray");
#endif
  emit_typedef("struct { LRECORD_HEADER uint8  data[unspecified]; }","sbvector_");
  emit_typedef("sbvector_ *","Sbvector");
#ifdef TYPECODES
  printf("#define SSTRING_HEADER  VAROBJECT_HEADER uintL tfl;\n");
#else
  printf("#define SSTRING_HEADER  VAROBJECT_HEADER\n");
#endif
  emit_typedef("struct { SSTRING_HEADER }","sstring_");
  emit_typedef("sstring_ *","Sstring");
  emit_typedef("struct { SSTRING_HEADER chart data[unspecified]; }","snstring_");
  sprintf(buf,"struct { LRECORD_HEADER gcv_object_t data[unspecified]%s; }",attribute_aligned_object);
  emit_typedef(buf,"svector_");
  emit_typedef("svector_ *","Svector");
#ifdef TYPECODES
  printf("#define lrecord_length(ptr)  ((ptr)->length)\n");
#else
  printf("#define lrecord_length(ptr)  ((ptr)->tfl >> 8)\n");
#endif
  printf("#define sarray_length(ptr)  lrecord_length(ptr)\n");
  printf("#define Sarray_length(obj)  sarray_length(TheSarray(obj))\n");
  printf("#define sbvector_length(ptr)  sarray_length(ptr)\n");
  printf("#define Sbvector_length(obj)  sbvector_length(TheSbvector(obj))\n");
#ifdef TYPECODES
  printf("#define sstring_length(ptr)  ((ptr)->tfl >> 6)\n");
#else
  printf("#define sstring_length(ptr)  ((ptr)->tfl >> 10)\n");
#endif
  printf("#define Sstring_length(obj)  sstring_length(TheSstring(obj))\n");
#ifdef TYPECODES
  printf("#define sstring_eltype(ptr)  (((ptr)->tfl >> 4) & 3)\n");
#else
  printf("#define sstring_eltype(ptr)  ((record_type(ptr) - %d) >> 1)\n",Rectype_S8string);
#endif
  printf("extern bool string_equal (object string1, object string2);\n");
  #ifdef TYPECODES
   printf("#define Array_type_simple_bit_vector(atype)  (%d+((atype)<<%d)",Array_type_sbvector,TB0);
   if (TB0+1 != TB1) printf("+((atype)&%d)",bit(TB0+1)-bit(TB1));
   if (TB1+1 != TB2) printf("+((atype)&%d)",bit(TB1+1)-bit(TB2));
   printf(")\n");
  #endif
#if notused
  sprintf(buf,"struct { XRECORD_HEADER gcv_object_t pack_external_symbols%s; gcv_object_t pack_internal_symbols%s; gcv_object_t pack_shadowing_symbols%s; gcv_object_t pack_use_list%s; gcv_object_t pack_used_by_list%s; gcv_object_t pack_name%s; gcv_object_t pack_nicknames%s; } *",attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object,attribute_aligned_object);
  emit_typedef(buf,"Package");
#endif
  emit_typedef("Srecord","Structure");
  printf("#define structure_types   recdata[0]\n");
  sprintf(buf,"struct { SRECORD_HEADER gcv_object_t inst_class_version%s; gcv_object_t other[unspecified]%s; } *",attribute_aligned_object,attribute_aligned_object);
  emit_typedef(buf,"Instance");
  printf("typedef void Values;\n"); /* emit_typedef useless: no sizeof(void) */
  emit_typedef_f("Values (*%s)()","lisp_function_t");
  sprintf(buf,"struct { lisp_function_t function; gcv_object_t name%s; gcv_object_t keywords%s; uintW argtype; uintW req_anz; uintW opt_anz; uintB rest_flag; uintB key_flag; uintW key_anz; uintW seclass; } %%s",attribute_aligned_object,attribute_aligned_object);
#if defined(NO_TYPECODES) && (alignment_long < 4) && defined(GNU)
  strcat(buf," __attribute__ ((aligned (4)))");
#endif
  emit_typedef_f(buf,"subr_t");
  emit_typedef("subr_t *","Subr");
  emit_typedef("enum { subr_norest, subr_rest }","subr_rest_t");
  emit_typedef("enum { subr_nokey, subr_key, subr_key_allow }","subr_key_t");
#ifdef TYPECODES
  printf1("#define make_machine(ptr)  type_pointer_object(%d,ptr)\n",(tint)machine_type);
#else
 #if defined(WIDE_AUXI)
  printf1("#define make_machine(ptr)  as_object_with_auxi((aint)(ptr)+%d)\n",machine_bias);
 #else
  printf1("#define make_machine(ptr)  as_object((oint)(ptr)+%d)\n",machine_bias);
 #endif
#endif
  printf3("#define make_system(data)  type_data_object(%d,%x | (%x & (data)))\n",(tint)system_type,(oint)(bit(oint_data_len-1) | bit(0)),(oint)(bitm(oint_data_len)-1));
  printf("#define unbound  make_system(0x%x)\n",0xFFFFFFUL);
  printf("#define nullobj  make_machine(0)\n");
#ifdef TYPECODES
 #if !((oint_addr_shift==0) && (addr_shift==0))
  printf("#define pointable(obj)  ((void*)upointer(obj))\n");
 #else
  #if !(((tint_type_mask<<oint_type_shift) & addressbus_mask) == 0)
  printf1("#define pointable(obj)  ((void*)((aint)as_oint(obj) & %x))\n",(aint)oint_addr_mask | ~addressbus_mask);
  #else
  printf("#define pointable(obj)  ((void*)as_oint(obj))\n");
  #endif
 #endif
 #ifdef DEBUG_GCSAFETY
  printf("static inline void* gcsafety_pointable (gcv_object_t obj) { return pointable(obj); }\n");
  printf("static inline void* gcsafety_pointable (object obj) { return pointable((gcv_object_t)obj); }\n");
  printf("#undef pointable\n");
  printf("#define pointable gcsafety_pointable\n");
 #endif
 #if defined(DEBUG_GCSAFETY)
  #define printf_type_pointable(type)  printf("pointable(obj)");
 #elif defined(WIDE_STRUCT)
  #define printf_type_pointable(type)  printf("((void*)((obj).u.both.addr))");
 #elif !((oint_addr_shift==0) && (addr_shift==0) && (((tint_type_mask<<oint_type_shift) & addressbus_mask) == 0))
  #if (addr_shift==0)
   #define printf_type_pointable(type)  \
     if ((oint_addr_shift==0) && ((type_zero_oint(type) & addressbus_mask) == 0)) \
       printf("((void*)(aint)as_oint(obj))");                           \
     else                                                               \
       printf("((void*)(aint)pointable(obj))");
  #elif !(addr_shift==0)
   #define printf_type_pointable(type)  \
    if (optimized_upointer(type_data_object(type,0)) == 0)      \
      printf("((void*)(aint)optimized_upointer(obj))");         \
    else                                                        \
      printf("((void*)(aint)pointable(obj))");
  #endif
 #else
  #define printf_type_pointable(type)  printf("((void*)(aint)as_oint(obj))");
 #endif
  printf("#define TheCons(obj)  ((Cons)("); printf_type_pointable(cons_type); printf("))\n");
#if notused
  printf("#define TheRatio(obj)  ((Ratio)("); printf_type_pointable(ratio_type|bit(sign_bit_t)); printf("))\n");
  printf("#define TheComplex(obj)  ((Complex)("); printf_type_pointable(complex_type); printf("))\n");
#endif
  printf("#define TheSymbol(obj)  ((Symbol)("); printf_type_pointable(symbol_type); printf("))\n");
  printf("#define TheBignum(obj)  ((Bignum)("); printf_type_pointable(bignum_type|bit(sign_bit_t)); printf("))\n");
#if notused
  printf("#define TheSarray(obj)  ((Sarray)("); printf_type_pointable(sbvector_type|sb2vector_type|sb4vector_type|sb8vector_type|sb16vector_type|sb32vector_type|sstring_type|svector_type); printf("))\n");
#endif
  printf("#define TheSbvector(obj)  ((Sbvector)("); printf_type_pointable(sbvector_type|sb2vector_type|sb4vector_type|sb8vector_type|sb16vector_type|sb32vector_type); printf("))\n");
  printf("#define TheSstring(obj)  ((Sstring)("); printf_type_pointable(sstring_type); printf("))\n");
  printf("#define TheSvector(obj)  ((Svector)("); printf_type_pointable(svector_type); printf("))\n");
  printf("#define TheRecord(obj)  ((Record)("); printf_type_pointable(closure_type|structure_type|stream_type|orecord_type|instance_type); printf("))\n");
  printf("#define TheSrecord(obj)  ((Srecord)("); printf_type_pointable(closure_type|structure_type|orecord_type|instance_type); printf("))\n");
#if notused
  printf("#define TheXrecord(obj)  ((Xrecord)("); printf_type_pointable(stream_type|orecord_type); printf("))\n");
  printf("#define ThePackage(obj)  ((Package)("); printf_type_pointable(orecord_type); printf("))\n");
#endif
  printf("#define TheStructure(obj)  ((Structure)("); printf_type_pointable(structure_type); printf("))\n");
  printf("#define TheInstance(obj)  ((Instance)("); printf_type_pointable(instance_type); printf("))\n");
  printf("#define TheSubr(obj)  ((Subr)("); printf_type_pointable(subr_type); printf("))\n");
#if notused
  printf("#define TheMachine(obj)  ((void*)("); printf_type_pointable(machine_type); printf("))\n");
#endif
#else
 #if defined(DEBUG_GCSAFETY)
  printf("static inline aint cgci_pointable (object obj) { return obj.one_o; }\n");
  printf("static inline aint cgci_pointable (gcv_object_t obj) { return obj.one_o; }\n");
  printf("static inline aint pgci_pointable (object obj) { if (!(gcinvariant_object_p(obj) || gcinvariant_symbol_p(obj) || obj.allocstamp == alloccount)) abort(); return obj.one_o; }\n");
  printf("static inline aint pgci_pointable (gcv_object_t obj) { return obj.one_o; }\n");
  printf("static inline aint ngci_pointable (object obj) { if (!(gcinvariant_symbol_p(obj) || obj.allocstamp == alloccount)) abort(); return obj.one_o; }\n");
  printf("static inline aint ngci_pointable (gcv_object_t obj) { return obj.one_o; }\n");
 #elif defined(WIDE_AUXI)
  printf("#define cgci_pointable(obj)  (obj).one_o\n");
  printf("#define pgci_pointable(obj)  (obj).one_o\n");
  printf("#define ngci_pointable(obj)  (obj).one_o\n");
 #else
  printf("#define cgci_pointable(obj)  as_oint(obj)\n");
  printf("#define pgci_pointable(obj)  as_oint(obj)\n");
  printf("#define ngci_pointable(obj)  as_oint(obj)\n");
 #endif
  printf1("#define TheCons(obj)  ((Cons)(ngci_pointable(obj)-%d))\n",cons_bias);
#if notused
  printf1("#define TheRatio(obj)  ((Ratio)(ngci_pointable(obj)-%d))\n",varobject_bias);
  printf1("#define TheComplex(obj)  ((Complex)(ngci_pointable(obj)-%d))\n",varobject_bias);
#endif
  printf1("#define TheSymbol(obj)  ((Symbol)(ngci_pointable(obj)-%d))\n",varobject_bias);
  printf1("#define TheBignum(obj)  ((Bignum)(ngci_pointable(obj)-%d))\n",varobject_bias);
#if notused
  printf1("#define TheSarray(obj)  ((Sarray)(ngci_pointable(obj)-%d))\n",varobject_bias);
#endif
  printf1("#define TheSbvector(obj)  ((Sbvector)(ngci_pointable(obj)-%d))\n",varobject_bias);
  printf1("#define TheSstring(obj)  ((Sstring)(ngci_pointable(obj)-%d))\n",varobject_bias);
  printf1("#define TheSvector(obj)  ((Svector)(ngci_pointable(obj)-%d))\n",varobject_bias);
  printf1("#define TheRecord(obj)  ((Record)(ngci_pointable(obj)-%d))\n",varobject_bias);
  printf1("#define TheSrecord(obj)  ((Srecord)(ngci_pointable(obj)-%d))\n",varobject_bias);
#if notused
  printf1("#define TheXrecord(obj)  ((Xrecord)(ngci_pointable(obj)-%d))\n",varobject_bias);
  printf1("#define ThePackage(obj)  ((Package)(ngci_pointable(obj)-%d))\n",varobject_bias);
#endif
  printf1("#define TheStructure(obj)  ((Structure)(ngci_pointable(obj)-%d))\n",varobject_bias);
  printf1("#define TheInstance(obj)  ((Instance)(ngci_pointable(obj)-%d))\n",varobject_bias);
  printf1("#define TheSubr(obj)  ((Subr)(cgci_pointable(obj)-%d))\n",subr_bias);
#if notused
  printf1("#define TheMachine(obj)  ((void*)(cgci_pointable(obj)-%d))\n",machine_bias);
#endif
#endif
  printf("#define Car(obj)  (TheCons(obj)->car)\n");
  printf("#define Cdr(obj)  (TheCons(obj)->cdr)\n");
  printf("#define Symbol_value(obj)  (TheSymbol(obj)->symvalue)\n");
  printf("#define Symbol_function(obj)  (TheSymbol(obj)->symfunction)\n");
  printf("#define Symbol_plist(obj)  (TheSymbol(obj)->proplist)\n");
  printf("#define Symbol_name(obj)  (TheSymbol(obj)->pname)\n");
  printf("#define Symbol_package(obj)  (TheSymbol(obj)->homepackage)\n");
#if defined(DEBUG_GCSAFETY)
  printf("#define eq(obj1,obj2)  (pgci_pointable(obj1) == pgci_pointable(obj2))\n");
#elif defined(WIDE_STRUCT) || defined(OBJECT_STRUCT)
  printf("#define eq(obj1,obj2)  (as_oint(obj1) == as_oint(obj2))\n");
#elif defined(WIDE_AUXI)
  printf("#define eq(obj1,obj2)  ((obj1).one_o == (obj2).one_o)\n");
#else
  printf("#define eq(obj1,obj2)  ((obj1) == (obj2))\n");
#endif
  printf("#define nullp(obj)  (eq(obj,NIL))\n");
  printf("#define boundp(obj) (!eq(obj,unbound))\n");
  printf("#define missingp(obj) (!boundp(obj) || nullp(obj))\n");
#ifdef TYPECODES
 #if defined(cons_bit_o)
  #ifdef fast_mtypecode
   #ifdef WIDE_STRUCT
  printf("#define consp(obj)  (typecode(obj) & bit(%d))\n",cons_bit_t);
  printf("#define atomp(obj)  ((typecode(obj) & bit(%d))==0)\n",cons_bit_t);
   #else
  printf("#define consp(obj)  (wbit_test(as_oint(obj),%d))\n",cons_bit_o);
  printf("#define atomp(obj)  (!wbit_test(as_oint(obj),%d))\n",cons_bit_o);
   #endif
  printf("#define mconsp(obj)  (mtypecode(obj) & bit(%d))\n",cons_bit_t);
  printf("#define matomp(obj)  ((mtypecode(obj) & bit(%d))==0)\n",cons_bit_t);
  #else
  printf("#define consp(obj)  (wbit_test(as_oint(obj),%d))\n",cons_bit_o);
  printf("#define atomp(obj)  (!wbit_test(as_oint(obj),%d))\n",cons_bit_o);
  printf("#define mconsp(obj)  consp(obj)\n");
  printf("#define matomp(obj)  atomp(obj)\n");
  #endif
 #else
  printf2("#define consp(obj)  (typecode(obj) == %d)\n",(tint)cons_type);
  printf2("#define atomp(obj)  (!(typecode(obj) == %d))\n",(tint)cons_type);
  printf2("#define mconsp(obj)  (mtypecode(obj) == %d)\n",(tint)cons_type);
  printf2("#define matomp(obj)  (!(mtypecode(obj) == %d))\n",(tint)cons_type);
 #endif
#else
  printf2("#define consp(obj)  ((as_oint(obj) & %d) == %d)\n",7,cons_bias);
  printf("#define mconsp(obj)  consp(obj)\n");
  printf("#define atomp(obj)  (!consp(obj))\n");
  printf("#define matomp(obj)  atomp(obj)\n");
#endif
  printf("#define listp(obj)  (nullp(obj) || consp(obj))\n");
#ifndef TYPECODES
  printf2("#define varobjectp(obj)  ((as_oint(obj) & %d) == %d)\n",3,varobject_bias);
#endif
#ifdef TYPECODES
 #if defined(symbol_bit_o)
  #ifdef WIDE_STRUCT
  printf("#define symbolp(obj)  (typecode(obj) & bit(%d))\n",symbol_bit_t);
  #else
  printf("#define symbolp(obj)  (wbit_test(as_oint(obj),%d))\n",symbol_bit_o);
  #endif
 #else
  printf1("#define symbolp(obj)  (typecode(obj) == %d)\n",(tint)symbol_type);
 #endif
#else
  printf1("#define symbolp(obj)  (varobjectp(obj) && (Record_type(obj) == %d))\n",Rectype_Symbol);
#endif
#ifdef case_stream
  printf("#define builtin_stream_p(obj) (typecode(obj)==%d)\n",stream_type);
#else
  printf("#define builtin_stream_p(obj) (orecordp(obj) && (Record_type(obj) == %d))\n",Rectype_Stream);
#endif
#if notused
#ifdef TYPECODES
 #ifdef WIDE_STRUCT
  printf("#define numberp(obj)  (typecode(obj) & bit(%d))\n",number_bit_t);
 #else
  printf("#define numberp(obj)  (wbit_test(as_oint(obj),%d))\n",number_bit_o);
 #endif
#else
 printf2("#define immediate_number_p(obj)  ((as_oint(obj) & %d) == %d)\n",(4 << imm_type_shift) | immediate_bias,(fixnum_type&sfloat_type));
#endif
#endif
#ifdef TYPECODES
  printf2("#define vectorp(obj)  ((tint)(typecode(obj) - %d) <= (tint)%d)\n",(tint)sbvector_type,(tint)(vector_type-sbvector_type));
#else
  printf2("#define vectorp(obj)  (varobjectp(obj) && ((uintB)(Record_type(obj) - %d) <= %d))\n",Rectype_vector,Rectype_string-Rectype_vector);
#endif
#ifdef TYPECODES
  printf1("#define simple_vector_p(obj)  (typecode(obj) == %d)\n",(tint)svector_type);
#else
  printf1("#define simple_vector_p(obj)  (varobjectp(obj) && (Record_type(obj) == %d))\n",Rectype_Svector);
#endif
#ifdef TYPECODES
  printf2("#define general_vector_p(obj)  ((typecode(obj) & ~%d) == %d)\n",(tint)bit(notsimple_bit_t),(tint)svector_type);
#else
  printf2("#define general_vector_p(obj)  (varobjectp(obj) && ((Record_type(obj) & ~%d) == %d))\n",Rectype_Svector^Rectype_vector,Rectype_Svector&Rectype_vector);
#endif
#ifdef TYPECODES
  printf1("#define simple_string_p(obj)  (typecode(obj) == %d)\n",(tint)sstring_type);
#else
  printf("#define simple_string_p(obj)  (varobjectp(obj) && ((uintB)(Record_type(obj) - %d) <= %d))\n",Rectype_S8string,Rectype_reallocstring - Rectype_S8string);
#endif
#ifdef TYPECODES
  printf2("#define stringp(obj)  ((typecode(obj) & ~%d) == %d)\n",(tint)bit(notsimple_bit_t),(tint)sstring_type);
#else
  printf("#define stringp(obj)  (varobjectp(obj) && ((uintB)(Record_type(obj) - %d) <= %d))\n",Rectype_S8string,Rectype_reallocstring - Rectype_S8string);
#endif
#ifdef TYPECODES
  printf("#define simple_bit_vector_p(atype,obj)  (typecode(obj) == Array_type_simple_bit_vector(atype))\n");
#else
  printf1("#define simple_bit_vector_p(atype,obj)  (varobjectp(obj) && (Record_type(obj) == %d+(atype)))\n",Rectype_Sbvector);
#endif
#ifdef TYPECODES
  printf1("#define bit_vector_p(atype,obj)  ((typecode(obj) & ~%d) == Array_type_simple_bit_vector(atype))\n",(tint)bit(notsimple_bit_t));
#else
  printf2("#define bit_vector_p(atype,obj)  (varobjectp(obj) && ((Record_type(obj) & ~%d) == %d+(atype)))\n",Rectype_Sbvector^Rectype_bvector,Rectype_Sbvector&Rectype_bvector);
#endif
#ifdef TYPECODES
  printf2("#define arrayp(obj)  ((tint)(typecode(obj) - %d) <= (tint)%d)\n",(tint)mdarray_type,(tint)(vector_type-mdarray_type));
#else
  printf2("#define arrayp(obj)  (varobjectp(obj) && ((uintB)(Record_type(obj)-%d) <= %d))\n",Rectype_vector,Rectype_mdarray-Rectype_vector);
#endif
  printf("extern object array_displace_check (object array, uintL size, uintL* index);\n");
  printf("extern uintL vector_length (object vector);\n");
  printf("#define simple_nilarray_p(obj)  nullp(obj)\n");
  printf("nonreturning_function(extern, fehler_nilarray_retrieve, (void));\n");
#ifdef TYPECODES
  printf1("#define instancep(obj)  (typecode(obj)==%d)\n",(tint)instance_type);
#else
  printf1("#define instancep(obj)  (varobjectp(obj) && (Record_type(obj) == %d))\n",Rectype_Instance);
#endif
#ifdef TYPECODES
  printf1("#define orecordp(obj)  (typecode(obj)==%d)\n",(tint)orecord_type);
#else
  printf("#define orecordp(obj)  varobjectp(obj)\n");
#endif
#ifdef case_structure
  printf1("#define structurep(obj)  (typecode(obj)==%d)\n",(tint)structure_type);
#else
  printf("#define structurep(obj)  (orecordp(obj) && (Record_type(obj) == %d))\n",Rectype_Structure);
#endif
#if notused
  printf("#define packagep(obj)  (orecordp(obj) && (Record_type(obj) == %d))\n",Rectype_Package);
#endif
#ifdef TYPECODES
  printf("#define charp(obj)  (typecode(obj)==%d)\n",(tint)char_type);
#else
  printf("#define charp(obj)  ((as_oint(obj) & %d) == %d)\n",(7 << imm_type_shift) | immediate_bias,char_type);
#endif
#ifdef TYPECODES
  printf2("#define integerp(obj)  ((typecode(obj) & ~%d) == %d)\n",(tint)((fixnum_type|bignum_type|bit(sign_bit_t)) & ~(fixnum_type&bignum_type)),(tint)(fixnum_type&bignum_type));
#else
  printf3("#define integerp(obj)  (((as_oint(obj) & %d) == %d) || (varobjectp(obj) && (Record_type(obj) == %d)))\n",(6 << imm_type_shift) | immediate_bias,fixnum_type,Rectype_Bignum);
#endif
#ifdef TYPECODES
  printf2("#define fixnump(obj)  ((typecode(obj) & ~%d) == %d)\n",(tint)bit(sign_bit_t),(tint)fixnum_type);
#else
  printf2("#define fixnump(obj)  ((as_oint(obj) & %d) == %d)\n",(6 << imm_type_shift) | immediate_bias,fixnum_type);
#endif
#ifdef TYPECODES
  printf1("#define posfixnump(obj)  (typecode(obj) == %d)\n",(tint)fixnum_type);
#else
  printf2("#define posfixnump(obj)  ((as_oint(obj) & %d) == %d)\n",(7 << imm_type_shift) | immediate_bias,fixnum_type);
#endif
#ifdef TYPECODES
  printf2("#define bignump(obj)  ((typecode(obj) & ~%d) == %d)\n",(tint)bit(sign_bit_t),(tint)bignum_type);
#else
  printf1("#define bignump(obj)  (varobjectp(obj) && (Record_type(obj) == %d))\n",Rectype_Bignum);
#endif
#ifdef TYPECODES
  printf1("#define posbignump(obj)  (typecode(obj) == %d)\n",(tint)bignum_type);
#else
  printf1("#define posbignump(obj)  (varobjectp(obj) && (Record_type(obj) == %d) && ((Record_flags(obj) & bit(7)) == 0))\n",Rectype_Bignum);
#endif
#if notused
#ifdef TYPECODES
  printf2("#define ratiop(obj)  ((typecode(obj) & ~%d) == %d)\n",(tint)bit(sign_bit_t),(tint)ratio_type);
#else
  printf1("#define ratiop(obj)  (varobjectp(obj) && (Record_type(obj) == %d))\n",Rectype_Ratio);
#endif
#ifdef TYPECODES
  printf2("#define floatp(obj)  ((typecode(obj) & ~%d) == %d)\n",(tint)((sfloat_type|ffloat_type|dfloat_type|lfloat_type|bit(sign_bit_t)) & ~(sfloat_type&ffloat_type&dfloat_type&lfloat_type)),(tint)(sfloat_type&ffloat_type&dfloat_type&lfloat_type));
#else
  printf4("#define floatp(obj)  (((as_oint(obj) & %d) == %d) || (varobjectp(obj) && ((uintB)(Record_type(obj)-%d) <= %d)))\n",(6 << imm_type_shift) | immediate_bias,sfloat_type,Rectype_Lfloat,Rectype_Ffloat-Rectype_Lfloat);
#endif
#ifdef TYPECODES
  printf2("#define short_float_p(obj)  ((typecode(obj) & ~%d) == %d)\n",(tint)bit(sign_bit_t),(tint)sfloat_type);
#else
  printf2("#define short_float_p(obj)  ((as_oint(obj) & &d) == %d)\n",(6 << imm_type_shift) | immediate_bias,sfloat_type);
#endif
#endif
#ifdef TYPECODES
  printf2("#define single_float_p(obj)  ((typecode(obj) & ~%d) == %d)\n",(tint)bit(sign_bit_t),(tint)ffloat_type);
#else
  printf1("#define single_float_p(obj)  (varobjectp(obj) && (Record_type(obj) == %d))\n",Rectype_Ffloat);
#endif
#ifdef TYPECODES
  printf2("#define double_float_p(obj)  ((typecode(obj) & ~%d) == %d)\n",(tint)bit(sign_bit_t),(tint)dfloat_type);
#else
  printf1("#define double_float_p(obj)  (varobjectp(obj) && (Record_type(obj) == %d))\n",Rectype_Dfloat);
#endif
#if notused
#ifdef TYPECODES
  printf2("#define long_float_p(obj)  ((typecode(obj) & ~%d) == %d)\n",(tint)bit(sign_bit_t),(tint)lfloat_type);
#else
  printf1("#define long_float_p(obj)  (varobjectp(obj) && (Record_type(obj) == %d))\n",Rectype_Lfloat);
#endif
#ifdef TYPECODES
  printf1("#define complexp(obj)  (typecode(obj) == %d)\n",(tint)complex_type);
#else
  printf1("#define complexp(obj)  (varobjectp(obj) && (Record_type(obj) == %d))\n",Rectype_Complex);
#endif
#endif
#ifdef TYPECODES
 #ifdef WIDE_STRUCT
  printf("#define positivep(obj)  ((typecode(obj) & bit(%d)) == 0)\n",sign_bit_t);
 #else
  printf("#define positivep(obj)  (!wbit_test(as_oint(obj),%d))\n",sign_bit_o);
 #endif
#else
  printf2("#define positivep(obj)  ((as_oint(obj) & wbit(1)) ? (as_oint(obj) & %d) == 0 : (Record_flags(obj) & %d) == 0)\n",wbit(sign_bit_o),bit(7));
#endif
#ifdef TYPECODES
  printf("#define FN_positivep(obj)  positivep(obj)\n");
#else
  printf1("#define FN_positivep(obj)  ((as_oint(obj) & %d) == 0)\n",wbit(sign_bit_o));
#endif
#ifdef TYPECODES
  printf("#define BN_positivep(obj)  positivep(obj)\n");
#else
  printf1("#define BN_positivep(obj)  ((Record_flags(obj) & %d) == 0)\n",bit(7));
#endif
  printf2("#define uint8_p(obj)  ((as_oint(obj) & ~%x) == %x)\n",(oint)0xFF << oint_data_shift,as_oint(Fixnum_0));
  printf3("#define sint8_p(obj)  (((as_oint(obj) ^ (FN_positivep(obj) ? 0 : %x)) & ~%x) == %x)\n",as_oint(Fixnum_minus1)^as_oint(Fixnum_0),(oint)0x7F << oint_data_shift,as_oint(Fixnum_0));
  printf2("#define uint16_p(obj)  ((as_oint(obj) & ~%x) == %x)\n",(oint)0xFFFF << oint_data_shift,as_oint(Fixnum_0));
  printf3("#define sint16_p(obj)  (((as_oint(obj) ^ (FN_positivep(obj) ? 0 : %x)) & ~%x) == %x)\n",as_oint(Fixnum_minus1)^as_oint(Fixnum_0),(oint)0x7FFF << oint_data_shift,as_oint(Fixnum_0));
#if (oint_data_len>=32)
  printf2("#define uint32_p(obj)  ((as_oint(obj) & ~%x) == %x)\n",(oint)0xFFFFFFFF << oint_data_shift,as_oint(Fixnum_0));
#else
  printf3("#define uint32_p(obj)  (posfixnump(obj) || (posbignump(obj) && (Bignum_length(obj) <= %d) && ((Bignum_length(obj) < %d) || (TheBignum(obj)->data[0] < (uintD)bit(%d)) )))\n",ceiling(33,intDsize),ceiling(33,intDsize),32%intDsize);
#endif
#if (oint_data_len>=31)
  printf3("#define sint32_p(obj)  (((as_oint(obj) ^ (FN_positivep(obj) ? 0 : %x)) & ~%x) == %x)\n",as_oint(Fixnum_minus1)^as_oint(Fixnum_0),(oint)0x7FFFFFFF << oint_data_shift,as_oint(Fixnum_0));
#else
  printf3("#define sint32_p(obj)  (fixnump(obj) || (bignump(obj) && (Bignum_length(obj) <= %d) && ((Bignum_length(obj) < %d) || ((TheBignum(obj)->data[0] ^ (BN_positivep(obj) ? (uintD)0 : ~(uintD)0)) < (uintD)bit(%d)) )))\n",ceiling(32,intDsize),ceiling(32,intDsize),31%intDsize);
#endif
  printf3("#define uint64_p(obj)  (posfixnump(obj) || (posbignump(obj) && (Bignum_length(obj) <= %d) && ((Bignum_length(obj) < %d) || (TheBignum(obj)->data[0] < (uintD)bit(%d)) )))\n",ceiling(65,intDsize),ceiling(65,intDsize),64%intDsize);
  printf3("#define sint64_p(obj)  (fixnump(obj) || (bignump(obj) && (Bignum_length(obj) <= %d) && ((Bignum_length(obj) < %d) || ((TheBignum(obj)->data[0] ^ (BN_positivep(obj) ? (uintD)0 : ~(uintD)0)) < (uintD)bit(%d)) )))\n",ceiling(64,intDsize),ceiling(64,intDsize),63%intDsize);
 #if (int_bitsize==16)
   printf("#define uint_p  uint16_p\n");
   printf("#define sint_p  sint16_p\n");
 #else
   printf("#define uint_p  uint32_p\n");
   printf("#define sint_p  sint32_p\n");
 #endif
 #if (long_bitsize==32)
   printf("#define ulong_p  uint32_p\n");
   printf("#define slong_p  sint32_p\n");
 #else
   printf("#define ulong_p  uint64_p\n");
   printf("#define slong_p  sint64_p\n");
 #endif
#if (defined(GNU) || defined(INTEL)) && defined(I80386) && !defined(NO_ASM)
  printf("%s\n","#define SP()  ({var aint __SP; __asm__ __volatile__ (\"movl %%esp,%0\" : \"=g\" (__SP) : ); __SP; })");
#endif
#if !defined(STACK_register)
  printf("extern gcv_object_t* STACK;\n");
#else
  printf("#ifndef IN_MODULE_CC\n");
  printf("register gcv_object_t* STACK __asm__(\"%s\");\n",STACK_register);
  printf("#endif\n");
#endif
#ifdef HAVE_SAVED_mv_count
  printf("extern uintC saved_mv_count;\n");
#endif
#ifdef HAVE_SAVED_value1
  printf("extern object saved_value1;\n");
#endif
#ifdef HAVE_SAVED_back_trace
  printf("extern p_backtrace_t saved_back_trace;\n");
#endif
#if defined(HAVE_SAVED_STACK)
  printf("extern gcv_object_t* saved_STACK;\n");
#endif
  printf("#define begin_call()");
#ifdef HAVE_SAVED_mv_count
  printf(" saved_mv_count = mv_count;");
#endif
#ifdef HAVE_SAVED_value1
  printf(" saved_value1 = value1;");
#endif
#ifdef HAVE_SAVED_back_trace
  printf(" saved_back_trace = back_trace;");
#endif
#ifdef HAVE_SAVED_STACK
  printf(" saved_STACK = STACK;");
#endif
  printf("\n");
  printf("#define end_call()");
#ifdef HAVE_SAVED_mv_count
  printf(" mv_count = saved_mv_count;");
#endif
#ifdef HAVE_SAVED_value1
  printf(" value1 = saved_value1;");
#endif
#ifdef HAVE_SAVED_back_trace
  printf(" back_trace = saved_back_trace;");
#endif
#ifdef HAVE_SAVED_STACK
  printf(" saved_STACK = (gcv_object_t*)NULL;");
#endif
  printf("\n");
  printf("#define begin_callback()  ");
#ifdef HAVE_SAVED_REGISTERS
  printf("{ struct registers * registers = alloca(sizeof(struct registers));");
 #ifdef STACK_register
  printf(" registers->STACK_register_contents = STACK_reg;");
 #endif
 #ifdef mv_count_register
  printf(" registers->mv_count_register_contents = mv_count_reg;");
 #endif
 #ifdef value1_register
  printf(" registers->value1_register_contents = value1_reg;");
 #endif
 #ifdef back_trace_register
  printf(" registers->back_trace_register_contents = back_trace_reg;");
 #endif
 #ifdef HAVE_SAVED_STACK
  printf(" STACK = saved_STACK;");
 #endif
  printf(" { var gcv_object_t* top_of_frame = STACK; pushSTACK(as_object((aint)callback_saved_registers)); finish_frame(CALLBACK); } callback_saved_registers = registers; } ");
#endif
  printf("end_call()\n");
  printf("#define end_callback() ");
#ifdef HAVE_SAVED_mv_count
  printf(" saved_mv_count = mv_count;");
#endif
#ifdef HAVE_SAVED_value1
  printf(" saved_value1 = value1;");
#endif
#ifdef HAVE_SAVED_back_trace
  printf(" saved_back_trace = back_trace;");
#endif
#ifdef HAVE_SAVED_REGISTERS
  printf(" { struct registers * registers = callback_saved_registers; if (!(framecode(STACK_(0)) == CALLBACK_frame_info)) abort(); callback_saved_registers = (struct registers *)(aint)as_oint(STACK_(1)); skipSTACK(2);");
 #ifdef HAVE_SAVED_STACK
  printf(" saved_STACK = STACK;");
 #endif
 #ifdef STACK_register
  printf(" STACK_reg = registers->STACK_register_contents;");
 #endif
 #ifdef mv_count_register
  printf(" mv_count_reg = registers->mv_count_register_contents;");
 #endif
 #ifdef value1_register
  printf(" value1_reg = registers->value1_register_contents;");
 #endif
 #ifdef back_trace_register
  printf(" back_trace_reg = registers->back_trace_register_contents;");
 #endif
  printf(" }");
#endif
  printf("\n");
#ifdef NO_ASYNC_INTERRUPTS
  printf("#define begin_system_call()\n");
  printf("#define end_system_call()\n");
#else
  printf("#define begin_system_call()  begin_call()\n");
  printf("#define end_system_call()  end_call()\n");
#endif
#if notused
  printf("#define check_STACK()  if (STACK_overflow()) STACK_ueber()\n");
 #ifdef STACK_DOWN
   printf("#define STACK_overflow()  ( (aint)STACK < (aint)STACK_bound )\n");
   printf("#define get_space_on_STACK(n)  if ( (aint)STACK < (aint)STACK_bound + (aint)(n) ) STACK_ueber()\n");
 #else
   printf("#define STACK_overflow()  ( (aint)STACK > (aint)STACK_bound )\n");
   printf("#define get_space_on_STACK(n)  if ( (aint)STACK + (aint)(n) > (aint)STACK_bound ) STACK_ueber()\n");
 #endif
 printf("extern void* STACK_bound;\n");
 printf("nonreturning_function(extern, STACK_ueber, (void));\n");
#endif
  printf("nonreturning_function(extern, fehler_notreached, (const char * file, uintL line));\n");
#ifndef LANGUAGE_STATIC
 #ifndef GNU_GETTEXT
  printf("#define GETTEXT(english) english\n");
 #else
  printf("extern const char * clgettext (const char * msgid);\n");
  printf("#define GETTEXT clgettext\n");
 #endif
#endif
  printf("extern object allocate_cons (void);\n");
#if notused
  printf("extern object make_symbol (object string);\n");
#endif
  printf("extern object allocate_vector (uintL len);\n");
  printf("#define Atype_32Bit %d\n",Atype_32Bit);
  printf("#define Atype_8Bit %d\n",Atype_8Bit);
  printf("#define Atype_Bit %d\n",Atype_Bit);
  printf("extern object allocate_bit_vector (uintB atype, uintL len);\n");
#ifdef UNICODE
  printf("extern object allocate_s32string (uintL len);\n");
  printf("#define allocate_string(len)  allocate_s32string(len)\n");
#else
  printf("extern object allocate_s8string (uintL len);\n");
  printf("#define allocate_string(len)  allocate_s8string(len)\n");
#endif
#ifdef asciz_length
 #if defined(GNU) && (SAFETY < 2) && (__GNUC__ >= 2)
  printf("#define asciz_length(a)  ((uintL)__builtin_strlen(a))\n");
 #else
  printf("#define asciz_length(a)  ((uintL)strlen(a))\n");
 #endif
#else
  printf("extern uintL asciz_length (const char * asciz);\n");
#endif
#if notused
#ifdef asciz_length
  printf("#define asciz_equal(a1,a2)  (__builtin_strcmp(a1,a2)==0)\n");
#else
  printf("extern bool asciz_equal (const char * asciz1, const char * asciz2);\n");
#endif
  emit_typedef_f("Values %s(void)","subr_norest_function_t");
  emit_typedef_f("Values %s(uintC argcount, object* rest_args_pointer)","subr_rest_function_t");
#endif
  printf("extern struct subr_tab_ {\n");
  #undef LISPFUN
  #define LISPFUN(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords) \
    printf("  subr_t %s;\n",STRING(D_##name));
  #include "subr.c"
  #undef LISPFUN
  printf("} subr_tab_data;\n");
#if !defined(MAP_MEMORY_TABLES)
  printf("#define subr_tab  subr_tab_data\n");
 #ifdef TYPECODES
  printf1("#define subr_tab_ptr_as_object(subr_addr)  (type_constpointer_object(%d,subr_addr))\n",(tint)subr_type);
 #else
  #ifdef WIDE_AUXI
  printf1("#define subr_tab_ptr_as_object(subr_addr)  as_object_with_auxi((aint)(subr_addr)+%d)\n",subr_bias);
  #else
  printf1("#define subr_tab_ptr_as_object(subr_addr)  as_object((oint)(subr_addr)+%d)\n",subr_bias);
  #endif
 #endif
  printf("#define L(name)  subr_tab_ptr_as_object(&subr_tab.D_##name)\n");
#else
  printf1("#define subr_tab_addr  ((struct subr_tab_ *)type_zero_oint(%d))\n",(tint)subr_type);
  printf("#define subr_tab  (*subr_tab_addr)\n");
  printf("#define subr_tab_ptr_as_object(subr_addr)  (as_object((oint)(subr_addr)))\n");
  printf("#define L(name)  subr_tab_ptr_as_object(&subr_tab_addr->D_##name)\n");
#endif
  printf("extern struct symbol_tab_ {\n");
  #define LISPSYM(name,printname,package)  \
    printf("  symbol_ %s;\n",STRING(S_##name));
  #include "constsym.c"
  #undef LISPSYM
  printf("} symbol_tab_data;\n");
  printf("#define S(name)  S_help_(S_##name)\n");
#if !defined(MAP_MEMORY_TABLES)
  printf("#define symbol_tab  symbol_tab_data\n");
 #ifdef TYPECODES
  printf1("#define S_help_(name)  (type_constpointer_object(%d,&symbol_tab.name))\n",(tint)symbol_type);
 #else
  #if defined(WIDE_AUXI)
  printf1("#define S_help_(name)  as_object_with_auxi((aint)&symbol_tab.name+%d)\n",varobject_bias);
  #elif defined(OBJECT_STRUCT)
  printf1("#define S_help_(name)  as_object((oint)&symbol_tab.name+%d)\n",varobject_bias);
  #else
  printf1("#define S_help_(name)  objectplus(&symbol_tab.name,%d)\n",varobject_bias);
  #endif
 #endif
#else
  printf1("#define symbol_tab_addr ((struct symbol_tab_ *)type_zero_oint(%d))\n",(tint)symbol_type);
#if notused
  printf("#define symbol_tab  (*symbol_tab_addr)\n");
#endif
  printf("#define S_help_(name)  (as_object((oint)(&symbol_tab_addr->name)))\n");
#endif
  printf("#define NIL  S(nil)\n");
  printf("#define T    S(t)\n");
#if defined(DEBUG_GCSAFETY)
  printf("static inline bool gcinvariant_symbol_p (object obj) { if (");
 #ifdef TYPECODES
  printf("symbolp(obj)");
 #else
  printf("varobjectp(obj)");
 #endif
  printf(" && (");
 #if !defined(MAP_MEMORY_TABLES)
  #ifdef TYPECODES
  printf2("(as_oint(obj) >> %d) - %d", oint_addr_shift-addr_shift, (aint)(tint)symbol_type<<oint_type_shift);
  #else
  printf1("as_oint(obj) - %d", varobject_bias);
  #endif
 #else
  printf("as_oint(obj)");
 #endif
  printf(" - (aint)&symbol_tab < sizeof(symbol_tab))) return true; else return false; }\n");
#endif
  printf("extern struct object_tab_ {\n");
  #define LISPOBJ(name,init)  printf("  gcv_object_t %s;\n",STRING(name));
  #include "constobj.c"
  #undef LISPOBJ
  printf("} object_tab;\n");
  printf("#define GLO(name)  (object_tab.name)\n");
  printf("extern uintC module_count;\n");
  emit_typedef("struct { const char* packname; const char* symname; }","subr_initdata_t");
  emit_typedef("struct { const char* initstring; }","object_initdata_t");
  strcpy(buf,"struct module_t { const char* name; subr_t* stab; const uintC* stab_size; gcv_object_t* otab; const uintC* otab_size; bool initialized; const subr_initdata_t* stab_initdata; const object_initdata_t* otab_initdata; void (*initfunction1) (struct module_t *); void (*initfunction2) (struct module_t *);");
#ifdef DYNAMIC_MODULES
  strcat(buf," struct module_t * next;");
#endif
  strcat(buf," }"); emit_typedef(buf,"module_t");
#ifdef DYNAMIC_MODULES
  printf("BEGIN_DECLS\n");
  printf("extern void add_module (module_t * new_module);\n");
  printf("END_DECLS\n");
#else
  printf("extern module_t modules[];\n");
#endif
#ifdef STACK_DOWN
  printf("#define STACK_(n)  (STACK[(sintP)(n)])\n");
  printf("#define skipSTACKop  +=\n");
  printf("#define STACKop      +\n");
#else
  printf("#define STACK_(n)  (STACK[-1-(sintP)(n)])\n");
  printf("#define skipSTACKop  -=\n");
  printf("#define STACKop      -\n");
#endif
  { var int i;
    for (i=0; i<=10; i++) printf("#define STACK_%d  (STACK_(%d))\n",i,i); }
#if defined(GNU) && defined(MC680X0) && !defined(NO_ASM) && !defined(WIDE) && defined(STACK_register)
 #ifdef STACK_DOWN
  printf("#define pushSTACK(obj)  ({ __asm__ __volatile__ (\"movel %%0,%s%s@-\" : : \"g\" ((object)(obj)) : \"%s\" ); })\n",REGISTER_PREFIX,STACK_register,STACK_register);
  printf("#define popSTACK()  ({var object __result; __asm__ __volatile__ (\"movel %s%s@+,%%0\" : \"=g\" (__result) : : \"%s\" ); __result; })\n",REGISTER_PREFIX,STACK_register,STACK_register);
 #else
  printf("#define pushSTACK(obj)  ({ __asm__ __volatile__ (\"movel %%0,%s%s@+\" : : \"g\" ((object)(obj)) : \"%s\" ); })\n",REGISTER_PREFIX,STACK_register,STACK_register);
  printf("#define popSTACK()  ({var object __result; __asm__ __volatile__ (\"movel %s%s@-,%%0\" : \"=g\" (__result) : : \"%s\" ); __result; })\n",REGISTER_PREFIX,STACK_register,STACK_register);
 #endif
#else
  printf("#define pushSTACK(obj)  (STACK_(-1) = (obj), STACK skipSTACKop -1)\n");
  printf("#define popSTACK()  (STACK skipSTACKop 1, STACK_(-1))\n");
#endif
  printf("#define skipSTACK(n)  (STACK skipSTACKop (sintP)(n))\n");

#if notused
  printf("#define mv_limit %d\n",mv_limit);
#endif
#if !defined(mv_count_register)
  printf("extern uintC mv_count;\n");
#else
  printf("#ifndef IN_MODULE_CC\n");
  printf("register uintC mv_count __asm__(\"%s\");\n",mv_count_register);
  printf("#endif\n");
#endif
  printf("extern object mv_space [%d];\n",mv_limit-1);
#if !defined(value1_register)
  printf("#define value1  mv_space[0]\n");
#else
  printf("#ifndef IN_MODULE_CC\n");
  printf("register object value1 __asm__(\"%s\");\n",value1_register);
  printf("#endif\n");
#endif
#if notused
  printf("nonreturning_function(extern, fehler_mv_zuviel, (object caller));\n");
#endif
  printf("struct backtrace_t {\n  struct backtrace_t* bt_next;\n  gcv_object_t bt_caller;\n  gcv_object_t *bt_stack;\n  int bt_num_arg;\n};\n");
  emit_typedef("struct backtrace_t *","p_backtrace_t");
  printf("#define subr_self  back_trace->bt_caller\n");
#if !defined(back_trace_register)
  printf("extern p_backtrace_t back_trace;\n");
#else
  printf("#ifndef IN_MODULE_CC\n");
  printf("register p_backtrace_t back_trace __asm__(\"%s\");\n",back_trace_register);
  printf("#endif\n");
#endif
  { int i = 2;
    for (; i <=9 ; i++)
      printf("#define value%d  mv_space[%d]\n",i,i-1);
  }
  printf("#define VALUES0 do{ value1 = NIL; mv_count = 0; }while(0)\n");
  printf("#define VALUES1(A) do{ value1 = (A); mv_count = 1; }while(0)\n");
  printf("#define VALUES2(A,B) do{ value1 = (A); value2 = (B); mv_count = 2;}while(0)\n");
  printf("#define VALUES3(A,B,C) do{ value1 = (A); value2 = (B); value3 = (C); mv_count = 3;}while(0)\n");
  printf("#define VALUES_IF(C) do{ value1 = (C) ? T : NIL; mv_count = 1; }while(0)\n");
  printf("#define args_end_pointer  STACK\n");
#if notused
  printf("#define set_args_end_pointer(new_args_end_pointer)  STACK = (new_args_end_pointer)\n");
  #ifdef STACK_DOWN
    printf("#define NEXT(argpointer)  (*(--(argpointer)))\n");
    printf("#define BEFORE(argpointer)  (*((argpointer)++))\n");
  #else
    printf("#define NEXT(argpointer)  (*((argpointer)++))\n");
    printf("#define BEFORE(argpointer)  (*(--(argpointer)))\n");
  #endif
  printf("#define Next(pointer)  (*(STACKpointable(pointer) STACKop -1))\n");
  printf("#define Before(pointer)  (*(STACKpointable(pointer) STACKop 0))\n");
#endif
#ifdef HAVE_SAVED_REGISTERS
  printf1("#define CALLBACK_frame_info  %d\n",CALLBACK_frame_info);
#endif
#ifdef TYPECODES
  printf("#define framecode(bottomword)  mtypecode(bottomword)\n");
#else
  printf1("#define framecode(bottomword)  (as_oint(bottomword) & minus_wbit(%d))\n",FB1);
#endif
#ifdef TYPECODES
 #if !defined(SINGLEMAP_MEMORY_STACK)
  printf("#define framebottomword(type,top_of_frame,bot_of_frame)  type_pointer_object(type,top_of_frame)\n");
 #else
  printf1("#define framebottomword(type,top_of_frame,bot_of_frame)  as_object(type_zero_oint(type)-type_zero_oint(%d)+(oint)(top_of_frame))\n",(tint)system_type);
 #endif
  printf("#define finish_frame(frametype)  pushSTACK(framebottomword(frametype##_frame_info,top_of_frame,bot_of_frame_ignored))\n");
#else
 #ifdef STACK_UP
  printf("#define framebottomword(type,top_of_frame,bot_of_frame)  as_object((oint)(type)+(oint)((uintP)(bot_of_frame)-(uintP)(top_of_frame)))\n");
 #endif
 #ifdef STACK_DOWN
  printf("#define framebottomword(type,top_of_frame,bot_of_frame)  as_object((oint)(type)+(oint)((uintP)(top_of_frame)-(uintP)(bot_of_frame)))\n");
 #endif
  printf("#define finish_frame(frametype)  (STACK_(-1) = framebottomword(frametype##_frame_info,top_of_frame,STACK STACKop -1), skipSTACK(-1))\n");
#endif
#if notused
  printf("extern Values apply (object fun, uintC args_on_stack, object other_args);\n");
#endif
  printf("extern Values funcall (object fun, uintC argcount);\n");
#if notused
  printf("extern Values eval (object form);\n");
#endif
  printf("#define LISPFUNN(name,req_anz)  LISPFUN(name,sec,req_anz,0,norest,nokey,0,NIL)\n");
  printf("#define LISPFUN_B(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)  extern Values C_##name subr_##rest_flag##_function_args\n");
  printf("#define subr_norest_function_args  (void)\n");
  printf("#define subr_rest_function_args  (uintC argcount, object* rest_args_pointer)\n");
  printf("#define LISPFUN_F(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)  { (lisp_function_t)(&C_##name), nullobj, nullobj, 0, req_anz, opt_anz, (uintB)subr_##rest_flag, (uintB)subr_##key_flag, key_anz, sec},\n");
  printf("#define LISPFUN  LISPFUN_B\n");
#ifdef UNICODE
  printf("extern object n_char_to_string (const char* charptr, uintL len, object encoding);\n");
#else
  printf("#define n_char_to_string(charptr,len,encoding)  n_char_to_string_(charptr,len)\n");
  printf("extern object n_char_to_string_ (const char* charptr, uintL len);\n");
#endif
  printf("extern object gethash (object obj, object ht);\n");
#ifdef UNICODE
  printf("extern object asciz_to_string (const char * asciz, object encoding);\n");
#else
  printf("#define asciz_to_string(asciz,encoding)  asciz_to_string_(asciz)\n");
  printf("extern object asciz_to_string_ (const char * asciz);\n");
#endif
  printf("extern object ascii_to_string (const char * asciz);\n");
  printf("extern object string_concat (uintC argcount);\n");
#ifdef UNICODE
  printf("extern object string_to_asciz (object obj, object encoding);\n");
#else
  printf("#define string_to_asciz(obj,encoding)  string_to_asciz_(obj)\n");
  printf("extern object string_to_asciz_ (object obj);\n");
#endif
  printf("extern object unpack_string_ro (object string, uintL* len, uintL* offset);\n");
#if defined(UNICODE)
  printf("extern uintL cslen_f (object encoding, const chart*src, uintL srclen);\n");
  printf("extern void cstombs_f (object encoding, const chart *src, uintL srclen, uintB* dest, uintL destlen);\n");
#else
  printf("#define cslen_f(e,s,l)  cslen_ff(s,l)\n");
  printf("extern uintL cslen_ff (const chart*src, uintL srclen);\n");
  printf("#define cstombs_f(e,s,l,d,n) cstombs_ff(s,l,d,n)\n");
  printf("extern void cstombs_ff (const chart *src, uintL srclen, uintB* dest, uintL destlen);\n");
#endif
  printf("#define with_string_0(string,encoding,ascizvar,statement) ");
  printf("    do { var uintL ascizvar##_len;");
  printf("    var uintL ascizvar##_offset;");
  printf("    var object ascizvar##_string = unpack_string_ro(string,&ascizvar##_len,&ascizvar##_offset);");
  printf("    var const chart* ptr1;");
  printf("    unpack_sstring_alloca(ascizvar##_string,ascizvar##_len,ascizvar##_offset, ptr1=);");
  printf("   {var uintL ascizvar##_bytelen = cslen_f(encoding,ptr1,ascizvar##_len);");
  printf("    var DYNAMIC_ARRAY(ascizvar##_data,uintB,ascizvar##_bytelen+1);");
  printf("    cstombs_f(encoding,ptr1,ascizvar##_len,&ascizvar##_data[0],ascizvar##_bytelen);");
  printf("    ascizvar##_data[ascizvar##_bytelen] = 0;");
  printf("    {var char* ascizvar = (char*) &ascizvar##_data[0];");
  printf("     statement");
  printf("    }");
  printf("    FREE_DYNAMIC_ARRAY(ascizvar##_data);");
  printf("  }} while(0)\n");
  printf("#define with_sstring_0(string,encoding,ascizvar,statement)");
  printf("  do { var object ascizvar##_string = (string);");
  printf("    sstring_un_realloc(ascizvar##_string);");
  printf("   {var uintL ascizvar##_len = Sstring_length(ascizvar##_string);");
  printf("    var const chart* ptr1;");
  printf("    unpack_sstring_alloca(ascizvar##_string,ascizvar##_len,0, ptr1=);");
  printf("   {var uintL ascizvar##_bytelen = cslen_f(encoding,ptr1,ascizvar##_len);");
  printf("    var DYNAMIC_ARRAY(ascizvar##_data,uintB,ascizvar##_bytelen+1);");
  printf("    cstombs_f(encoding,ptr1,ascizvar##_len,&ascizvar##_data[0],ascizvar##_bytelen);");
  printf("    ascizvar##_data[ascizvar##_bytelen] = 0;");
  printf("    {var char* ascizvar = (char*) &ascizvar##_data[0];");
  printf("     statement");
  printf("    }");
  printf("    FREE_DYNAMIC_ARRAY(ascizvar##_data);");
  printf("  }}} while(0)\n");

#ifdef HAVE_SMALL_SSTRING
  printf("extern void copy_8bit_16bit (const uint8* src, uint16* dest, uintL len);\n");
  printf("extern void copy_8bit_32bit (const uint8* src, uint32* dest, uintL len);\n");
  printf("extern void copy_16bit_8bit (const uint16* src, uint8* dest, uintL len);\n");
  printf("extern void copy_16bit_16bit (const uint16* src, uint16* dest, uintL len);\n");
  printf("extern void copy_16bit_32bit (const uint16* src, uint32* dest, uintL len);\n");
  printf("extern void copy_32bit_8bit (const uint32* src, uint8* dest, uintL len);\n");
  printf("extern void copy_32bit_16bit (const uint32* src, uint16* dest, uintL len);\n");
 #ifdef TYPECODES
  printf("#define TheS8string(obj) ((S8string)(types_pointable(sstring_type,obj)))\n");
  printf("#define TheS16string(obj) ((S16string)(types_pointable(sstring_type,obj)))\n");
  printf("#define TheS32string(obj) ((S32string)(types_pointable(sstring_type,obj)))\n");
 #else
  printf("#define TheS8string(obj) ((S8string)(ngci_pointable(obj)-%d))\n",varobject_bias);
  printf("#define TheS16string(obj) ((S16string)(ngci_pointable(obj)-%d))\n",varobject_bias);
  printf("#define TheS32string(obj) ((S32string)(ngci_pointable(obj)-%d))\n",varobject_bias);
 #endif

  emit_typedef("uint8","cint8");
  emit_typedef("uint16","cint16");
  emit_typedef("uint32","cint32");
  printf("#define STRUCT_SSTRING(cint_type)  struct { SSTRING_HEADER cint_type data[unspecified]; }\n");
  emit_typedef("STRUCT_SSTRING(cint8)","s8string_");
  emit_typedef("s8string_ *","S8string");
  emit_typedef("STRUCT_SSTRING(cint16)","s16string_");
  emit_typedef("s16string_ *","S16string");
  emit_typedef("STRUCT_SSTRING(cint32)","s32string_");
  emit_typedef("s32string_ *","S32string");

  printf("#define unpack_sstring_alloca(string,len,offset,charptr_assignment)");
  printf("  if (simple_nilarray_p(string)) {");
  printf("    if ((len) > 0) fehler_nilarray_retrieve();");
  printf("    charptr_assignment NULL;");
  printf("  } else if (sstring_eltype(TheSstring(string)) == %d) {",Sstringtype_32Bit);
  printf("    charptr_assignment (const chart*) &TheS32string(string)->data[offset];");
  printf("  } else {");
  printf("    var chart* _unpacked_ = (chart*)alloca((len)*sizeof(chart));");
  printf("    if ((len) > 0) {");
  printf("      if (sstring_eltype(TheSstring(string)) == %d)",Sstringtype_16Bit);
  printf("        copy_16bit_32bit(&TheS16string(string)->data[offset],(cint32*)_unpacked_,len);");
  printf("      else if (sstring_eltype(TheSstring(string)) == %d)",Sstringtype_8Bit);
  printf("        copy_8bit_32bit(&TheS8string(string)->data[offset],(cint32*)_unpacked_,len);");
  printf("      else");
  printf("        NOTREACHED;");
  printf("    }");
  printf("    charptr_assignment (const chart*) _unpacked_;");
  printf("  }\n");
#else
  printf("#define unpack_sstring_alloca(string,len,offset,charptr_assignment)");
  printf("  if (simple_nilarray_p(string)) {");
  printf("    if ((len) > 0) fehler_nilarray_retrieve();");
  printf("    charptr_assignment NULL;");
  printf("  } else {");
  printf("    charptr_assignment (const chart*) &TheSnstring(string)->data[offset];");
  printf("  }\n");
#endif

  printf("#define TheAsciz(obj)  ((char*)(&TheSbvector(obj)->data[0]))\n");
  printf("extern object vectorof (uintC len);\n");
#if notused
  printf("extern object allocate_bit_vector_0 (uintL len);\n");
  printf("extern chart up_case (chart ch);\n");
  printf("extern chart down_case (chart ch);\n");
  printf("extern chart* unpack_string (object string, uintL* len);\n");
  printf("extern object make_list (uintL len);\n");
#endif
  printf("extern object listof (uintC len);\n");
  printf("extern object nreverse (object list);\n");
  printf("extern object deleteq (object list, object obj);\n");
  printf("extern object memq (const object obj, const object lis);\n");
  printf("typedef enum { condition=%d, serious_condition=%d, error=%d, program_error=%d, source_program_error=%d, control_error=%d, arithmetic_error=%d, division_by_zero=%d, floating_point_overflow=%d, floating_point_underflow=%d, cell_error=%d, unbound_variable=%d, undefined_function=%d, unbound_slot=%d, type_error=%d, keyword_error=%d, charset_type_error=%d, package_error=%d, print_not_readable=%d, parse_error=%d, stream_error=%d, end_of_file=%d, reader_error=%d, file_error=%d, os_error=%d, storage_condition=%d, interrupt_condition=%d, warning=%d } condition_t;\n",condition, serious_condition, error, program_error, source_program_error, control_error, arithmetic_error, division_by_zero, floating_point_overflow, floating_point_underflow, cell_error, unbound_variable, undefined_function, unbound_slot, type_error, keyword_error, charset_type_error, package_error, print_not_readable, parse_error, stream_error, end_of_file, reader_error, file_error, os_error, storage_condition, interrupt_condition, warning);
  printf("nonreturning_function(extern, fehler, (condition_t errortype, const char * errorstring));\n");
  printf("extern void check_value (condition_t errortype, const char * errorstring);\n");
  printf("nonreturning_function(extern, OS_error, (void));\n");
  printf("nonreturning_function(extern, OS_file_error, (object pathname));\n");
  printf("nonreturning_function(extern, OS_filestream_error, (object stream));\n");
#if notused
  printf("nonreturning_function(extern, fehler_list, (object obj));\n");
  printf("extern object check_list_replacement (object obj);\n");
  printf("static inline object check_list (object obj) {"
          " if (!listp(obj))"
            " obj = check_list_replacement(obj);"
          " return obj;"
        " }\n");
#endif
  printf("nonreturning_function(extern, fehler_proper_list, (object caller, object obj));\n");
#if notused
  printf("nonreturning_function(extern, fehler_kein_svector, (object caller, object obj));\n");
  printf("nonreturning_function(extern, fehler_vector, (object obj));\n");
#endif
  printf("extern object check_posfixnum_replacement (object obj);\n");
  printf("static inline object check_posfixnum (object obj) {"
          " if (!posfixnump(obj))"
            " obj = check_posfixnum_replacement(obj);"
          " return obj;"
        " }\n");
#if notused
  printf("extern object check_char_replacement (object obj);\n");
  printf("static inline object check_char (object obj) {"
          " if (!charp(obj))"
            " obj = check_char_replacement(obj);"
          " return obj;"
        " }\n");
#endif
  printf("extern object check_string_replacement (object obj);\n");
  printf("static inline object check_string (object obj) {"
          " if (!stringp(obj))"
            " obj = check_string_replacement(obj);"
          " return obj;"
        " }\n");
#if notused
  printf("nonreturning_function(extern, fehler_sstring, (object obj));\n");
#endif
  printf("nonreturning_function(extern, fehler_string_integer, (object obj));\n");
  printf("nonreturning_function(extern, fehler_key_odd, (uintC argcount, object caller));\n");
  printf("nonreturning_function(extern, fehler_key_badkw, (object fun, object key, object val, object kwlist));\n");
  printf("extern object check_uint8_replacement (object obj);\n");
  printf("static inline object check_uint8 (object obj) {"
          " if (!uint8_p(obj))"
            " obj = check_uint8_replacement(obj);"
          " return obj;"
        " }\n");
  printf("extern object check_sint8_replacement (object obj);\n");
  printf("static inline object check_sint8 (object obj) {"
          " if (!sint8_p(obj))"
            " obj = check_sint8_replacement(obj);"
          " return obj;"
        " }\n");
  printf("extern object check_uint16_replacement (object obj);\n");
  printf("static inline object check_uint16 (object obj) {"
          " if (!uint16_p(obj))"
            " obj = check_uint16_replacement(obj);"
          " return obj;"
        " }\n");
  printf("extern object check_sint16_replacement (object obj);\n");
  printf("static inline object check_sint16 (object obj) {"
          " if (!sint16_p(obj))"
            " obj = check_sint16_replacement(obj);"
          " return obj;"
        " }\n");
  printf("extern object check_uint32_replacement (object obj);\n");
  printf("static inline object check_uint32 (object obj) {"
          " if (!uint32_p(obj))"
            " obj = check_uint32_replacement(obj);"
          " return obj;"
        " }\n");
  printf("extern object check_sint32_replacement (object obj);\n");
  printf("static inline object check_sint32 (object obj) {"
          " if (!sint32_p(obj))"
            " obj = check_sint32_replacement(obj);"
          " return obj;"
        " }\n");
  printf("extern object check_uint64_replacement (object obj);\n");
  printf("static inline object check_uint64 (object obj) {"
          " if (!uint64_p(obj))"
            " obj = check_uint64_replacement(obj);"
          " return obj;"
        " }\n");
  printf("extern object check_sint64_replacement (object obj);\n");
  printf("static inline object check_sint64 (object obj) {"
          " if (!sint64_p(obj))"
            " obj = check_sint64_replacement(obj);"
          " return obj;"
        " }\n");
  printf("extern object check_uint_replacement (object obj);\n");
  printf("static inline object check_uint (object obj) {"
          " if (!uint_p(obj))"
            " obj = check_uint_replacement(obj);"
          " return obj;"
        " }\n");
  printf("extern object check_sint_replacement (object obj);\n");
  printf("static inline object check_sint (object obj) {"
          " if (!sint_p(obj))"
            " obj = check_sint_replacement(obj);"
          " return obj;"
        " }\n");
  printf("extern object check_ulong_replacement (object obj);\n");
  printf("static inline object check_ulong (object obj) {"
          " if (!ulong_p(obj))"
            " obj = check_ulong_replacement(obj);"
          " return obj;"
        " }\n");
  printf("extern object check_slong_replacement (object obj);\n");
  printf("static inline object check_slong (object obj) {"
          " if (!slong_p(obj))"
            " obj = check_slong_replacement(obj);"
          " return obj;"
        " }\n");
  printf("extern object check_ffloat_replacement (object obj);\n");
  printf("static inline object check_ffloat (object obj) {"
          " if (!single_float_p(obj))"
            " obj = check_ffloat_replacement(obj);"
          " return obj;"
        " }\n");
  printf("extern object check_dfloat_replacement (object obj);\n");
  printf("static inline object check_dfloat (object obj) {"
          " if (!double_float_p(obj))"
            " obj = check_dfloat_replacement(obj);"
          " return obj;"
        " }\n");
  printf("extern double to_double (object obj);\n");
  printf("extern int to_int (object obj);\n");
  printf("extern object find_package (object string);\n");
  printf("extern uintBWL intern (object string, object pack, object* sym_);\n");
  printf("extern object intern_keyword (object string);\n");
  printf("extern object object_out (object obj);\n");
  printf("#define OBJECT_OUT(obj,label)");
  printf(" (printf(\"[%%s:%%d] %%s: %%s:\\n\",__FILE__,__LINE__,STRING(obj),label),");
  printf("  obj=object_out(obj))\n");

  printf("typedef enum { DIRECTION_PROBE=%d, DIRECTION_INPUT=%d, DIRECTION_INPUT_IMMUTABLE=%d, DIRECTION_OUTPUT=%d, DIRECTION_IO=%d} direction_t;\n",
         DIRECTION_PROBE, DIRECTION_INPUT, DIRECTION_INPUT_IMMUTABLE,
         DIRECTION_OUTPUT, DIRECTION_IO);
  printf("extern direction_t check_direction (object dir);\n");
  emit_typedef("enum { IF_DOES_NOT_EXIST_UNBOUND, IF_DOES_NOT_EXIST_ERROR, IF_DOES_NOT_EXIST_NIL, IF_DOES_NOT_EXIST_CREATE }","if_does_not_exist_t");
  printf("extern if_does_not_exist_t check_if_does_not_exist (object if_not_exist);\n");
  printf("extern object if_does_not_exist_symbol (if_does_not_exist_t if_not_exist);\n");
  emit_typedef("enum { IF_EXISTS_UNBOUND, IF_EXISTS_ERROR, IF_EXISTS_NIL, IF_EXISTS_RENAME, IF_EXISTS_RENAME_AND_DELETE, IF_EXISTS_SUPERSEDE, IF_EXISTS_APPEND, IF_EXISTS_OVERWRITE }","if_exists_t");
  printf("extern if_exists_t check_if_exists (object if_exists);\n");
  printf("extern object if_exists_symbol (if_exists_t if_exists);\n");
#if defined(UNIX)
  printf("#include <time.h>\n");
  printf("extern object convert_time_to_universal (const time_t* time);\n");
#endif
#if defined(UNIX_CYGWIN32)
  printf("#include <windows.h>\n");
  printf("extern long to_time_t_ (FILETIME * ptr);\n");
#endif
#if defined(WIN32_NATIVE)
  printf("#include <windows.h>\n");
  printf("extern object convert_time_to_universal (const FILETIME* time);\n");
#endif
  printf("#define UNIX_LISP_TIME_DIFF 2208988800UL\n");
#if defined(WIN32_NATIVE)
  printf("#define Handle HANDLE\n");
#elif defined(UNIX)
  printf("#define Handle uintW\n");
#else
  printf("#error \"what is Handle on your platform?!\"\n");
#endif
  printf("extern Handle handle_dup (Handle old_handle, Handle new_handle);\n");
  printf("extern Handle stream_lend_handle (object stream, bool inputp, int * handletype);\n");
  printf("extern uintL read_byte_array (const gcv_object_t* stream_, const gcv_object_t* bytearray_, uintL start, uintL len, bool no_hang);\n");
  printf("extern uintL write_byte_array (const gcv_object_t* stream_, const gcv_object_t* bytearray_, uintL start, uintL len, bool no_hang);\n");
  printf("extern void builtin_stream_close (const gcv_object_t* stream_);\n");
  printf("extern object file_stream_truename (object s);\n");
  printf("extern object open_file_stream_handle (object stream, Handle *fd);\n");
  printf("extern int write_helper (Handle fd, const void* buf, int nbyte, bool no_hang);\n");
  printf("extern int read_helper (Handle fd, void* buf, int nbyte, bool no_hang);\n");
  printf("extern object addr_to_string (short type, char *addr);\n");
  printf("extern struct hostent* resolve_host (object arg);\n");
  printf("#define strm_buffered_bufflen %d\n",strm_buffered_bufflen);
#if notused
  printf("extern bool eql (object obj1, object obj2);\n");
  printf("extern bool equal (object obj1, object obj2);\n");
  printf("extern bool equalp (object obj1, object obj2);\n");
  printf("extern object get (object symbol, object key);\n");
#endif
  printf("extern object L_to_I (sint32 wert);\n");
#if (intLsize<=oint_data_len)
  printf("#define UL_to_I(wert)  fixnum((uintL)(wert))\n");
#else
  printf("extern object UL_to_I (uintL wert);\n");
#endif
  printf("extern object L2_to_I (sint32 wert_hi, uint32 wert_lo);\n");
  printf("extern object UL2_to_I (uint32 wert_hi, uint32 wert_lo);\n");
#ifdef intQsize
  printf("extern object Q_to_I (sint64 wert);\n");
  printf("extern object UQ_to_I (uint64 wert);\n");
#endif
  printf("#define uint8_to_I(val)  fixnum((uint8)(val))\n");
  printf("#define sint8_to_I(val)  L_to_I((sint32)(sint8)(val))\n");
  printf("#define uint16_to_I(val)  fixnum((uint16)(val))\n");
  printf("#define sint16_to_I(val)  L_to_I((sint32)(sint16)(val))\n");
  printf("#define uint32_to_I(val)  UL_to_I((uint32)(val))\n");
  printf("#define sint32_to_I(val)  L_to_I((sint32)(val))\n");
#ifdef intQsize
  printf("#define uint64_to_I(val)  UQ_to_I((uint64)(val))\n");
  printf("#define sint64_to_I(val)  Q_to_I((sint64)(val))\n");
#else
  printf("#define uint64_to_I(val)  UL2_to_I((uint32)((val)>>32),(uint32)(val))\n");
  printf("#define sint64_to_I(val)  L2_to_I((sint32)((val)>>32),(uint32)(val))\n");
#endif
#if (int_bitsize==16)
  printf("#define uint_to_I(val)  uint16_to_I(val)\n");
  printf("#define sint_to_I(val)  sint16_to_I(val)\n");
#else  /* (int_bitsize==32) */
  printf("#define uint_to_I(val)  uint32_to_I(val)\n");
  printf("#define sint_to_I(val)  sint32_to_I(val)\n");
#endif
#if (long_bitsize==32)
  printf("#define ulong_to_I(val)  uint32_to_I(val)\n");
  printf("#define slong_to_I(val)  sint32_to_I(val)\n");
#else  /* (long_bitsize==64) */
  printf("#define ulong_to_I(val)  uint64_to_I(val)\n");
  printf("#define slong_to_I(val)  sint64_to_I(val)\n");
#endif
  printf("extern uintL I_to_UL (object obj);\n");
  printf("extern sintL I_to_L (object obj);\n");
#ifdef HAVE_LONGLONG
  printf("extern uint64 I_to_UQ (object obj);\n");
  printf("extern sint64 I_to_Q (object obj);\n");
#endif
  printf("#define I_to_uint8(obj)  (uint8)(as_oint(obj) >> %d)\n",oint_data_shift);
  printf("#define I_to_sint8(obj)  (sint8)(as_oint(obj) >> %d)\n",oint_data_shift);
  printf("#define I_to_uint16(obj)  (uint16)(as_oint(obj) >> %d)\n",oint_data_shift);
  printf("#define I_to_sint16(obj)  (sint16)(as_oint(obj) >> %d)\n",oint_data_shift);
#if (oint_data_len>=32)
  printf("#define I_to_uint32(obj)  (uint32)(as_oint(obj) >> %d)\n",oint_data_shift);
#else
  printf("#define I_to_uint32(obj)  I_to_UL(obj)\n");
#endif
#if (oint_data_len>=31)
  printf("#define I_to_sint32(obj)  (sint32)(as_oint(obj) >> %d)\n",oint_data_shift);
#else
  printf("#define I_to_sint32(obj)  I_to_L(obj)\n");
#endif
  printf("#define I_to_uint64(obj)  I_to_UQ(obj)\n");
  printf("#define I_to_sint64(obj)  I_to_Q(obj)\n");
#if (int_bitsize==16)
  printf("#define I_to_uint  I_to_uint16\n");
  printf("#define I_to_sint  I_to_sint16\n");
#else  /* (int_bitsize==32) */
  printf("#define I_to_uint  I_to_uint32\n");
  printf("#define I_to_sint  I_to_sint32\n");
#endif
#if (long_bitsize==32)
  printf("#define I_to_ulong  I_to_uint32\n");
  printf("#define I_to_slong  I_to_sint32\n");
#else  /* (long_bitsize==64) */
  printf("#define I_to_ulong  I_to_uint64\n");
  printf("#define I_to_slong  I_to_sint64\n");
#endif
#if notused
  printf("extern object I_1_plus_I (object x);\n");
  printf("extern object I_minus1_plus_I (object x);\n");
  printf("extern object I_I_plus_I (object x, object y);\n");
  printf("extern object I_I_minus_I (object x, object y);\n");
#endif
  printf("extern object c_float_to_FF (const ffloatjanus* val_);\n");
  printf("extern void FF_to_c_float (object obj, ffloatjanus* val_);\n");
  printf("extern object c_double_to_DF (const dfloatjanus* val_);\n");
  printf("extern void DF_to_c_double (object obj, dfloatjanus* val_);\n");
  #ifdef DYNAMIC_FFI
    printf("extern void register_foreign_variable (void* address, const char * name, uintBWL flags, uintL size);\n");
    printf("extern void register_foreign_function (void* address, const char * name, uintWL flags);\n");
    printf("extern object convert_from_foreign (object fvd, const void* data);\n");
    printf("extern void convert_to_foreign_mallocing (object fvd, object obj, void* data);\n");
    printf("extern void convert_to_foreign_nomalloc (object fvd, object obj, void* data);\n");
  #endif
  #ifdef FOREIGN
    printf("#define FOREIGN %s\n",STRINGIFY(FOREIGN));
    emit_typedef("struct { XRECORD_HEADER void* fp_pointer;} *","Fpointer");
    printf("#define fpointerp(obj) (orecordp(obj) && (Record_type(obj) == %d))\n",Rectype_Fpointer);
    #ifdef TYPECODES
      printf("#define TheFpointer(obj)  ((Fpointer)("); printf_type_pointable(orecord_type); printf("))\n");
    #else
      printf("#define TheFpointer(obj)  ((Fpointer)(ngci_pointable(obj)-%d))\n",varobject_bias);
    #endif
    printf("extern object allocate_fpointer (FOREIGN foreign);\n");
    printf("#define fp_validp(ptr)  ((record_flags(ptr) & bit(7)) == 0)\n");
    printf("extern object check_fpointer_replacement (object obj, bool restart_p);\n");
    printf("static inline object check_fpointer (object obj, bool restart_p) {"
           "  if (!(fpointerp(obj) && fp_validp(TheFpointer(obj))))"
           "    obj = check_fpointer_replacement(obj,restart_p);"
           "  return obj;"
           "}\n");
  #endif
  printf("extern void* my_malloc (size_t size);\n");
  printf("#define unused %s\n",STRINGIFY(unused));
  printf("enum { seclass_foldable, seclass_no_se, seclass_read, seclass_write, seclass_default};\n");
  /* Additional stuff for modules. */
  printf("#define DEFMODULE(module_name,package_name)\n");
  printf("#define DEFUN(funname,lambdalist,signature) LISPFUN signature\n");
  printf("#define DEFUNF DEFUN\n");
  printf("#define DEFUNN DEFUN\n");
  printf("#define DEFUNR DEFUN\n");
  printf("#define DEFUNW DEFUN\n");
  printf("#define DEFUND DEFUN\n");
  printf("#define DEFVAR(varname)\n");
  /* done - check for errors, close test files &c */
  if (ferror(stdout)) exit(1);
  if (ferror(header_f)) exit(1);
  if (test_f) {
    fprintf(test_f,"  return 0;\n}\n");
    if (ferror(test_f)) exit(1);
    fclose(test_f);
    fprintf(stderr,"wrote %d tests\n",test_count);
  }
  exit(0);
}
