# CCMP2C - C code macro preprocessor, generating C
# Bruno Haible 22.6.1997

# This preprocessor takes a text file containing macros of the form
#   ##define MACRONAME(macroarg,...)  expansion
# and generates some C code which prints the expansion.
# Example input:
#   ##define DEFACCESSOR(clas,name)  \
#     DEFREADER(clas,name)           \
#     DEFWRITER(clas,name)
#   ##define DEFREADER(clas,name) \
#     LISPFUNN(clas##_##name,2) \
#     { value1 = *get_slot_##clas##_##name(popSTACK()); }
#   ##define DEFWRITER(clas,name) \
#     LISPFUNN(clas##_##name##_setter,3) \
#     { var object newvalue = popSTACK(); \
#       value1 = *get_slot_##clas##_##name(popSTACK()) = newvalue1; }
#   DEFACCESSOR(ship,x)
# Example output (modulo #line statements): something like that:
#   static void emit_DEFACCESSOR(TEXT* clas, TEXT* name);
#   static void emit_DEFREADER(TEXT* clas, TEXT* name);
#   static void emit_DEFWRITER(TEXT* clas, TEXT* name);
#   static void emit_DEFACCESSOR(TEXT* clas, TEXT* name) {
#     emit("  "); emit_DEFREADER(clas,name); emit("           \n");
#     emit("  "); emit_DEFWRITER(clas,name);
#   }
#   static void emit_DEFREADER(TEXT* clas, TEXT* name) {
#     emit("  LISPFUNN("); emit(clas); emit("_"); emit(name); emit(",2) \n");
#     emit("  { value1 = *get_slot_"); emit(clas); emit("_"); emit(name); emit("(popSTACK()); }");
#   }
#   static void emit_DEFWRITER(TEXT* clas, TEXT* name) {
#     emit("  LISPFUNN("); emit(clas); emit("_"); emit(name); emit("_setter,3) \n");
#     emit("  { var object newvalue = popSTACK(); \n");
#     emit("    value1 = *get_slot_"); emit(clas); emit("_"); emit(name); emit("(popSTACK()) = newvalue1; }");
#   }
#   emit_DEFACCESSOR("ship","x"); emit("\n");


#define local static
#define global
#define var
#define loop  while (1)
#define until(exp)  while (!(exp))
typedef unsigned char  uintB;
typedef unsigned short  uintW;
typedef unsigned long  uintL;
typedef int  boolean;
#define FALSE 0
#define TRUE 1

#include <stdlib.h>
#include <string.h>

#include <stdio.h>
#define fopen_read_ascii  "r"
#define fopen_write_ascii  "w"
#define fputc  putc
#define fgetc  getc

#ifndef NULL
#define NULL ((void*)0)
#endif

#ifdef __cplusplus
extern "C" void exit(int);
#endif


#define MAXARGCOUNT  50  # maximum number of macro arguments
#define CPLUSPLUS_COMMENTS  1  # define as 1 if C++ style comments "//" shall be understood
#define COMMENT5_COMMENTS  1  # define as 1 if comment5 style comments "# " shall be understood
#define MAXFUNCLENGTH  20  # functions are split after this many statements

# get a line, terminated with '\n', or NULL if EOF is encountered
local unsigned char * get_line (fp)
  FILE * fp;
  {
    var int len = 1;
    var unsigned char * line = (unsigned char *) malloc(len);
    var int index = 0;
    loop
      {
        var int c = getc(fp);
        if (c==EOF) { free(line); return (unsigned char *) 0; }
        if (index >= len)
          { len = 2*len; line = (unsigned char *) realloc((void*)line,len); }
        if (!line) { fprintf(stderr,"Out of memory.\n"); exit(1); }
        line[index++] = c;
        if (c=='\n') break;
      }
    return line;
  }

# Simple strings

typedef struct { uintL length; unsigned char * data; } string;

# string comparison, returns TRUE when they are equal (not like strcmp!)
local boolean string_compare (s1,s2)
  string* s1;
  string* s2;
  { if (s1->length==s2->length)
      { var uintL count = s1->length;
        var unsigned char * p1 = s1->data;
        var unsigned char * p2 = s2->data;
        while (count > 0)
          { if (*p1 != *p2) return FALSE;
            p1++; p2++; count--;
          }
        return TRUE;
      }
    else
      { return FALSE; }
  }

# A list of strings

typedef struct { uintL index; uintL size; string* data; } stringlist;

local void stringlist_init (l)
  stringlist* l;
  {
    l->data = (string*) malloc((l->size=1)*sizeof(string));
    if (!l->data) { fprintf(stderr,"Out of memory.\n"); exit(1); }
    l->index = 0;
  }

local void stringlist_add (l,s)
  stringlist* l;
  string* s;
  {
    if (l->index >= l->size)
      {
        l->data = (string*) realloc((void*)l->data,(l->size=2*l->size+1)*sizeof(string));
        if (!l->data) { fprintf(stderr,"Out of memory.\n"); exit(1); }
      }
    l->data[l->index++] = *s;
  }

local boolean stringlist_lookup (l,s)
  stringlist* l;
  string* s;
  {
    var uintL i;
    for (i = l->index; i > 0; )
      {
        i--;
        if (string_compare(s,&l->data[i])) return TRUE;
      }
    return FALSE;
  }

# The list of globally defined macros

local stringlist macronames;

# A counter for temporary variable names

local int gensym_count = 0;

# During output, we split the code in many small functions, so that it's
# easier to compile. (Some C compilers have problems compiling functions
# with a lot of code and no if/for/do/while control structure because this
# function is a single huge "basic block".)

local int func_number = 0;      # highest number of function
local int func_statements = -1; # number of statements, -1 when func is closed
local stringlist main_output;   # lines for main(), including the control lines

local void finish_func ()
  {
    if (func_statements >= 0)
      { printf("}\n"); func_statements = -1; }
  }

local void new_statement ()
  {
    if (func_statements >= MAXFUNCLENGTH) { finish_func(); }
    if (func_statements < 0)
      {
        func_number++; func_statements = 0;
        printf("void main%d (TEXT* curr)\n",func_number);
        printf("{\n");
       {var string main_line;
        main_line.data = (unsigned char *) malloc(30);
        if (!main_line.data) { fprintf(stderr,"Out of memory.\n"); exit(1); }
        sprintf((char*)main_line.data,"$  main%d(&curr);",func_number);
        main_line.length = strlen((char*)main_line.data);
        stringlist_add(&main_output,&main_line);
      }}
    func_statements++;
  }

# tests whether a line contains a ##if/##else/##endif directive
local boolean control_line_p (line)
  unsigned char * line;
  {
    return (line[0]=='#' && line[1]=='#'
            && (   (   line[2]=='i'
                    && line[3]=='f'
                    && ((line[4]==' ' || line[4]=='\t')
                        || (    line[4]=='d'
                            && line[5]=='e'
                            && line[6]=='f'
                            && (line[7]==' ' || line[7]=='\t')
                           )
                        || (   line[4]=='n'
                            && line[5]=='d'
                            && line[6]=='e'
                            && line[7]=='f'
                            && (line[8]==' ' || line[8]=='\t')
                           )
                   )   )
                || (   line[2]=='e'
                    && line[3]=='l'
                    && (   (   line[4]=='s'
                            && line[5]=='e'
                            && (line[6]==' ' || line[6]=='\t' || line[6]=='\n')
                           )
                        || (   line[4]=='i'
                            && line[5]=='f'
                            && (line[6]==' ' || line[6]=='\t')
                           )
                   )   )
                || (   line[2]=='e'
                    && line[3]=='n'
                    && line[4]=='d'
                    && line[5]=='i'
                    && line[6]=='f'
                    && (line[7]==' ' || line[7]=='\t' || line[7]=='\n')
                   )
           )   );
  }

# tests whether a line contains a macro definition
local boolean define_line_p (line)
  unsigned char * line;
  {
    return (line[0]=='#' && line[1]=='#'
            && line[2]=='d'
            && line[3]=='e'
            && line[4]=='f'
            && line[5]=='i'
            && line[6]=='n'
            && line[7]=='e'
            && (line[8]==' ' || line[8]=='\t')
           );
  }

# reads the continuation lines of a macro definition line and returns
# a string containing the entire macro definition (with backslash-newline
# replaced by newline)
local void whole_definition (firstline,infile, result)
  unsigned char * firstline;
  FILE * infile;
  string * result;
  {
    var uintL size = 1;
    var unsigned char * whole = (unsigned char *) malloc(size);
    var uintL len = 0;
    var unsigned char * line = firstline;
    var uintL linelen;
    var boolean backslashed;
    loop
      {
        if (!whole) { fprintf(stderr,"Out of memory.\n"); exit(1); }
        linelen = 0; while (line[linelen] != '\n') { linelen++; }
        backslashed = (linelen > 0 && line[linelen-1]=='\\');
        if (backslashed) linelen--;
        until (size >= len + 1 + linelen)
          { whole = (unsigned char *) realloc((void*)whole,(size = 2*size+1));
            if (!whole) { fprintf(stderr,"Out of memory.\n"); exit(1); }
          }
        {var unsigned char * ptr = &whole[len];
         if (len > 0) { *ptr++ = '\n'; len++; }
         {var uintL i; for (i = 0; i < linelen; i++) ptr[i] = line[i]; }
         len += linelen;
        }
        free(line);
        if (!backslashed) break;
        line = get_line(infile);
        if (!line) { fprintf(stderr,"Encountered EOF during macro definition.\n"); exit(1); }
      }
    result->data = whole; result->length = len;
  }

# tokenisation of a buffer

typedef struct { unsigned char * buf;
                 uintL position;
                 uintL endindex;
               }
        BUFFILE;

local int next_char (infile)
  BUFFILE* infile;
  {
    if (infile->position == infile->endindex)
      { return EOF; }
    else
      { return infile->buf[infile->position++]; }
  }

local int peek_char (infile)
  BUFFILE* infile;
  {
    if (infile->position == infile->endindex)
      { return EOF; }
    else
      { return infile->buf[infile->position]; }
  }

enum token_type { eof, eofcomment, eol, ident, number, charconst, stringconst, sep, expr };

typedef struct { enum token_type type;
                 uintL startindex; # Startindex im Buffer
                 uintL endindex; # Endindex im Buffer
                 # bei sep (Operator/Separator):
                 uintB ch;
               }
        token_;
typedef token_* Token;

typedef struct { uintL index;
                 uintL size;
                 token_* data;
               }
        Tokens;

local void tokens_init (tokens)
  Tokens* tokens;
  { tokens->index = tokens->size = 0;
    tokens->data = (token_*) malloc((tokens->size=1000)*sizeof(token_));
  }

local void tokens_del (tokens)
  Tokens* tokens;
  { if (tokens->size > 0) free(tokens->data); }

# Holt das nächste Token:
# (Innerhalb von Präprozessor-Direktiven zählt Zeilenende als eigenes Token,
# und '#' leitet keine verschachtelte Präprozessor-Direktive ein.)
local Token nexttoken (infile,tokens,within_comment,within_prep_directive)
  BUFFILE* infile;
  Tokens* tokens;
  boolean within_comment;
  boolean within_prep_directive;
  { if (tokens->index >= tokens->size)
      { tokens->data =
          (tokens->size==0 ? (token_*) malloc((tokens->size=1)*sizeof(token_))
                           : (token_*) realloc((void*)tokens->data,(tokens->size=2*tokens->size+1)*sizeof(token_))
          );
        if (!tokens->data) { fprintf(stderr,"Out of memory.\n"); exit(1); }
      }
   {var Token token = &tokens->data[tokens->index]; # Platz fürs nächste Token
    var int c;
    if (within_comment) goto in_comment;
    restart:
      token = &tokens->data[tokens->index];
      token->startindex = infile->position;
      c = next_char(infile);
      switch (c)
        { case EOF:
            # EOF
            token->type = eof; goto fertig;
          case ' ': case '\v': case '\t':
            # Whitespace. überlesen
            goto restart;
          case '\n':
            # Zeilenende
            if (within_prep_directive)
              { token->type = eol; goto fertig; } # als Token zurück
              else
              { goto restart; } # überlesen
          case '\\':
            if (peek_char(infile)=='\n')
              # Zeilenende nach '\'. überlesen
              { next_char(infile); goto restart; }
              else
              goto separator;
          case '/':
            if (peek_char(infile) == '*')
              # Kommentar
              { next_char(infile);
               in_comment:
                loop { c = next_char(infile);
                       if (c==EOF)
                         { token->type = eofcomment; goto fertig; } # war: fflush(stdout); fprintf(stderr,"unterminated comment\n"); break;
                       if ((c=='*') && (peek_char(infile)=='/'))
                         { next_char(infile); break; }
                     }
                goto restart;
              }
              else
            #if CPLUSPLUS_COMMENTS
            if (peek_char(infile) == '/')
              # Kommentar
              { next_char(infile);
                do { c = next_char(infile); }
                   until (c==EOF || c=='\n');
                goto restart;
              }
              else
            #endif
              goto separator;
          case '*':
            if (peek_char(infile) == '/')
              # illegales Kommentar-Ende
              { fflush(stdout); fprintf(stderr,"end of comment without corresponding start of comment\n"); }
            goto separator;
          case '#':
            if (within_prep_directive)
              { goto separator; }
              else
            #if COMMENT5_COMMENTS
            if (peek_char(infile) == ' ')
              # Kommentar
              { next_char(infile);
                do { c = next_char(infile); }
                   until (c==EOF || c=='\n');
                goto restart;
              }
              else
            #endif
              { # Präprozessor-Anweisung.
                # Bis Zeilenende oder EOF lesen.
                loop
                  { var Token subtoken = nexttoken(infile,tokens,FALSE,TRUE);
                    if ((subtoken->type == eof) || (subtoken->type == eofcomment)
                        || (subtoken->type == eol))
                      break;
                  }
                goto restart; # und überlesen
              }
          case '.':
            c = peek_char(infile);
            if (!(((c>='0') && (c<='9')) || (c=='.'))) goto separator;
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            # Zahl. Weiterlesen, solange alphanumerisches Zeichen oder '.':
            loop
              { c = peek_char(infile);
                if (((c>='0') && (c<='9'))
                    || ((c>='A') && (c<='Z')) || ((c>='a') && (c<='z'))
                    || (c=='.')
                   )
                  { next_char(infile); }
                  else
                  break;
              }
            token->type = number; goto fertig;
          case '\'':
            # Character-Konstante
            loop
              { c = next_char(infile);
                if (c==EOF) { fflush(stdout); fprintf(stderr,"unterminated character constant\n"); break; }
                if (c=='\'') break;
                if (c=='\\') { c = next_char(infile); }
              }
            token->type = charconst; goto fertig;
          case '\"':
            # String-Konstante
            loop
              {
                c = next_char(infile);
                if (c==EOF) { fflush(stdout); fprintf(stderr,"unterminated string constant\n"); break; }
                if (c=='\"') break;
                if (c=='\\') { c = next_char(infile); }
              }
            token->type = stringconst; goto fertig;
          case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
          case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
          case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
          case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
          case 'Y': case 'Z':
          case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
          case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
          case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
          case 's': case 't': case 'u': case 'v': case 'w': case 'x':
          case 'y': case 'z':
          case '_':
            # Identifier. alles alphanumerische überlesen.
            loop
              { c = peek_char(infile);
                if (   ((c>='0') && (c<='9'))
                    || ((c>='A') && (c<='Z')) || ((c>='a') && (c<='z'))
                    || (c=='_')
                   )
                  { next_char(infile); }
                  else
                  break;
              }
            token->type = ident; goto fertig;
          default:
          separator:
            token->type = sep; token->ch = c; goto fertig;
        }
    fertig:
    token->endindex = infile->position;
    tokens->index++;
    return token;
  }}

# tokenifies a buffer and returns the list of tokens
local void tokenify (line,startindex,endindex,within_comment,within_prep_directive,tokens)
  unsigned char * line;
  uintL startindex;
  uintL endindex;
  boolean* within_comment;
  boolean within_prep_directive;
  Tokens* tokens;
  {
    var BUFFILE infile;
    infile.buf = line; infile.position = startindex; infile.endindex = endindex;
    loop
      { Token next = nexttoken(&infile,tokens,*within_comment,within_prep_directive);
        if (next->type==eof) { *within_comment = FALSE; break; }
        if (next->type==eofcomment) { *within_comment = TRUE; break; }
      }
    tokens->index--;
  }

# Emit an indentation.
local void emit_indent (indent)
  uintL indent;
  { while (indent > 0) { putchar(' '); indent--; } }

# Emit a string.
local void emit_string (s)
  string* s;
  {
    var uintL i;
    for (i = 0; i < s->length; i++) putchar(s->data[i]);
  }

# Emits a string containing a control directive.
# (Skip the first characters and the newlines. Terminate with a newline.)
local void emit_control_directive (s)
  string* s;
  {
    var uintL i;
    for (i = 1; i < s->length; i++)
      if (s->data[i] != '\n')
        putchar(s->data[i]);
    putchar('\n');
  }

# Emit the code for generating a literal string.
local void emit_literal (line,startindex,endindex,textvar,indent)
  unsigned char * line;
  uintL startindex;
  uintL endindex;
  char * textvar;
  uintL indent;
  {
    var uintL i;
   restart:
    if (startindex == endindex) return;
    emit_indent(indent);
    printf("TEXT_addstring(%s,\"",textvar);
    for (i = startindex; i < endindex; i++)
      { var unsigned char ch = line[i];
        if (ch == '\\' || ch == '"')
          { putchar('\\'); putchar(ch); }
        else if (ch >= ' ' && ch != '\'')
          { putchar(ch); }
        else if (ch == '\n')
          { printf("\\n"); printf("\");\n"); startindex = i+1; goto restart; }
        else if (ch == '\t')
          { printf("\\t"); }
        else if (ch == '\b')
          { printf("\\b"); }
        else if (ch == '\r')
          { printf("\\r"); }
        else if (ch == '\f')
          { printf("\\f"); }
        else if (ch == '\v')
          { printf("\\v"); }
        else
          { putchar('\\');
            putchar('0'+((ch/64)%8));
            putchar('0'+((ch/8)%8));
            putchar('0'+(ch%8));
          }
      }
    printf("\");\n");
  }

# Emit the code for generating a certain string.
local void emit_expansion (line,startindex,endindex,within_comment,macroargs,textvar,indent)
  unsigned char * line;
  uintL startindex;
  uintL endindex;
  boolean* within_comment;
  stringlist* macroargs;
  char * textvar;
  uintL indent;
  {
    var Tokens tokens;
    tokens_init(&tokens);
    # Tokenify the buffer.
    tokenify(line,startindex,endindex,within_comment,macroargs!=NULL,&tokens);
    # "#" and "##" eat their surrounding spaces.
    if (macroargs)
      { var uintL tokindex;
        for (tokindex = 0; tokindex < tokens.index; tokindex++)
          { var Token t = &tokens.data[tokindex];
            if (t->type == sep && t->ch == '#')
              { tokens.data[tokindex].startindex =
                  (tokindex > 0 ? tokens.data[tokindex-1].endindex : startindex);
                tokens.data[tokindex].endindex =
                  (tokindex < tokens.index-1 ? tokens.data[tokindex+1].startindex : endindex);
              }
          }
      }
    # Process the tokens. Only some ident tokens are active, and "#" tokens
    # are skipped.
    {
      var uintL currindex;
      var uintL tokindex;
      currindex = startindex;
      for (tokindex = 0; tokindex < tokens.index; tokindex++)
        { var Token t = &tokens.data[tokindex];
          if (t->type == sep && t->ch == '#' && macroargs)
            { emit_literal(line,currindex,t->startindex,textvar,indent);
              currindex = t->endindex;
            }
          else if (t->type == ident)
            { var string id;
              id.length = t->endindex - t->startindex;
              id.data = &line[t->startindex];
              if (macroargs && stringlist_lookup(macroargs,&id))
                { # Emit a reference to one of the macro arguments.
                  emit_literal(line,currindex,t->startindex,textvar,indent);
                  emit_indent(indent);
                  printf("TEXT_addtext(%s,",textvar);
                  printf("arg_"); emit_string(&id); printf(");\n");
                  currindex = t->endindex;
                }
              else if (stringlist_lookup(&macronames,&id)
                       && tokindex < tokens.index-1
                       && tokens.data[tokindex+1].type == sep
                       && tokens.data[tokindex+1].ch == '('
                      )
                { # Emit a macro call.
                  var uintL param_count = 0;
                  var struct param { uintL startindex; uintL endindex; char temp[12]; } params[MAXARGCOUNT];
                  emit_literal(line,currindex,t->startindex,textvar,indent);
                  tokindex += 2;
                  if (tokindex >= tokens.index) goto no_more_tokens;
                  if (tokens.data[tokindex].type == sep
                      && tokens.data[tokindex].ch == ')')
                    { # Macro call without arguments
                      emit_indent(indent);
                      printf("emit_"); emit_string(&id); printf("(%s);\n",textvar);
                    }
                  else
                    { # Macro call with arguments
                      loop
                        { if (param_count == MAXARGCOUNT)
                            { fprintf(stderr,"too many macro arguments\n"); exit(1); }
                          gensym_count++;
                          sprintf(params[param_count].temp,"&temp%04d",gensym_count);
                          params[param_count].startindex = params[param_count].endindex = tokens.data[tokindex].startindex;
                          { var uintL paren_count = 0;
                            loop
                              { if (tokens.data[tokindex].type == sep)
                                  { if (tokens.data[tokindex].ch == '(')
                                      { paren_count++; }
                                    else if (tokens.data[tokindex].ch == ')')
                                      { if (paren_count == 0) break; else paren_count--; }
                                    else if (tokens.data[tokindex].ch == ',')
                                      { if (paren_count == 0) break; }
                                  }
                                params[param_count].endindex = tokens.data[tokindex].endindex;
                                tokindex++;
                                if (tokindex >= tokens.index) goto no_more_tokens;
                          }   }
                          param_count++;
                          if (tokens.data[tokindex].ch == ')') break;
                          tokindex++;
                          if (tokindex >= tokens.index) goto no_more_tokens;
                        }
                      if (FALSE)
                        { no_more_tokens:
                            fprintf(stderr,"unterminated macro call\n"); exit(1);
                        }
                      { var uintL i;
                        for (i = 0; i < param_count; i++)
                          { var boolean dummy = FALSE;
                            emit_indent(indent); printf("{\n");
                            indent += 2;
                            emit_indent(indent); printf("TEXT %s;\n",&params[i].temp[1]);
                            emit_indent(indent); printf("TEXT_init(%s);\n",params[i].temp);
                            emit_expansion(line,params[i].startindex,params[i].endindex,&dummy,macroargs,params[i].temp,indent);
                          }
                        emit_indent(indent);
                        printf("emit_"); emit_string(&id); printf("(%s",textvar);
                        for (i = 0; i < param_count; i++)
                          { printf(",%s",params[i].temp); }
                        printf(");\n");
                        for (i = param_count; i > 0; )
                          { i--;
                            emit_indent(indent); printf("TEXT_del(%s);\n",params[i].temp);
                            indent -= 2;
                            emit_indent(indent); printf("}\n");
                          }
                      }
                    }
                  currindex = tokens.data[tokindex].endindex;
                }
            }
        }
      emit_literal(line,currindex,endindex,textvar,indent);
    }
    tokens_del(&tokens);
  }

global int main (argc,argv)
  int argc;
  char* argv[];
{
  if (argc != 2)
    {
      fprintf(stderr,"Usage: %s inputfile\n",argv[0]);
      exit(1);
    }
  {
    var char* infilename = argv[1];
    var FILE* infile;
    # Emit prologue.
    printf("#include <stdio.h>\n");
    printf("#include <stdlib.h>\n");
    printf("#include <string.h>\n");
    printf("#ifdef __cplusplus\n");
    printf("extern \"C\" void exit(int);\n");
    printf("#endif\n");
    printf("\n");
    printf("typedef struct { FILE* file; char* buf; int buflen; int len; } TEXT;\n");
    printf("static void TEXT_init1 (TEXT* text, FILE* file)\n");
    printf("{\n");
    printf("  text->file = file;\n");
    printf("}\n");
    printf("static void TEXT_init (TEXT* text)\n");
    printf("{\n");
    printf("  text->file = (FILE*)0;\n");
    printf("  text->buf = (char*)malloc(text->buflen=1);\n");
    printf("  if (!text->buf) { fprintf(stderr,\"Out of memory.\\n\"); exit(1); }\n");
    printf("  text->buf[text->len = 0] = '\\0';\n");
    printf("}\n");
    printf("static void TEXT_del (TEXT* text)\n");
    printf("{\n");
    printf("  if (!text->file) { free(text->buf); }\n");
    printf("}\n");
    printf("static void TEXT_addstring (TEXT* text, char* s)\n");
    printf("{\n");
    printf("  if (text->file)\n");
    printf("    { fprintf(text->file,\"%%s\",s); }\n");
    printf("  else\n");
    printf("    {\n");
    printf("      int slen = strlen(s);\n");
    printf("      int newlen = text->len + slen + 1;\n");
    printf("      if (newlen > text->buflen)\n");
    printf("        { text->buf = (char*)realloc(text->buf,text->buflen=2*newlen);\n");
    printf("          if (!text->buf) { fprintf(stderr,\"Out of memory.\\n\"); exit(1); }\n");
    printf("        }\n");
    printf("      strcpy(text->buf+text->len,s);\n");
    printf("      text->len += slen;\n");
    printf("    }\n");
    printf("}\n");
    printf("static void TEXT_addtext (TEXT* text, TEXT* s)\n");
    printf("{\n");
    printf("  if (s->file)\n");
    printf("    { abort(); }\n");
    printf("  else\n");
    printf("    { TEXT_addstring(text,s->buf); }\n");
    printf("}\n");
    # Emit forward declarations.
    stringlist_init(&macronames);
    {
      if ((infile = fopen(infilename,fopen_read_ascii))==NULL) { exit(1); }
      while (1)
        {
          var unsigned char * line = get_line(infile);
          if (!line) break;
          if (define_line_p(line))
            {
              var string definition;
              var boolean within_comment;
              var Tokens tokens;
              var string id;
              var uintL tokindex;
              var stringlist macroargs;
              whole_definition(line,infile,&definition);
              within_comment = FALSE;
              tokens_init(&tokens);
              tokenify(definition.data,9,definition.length,&within_comment,TRUE,&tokens);
              if (tokens.index == 0) { fprintf(stderr,"macro name missing\n"); exit(1); }
              if (tokens.data[0].type != ident) { fprintf(stderr,"bad macro name\n"); exit(1); }
              id.length = tokens.data[0].endindex - tokens.data[0].startindex;
              id.data = &definition.data[tokens.data[0].startindex];
              stringlist_add(&macronames,&id);
              stringlist_init(&macroargs);
              tokindex = 1;
              if (!(tokindex < tokens.index
                    && tokens.data[tokindex].type == sep
                    && tokens.data[tokindex].ch == '('
                 ) )
                { fprintf(stderr,"macro parameter list missing\n"); exit(1); }
              loop
                { tokindex++;
                  if (!(tokindex < tokens.index)) { fprintf(stderr,"unterminated macro parameter list\n"); exit(1); }
                  if (tokindex==2 && tokens.data[tokindex].type == sep && tokens.data[tokindex].ch == ')') break;
                  if (!(tokens.data[tokindex].type == ident)) { fprintf(stderr,"missing parameter name in macro parameter list\n"); exit(1); }
                  { var string pid;
                    pid.length = tokens.data[tokindex].endindex - tokens.data[tokindex].startindex;
                    pid.data = &definition.data[tokens.data[tokindex].startindex];
                    stringlist_add(&macroargs,&pid);
                  }
                  tokindex++;
                  if (!(tokindex < tokens.index)) { fprintf(stderr,"unterminated macro parameter list\n"); exit(1); }
                  if (tokens.data[tokindex].type == sep && tokens.data[tokindex].ch == ')') break;
                  if (!(tokens.data[tokindex].type == sep && tokens.data[tokindex].ch == ','))
                    { fprintf(stderr,"syntax error in macro parameter list\n"); exit(1); }
                }
              tokens_del(&tokens);
              printf("static void emit_"); emit_string(&id); printf(" (TEXT* curr");
              { var uintL i;
                for (i = 0; i < macroargs.index; i++)
                  { printf(", TEXT* arg_"); emit_string(&macroargs.data[i]); }
              }
              printf(");\n");
            }
          else if (control_line_p(line))
            {
              var string definition;
              whole_definition(line,infile,&definition);
              emit_control_directive(&definition);
              free(definition.data);
            }
          else
            { free(line); }
        }
      fclose(infile);
    }
    # Process file and emit most of the stuff.
    stringlist_init(&main_output);
    {
      var boolean within_comment = FALSE;
      if ((infile = fopen(infilename,fopen_read_ascii))==NULL) { exit(1); }
      while (1)
        {
          var unsigned char * line = get_line(infile);
          if (!line) break;
          if (define_line_p(line))
            {
              var string definition;
              var boolean dummy;
              var Tokens tokens;
              var string id;
              var uintL tokindex;
              var stringlist macroargs;
              var uintL expansion_startindex;
              whole_definition(line,infile,&definition);
              dummy = FALSE;
              tokens_init(&tokens);
              tokenify(definition.data,9,definition.length,&dummy,TRUE,&tokens);
              if (tokens.index == 0) { fprintf(stderr,"macro name missing\n"); exit(1); }
              if (tokens.data[0].type != ident) { fprintf(stderr,"bad macro name\n"); exit(1); }
              id.length = tokens.data[0].endindex - tokens.data[0].startindex;
              id.data = &definition.data[tokens.data[0].startindex];
              stringlist_init(&macroargs);
              tokindex = 1;
              if (!(tokindex < tokens.index
                    && tokens.data[tokindex].type == sep
                    && tokens.data[tokindex].ch == '('
                 ) )
                { fprintf(stderr,"macro parameter list missing\n"); exit(1); }
              loop
                { tokindex++;
                  if (!(tokindex < tokens.index)) { fprintf(stderr,"unterminated macro parameter list\n"); exit(1); }
                  if (tokindex==2 && tokens.data[tokindex].type == sep && tokens.data[tokindex].ch == ')') break;
                  if (!(tokens.data[tokindex].type == ident)) { fprintf(stderr,"missing parameter name in macro parameter list\n"); exit(1); }
                  { var string pid;
                    pid.length = tokens.data[tokindex].endindex - tokens.data[tokindex].startindex;
                    pid.data = &definition.data[tokens.data[tokindex].startindex];
                    stringlist_add(&macroargs,&pid);
                  }
                  tokindex++;
                  if (!(tokindex < tokens.index)) { fprintf(stderr,"unterminated macro parameter list\n"); exit(1); }
                  if (tokens.data[tokindex].type == sep && tokens.data[tokindex].ch == ')') break;
                  if (!(tokens.data[tokindex].type == sep && tokens.data[tokindex].ch == ','))
                    { fprintf(stderr,"syntax error in macro parameter list\n"); exit(1); }
                }
              tokindex++;
              expansion_startindex = (tokindex < tokens.index ? tokens.data[tokindex].startindex : definition.length);
              tokens_del(&tokens);
              finish_func();
              printf("static void emit_"); emit_string(&id); printf("(TEXT* curr");
              { var uintL i;
                for (i = 0; i < macroargs.index; i++)
                  { printf(", TEXT* arg_"); emit_string(&macroargs.data[i]); }
              }
              printf(")\n");
              printf("{\n");
              dummy = FALSE;
              emit_expansion(definition.data,expansion_startindex,definition.length,&dummy,&macroargs,"curr",2);
              printf("}\n");
            }
          else if (control_line_p(line))
            { # Control directive.
              var string definition;
              whole_definition(line,infile,&definition);
              finish_func();
              emit_control_directive(&definition);
              stringlist_add(&main_output,&definition);
            }
          else
            { # Normal text.
              var uintL linelen;
              linelen = 0; while (line[linelen] != '\n') { linelen++; }
              new_statement();
              emit_expansion (line,0,linelen+1,&within_comment,NULL,"curr",2);
              free(line);
            }
        }
      if (within_comment) { fflush(stdout); fprintf(stderr,"unterminated comment\n"); exit(1); }
      fclose(infile);
    }
    finish_func();
    # Emit main function.
    printf("int main () {\n");
    printf("  TEXT curr;\n");
    printf("  TEXT_init1(&curr,stdout);\n");
    {
      var uintL i;
      for (i = 0; i < main_output.index; i++)
        emit_control_directive(&main_output.data[i]);
    }
    printf("  if (ferror(stdout)) { exit(1); }\n");
    printf("  exit(0);\n");
    printf("}\n");
    # Done.
    if (ferror(stdout)) { exit(1); }
    exit(0);
  }
}
