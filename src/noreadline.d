# Ein Ersatz für die GNU readline()-Library.
# Bruno Haible 1992-1999

# These are the only things we need from lispbibl.c :
#define global
#define local static
#define var
#ifdef __cplusplus
  #define NULL  0
#else
  #define NULL  ((char*) 0L)
#endif

typedef int Function ();
typedef void VFunction ();
typedef char *CPFunction ();
typedef char **CPPFunction ();

global int rl_present_p = 0; # readline()-Library nicht vorhanden

global char* rl_readline_name;
global CPPFunction* rl_attempted_completion_function;
global CPFunction* rl_completion_entry_function;

global char* rl_basic_word_break_characters;
global char* rl_basic_quote_characters;
global char* rl_completer_quote_characters;

global char* rl_line_buffer;
global int rl_already_prompted;

global char* readline(prompt)
  var char* prompt;
  { return NULL; }

global void rl_deprep_terminal()
  { ; }

global char* filename_completion_function(text,state)
  var char* text;
  var int state;
  { return NULL; }

global void add_history(line)
  var char* line;
  { ; }

global VFunction* rl_named_function(string)
  var char* string;
  { return NULL; }

global int rl_bind_key(key,function)
  var int key;
  var VFunction* function;
  { return 0; }

