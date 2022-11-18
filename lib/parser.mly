%{
  open Parsed_ast
%}

%token <string> WORD
%token <int> INT
%token NEWLINE
%token COLON
%token EOF

%type <ast> line label instruction
%type <arg> arg

%start <parsed_program> prog
%%

prog:
  | NEWLINE* l=separated_nonempty_list(NEWLINE*, line) EOF { l }
  ;
line:
  | label { $1 }
  | instruction { $1 }
  ;
label:
  | WORD COLON { Label $1 }
  ;
instruction:
  | WORD a=option(arg) { Instruction ($1, a) }
  ;
arg:
  | INT { ArgInt $1 }
  | WORD { ArgLabel $1 }
  ;
