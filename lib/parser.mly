%{
  open Parsed_ast
%}

%token <string> WORD
%token <int> INT
%token <string> STRING
%token COLON
%token EOF

%type <ast> line label instruction
%type <arg> arg

%start <ast option> prog
%%

prog:
  | line EOF { Some $1 }
  | EOF { None }
  ;
line:
  | label { $1 }
  | instruction { $1 }
  ;
label:
  | WORD COLON { Label $1 }
  ;
instruction:
  | WORD arg { Instruction ($1, Some $2) }
  | WORD { Instruction ($1, None) }
  ;
arg:
  | INT { ArgInt $1 }
  | WORD { ArgLabel $1 }
  | STRING { ArgString $1 }
  ;
