%{
  open Parsed_ast
%}

%token <string> WORD
%token <int> INT
%token COLON
%token EOF

%start <Parsed_ast.parsed_program> prog
%%

prog:
  | list(line) EOF { $1 }
  ;

line:
  | instruction { $1 }
  | label { $1 }
  ;

instruction:
  | WORD { Instruction ($1, None) }
  | WORD arg { Instruction ($1, Some $2) }
  ;

arg:
  | INT { ArgInt $1 }
  ;

label:
  | WORD COLON { Label $1 }
  ;
