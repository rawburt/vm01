%{
  open Parsed_ast
%}

%token <int> INT
%token <string> LABEL
%token <string> INSTRUCTION
%token NEWLINE
%token EOF

%start <Parsed_ast.parsed_program> prog
%%

prog:
  | e = expression_list; EOF { e }
  ;

expression_list:
  | e = expression { [e] }
  | e = expression; NEWLINE; es = expression_list { e :: es }
  ;

expression:
  | l = label { AstLabel l }
  | i = INSTRUCTION; v = instruction_value { AstInstruction (ArgInstruction (i, v)) }
  | i = INSTRUCTION { AstInstruction (BasicInstruction i) }
  ;

instruction_value:
  | l = label { ArgLabel l }
  | v = value { ArgVal v }
  ;

label:
  | l = LABEL { Label l }
  ;

value:
  | i = INT { Int i }
  ;
