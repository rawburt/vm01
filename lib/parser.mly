%{
  open Parsed_ast
%}

%token <int> INT
%token <string> LABEL
%token <string> INSTRUCTION
%token TRUE
%token FALSE
%token NEWLINE
%token EOF

%start <Parsed_ast.p_ast list> prog
%%

prog:
  | e = expression_list; EOF { e }
  ;

expression_list:
  | e = expression { [e] }
  | e = expression; NEWLINE; es = expression_list { e :: es }
  ;

expression:
  | l = LABEL { PAstLabel l }
  | i = INSTRUCTION; v = value { PAstInstruction (i, Some (v)) }
  | i = INSTRUCTION { PAstInstruction (i, None) }
  ;

value:
  | i = INT { PValInt i }
  | TRUE { PValBool true }
  | FALSE { PValBool false }
  ;
