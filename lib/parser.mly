%{
  open Parsed_ast
%}

%token <string> WORD
%token NEWLINE
%token EOF

%start <Parsed_ast.parsed_program> prog
%%

prog:
  | w = word_list; EOF { w }
  ;

word_list:
  | w = word { [w] }
  | w = word; NEWLINE; ws = word_list { w :: ws }
  ;

word:
  | i = WORD { Instruction i }
  ;

