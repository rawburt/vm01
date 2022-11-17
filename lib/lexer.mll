{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let label = letter+ ':'

rule read =
  parse
  | '\n'+ { NEWLINE }
  | white { read lexbuf }
  | label { LABEL (Lexing.lexeme lexbuf) }
  | id { INSTRUCTION (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
