{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
  | '\n'+ { NEWLINE }
  | white { read lexbuf }
  | id { WORD (Lexing.lexeme lexbuf) }
  | eof { EOF }
