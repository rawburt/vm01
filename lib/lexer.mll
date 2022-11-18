{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let newline = '\r' | '\n' | "\r\n"
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
  | newline+ { NEWLINE }
  | white { read lexbuf }
  | id { WORD (Lexing.lexeme lexbuf) }
  | eof { EOF }
