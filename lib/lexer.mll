{
  open Parser
}

let word = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']* 

rule read =
  parse
  | "#" [^'\n']* '\n' { Lexing.new_line lexbuf; read lexbuf }
  | '\n'+ { NEWLINE }
  | [' ' '\t'] { read lexbuf }
  | ':' { COLON }
  | ['0'-'9']+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | word { WORD (Lexing.lexeme lexbuf) }
  | eof { EOF }
