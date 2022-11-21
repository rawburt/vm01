{
  open Parser

  let string_buff = Buffer.create 256

  let char_for_backslash = function
    | 'n' -> '\010'
    | 'r' -> '\013'
    | 'b' -> '\008'
    | 't' -> '\009'
    | c   -> c
}

let word = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let backslash_escapes = ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

rule read = parse
  | "#" [^'\n']* '\n' { Lexing.new_line lexbuf; read lexbuf }
  | "#" [^'\n']* eof { EOF }
  | '\n'+ { Lexing.new_line lexbuf; read lexbuf }
  | [' ' '\t'] { read lexbuf }
  | ':' { COLON }
  | '"' { Buffer.clear string_buff;
          string lexbuf;
          STRING (Buffer.contents string_buff) }
  | ['0'-'9']+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | word { WORD (Lexing.lexeme lexbuf) }
  | eof { EOF }
and string = parse
  | '"' { () }
  | '\\' (backslash_escapes as c) { Buffer.add_char string_buff (char_for_backslash c);
                                    string lexbuf }
  | _ as c { Buffer.add_char string_buff c; string lexbuf }
