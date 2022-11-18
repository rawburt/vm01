module Parsed_ast = Parsed_ast

let parse s = 
  let lexbuf = Lexing.from_string s in
  try
    let ast = Parser.prog Lexer.read lexbuf in
    ast
  with _ as e -> 
    print_endline "ERROR"; 
    string_of_int lexbuf.lex_curr_p.pos_lnum |> print_endline;
    string_of_int (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol) |> print_endline;
    raise e
