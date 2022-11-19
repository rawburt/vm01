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

let readfile filename =
  let finalize p = List.rev(p) in
  let program = ref [] in
  let chan = open_in filename in
  try
    while true do
      match parse (input_line chan) with
      | Some p -> program := p :: !program
      | None -> ()
    done;
    finalize (!program)
  with End_of_file ->
    close_in chan;
    finalize (!program)

let parse_file file =
  readfile file
