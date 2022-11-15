module Parser = Parser
module Lexer = Lexer
module Parsed_ast = Parsed_ast

let parse s = 
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
