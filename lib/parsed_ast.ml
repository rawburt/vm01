type value =
  | Int of int
  | Bool of bool
  [@@deriving show]

type label = Label of string
[@@deriving show]

type arg = 
  | ArgLabel of label 
  | ArgVal of value
  [@@deriving show]

type instruction = 
  | BasicInstruction of string 
  | ArgInstruction of string * arg
  [@@deriving show]

type ast = 
  | AstLabel of label 
  | AstInstruction of instruction
  [@@deriving show]

type parsed_program = ast list
[@@deriving show]
