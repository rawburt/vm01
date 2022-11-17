type ast = 
  | Instruction of string
  [@@deriving show]

type parsed_program = ast list
[@@deriving show]
