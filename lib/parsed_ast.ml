type arg = ArgInt of int
[@@deriving show]

type ast = 
  | Instruction of string * arg option
  | Label of string
  [@@deriving show]

type parsed_program = ast list
[@@deriving show]
