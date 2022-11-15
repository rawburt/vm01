type p_val =
  | PValInt of int
  | PValBool of bool
  | PValString of string

type p_ast =
  | PAstLabel of string
  | PAstInstruction of string * p_val option
