type value =
  | VInt of int
  | VBool of bool

type opcode =
  | OPush of value
  | OTime
  | ODup
