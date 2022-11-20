type value = 
  | Int of int
  [@@deriving show]

type instruction =
  | Push of value
  | Time
  | Dup
  | Print
  | Xor
  | Shr
  | Shl
  [@@deriving show]
