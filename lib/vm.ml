type stack_obj =
  | SO_INT of int
  | SO_BOOL of bool

type stack = stack_obj list

type opcode =
  | OP_PUSH of stack_obj
  | OP_POP
  | OP_ADD
  | OP_AND
  | OP_OR
  | OP_PRINT

type program = opcode array

type vm_state = {
  stack : stack ref;
  program : program;
  counter : int ref;
}

let run_program () = print_endline "ok"
