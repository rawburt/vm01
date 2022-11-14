type stack_obj =
  | SO_BOOL of bool
  | SO_INT of int

type stack = stack_obj list

type opcode =
  | OP_PUSH of stack_obj
  | OP_PRINT
  | OP_AND
  | OP_ADD

type vm_error = StackSize | ArgumentType

exception VM_ERROR of vm_error

let string_of_stack_obj = function
  | SO_BOOL b -> string_of_bool b
  | SO_INT i -> string_of_int i

let print_op obj =
  print_endline (string_of_stack_obj obj)

let and_op left right =
  match left, right with
  | SO_BOOL a, SO_BOOL b -> SO_BOOL (a && b)
  | _ -> raise (VM_ERROR ArgumentType)

let add_op left right =
  match left, right with
  | SO_INT a, SO_INT b -> SO_INT (a + b)
  | _ -> raise (VM_ERROR ArgumentType)

let pop = function
  | head :: stack -> (head, stack)
  | _ -> raise (VM_ERROR StackSize)

let pop2 stack =
  let (a, s)  = pop stack in
  let (b, s') = pop s in
  (a, b, s')

let run_program program =
  let len = Array.length(program) in
  let rec loop pc stack =
    if pc < len then
      match program.(pc) with
      | OP_PUSH obj ->
          loop (pc + 1) (obj :: stack)
      | OP_AND ->
          let (l, r, s) = pop2 stack in
          loop (pc + 1) (and_op l r :: s)
      | OP_ADD ->
          let (l, r, s) = pop2 stack in
          loop (pc + 1) (add_op l r :: s)
      | OP_PRINT ->
          let (obj, s) = pop stack in
          print_op obj;
          loop (pc + 1) s
    else
      ()
  in
  loop 0 []
