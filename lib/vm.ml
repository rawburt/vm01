type stack_obj =
  | SO_INT of int

type stack = stack_obj list

type opcode =
  | OP_PUSH of stack_obj
  | OP_ADD
  | OP_PRINT

type vm_error = StackSize

exception VM_ERROR of vm_error

let string_of_stack_obj = function
  | SO_INT i -> string_of_int i

let print_obj obj =
  print_endline (string_of_stack_obj obj)

let add left right =
  match left, right with
  | SO_INT a, SO_INT b -> SO_INT (a + b)

let pop = function
  | head :: stack -> (head, stack)
  | _ -> raise (VM_ERROR StackSize)

let run_program program =
  let len = Array.length(program) in
  let rec loop pc stack =
    if pc < len then
      match program.(pc) with
      | OP_PUSH obj ->
          loop (pc + 1) (obj :: stack)
      | OP_ADD ->
          let (l, s)  = pop stack in
          let (r, s') = pop s in
          loop (pc + 1) (add l r :: s')
      | OP_PRINT ->
          let (obj, s) = pop stack in
          print_obj obj;
          loop (pc + 1) s
    else
      ()
  in
  loop 0 []
