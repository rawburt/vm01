type stack_obj =
  | SO_INT of int

type stack = stack_obj list

type opcode =
  | OP_PUSH of stack_obj
  | OP_ADD
  | OP_PRINT

type vm_error = StackSize

exception VM_ERROR of vm_error

type vm_state = {
  program : opcode array;
  mutable stack : stack;
  mutable pc : int;
}

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

let run_program vm =
  let len = Array.length(vm.program) in
  while vm.pc < len do
    match vm.program.(vm.pc) with
    | OP_PUSH obj ->
        vm.stack <- obj :: vm.stack;
        vm.pc <- vm.pc + 1;
    | OP_ADD ->
        let (a, s) = pop vm.stack in
        let (b, s') = pop s in
        vm.stack <- add a b :: s';
        vm.pc <- vm.pc + 1;
    | OP_PRINT ->
        let (a, stack) = pop vm.stack in
        print_obj a;
        vm.stack <- stack;
        vm.pc <- vm.pc + 1;
  done
