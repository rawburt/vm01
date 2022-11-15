type value =
  | VInt of int
  | VBool of bool
  | VString of string

type opcode =
  | OPush of value
  | OPop
  | OPrint

type program = opcode array

exception VmError of string

let string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VString s -> s

let pop = function
  | h :: stack -> (h, stack)
  | _ -> raise (VmError "stack size too small")

let run_program program =
  let len = Array.length(program) in
  let rec loop pc stack =
    if pc >= len then () else
    match program.(pc) with
    | OPush v -> loop (pc + 1) (v :: stack)
    | OPop -> let _, s = pop stack in loop (pc + 1) s
    | OPrint ->
        let v, s = pop stack in
        print_string (string_of_value v);
        loop (pc + 1) s
  in
  loop 0 []
