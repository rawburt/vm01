type value =
  | VInt of int
  | VBool of bool
  | VString of string

type opcode =
  | OPush of value
  | OPop
  | ODup
  | OPrint
  | OTime
  | OMul
  | OMod
  | OShl
  | OShr
  | OXor

type program = opcode array

exception VmError of string

let string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VString s -> s

let mul_op = function
  | VInt l, VInt r -> VInt (l * r)
  | _ -> raise (VmError "type error")

let mod_op = function
  | VInt l, VInt r -> VInt (l mod r)
  | _ -> raise (VmError "type error")

let shl_op = function
  | VInt l, VInt r -> VInt (Int.shift_left l r)
  | _ -> raise (VmError "type error")

let shr_op = function
  | VInt l, VInt r -> VInt (Int.shift_right l r)
  | _ -> raise (VmError "type error")

let xor_op = function
  | VInt l, VInt r -> VInt (Int.logxor l r)
  | _ -> raise (VmError "type error")

let push v stack =
  print_endline ("push: " ^ (string_of_value v));
  (v :: stack)

let pop = function
  | h :: stack -> 
      print_endline ("pop: " ^ (string_of_value h));
      (h, stack)
  | _ -> raise (VmError "stack size too small")

let pop2 stack =
  let (l, s)  = pop stack in
  let (r, s') = pop s in
  (l, r, s')

let run_program program =
  let len = Array.length(program) in
  let rec loop pc stack =
    if pc >= len then () else
      let binop f stack =
        let (l, r, s) = pop2 stack in
        let m = f (l, r) in
        loop (pc + 1) (push m s)
      in
      match program.(pc) with
      | OPush v -> loop (pc + 1) (push v stack)
      | OPop -> let _, s = pop stack in loop (pc + 1) s
      | ODup -> 
          let (v, s) = pop stack in
          let s' = push v (push v s) in
          loop (pc + 1) s'
      | OPrint ->
          let v, s = pop stack in
          print_string (string_of_value v);
          loop (pc + 1) s
      | OTime ->
          let t = int_of_float (Unix.time ()) in
          loop (pc + 1) (push (VInt t) stack)
      | OMul -> binop mul_op stack
      | OMod -> binop mod_op stack
      | OShl -> binop shl_op stack
      | OShr -> binop shr_op stack
      | OXor -> binop xor_op stack
  in
  loop 0 []
