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
  | Mod
  [@@deriving show]

let string_of_value = function
  | Int i -> string_of_int i

let print_value value =
  print_string (string_of_value value)

let time_op () =
  let time = Unix.time () in
  Int (int_of_float time)

let xor_op a b =
  match a, b with
  | Int l, Int r -> Int (Int.logxor r l)

let shr_op a b =
  match a, b with
  | Int l, Int r -> Int (Int.shift_right r l)

let shl_op a b =
  match a, b with
  | Int l, Int r -> Int (Int.shift_left r l)

let mod_op a b =
  match a, b with
  | Int l, Int r -> Int (r mod l)

exception Runtime_error of string

let pop = function
  | head :: rest -> (head, rest)
  | [] -> raise (Runtime_error "stack is empty")

let print_stack stack =
  List.iter (fun i -> print_value i; print_string "\n") stack;
  print_endline ""

let run debug program =
  let len = Array.length(program) in
  let rec loop pc stack =
    if pc >= len then () else begin
      if debug then print_stack stack else ();
      match program.(pc) with
      | Push v -> loop (pc + 1) (v :: stack)
      | Time -> loop (pc + 1) (time_op () :: stack)
      | Dup ->
          let head, _ = pop stack in
          loop (pc + 1) (head :: stack)
      | Print ->
          let head, _ = pop stack in
          print_value head;
          loop (pc + 1) stack
      | Xor ->
          let a, s = pop stack in
          let b, s' = pop s in
          loop (pc + 1) (xor_op a b :: s')
      | Shr ->
          let a, s = pop stack in
          let b, s' = pop s in
          loop (pc + 1) (shr_op a b :: s')
      | Shl ->
          let a, s = pop stack in
          let b, s' = pop s in
          loop (pc + 1) (shl_op a b :: s')
      | Mod ->
          let a, s = pop stack in
          let b, s' = pop s in
          loop (pc + 1) (mod_op a b :: s')
    end
  in
  loop 0 []
