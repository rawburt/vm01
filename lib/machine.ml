type value =
  | Int of int
  | String of string
  | Bool of bool
  [@@deriving show]

type instruction =
  | Push of value
  | Pop
  | Time
  | Dup
  | Over
  | Swap
  | Print
  | Xor
  | Shr
  | Shl
  | Mod
  | Add
  | Input
  | Toint
  | Lt
  | Gt
  | Eq
  | Jump of int
  | Jumpif of int
  [@@deriving show]

let string_of_value = function
  | Int i -> string_of_int i
  | String s -> s
  | Bool b -> string_of_bool b

let print_value value =
  print_string (string_of_value value)

let time_op () =
  let time = Unix.time () in
  Int (int_of_float time)

exception Runtime_error of string

let xor_op a b =
  match a, b with
  | Int l, Int r -> Int (Int.logxor r l)
  | _ -> raise (Runtime_error "type error for xor")

let shr_op a b =
  match a, b with
  | Int l, Int r -> Int (Int.shift_right r l)
  | _ -> raise (Runtime_error "type error for shr")

let shl_op a b =
  match a, b with
  | Int l, Int r -> Int (Int.shift_left r l)
  | _ -> raise (Runtime_error "type error for shl")

let mod_op a b =
  match a, b with
  | Int l, Int r -> Int (r mod l)
  | _ -> raise (Runtime_error "type error for mod")

let add_op a b =
  match a, b with
  | Int l, Int r -> Int (r + l)
  | _ -> raise (Runtime_error "type error for add")

let toint_op = function
  | Int i -> Int i
  | String s -> begin
      try
        Int (int_of_string s)
      with Failure _ -> Int 0
  end
  | Bool b -> if b then Int 1 else Int 0

let lt_op a b =
  match a, b with
  | Int l, Int r -> Bool (l < r)
  | _ -> raise (Runtime_error "type error for lt")

let gt_op a b =
  match a, b with
  | Int l, Int r -> Bool (l > r)
  | _ -> raise (Runtime_error "type error for lt")

let eq_op a b = Bool (a = b)

let pop = function
  | head :: rest -> (head, rest)
  | [] -> raise (Runtime_error "stack is empty")

let binop f stack =
  let a, s = pop stack in
  let b, s' = pop s in
  (f a b :: s')

let print_debug stack instruction =
  List.iter (fun i -> print_value i; print_string "\n") stack;
  print_endline (show_instruction instruction);
  print_endline ""

let run debug program =
  let len = Array.length(program) in
  let rec loop pc stack =
    if pc >= len then () else begin
      if debug then print_debug stack program.(pc) else ();
      match program.(pc) with
      | Push v -> loop (pc + 1) (v :: stack)
      | Pop ->
          let _, s' = pop stack in
          loop (pc + 1) s'
      | Time -> loop (pc + 1) (time_op () :: stack)
      | Dup ->
          let head, _ = pop stack in
          loop (pc + 1) (head :: stack)
      | Over ->
          let _, s = pop stack in
          let n2, _ = pop s in
          loop (pc + 1) (n2 :: stack)
      | Swap ->
          let n1, s = pop stack in
          let n2, s' = pop s in
          loop (pc + 1) (n2 :: n1 :: s')
      | Print ->
          let head, s = pop stack in
          print_value head;
          loop (pc + 1) s;
      | Xor -> loop (pc + 1) (binop xor_op stack)
      | Shr -> loop (pc + 1) (binop shr_op stack)
      | Shl -> loop (pc + 1) (binop shl_op stack)
      | Mod -> loop (pc + 1) (binop mod_op stack)
      | Add -> loop (pc + 1) (binop add_op stack)
      | Input ->
          let input = read_line () in
          loop (pc + 1) (String input :: stack)
      | Toint ->
          let a, s = pop stack in
          loop (pc + 1) (toint_op a :: s)
      | Lt -> loop (pc + 1) (binop lt_op stack)
      | Gt -> loop (pc + 1) (binop gt_op stack)
      | Eq -> loop (pc + 1) (binop eq_op stack)
      | Jump i -> loop i stack
      | Jumpif i -> begin
          match stack with
          | Bool b :: rest ->
              let loc = if b then i else (pc + 1) in
              loop loc rest
          | _ -> raise (Runtime_error "type error for jumpif")
      end
    end
  in
  loop 0 []
