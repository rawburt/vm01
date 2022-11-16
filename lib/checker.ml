module P = Parsed_ast

exception CheckerError of string

let instructions =
  [
    ("time", false);
    ("dup", false);
    ("push", true);
    ("shr", false);
    ("xor", false);
    ("print", false)
  ]

let check = function
  | P.PAstInstruction (name, arg_opt) -> begin
      let i = List.find_opt (fun (n, _) -> n = name) instructions in
      match i with
      | None -> raise (CheckerError ("unknown instruction: " ^ name))
      | Some (_, arg) -> begin
          match arg_opt with
          | None -> 
              if arg 
              then raise (CheckerError ("expected arg: " ^ name) )
              else true
          | Some _ ->
              if arg then true
              else raise (CheckerError ("unexpected arg: " ^ name))
      end
  end
  | P.PAstLabel _ -> true

let checker ast = List.for_all check ast
