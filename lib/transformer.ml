module P = Parsed_ast
module O = Opcode

exception TransformError of string

let mk_val = function
  | None -> raise (TransformError "missing value")
  | Some v ->
      match v with
      | P.PValInt i -> O.VInt i
      | P.PValBool b -> O.VBool b

let mk_instruction arg = function
  | "time" -> O.OTime
  | "dup" -> O.ODup
  | "push" -> O.OPush (mk_val arg)
  | _ as i -> raise (TransformError ("unexpected instruction: " ^ i))

let rec transform parsed_ast =
  match parsed_ast with
  | [] -> []
  | P.PAstInstruction (name, arg) :: rest -> 
      mk_instruction arg name :: transform rest
  | P.PAstLabel _ :: rest -> transform rest
