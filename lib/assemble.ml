module P = Parsed_ast
module M = Machine

let extract_labels ast =
  let rec loop i labels left = function
    | P.Label name :: rest ->
        loop i ((name, i) :: labels) left rest
    | instruction :: rest ->
        loop (i + 1) labels (instruction :: left) rest
    | [] -> (labels, left)
  in
  let labels, ast' = loop 0 [] [] ast in
  (labels, List.rev ast')

exception Assemble_error of string

let rec find_label_loc name = function
  | (n, i) :: rest ->
      if n = name
      then i
      else find_label_loc name rest
  | [] -> raise (Assemble_error ("unknown label: " ^ name))

let rec replace_labels labels = function
  | [] -> []
  | P.Label _ :: _ -> raise (Assemble_error "unexpected label")
  | head :: rest ->
      let head' =
        match head with
        | P.Instruction (ins, Some (P.ArgLabel name)) ->
            let id = find_label_loc name labels in
            P.Instruction (ins, Some (P.ArgInt id))
        | _ -> head
      in
      head' :: (replace_labels labels rest)

let mk_value = function
  | P.ArgLabel _ -> raise (Assemble_error "unexpected label")
  | P.ArgInt i -> M.Int i
  | P.ArgString s -> M.String s

let mk_push = function
  | Some v -> M.Push (mk_value v)
  | None -> raise (Assemble_error "expected arg for instruction: push")

let mk_command = function
  | "time" -> M.Time
  | "dup" -> M.Dup
  | "print" -> M.Print
  | "xor" -> M.Xor
  | "shr" -> M.Shr
  | "shl" -> M.Shl
  | "mod" -> M.Mod
  | "input" -> M.Input
  | "toint" -> M.Toint
  | _ as name -> raise (Assemble_error ("unknown instruction: " ^ name))

let mk_instruction = function
  | P.Instruction (name, arg) -> begin
      match name with
      | "push" -> mk_push arg
      | _ ->
          if Option.is_some arg
          then raise (Assemble_error ("unexpected arg for instruction: " ^ name))
          else mk_command name
  end
  | P.Label _ -> raise (Assemble_error "unexpected label")

let assemble ast =
  let labels, ast' = extract_labels ast in
  let ast'' = replace_labels labels ast' in
  List.map mk_instruction ast'' |> List.to_seq |> Array.of_seq
