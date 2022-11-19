module P = Parsed_ast

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

let rec find_label name = function
  | (n, i) :: rest ->
      if n = name
      then Some (n, i)
      else find_label name rest
  | [] -> None

exception Assemble_error of string

let rec replace_labels labels = function
  | [] -> []
  | P.Label _ :: _ -> raise (Assemble_error "unexpected label")
  | P.Instruction (ins, Some (P.ArgLabel name)) :: rest -> begin
      match find_label name labels with
      | None -> raise (Assemble_error ("unknown label: " ^ name))
      | Some (_, id) ->
          let instruction = P.Instruction (ins, Some (P.ArgInt id)) in
          instruction :: (replace_labels labels rest)
  end
  | (_ as head) :: rest -> head :: (replace_labels labels rest)
