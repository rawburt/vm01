module P = Parsed_ast

let extract_labels ast =
  let rec loop i labels left = function
    | P.Label name :: rest ->
        loop i ((name, i) :: labels) left rest
    | instruction :: rest ->
        loop (i + 1) labels (instruction :: left) rest
    | [] -> (labels, left)
  in
  loop 0 [] [] ast
