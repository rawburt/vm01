type config = {
  mutable infile : string;
  ast : bool ref;
}

let usage_msg = "vm01 [-ast] file"

let config = {infile = ""; ast = ref false}

let anon_fun filename = config.infile <- filename

let speclist = [
  ("-ast", Arg.Set config.ast, "Output parsed AST");
]

let () =
  Arg.parse speclist anon_fun usage_msg;
  let parsed = Vm01.parse_file config.infile in
  if !(config.ast) then
    Vm01.Parsed_ast.show_parsed_program parsed |> print_endline
  else
    print_endline "ok"

