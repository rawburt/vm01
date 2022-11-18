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

let readfile filename =
  let lines = ref "" in
  let chan = open_in filename in
  try
    while true do
      lines := !lines ^ "\n" ^ input_line chan
    done; !lines
  with End_of_file -> close_in chan; !lines

let () =
  Arg.parse speclist anon_fun usage_msg;
  let file_contents = readfile config.infile in
  let parsed = Vm01.parse file_contents in
  if !(config.ast) then
    Vm01.Parsed_ast.show_parsed_program parsed |> print_endline
  else
    print_endline "ok"

