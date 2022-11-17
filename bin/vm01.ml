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
  print_endline "ok"

