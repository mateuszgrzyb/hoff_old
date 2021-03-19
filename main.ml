

let parse (inx: in_channel) = 
  let lexbuf = Lexing.from_channel inx in
  Parser.g_decls Lexer.read lexbuf

let compiler () = 

  ignore (Parsing.set_trace false);
  
  let filename = "../src/test.hff" in
  let inx = Core.In_channel.create filename in

  let decls = parse inx in
  let gen = new Codegen.codegen "test" in
  let code = gen#generate_module decls in
  print_endline (code)  
    
let () = compiler ()
