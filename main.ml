

let parse (inx: in_channel) = 
  let lexbuf = Lexing.from_channel inx in
  Parser.g_decls Lexer.read lexbuf

let compiler () = 

  ignore (Parsing.set_trace true);
  
  let filename = "./misc/test.hff" in
  let inx = Core.In_channel.create filename in

  let decls = parse inx in
  (*
  print_endline (Ast.show_module_t (Mod ("test", decls)))
  *)
  let code = Codegen.generate "test" decls in
  print_endline (code)  
    
let () = compiler ()
