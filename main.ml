

let main () = 

  ignore (Parsing.set_trace false);
  
  let filename = "test.hff" in
  let inx = Core.In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in

  (*
  ignore (Parser.decls Lexer.read lexbuf)
  *)

  let m = Ast.Mod ("test", Parser.g_decls Lexer.read lexbuf) in
  let gen = new Codegen.codegen m in
  let code = gen#generate in
  print_endline (code)
  (*
  print_endline (Ast.show_module_t module_)
  *)

    
let () = main ()
