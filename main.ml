
open Typedast

let parse (inx: in_channel) = 
  let lexbuf = Lexing.from_channel inx in
  Parser.g_decls Lexer.read lexbuf

let compiler () = 

  ignore (Parsing.set_trace false);
  
  let filename = "../misc/test.hff" in
  let inx = Core.In_channel.create filename in

  let decls = parse inx in
  let gen = new Codegen.codegen "test" in
  let code = gen#generate_module decls in
  print_endline (code)  
    
let () = 

  let m1 = Mod ("module1", [
    GFunDecl (
      true,
      "function1", 
      [("a", "Double"); ("b", "Double")], "Double",
      If (Num 1., Num 2., Num 3.)
    )
  ]) in print_endline (show_module_t m1)
