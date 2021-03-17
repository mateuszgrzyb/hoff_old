
(*


let lexer () = 
  print_endline "Enter filename:";

  let filename = read_line () in
  let inx = Core.In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in

  print_endline "start";
 
  try
    while true do
      print_endline (Lexer.debug lexbuf);
    done;
  with Lexer.EndLoop -> ();

  print_endline "end"

*)

let parse (inx: in_channel) (name: string): Ast.module_t = 
  let lexbuf = Lexing.from_channel inx in
  let decls = Parser.decls Lexer.read lexbuf in
  Ast.Mod (name, decls)

let main () = 

  ignore (Parsing.set_trace false);
  
  let filename = "test.hff" in
  let inx = Core.In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in

  (*
  ignore (Parser.decls Lexer.read lexbuf)
  *)

  let module_ = Ast.Mod ("test", Parser.decls Lexer.read lexbuf) in
  print_endline (Ast.show_module_t module_)

    
let () = main ()
