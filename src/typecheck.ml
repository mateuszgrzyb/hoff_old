
module TA = Typedast

exception TypeError of string

let type_of_string (s: string) =
  match s with
  | "Int" -> TA.IntT
  | "Float" -> TA.FloatT
  | "Bool" -> TA.BoolT
  | "String" -> TA.StringT
  | unknw -> raise (TypeError ("Unknown type: " ^ unknw))

let match_types (t1: TA.type_t) (t2: TA.type_t): unit =
  if t1 != t2 
  then raise (TypeError "Types do not match")
  
let rec typecheck_module (m: Ast.module_t): TA.module_t =
  match m with
  | Ast.Mod (name, decls) ->
    let checked_decls = List.map typecheck_g_decl decls in
     TA.Mod (name, checked_decls) 

and typecheck_g_decl (g_decl: Ast.g_decl_t): TA.g_decl_t =
  match g_decl with
  | Ast.GConstDecl (n, t, expr) -> 
    let t' = type_of_string t in
    let expr' = typecheck_expr expr in
    TA.GConstDecl (n, t', expr')
  | Ast.GFunDecl (p, n, args, r, b) ->
    TA.GFunDecl (p, n, args, r, b)
