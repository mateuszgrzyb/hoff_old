
open Typedast

exception BadOperator of string
exception NoSuchFunction of string
exception NoSuchConstant of string
exception NameExists of string
exception TypeError of string

let rec zip a b = 
  match a, b with
  | [], [] -> []
  | x::xs, y::ys -> (x, y)::(zip xs ys)
  | _ -> failwith "Bad lenght"

class fun_name (name: string) = object
  val name = name
  val mutable i = 0
  method generate =
    let new_name = name ^ (string_of_int i) in
    i <- i + 1;
    new_name
  end
let llvm_none = Llvm.const_float (Llvm.double_type (Llvm.global_context ())) 0.0

let codegen (m: module_t) = 
  let (name, decls) = match m with Mod (n, d) -> (n, d) in
  let context = Llvm.global_context () in
  let module_ = Llvm.create_module context name in
  let builder = Llvm.builder context in
  let double_t = Llvm.double_type context in
  let local_variables = Hashtbl.create 10 in
  let local_functions = Hashtbl.create 10 in
  let lambda_name = new fun_name "HOFF_LAMBDA" in
  let local_name = new fun_name "HOFF_LOCAL" in
  (*val expr_name = new fun_name "HOFF_EXPR"*)

  let rec generate_generic_fundecl public name args body =
    let arg_types = Array.make (List.length args) double_t in
    let f_t = Llvm.function_type double_t arg_types in
    let f = Llvm.declare_function name f_t module_ in 
    
    if not public 
    then Llvm.set_visibility Llvm.Visibility.Hidden f 
    else (); 

    List.iter (fun (name, arg) ->
      Llvm.set_value_name name arg;
      Hashtbl.add local_variables name arg
    ) (zip args (Array.to_list (Llvm.params f)));

    let bb = Llvm.append_block context "entry" f in
    Llvm.position_at_end bb builder;    
    let llvm_body = generate_expr body in

    ignore (Llvm.build_ret llvm_body builder);
    
    List.iter (fun arg ->
      Hashtbl.remove local_variables arg
    ) args;

    f

  and generate_g_decl = function 
    | GConstDecl (name, type_, expr) -> generate_g_constdecl name type_ expr
    | GFunDecl (public, name, args, rtype, body) -> generate_g_fundecl public name args rtype body
    (*
    | GExpr expr -> ignore (self#generate_eval_expr expr)
    *)

  and generate_g_constdecl (n: id_t) (t: type_t) (e: expr_t): unit = 
    let llv = generate_expr t e in
    ignore (Llvm.define_global n llv module_)

  and generate_g_fundecl (p: public_t) (n: id_t) (a: typed_id_t list) (r: type_t) (b: expr_t): unit =
    ignore (generate_generic_fundecl p n a r b)

  and generate_expr (t: type_t) (e: expr_t): Llvm.llvalue = match e with 
    | If (b, e, f)      -> generate_if t b e f 
    | Let (d, e)        -> generate_let t d e
    | BinOp (l, o, r)   -> generate_binop t l o r
    | Lambda (a, rt, b) -> generate_lambda t a rt b
    | Num (n)           -> generate_num n
    | Const (id)        -> generate_const id
    | Fun (n, a)        -> generate_fun n a

  and generate_if bexpr expr1 expr2 =
    
    let if_bb = Llvm.insertion_block builder in
    let f = Llvm.block_parent if_bb in
    let llvm_bexpr_bool = generate_expr bexpr in
    (*
    let llvm_bexpr = self#generate_expr bexpr in
    let zero = Llvm.const_float double_t 0.0 in
    let llvm_bexpr_bool = Llvm.build_fcmp Llvm.Fcmp.One llvm_bexpr zero "ifblock" builder in
    *)
    
    let then_bb = Llvm.append_block context "thenblock" f in
    Llvm.position_at_end then_bb builder;
    let llvm_expr1 = generate_expr expr1 in 
    let new_then_bb = Llvm.insertion_block builder in

    let else_bb = Llvm.append_block context "elseblock" f in
    Llvm.position_at_end else_bb builder;
    let llvm_expr2 = generate_expr expr2 in 
    let new_else_bb = Llvm.insertion_block builder in

    let fi_bb = Llvm.append_block context "fiblock" f in
    Llvm.position_at_end fi_bb builder;
    let result = [(llvm_expr1, new_then_bb); (llvm_expr2, new_else_bb)] in
    let phi = Llvm.build_phi result "phi" builder in
    
    Llvm.position_at_end if_bb builder;
    ignore (Llvm.build_cond_br llvm_bexpr_bool then_bb else_bb builder);
    
    Llvm.position_at_end new_then_bb builder;
    ignore (Llvm.build_br fi_bb builder);
    Llvm.position_at_end new_else_bb builder;
    ignore (Llvm.build_br fi_bb builder);

    Llvm.position_at_end fi_bb builder;
    phi


  and generate_let decls expr =
    
    let let_bb = Llvm.insertion_block builder in
    
    List.iter (fun decl ->
      match decl with
      | ConstDecl (name, type_, expr) -> 
        Hashtbl.add local_variables name (generate_expr type_ expr)
      | FunDecl (name, args, rtype, body) -> 
        Hashtbl.add local_functions name (generate_l_fundecl args rtype body)
    ) decls;

    Llvm.position_at_end let_bb builder;
    let llvm_expr = generate_expr expr in

    List.iter (fun decl ->
      match decl with
      | ConstDecl (name, _, _) -> Hashtbl.remove local_variables name
      | FunDecl (name, _, _, _) -> Hashtbl.remove local_functions name
    ) decls;

    llvm_expr
  
  and generate_binop lh op rh = 
    let lh_value = generate_expr lh in
    let rh_value = generate_expr rh in
    match op with
 
    | Add -> Llvm.build_fadd lh_value rh_value "addexpr" builder 
    | Sub -> Llvm.build_fsub lh_value rh_value "subexpr" builder
    | Mul -> Llvm.build_fmul lh_value rh_value "mulexpr" builder
    | Div -> Llvm.build_fdiv lh_value rh_value "divexpr" builder
 
    | Lt  -> Llvm.build_fcmp Llvm.Fcmp.Olt lh_value rh_value "ltexpr"  builder 
    | Le  -> Llvm.build_fcmp Llvm.Fcmp.Ole lh_value rh_value "leexpr"  builder
    | Ge  -> Llvm.build_fcmp Llvm.Fcmp.Oge lh_value rh_value "geexpr"  builder
    | Gt  -> Llvm.build_fcmp Llvm.Fcmp.Ogt lh_value rh_value "gtexpr"  builder
 
    | And -> Llvm.build_and                lh_value rh_value "andexpr" builder 
    | Or  -> Llvm.build_or                 lh_value rh_value "orexpr"  builder
    | Eq  -> Llvm.build_fcmp Llvm.Fcmp.Oeq lh_value rh_value "eqexpr"  builder
    | Ne  -> Llvm.build_fcmp Llvm.Fcmp.One lh_value rh_value "neexpr"  builder

  and generate_lambda args body = 
    generate_generic_fundecl false (lambda_name#generate) args body
  
  and generate_l_fundecl args body =
    generate_generic_fundecl false (local_name#generate) args body

  
  and generate_num num = 
    Llvm.const_float double_t num

  and generate_const name = 
    match (Llvm.lookup_global name module_) with
    | Some g -> g 
    | None -> try
      Hashtbl.find local_variables name
    with Not_found -> 
      raise (NoSuchConstant ("No constant found with name: " ^ name))

  and generate_fun name args =
    let f = match (Llvm.lookup_function name module_) with 
    | Some f -> f 
    | None -> try
      Hashtbl.find local_functions name
    with Not_found ->
      raise (NoSuchFunction ("No function found with name: " ^ name)) in
    
    let llvm_args = Array.of_list (List.map generate_expr args) in
    Llvm.build_call f llvm_args "callexpr" builder 
  
  in 
  
  List.iter generate_g_decl decls;
  let result = match Llvm_analysis.verify_module module_ with
  | None -> Llvm.string_of_llmodule module_
  | Some error -> error in
  Llvm.dispose_module module_;
  result