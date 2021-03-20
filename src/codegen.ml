
open Ast

exception BadOperator of string
exception NoSuchFunction of string
exception NoSuchConstant of string
exception NameExists of string

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

class codegen (name: string) = 
let context = Llvm.global_context () in
let module_ = Llvm.create_module context name in
object (self)
  
  val builder = Llvm.builder context
  val double_t = Llvm.double_type context
  val module_begin = Llvm.global_begin module_
  val local_variables: (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 10
  val local_functions: (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 10
  val lambda_name = new fun_name "HOFF_LAMBDA"
  val local_name = new fun_name "HOFF_LOCAL"
  (*val expr_name = new fun_name "HOFF_EXPR"*)

  method generate_module decls = 
    List.iter self#generate_g_decl decls;
    match Llvm_analysis.verify_module module_ with
    | None -> Llvm.string_of_llmodule module_
    | Some error -> error

  method private generate_generic_fundecl public name args body =
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
    let llvm_body = self#generate_expr body in

    ignore (Llvm.build_ret llvm_body builder);
    
    List.iter (fun arg ->
      Hashtbl.remove local_variables arg
    ) args;

    f

  method private generate_g_decl = function 
    | GConstDecl (name, expr) -> self#generate_g_constdecl name expr
    | GFunDecl (public, name, args, body) -> self#generate_g_fundecl public name args body
    (*
    | GExpr expr -> ignore (self#generate_eval_expr expr)
    *)

  method private generate_g_constdecl name expr = 
    let llvm_expr = self#generate_expr expr in
    ignore (Llvm.define_global name llvm_expr module_)

  method private generate_g_fundecl public name args body =
    ignore (self#generate_generic_fundecl public name args body)

  method private generate_expr = function
    | If (bexpr, expr1, expr2) -> self#generate_if bexpr expr1 expr2
    | Let (decls, expr) ->        self#generate_let decls expr
    | BinOp (lh, op, rh) ->       self#generate_binop lh op rh
    | Lambda (args, body) ->      self#generate_lambda args body
    | Num (num) ->                self#generate_num num
    | Const (name) ->             self#generate_const name
    | Fun (name, args) ->         self#generate_fun name args

  method private generate_if bexpr expr1 expr2 =
    
    let if_bb = Llvm.insertion_block builder in
    let f = Llvm.block_parent if_bb in
    let llvm_bexpr_bool = self#generate_expr bexpr in
    (*
    let llvm_bexpr = self#generate_expr bexpr in
    let zero = Llvm.const_float double_t 0.0 in
    let llvm_bexpr_bool = Llvm.build_fcmp Llvm.Fcmp.One llvm_bexpr zero "ifblock" builder in
    *)
    
    let then_bb = Llvm.append_block context "thenblock" f in
    Llvm.position_at_end then_bb builder;
    let llvm_expr1 = self#generate_expr expr1 in 
    let new_then_bb = Llvm.insertion_block builder in

    let else_bb = Llvm.append_block context "elseblock" f in
    Llvm.position_at_end else_bb builder;
    let llvm_expr2 = self#generate_expr expr2 in 
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


  method private generate_let decls expr =
    
    let let_bb = Llvm.insertion_block builder in
    
    List.iter (fun decl ->
      match decl with
      | ConstDecl (name, expr) -> Hashtbl.add local_variables name (self#generate_expr expr)
      | FunDecl (name, args, body) -> Hashtbl.add local_functions name (self#generate_l_fundecl args body)
    ) decls;

    Llvm.position_at_end let_bb builder;
    let llvm_expr = self#generate_expr expr in

    List.iter (fun decl ->
      match decl with
      | ConstDecl (name, _) -> Hashtbl.remove local_variables name
      | FunDecl (name, _, _) -> Hashtbl.remove local_functions name
    ) decls;

    llvm_expr
  
  method private generate_binop lh op rh = 
    let lh_value = self#generate_expr lh in
    let rh_value = self#generate_expr rh in
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

  method private generate_lambda args body = 
    self#generate_generic_fundecl false (lambda_name#generate) args body
  
  method private generate_l_fundecl args body =
    self#generate_generic_fundecl false (local_name#generate) args body

  (*
  method private generate_eval_expr expr = 
    self#generate_generic_fundecl (expr_name#generate) [] expr
  *)
  
  method private generate_num num = 
    Llvm.const_float double_t num

  method private generate_const name = 
    match (Llvm.lookup_global name module_) with
    | Some g -> g 
    | None -> try
      Hashtbl.find local_variables name
    with Not_found -> 
      raise (NoSuchConstant ("No constant found with name: " ^ name))

  method private generate_fun name args =
    let f = match (Llvm.lookup_function name module_) with 
    | Some f -> f 
    | None -> try
      Hashtbl.find local_functions name
    with Not_found ->
      raise (NoSuchFunction ("No function found with name: " ^ name)) in
    
    let llvm_args = Array.of_list (List.map self#generate_expr args) in
    Llvm.build_call f llvm_args "callexpr" builder


  end