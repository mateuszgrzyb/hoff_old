
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

let llvm_none = Llvm.const_float (Llvm.double_type (Llvm.global_context ())) 0.0

class codegen (m: module_t) = 
let c = Llvm.global_context () in
let name = match m with | Mod (name, _) -> name in
object (self)
  
  val ast: module_t = m
  val module_ = Llvm.create_module c name
  val builder = Llvm.builder c
  val context = c
  val double_t = Llvm.double_type c
  val local_variables = Hashtbl.create 10

  method generate = match ast with
    | Mod (_, decl) ->
      List.iter self#generate_g_decl decl;
      Llvm.string_of_llmodule module_

  method private generate_g_decl = function 
    | GConstDecl (name, expr) -> self#generate_g_constdecl name expr
    | GFunDecl (name, args, body) -> self#generate_g_fundecl name args body

  method private generate_g_constdecl name expr = 
    let llvm_expr = self#generate_expr expr in
    ignore (Llvm.define_global name llvm_expr module_)

  method private generate_g_fundecl name args body =
    let arg_types = Array.make (List.length args) double_t in
    let f_t = Llvm.function_type double_t arg_types in
    let f = Llvm.declare_function name f_t module_ in

    List.iter (fun (name, arg) ->
      Llvm.set_value_name name arg;
      Hashtbl.add local_variables name arg
    ) (zip args (Array.to_list (Llvm.params f)));
    (*
    Array.iteri (fun i arg ->
      let name = args.(i) in
      Llvm.set_value_name name arg;
      Hashtbl.add local_variables name arg;
    ) (Llvm.params f);
    *)

    let bb = Llvm.append_block context "entry" f in

    Llvm.position_at_end bb builder;
    let return = self#generate_expr body in
    ignore (Llvm.build_ret return builder);
    
    List.iter (fun arg ->
      Hashtbl.remove local_variables arg
    ) args


  (*
  method private generate_constdecl name type_ val_ = ()
  method private generate_fundecl name args result body = ()
  *)

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
    let llvm_bexpr = self#generate_expr bexpr in
    let zero = Llvm.const_float double_t 0.0 in
    let llvm_bexpr_bool = Llvm.build_fcmp Llvm.Fcmp.One llvm_bexpr zero "ifblock" builder in
    
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
    List.iter (fun decl ->
      match decl with
      | ConstDecl (name, expr) -> Hashtbl.add local_variables name (self#generate_expr expr)
      | FunDecl _ -> ()
    ) decls;
    
    let llvm_expr = self#generate_expr expr in

    List.iter (fun decl ->
      match decl with
      | ConstDecl (name, _) -> Hashtbl.remove local_variables name
      | FunDecl _ -> ()
    ) decls;

    llvm_expr
  
  (*
  method private generate_if bexpr expr1 expr2 = llvm_none
  method private generate_let decls expr = llvm_none
  *)
  
  method private generate_binop lh op rh = 
    let lh_value = self#generate_expr lh in
    let rh_value = self#generate_expr rh in
    match op with
    | "+" -> Llvm.build_fadd lh_value rh_value "addexpr" builder 
    | "-" -> Llvm.build_fsub lh_value rh_value "subexpr" builder
    | "*" -> Llvm.build_fmul lh_value rh_value "mulexpr" builder
    | "/" -> Llvm.build_fdiv lh_value rh_value "divexpr" builder
    | _ -> raise (BadOperator ("Unknown operator:"))

  (*
  method private generate_lambda args body = llvm_none
  *)
  
  method private generate_lambda _ _ = llvm_none
  
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
    | None -> raise (NoSuchFunction ("No function found with name: " ^ name)) in
    let llvm_args = Array.of_list (List.map self#generate_expr args) in
    Llvm.build_call f llvm_args "callexpr" builder

  method private generate_decl = function 
    | ConstDecl (name, expr) -> self#generate_constdecl name expr
    | FunDecl (name, args, body) -> self#generate_fundecl name args body

  method private generate_constdecl _ _ = ()
  method private generate_fundecl _ _ _ = ()
end