
open Ast

exception BadOperator of string

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

    method generate: string = match ast with
      | Mod (_, decl) ->
        List.iter self#generate_decl decl;
        Llvm.string_of_llmodule module_

    method private generate_decl = function 
      | ConstDecl (name, type_, val_) -> self#generate_constdecl name type_ val_
      | FunDecl (name, args, result, body) -> self#generate_fundecl name args result body

    method private generate_constdecl name type_ val_ = ()
    method private generate_fundecl name args result body = ()

    method private generate_expr = function
      | If (bexpr, expr1, expr2) -> self#generate_if bexpr expr1 expr2
      | Let (decls, expr) ->        self#generate_let decls expr
      | BinOp (lh, op, rh) ->       self#generate_binop lh op rh
      | Lambda (args, body) ->      self#generate_lambda args body
      | Num (num) ->                self#generate_num num
      | Const (name) ->             self#generate_const name
      | Fun (name, args) ->         self#generate_fun name args

    method private generate_if bexpr expr1 expr2 = llvm_none
    method private generate_let decls expr = llvm_none
    
    method private generate_binop lh op rh = 
      let lh_value = self#generate_expr lh in
      let rh_value = self#generate_expr rh in
      match op with
      | "+" -> Llvm.build_fadd lh_value rh_value "addexpr" builder 
      | "-" -> Llvm.build_fsub lh_value rh_value "subexpr" builder
      | "*" -> Llvm.build_fmul lh_value rh_value "mulexpr" builder
      | "/" -> Llvm.build_fdiv lh_value rh_value "divexpr" builder
      | _ -> raise (BadOperator ("Unknown operator:"))

    method private generate_lambda args body = llvm_none
    method private generate_num num = 
      Llvm.const_float double_t num

    method private generate_const name = llvm_none
    method private generate_fun name args = llvm_none
  end