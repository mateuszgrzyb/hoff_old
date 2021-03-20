
type module_t = 
  | Mod of string * (g_decl_t list)
  [@@deriving show]

and g_decl_t =
  | GConstDecl of id_t * expr_t
  | GFunDecl of public_t * id_t * (id_t list) * expr_t
(*
  | GExpr of expr_t
*)
  [@@deriving show]

and public_t = bool

and id_t = string

(*
and type_t = string
  [@@deriving show]
*)

and expr_t =
  | If of expr_t * expr_t * expr_t
  | Let of (decl_t list) * expr_t
  | BinOp of expr_t * binop * expr_t
  | Lambda of (id_t list) * expr_t
  | Num of float
  | Const of id_t
  | Fun of id_t * (expr_t list)
  [@@deriving show]

and decl_t = 
  | ConstDecl of id_t * expr_t
  | FunDecl of id_t * (id_t list) * expr_t
  [@@deriving show]

and binop =
  | Add | Sub | Mul | Div 
  | And | Or  | Eq  | Ne
  | Lt  | Le  | Ge  | Gt
  [@@deriving show]


let main () =
  let m1 = Mod ("module1", [
    GFunDecl (
      true,
      "function1", 
      ["a"; "b"],
      If (Num 1., Num 2., Num 3.)
    )
  ]) in print_endline (show_module_t m1)



