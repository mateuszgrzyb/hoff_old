
type module_t = 
  | Mod of string * (g_decl_t list)
  [@@deriving show]

and g_decl_t =
  | GConstDecl of string * expr_t
  | GFunDecl of string * (string list) * expr_t
  [@@deriving show]

(*
and type_t = string
  [@@deriving show]
*)

and expr_t =
  | If of expr_t * expr_t * expr_t
  | Let of (decl_t list) * expr_t
  | BinOp of expr_t * binop * expr_t
  | Lambda of (string list) * expr_t
  | Num of float
  | Const of string
  | Fun of string * (expr_t list)
  [@@deriving show]

and decl_t = 
  | ConstDecl of string * expr_t
  | FunDecl of string * (string list) * expr_t
  [@@deriving show]

and binop =
  | Add | Sub | Mul | Div 
  | And | Or  | Eq  | Ne
  | Lt  | Le  | Ge  | Gt
  [@@deriving show]


let main () =
  let m1 = Mod ("module1", [
    GFunDecl (
      "function1", 
      ["a"; "b"],
      If (Num 1., Num 2., Num 3.)
    )
  ]) in print_endline (show_module_t m1)



