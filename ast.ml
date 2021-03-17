
type module_t = 
  | Mod of string * (decl_t list)
  [@@deriving show]

and decl_t =
  | ConstDecl of string * type_t * expr_t
  | FunDecl of string * (type_t list) * type_t * expr_t
  [@@deriving show]

and type_t = string
  [@@deriving show]

and expr_t =
  | If of expr_t * expr_t * expr_t
  | Let of (decl_t list) * expr_t
  | BinOp of expr_t * string * expr_t
  | Lambda of (string list) * expr_t
  | Num of float
  | Const of string
  | Fun of string * (expr_t list)
  [@@deriving show]

let main () =
  let m1 = Mod ("module1", [
    FunDecl (
      "function1", 
      ["Int"; "Int"],
      "Int",
      If (Num 1., Num 2., Num 3.)
    )
  ]) in print_endline (show_module_t m1)



