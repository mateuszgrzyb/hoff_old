
module type GenericAst = sig
  type t

  type module_t = 
    | Mod of string * (g_decl_t list)
    [@@deriving show]

  and g_decl_t =
    | GConstDecl of id_t * t * expr_t
    | GFunDecl of public_t * id_t * (typed_id_t list) * t * expr_t
  (*| GTypeDecl of id_t * type_decl_t*)
    [@@deriving show]

  and type_decl_t = 
    | Alias of t
    | Sum of product_t list

  and product_t = 
    | Product of constructor_t * (t list)

  and public_t = bool
  and id_t = string
  and typed_id_t = (id_t * t)
  and constructor_t = string

  and expr_t =
    | If of expr_t * expr_t * expr_t
    | Let of (decl_t list) * expr_t
    | BinOp of expr_t * binop * expr_t
    | Lambda of (id_t list) * expr_t
    | Lit of lit_t
    | Val of id_t
    | Fun of id_t * (expr_t list)
    [@@deriving show]

  and lit_t = 
    | Bool of bool
    | Int of int
    | Float of float
    | String of string

  and decl_t = 
    | ConstDecl of id_t * t * expr_t
    | FunDecl of id_t * (typed_id_t list) * t * expr_t
    [@@deriving show]

  and binop =
    | Add | Sub | Mul | Div 
    | And | Or  | Eq  | Ne
    | Lt  | Le  | Ge  | Gt
    | Sep
    [@@deriving show]

end