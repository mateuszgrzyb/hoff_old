
type module_t = 
  | Mod of string * (g_decl_t list)
  [@@deriving show]

and g_decl_t =
  | GConstDecl of id_t * type_t * expr_t
  | GFunDecl of public_t * id_t * (typed_id_t list) * type_t * expr_t
  | GTypeDecl of type_t * type_decl_t
  [@@deriving show]

and type_decl_t = 
  | Alias of type_t
  | Sum of product_t list

and product_t = 
  | Product of constructor_t * (type_t list)

and public_t = bool
and id_t = string
and type_t = string
and typed_id_t = (id_t * type_t)
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
  | Double of float

and decl_t = 
  | ConstDecl of id_t * type_t * expr_t
  | FunDecl of id_t * (typed_id_t list) * type_t * expr_t
  [@@deriving show]

and binop =
  | Add | Sub | Mul | Div 
  | And | Or  | Eq  | Ne
  | Lt  | Le  | Ge  | Gt
  | Sep
  [@@deriving show]


let main () =
  let m1 = Mod ("module1", [
    GTypeDecl ("Name", Alias("String"));
    GTypeDecl ("List", Sum([Product("Nil", []); Product("Cons", ["Int"; "List"])]));
    
    GFunDecl (
      true, 
      "function1", 
      [("a", "Double"); ("b", "Double")], 
      "Double", 
      If (Lit (Bool (true)), Lit (Float (2.0)), Lit (Float (3.0))));
  ]) in print_endline (show_module_t m1)



