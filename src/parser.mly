
%{
  open Ast
%}

%token <string> ID
%token <float> NUM

%token ASSIGN
%token LC RC
%token LM RM

%token IF THEN ELSE FI
%token LET AND IN TEL
%token CONST FUN COMMA

%token ADD SUB MUL DIV

%left ADD SUB
%left MUL DIV
%right NEG

%token EOF

%type <Ast.g_decl_t list> g_decls
//%type <unit> decls

%start g_decls
%%

/*
decls:
  | decl COMMA decls { () }
  | decl { () }
  | { () }
  ;

decl:
  | CONST { print_endline "const" }
  | FUN { print_endline "fun" }
  ;
*/

g_decls: 
  | g_decl g_decls { $1 :: $2 }
  | { [] }
  ;

// g_decl_t

g_decl:
  | CONST ID ASSIGN expr { GConstDecl ($2, $4) }
  | FUN ID LC args RC LM expr RM { GFunDecl ($2, $4, $7) }
  ;

args: 
  | arg COMMA args { $1 :: $3 }
  | arg { $1 :: [] }
  | { [] }
  ;

arg: ID { $1 } ; 

// expr_t

expr: 
  | IF expr THEN expr ELSE expr FI { If ($2, $4, $6) }
  | LET decls IN expr TEL { Let ($2, $4) }
  | LC expr RC { $2 }
  | expr ADD expr { BinOp ($1, "+", $3) }
  | expr SUB expr { BinOp ($1, "-", $3) }
  | expr MUL expr { BinOp ($1, "*", $3) }
  | expr DIV expr { BinOp ($1, "/", $3) }
  | NUM { Num $1 }
  | ID { Const $1 }
  | ID LC exprs RC { Fun ($1, $3) }
  ;

exprs:
  | expr COMMA exprs { $1 :: $3 }
  | expr { $1 :: [] }
  | { [] }
  ; 

// decl_t

decls: 
  | decl decls { $1 :: $2 }
  | { [] }
  ;

decl:
  | CONST ID ASSIGN expr { ConstDecl ($2, $4) }
  | FUN ID LC args RC LM expr RM { FunDecl ($2, $4, $7) }
  ;

%%
