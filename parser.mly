
%{
  open Ast
%}

%token <string> ID
%token <string> TID
%token <float> NUM

%token COLON ASSIGN
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

%type <Ast.decl_t list> decls
//%type <unit> decls

%start decls
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

decls: 
  | decl decls { $1 :: $2 }
  | { [] }
  ;

// decl_t

decl:
  | CONST ID COLON TID ASSIGN expr { ConstDecl ($2, $4, $6) }
  | FUN ID LC args RC COLON TID LM expr RM { FunDecl ($2, $4, $7, $9) }
  ;

// bad
args: 
  | arg COMMA args { $1 :: $3 }
  | arg { $1 :: [] }
  | { [] }
  ;

arg: ID COLON TID { $3 } ; 

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

// bad also
exprs:
  | expr COMMA exprs { $1 :: $3 }
  | expr { $1 :: [] }
  | { [] }
  ; 


%%
