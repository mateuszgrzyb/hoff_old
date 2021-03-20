
%{
  open Ast
%}

%token <string> ID PID
%token <float> NUM

%token ASSIGN
%token LC RC
%token LM RM

%token IF THEN ELSE FI
%token LET AND IN TEL
%token CONST FUN COMMA

%token ADD SUB MUL DIV

%token AND OR EQ NE
%token LT LE GE GT

%left AND OR
%left EQ NE
%left LT LE GE GT
%left ADD SUB
%left MUL DIV
%right NEG

%token EOF

%type <Ast.g_decl_t list> g_decls

%start g_decls
%%

// g_decl_t

g_decls: 
  | g_decl g_decls { $1 :: $2 }
  | { [] }
  ;

// g_decl_t

g_decl:
  | CONST ID ASSIGN expr { GConstDecl ($2, $4) }
  | FUN ID LC args RC LM expr RM { GFunDecl (true, $2, $4, $7) }
  | FUN PID LC args RC LM expr RM { GFunDecl (false, $2, $4, $7) }
//| expr { GExpr $1 }
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
  
  | expr ADD expr { BinOp ($1, Add, $3) }
  | expr SUB expr { BinOp ($1, Sub, $3) }
  | expr MUL expr { BinOp ($1, Mul, $3) }
  | expr DIV expr { BinOp ($1, Div, $3) }
  
  | expr AND expr { BinOp ($1, And, $3) }
  | expr OR  expr { BinOp ($1, Or,  $3) }
  | expr EQ  expr { BinOp ($1, Eq,  $3) }
  | expr NE  expr { BinOp ($1, Ne,  $3) }

  | expr LT expr  { BinOp ($1, Lt, $3) }
  | expr LE expr  { BinOp ($1, Le, $3) }
  | expr GE expr  { BinOp ($1, Ge, $3) }
  | expr GT expr  { BinOp ($1, Gt, $3) }

  | NUM { Num $1 }
  | ID { Const $1 }
  | ID LC exprs RC { Fun ($1, $3) }
  | FUN LC args RC LM expr RM { Lambda ($3, $6) }
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
