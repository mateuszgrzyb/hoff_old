
%{
  open Ast
%}

%token <string> ID PID TID

%token <int> INT
%token <float> FLOAT 
%token <bool> BOOL
%token <string> STRING

%token BAR ASSIGN COMMA COLON
%token LC RC
%token LM RM

%token IF THEN ELSE
%token LET AND IN
%token VAL FUN TYPE

%token ADD SUB MUL DIV

%token AND OR EQ NE
%token LT LE GE GT

%token SEP

%left SEP
%nonassoc LETIN
%nonassoc IFTHENELSE
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
  | EOF { [] }

// g_decl_t

g_decl:
  | VAL ID COLON TID ASSIGN expr { GConstDecl ($2, $4, $6) }
  | FUN ID LC args RC COLON TID LM expr RM { GFunDecl (true, $2, $4, $7, $9) }
  | FUN PID LC args RC COLON TID LM expr RM { GFunDecl (false, $2, $4, $7, $9) }
//| TYPE TID type_ { GTypeDecl($2, $3) }

type_:
  | ASSIGN TID { Alias($2) }
  | sums { Sum($1) }

sums:
  | BAR sum sums { $2 :: $3 }
  | BAR sum { $2 :: [] }

sum: TID LC prods RC { Product($1, $3) }

prods:
  | TID COMMA prods { $1 :: $3 }
  | TID { $1 :: [] }
  | { [] }

args: 
  | arg COMMA args { $1 :: $3 }
  | arg { $1 :: [] }
  | { [] }

arg: ID COLON TID { ($1, $3) }

// expr_t

expr: 
//| IF expr THEN expr ELSE expr FI { If ($2, $4, $6) }
  | IF expr THEN expr ELSE expr %prec IFTHENELSE { If ($2, $4, $6) }
//| LET decls IN expr TEL { Let ($2, $4) }
  | LET decls IN expr %prec LETIN { Let ($2, $4) }
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

  | expr SEP expr { BinOp ($1, Sep, $3) }

  | lit { Lit ($1) }
  | ID { Val ($1) }
  | ID LC exprs RC { Fun ($1, $3) }
  | FUN LC ids RC LM expr RM { Lambda ($3, $6) }

lit:
  | INT    { Int ($1) }
  | FLOAT  { Float ($1) }
  | BOOL   { Bool ($1) }
  | STRING { String ($1) }

exprs:
  | expr COMMA exprs { $1 :: $3 }
  | expr { $1 :: [] }
  | { [] } 

ids: 
  | ID COMMA ids { $1 :: $3 }
  | ID { $1 :: [] }
  | { [] }

// decl_t

decls: 
  | decl decls { $1 :: $2 }
  | { [] }

decl:
  | VAL ID COLON TID ASSIGN expr { ConstDecl ($2, $4, $6) }
  | FUN ID LC args RC COLON TID LM expr RM { FunDecl ($2, $4, $7, $9) }
//| ID COLON TID ASSIGN expr { ConstDecl ($1, $3, $5) }
//| ID LC args RC COLON TID LM expr RM { FunDecl ($1, $3, $6, $8) }

%%
