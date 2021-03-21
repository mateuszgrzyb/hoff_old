
{
   open Parser
   open Lexing
   exception EndLoop
   exception LexingError of string


}

let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z']

let op_char = ['!' '$' '%' '&' '*' '+' '-' '.' '/' '<' '=' '>' '?' '|' '~']
let op = op_char+

let num = digit+ ('.' digit*)?

(* 
let id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* 
*)

let tid = upper (upper | lower | digit | '_')*
let id = lower (upper | lower | digit | '_')*
let pid = '_' (upper | lower | digit | '_')*

let white = [' ' '\t']+
let newline = ('\n' | '\r' | "\r\n")+

let comment = '#' _

rule read = parse
   | white     { read lexbuf }
   | newline   { new_line lexbuf; read lexbuf }
   | comment   { read_comment lexbuf }

   | ";;"       { SEP }
   | ":"       { COLON }
   | "|"       { BAR }
   | "="       { ASSIGN }
   | ","       { COMMA }

   | "("       { LC }
   | ")"       { RC }
   | "{"       { LM }
   | "}"       { RM }

   | "+"       { ADD }
   | "-"       { SUB }
   | "*"       { MUL }
   | "/"       { DIV }

   | "&&"      { AND }
   | "||"      { OR }
   | "=="      { EQ }
   | "!="      { NE }

   | "<"       { LT }
   | "<="      { LE }
   | ">="      { GE }
   | ">"       { GT }

   | "fun"     { FUN }
   | "const"   { CONST }
   | "type"    { TYPE }
 
   | "if"      { IF }
   | "then"    { THEN }
   | "else"    { ELSE }

   | "let"     { LET }
   | "and"     { AND }
   | "in"      { IN }

   | tid       { TID (lexeme lexbuf) }
   | id        { ID (lexeme lexbuf) }
   | pid       { PID (lexeme lexbuf) }

   | num       { NUM (float_of_string (lexeme lexbuf)) }
   | _         { raise (LexingError ("Unexpected char: " ^ lexeme lexbuf)) }
   | eof       { EOF }

and read_comment = parse
   | newline   { new_line lexbuf; read lexbuf }
   | eof       { EOF }
   | _         { read_comment lexbuf }



(*
and debug = parse
   | white     { read lexbuf }
   | newline   { new_line lexbuf; read lexbuf }

   | ":"       { printf "COLON\n" }
   | "="       { printf "ASSIGN\n" }

   | "->"      { printf "ARROW\n" }
   | ","       { printf "COMMA\n" }
   
   | "("       { printf "LC\n" }
   | ")"       { printf "RC\n" }
   | "{"       { printf "LM\n" }
   | "}"       { printf "RM\n" }
 
   | "if"      { printf "IF\n" }
   | "then"    { printf "THEN\n" }
   | "else"    { printf "ELSE\n" }
   | "fi"      { printf "FI\n" }

   | "let"     { printf "LET\n" }
   | "and"     { printf "AND\n" }
   | "in"      { printf "IN\n" }
   | "tel"     { printf "TEL\n" }

   | id        { printf "ID: %s\n" (lexeme lexbuf) }
   | num       { printf "NUM: %F\n" (float_of_string (lexeme lexbuf)) }
   | _         { printf "Unknown character: %s\n" (lexeme lexbuf) }
   | eof       { raise EndLoop }
*)
