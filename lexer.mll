
{
   open Parser
   open Lexing
   exception EndLoop
   exception LexingError of string
}

let num = ['0'-'9']+('.' ['0'-'9']*)?
let tid = ['A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let white = [' ' '\t']+
let newline = ('\n' | '\r' | "\r\n")+

rule read = parse
   | white     { read lexbuf }
   | newline   { new_line lexbuf; read lexbuf }

   | ":"       { COLON }
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

   | "fun"     { FUN }
   | "const"   { CONST }
 
   | "if"      { IF }
   | "then"    { THEN }
   | "else"    { ELSE }
   | "fi"      { FI }

   | "let"     { LET }
   | "and"     { AND }
   | "in"      { IN }
   | "tel"     { TEL }

   | tid       { TID (lexeme lexbuf) }
   | id        { ID (lexeme lexbuf) }
   | num       { NUM (float_of_string (lexeme lexbuf)) }
   | _         { raise (LexingError ("Unexpected char: " ^ lexeme lexbuf)) }
   | eof       { EOF }


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
