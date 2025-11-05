{
open Parser
}

let white = [' ' '\t' '\r' '\n']+
let digit = ['0'-'9']
let float = digit+ '.' digit+
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z' '_']
let id = letter (letter | digit)*

rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "=>" { GEQ }
  | "+." { PLUS_DOT }
  | "-." { MINUS_DOT }
  | "*." { TIMES_DOT }
  | "/." { DIV_DOT }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | ":" { COLON }
  | "int" { INT_TYPE }
  | "bool" { BOOL_TYPE }
  | "float" { FLOAT_TYPE }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
