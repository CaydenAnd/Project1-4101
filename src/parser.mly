%{
open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token TRUE
%token FALSE
%token LEQ
%token GEQ
%token TIMES
%token DIV
%token PLUS
%token MINUS
%token TIMES_DOT
%token DIV_DOT
%token PLUS_DOT
%token MINUS_DOT
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token COLON
%token INT_TYPE
%token BOOL_TYPE
%token FLOAT_TYPE
%token EOF

%left LEQ GEQ
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left TIMES DIV TIMES_DOT DIV_DOT

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }

expr:
  | n = INT { Int n }
  | f = FLOAT { Float f }
  | x = ID { Var x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
  | e1 = expr; GEQ; e2 = expr { Binop (Geq, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
  | e1 = expr; DIV; e2 = expr { Binop (Div, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
  | e1 = expr; MINUS; e2 = expr { Binop (Sub, e1, e2) }
  | e1 = expr; TIMES_DOT; e2 = expr { Binop (FMult, e1, e2) }
  | e1 = expr; DIV_DOT; e2 = expr { Binop (FDiv, e1, e2) }
  | e1 = expr; PLUS_DOT; e2 = expr { Binop (FAdd, e1, e2) }
  | e1 = expr; MINUS_DOT; e2 = expr { Binop (FSub, e1, e2) }
  | LET; x = ID; COLON; t = typ; EQUALS; e1 = expr; IN; e2 = expr { Let (x, t, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LPAREN; e = expr; RPAREN { e }

typ:
  | INT_TYPE { Ast.TInt }
  | BOOL_TYPE { Ast.TBool }
  | FLOAT_TYPE { Ast.TFloat }
