{
open Parser
}

let white = [' ' '\t']+
let vertical = ['\n']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']+

rule read =
  parse
  | white { read lexbuf }

  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { PRODUCT }
  | "=" { EQUAL }

  | "true" { TRUE }
  | "false" { FALSE }
  | "&&" { AND }
  | "||" { OR }
  | "not" { NOT }


  | "(" { LPAREN }
  | ")" { RPAREN }
  | "::" { DOUBLECOLON }
  | vertical { VERTICAL }

  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "->" { TO }
  | "do" { DO }
  | "main" { MAIN }
  | "\\" { LAMBDA }
  | "." { DOT }

  | "int" { INT }
  | "bool" { BOOL }

  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | id { IDE (Lexing.lexeme lexbuf) }
  | eof { EOF }