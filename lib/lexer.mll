{
open Parser
open Base26p
}

let white = [' ' '\t' '\n' '\r']+
let vertical = ['\n']+
let letter = ['a'-'z' 'A'-'Z']
let lowerchr = ['a'-'z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let typevar = ['\''] lowerchr+
let num = ['0'-'9']+
let comment = "--" (['\t' ' '-'~'])* (* no newlines! *)

rule read =
  parse
  | white { read lexbuf }
  | comment { read lexbuf }

  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { PRODUCT }
  | "=" { EQUAL }

  | "true" { TRUE }
  | "false" { FALSE }
  | "&&" { AND }
  | "||" { OR }
  | "not" { NOT }
  | "<=" { LESSEQ }


  | "(" { LPAREN }
  | ")" { RPAREN }
  | ":" { COLON }
  | "." { DOT }

  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "->" { TO }
  | "let" { LET }
  | "in" { IN }
  | "fun" { LAMBDA }

  | "int" { INT }
  | "bool" { BOOL }
  | typevar {let s = Lexing.lexeme lexbuf in
             TYPEVAR(int_of_base26p (String.sub s 1 (String.length s - 1))) }

  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | id { IDE (Lexing.lexeme lexbuf) }
  | eof { EOF }