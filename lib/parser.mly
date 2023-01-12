%{
open Ast
%}

%token EOF

(* TOKENS FOR EXPRESSIONS *)
%token <int> NUM
%token PLUS
%token MINUS
%token PRODUCT
%token AND
%token OR
%token NOT
%token <string> IDE
%token IF
%token THEN
%token ELSE
%token TRUE
%token FALSE
%token EQUAL
%token LET
%token IN

(* SEPARATORS *)
%token LPAREN
%token RPAREN
%token DOUBLECOLON
%token VERTICAL

(* TOKENS FOR TYPES *)
%token INT
%token BOOL
%token TO

(* PROGRAM *)
%token MAIN
%token DO
%token LAMBDA
%token DOT
%start <program> prog

(* PRECEDENCE *)
%right TO
%left ELSE
%left AND OR
%left PLUS MINUS
%left PRODUCT
%left NOT


%%

prog:
    | sl = separated_nonempty_list(VERTICAL, typesig_or_decl); DOT; VERTICAL; m = main; EOF { let (tl,dl) = explode_typesig_or_decl_list sl in assert_no_doubles dl tl; (tl, dl, m) }

main:
    | MAIN EQUAL DO LPAREN e1 = expr RPAREN LPAREN e2 = expr RPAREN { EApp(e1, e2) }

typing:
    | INT { TInt }
    | BOOL { TBool }
    | t1 = typing TO t2 = typing { TFun(t1,t2) }
    | LPAREN t = typing RPAREN { t }

typesig:
    | x = IDE; DOUBLECOLON; t = typing; { (x, t) }

declaration:
    | x = IDE EQUAL e = expr { (x, e)}

typesig_or_decl:
    | t = typesig { STypeSig(t) }
    | d = declaration { SDecl(d) }

expr:
    | LAMBDA p = variable TO e = expr; { EFun(p, e) }
    | LPAREN e1 = expr RPAREN LPAREN e2 = expr RPAREN { EApp(e1, e2) }

    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { EIf(e1, e2, e3) }
    | LET x = IDE EQUAL e1 = expr IN e2 = expr { ELetIn(x,e1,e2) }

    | c = const { EConst(c) }
    | e1 = expr; op = binop; e2 = expr; { EBinOp(e1, op, e2) }
    | o = unop; e = expr; { EUnOp(o, e) }
    | x = variable { EVar(x) }
    | LPAREN e=expr RPAREN { e }

const:
    | n = number { CNum(n) }
    | b = boolean { CBool(b) }

number:
    | n = NUM; { n }

boolean:
    | TRUE; { true }
    | FALSE; { false }

variable:
    | x = IDE; { x }

%inline unop:
    | NOT; { UONot }

%inline binop:
    | PLUS { BOPlus }
    | PRODUCT { BOTimes }
    | MINUS { BOMinus }
    | AND { BOAnd }
    | OR { BOOr }

