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
%token LESSEQ
%token INTEQ
%token BOOLEQ
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

%right TO (* This precedence serves two purposes, since this token is used in both expression and typing rules:
1. Reduce abstractions lastly, letting their body extend as far right as possible (by the relative position with all the other tokens);
2. Right-associate the "->" type constructor (by the %right specification) *)
%nonassoc IN ELSE (* Reduce let-ins and if-then-elses lastly (but before abstractions), so their rightmost subexpression extends as far right as possible *)

(* Precedences for primitive operators, reduced in the following priority order (highest to lowest):
1. Arithmetic
2. Equality/Comparison
3. Boolean 
For example, "not 1 = 2 * 3" is parsed as "not (1 = (2 * 3))" *)
%left AND OR
%nonassoc NOT
%left INTEQ BOOLEQ EQUAL LESSEQ
%left PLUS MINUS
%left PRODUCT

%nonassoc IF LAMBDA LET IDE LPAREN TRUE FALSE NUM (* Tokens an expresison can start with. The relative priority level with respect to the level of APP determines the associativity of applications (left) *)
%nonassoc APP (* Reduce applications eagerly as soon as two space-separated expressions are read. For example, "foo 1*2" is parsed as "(foo 1)*2" instead of "foo (1*2)" *)
%%

prog:
    | sl = separated_nonempty_list(VERTICAL, typesig_or_decl); DOT; VERTICAL; m = main; EOF { let (tl,dl) = explode_typesig_or_decl_list sl in assert_no_doubles dl tl; (tl, dl, m) }

main:
    | MAIN EQUAL DO e = expr { e }

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
    | LAMBDA p = variable TO e = expr; { ET(EFun(p, e)) }
    | e1 = expr e2 = expr { ENT(EApp(e1, e2)) } %prec APP

    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { ENT(EIf(e1, e2, e3)) }
    | LET x = IDE EQUAL e1 = expr IN e2 = expr { ENT(ELetIn(x,e1,e2)) }

    | c = const { ET(c) }
    | e1 = expr; op = binop; e2 = expr; { ENT(EApp(ENT(EApp(ENT(EName(NBop op)), e1)), e2)) }
    | o = unop; e = expr; { ENT (EApp(ENT(EName(NUop o)), e)) }
    | LPAREN op = binop RPAREN { ENT (EName(NBop op))}
    | LPAREN op = unop RPAREN { ENT (EName(NUop op))}
    | x = variable { ENT (EName (NVar x)) }
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
    | INTEQ { BOIEq}
    | BOOLEQ { BOBEq }
    | LESSEQ { BOLeq }

