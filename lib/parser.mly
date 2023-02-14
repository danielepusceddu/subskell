%{
open Typescommon
open Typesstatic
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
%token <int> TYPEVAR
%token IF
%token THEN
%token ELSE
%token TRUE
%token FALSE
%token LESSEQ
%token EQUAL
%token LET
%token IN
%token DOT

(* SEPARATORS *)
%token LPAREN
%token RPAREN
%token COLON

(* TOKENS FOR TYPES *)
%token INT
%token BOOL
%token TO
%token LAMBDA

(* PROGRAM *)
%start <pexpr> prog

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
%left EQUAL LESSEQ
%left PLUS MINUS
%left PRODUCT

%nonassoc IF LAMBDA LET IDE LPAREN TRUE FALSE NUM (* Tokens an expresison can start with. The relative priority level with respect to the level of APP determines the associativity of applications (left) *)
%nonassoc APP (* Reduce applications eagerly as soon as two space-separated expressions are read. For example, "foo 1*2" is parsed as "(foo 1)*2" instead of "foo (1*2)" *)
%%

prog:
    | e = expr EOF { e }

typescheme:
    | l = TYPEVAR+ DOT t = typing { (l,t) }
    | t = typing { ([],t) }

typing:
    | INT { TInt }
    | BOOL { TBool }
    | t1 = typing TO t2 = typing { TFun(t1,t2) }
    | t = TYPEVAR { TVar(t) }
    | LPAREN t = typing RPAREN { t }

expr:
    | LAMBDA p = variable TO e = expr; { PFun(p, e) }
    | e1 = expr e2 = expr { PApp(e1, e2) } %prec APP

    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { PIf(e1, e2, e3) }
    | LET x = IDE EQUAL e1 = expr IN e2 = expr { PLetIn(x,None,e1,e2) }
    | LET x = IDE COLON tsch=typescheme EQUAL e1 = expr IN e2 = expr { PLetIn(x,Some tsch,e1,e2) }

    | c = const { c }
    | e1 = expr; op = binop; e2 = expr; { PApp(PApp(PName(NBop op), e1), e2) }
    | o = unop; e = expr; { PApp(PName(NUop o), e) }
    | LPAREN op = binop RPAREN { PName(NBop op)}
    | LPAREN op = unop RPAREN { PName(NUop op)}
    | x = variable { PName (NVar x) }
    | LPAREN e=expr RPAREN { e }

const:
    | n = number { PNum(n) }
    | b = boolean { PBool(b) }

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
    | EQUAL { BOEq }
    | LESSEQ { BOLeq }

