module ProjectParser

open CS334
open Parser

(* HELPER COMBINATORS *)

(*
 * Accepts whatever p accepts, surrounded by parens, i.e., (p), and
 * returns whatever p returns.
 * @param p A parser.
 *)
let inParens p =
    pbetween
        (pchar '(')
        (pchar ')')
        p

(* pcomment
 *   # Comments start with a hashtag but can't contain
 *   # one (or a newline).
 *)
let pnotcom = psat (fun c -> c <> '#' && c <> '\n')
let pcomment =
    pbetween (pchar '#') peof (pmany1 pnotcom)
    <|> pbetween (pchar '#') pws1 (pmany1 pnotcom)
    |>> (fun _ -> true)
    <!> "pcomment"

(* my_ws
 *   Let's consider any non-newline whitespace or
 *   a comment to be whitespace
 *)
let my_ws = pcomment <|> (pwsNoNL0 |>> (fun _ -> true))

(* pad p
 *   Parses p, surrounded by optional whitespace.
 *)
let pad p = pbetween my_ws my_ws p

(* GEODRAW GRAMMAR *)
let expr, exprImpl = recparser()

(* Number parsers *)
let int_num =
    pmany1 pdigit
    |>> (fun ds -> (float (stringify ds)))

let pint_num = int_num |>> Num

let float_num =
    (pseq
        (pmany0 pdigit)
        (pseq
            (pchar '.')
            (pmany1 pdigit)
            (fun (c, n) -> (c, n)))
        (fun (n', (c, n)) -> (float (stringify (n'@[c]@n)))))

let pfloat_num = float_num |>> Num

let pnumber = pfloat_num <|> pint_num
let number = int_num <|> float_num

(* Canvas parser *)                
let pCoords =
    (pseq
        (pright pws0 number)
        (pseq
            (pright pws0 (pchar ','))
            (pseq
                (pright pws0 number)
                pws0
                (fun (n, _) -> n))
            (fun (c, n) -> n))
//        (fun (n1, n2) -> (n1, n2)))
         (fun (n1, n2) -> Canvas(float n1,float n2))) 

let pCanvas = (pright (pstr "canvas") (inParens pCoords))

(* variable parsers *)
let px = ((pchar 'x') <|> (pchar 'X')) |>> (fun _ -> X)

let py = ((pchar 'y') <|> (pchar 'Y')) |>> (fun _ -> Y)

(* operation parsers *)
let pOpSymbol = (psat
                    (fun c ->
                        c =  '+' || c = '-' || c =  '/' || c = '*' || c= '^')) 

let oper, operImpl = recparser() 

let matchOper o1 c o2 =
    match c with
    | '+' -> Add(o1, o2)
    | '-' -> Sub(o1, o2)
    | '/' -> Div(o1, o2)
    | '*' -> Mult(o1, o2)
    | '^' -> Pow(o1, o2)
    | _ -> OperError

let pOp =
    inParens
        (pseq
            (pleft oper pws0)
            (pseq
                (pleft pOpSymbol pws0)
                oper
                (fun (c, o2) -> (c, o2)))
            (fun (o1, (c, o2)) -> matchOper o1 c o2))

operImpl := pOp <|> pnumber <|> px

(* equality parsers *)
let pEquality =
    pbetween pws0 pws0 (psat(fun c -> c = '=' || c = '<' || c = '>'))

let matchEquality c : Equality =
    match c with
    | '=' -> Equal
    | '<' -> Less
    | '>' -> Greater
    | _ -> EqualityError

(* Equation Parsers *)
let makeEquality y eq o =
    let eq' = matchEquality eq
    Equation(y, eq', o)

let pEquation =
    (pseq
        py
        (pseq 
            pEquality
            oper
            (fun (eq, o) -> (eq, o)))
        (fun (y, (eq, o)) -> makeEquality y eq o))

exprImpl := pEquation <|> pCanvas

(* pexprs
 *  Parses a sequence of expressions.  Sequences are
 *  delimited by whitespace (usually newlines).
 *)
let pexprs = pmany1 (pleft (pad expr) pws0) |>> Sequence

let grammar = pleft pexprs peof

(* parse
 *   User-friendly function that calls the GeoDraw parser
 *   and returns an optional Expr.
 *)
let parse i =
    let i' = prepare i
    match grammar i' with
    | Success(ast, _) -> Some ast
    | Failure(_, _) -> None

