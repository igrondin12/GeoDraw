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

(* GEODRAW GRAMMAR *)
let expr, exprImpl = recparser()

(* Number parsers *)
let int_num =
    pmany1 pdigit
    |>> (fun ds -> Num (float (stringify ds)))

let float_num =
    (pseq
        (pmany0 pdigit)
        (pseq
            (pchar '.')
            (pmany1 pdigit)
            (fun (c, n) -> (c, n)))
        (fun (n', (c, n)) -> Num (float (stringify (n'@[c]@n)))))

let number = float_num <|> int_num


(* variable parsers *)
let px = ((pchar 'x') <|> (pchar 'X')) |>> (fun _ -> X)

let py = ((pchar 'y') <|> (pchar 'Y')) |>> (fun _ -> Y)

(* operation parsers *)
let pOpSymbol = (psat (fun c -> c =  '+' || c = '-' || c =  '/' || c = '*')) 

let oper, operImpl = recparser() 

let matchOper o1 c o2 =
    match c with
    | '+' -> Add(o1, o2)
    | '-' -> Sub(o1, o2)
    | '/' -> Div(o1, o2)
    | '*' -> Mult(o1, o2)
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

operImpl := pOp <|> number <|> px

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

let pExpr =
    (pseq
        py
        (pseq 
            pEquality
            oper
            (fun (eq, o) -> (eq, o)))
        (fun (y, (eq, o)) -> makeEquality y eq o))

exprImpl := pExpr

let grammar = pleft expr peof

