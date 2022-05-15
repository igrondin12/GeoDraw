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

(*
 * Accepts whatever p accepts, surrounded by brackets, i.e., [p], and
 * returns whatever p returns.
 * @param p A parser
 *)
let inBrackets p =
    pbetween
        (pchar '[')
        (pchar ']')
        p

(* Accepts whatever p accepts, surrounded by quotes, i.e., 'p' or "p", and
 * returns whatever p returns.
 * @param p A parser
 *)
let inQuotes p =
    (pbetween
        (pchar '\"')
        (pchar '\"')
        p) <|>
    (pbetween
        (pchar '\'')
        (pchar '\'')
        p)


(* Returns a list of p separated by sep
 * @param p    A parser
 * @param sep  A separator parser.
 *)
let pmany2sep p sep =
    pseq
        p
        (pmany1 (pright sep p))
        (fun (x, xs) -> x::xs)

(* pcomment
 *   # Comments start with a hashtag but can't contain
 *   # one (or a newline).
 *)
let pnotcom = psat (fun c -> c <> '#' && c <> '\n')
let pcomment =
    pbetween (pchar '#') peof (pmany1 pnotcom)
    <|> pbetween (pchar '#') pws1 (pmany1 pnotcom)
    |>> (fun _ -> true)
 //   <!> "pcomment"

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

let neg_int_num =
    (pseq
        (pchar '-')
        (int_num)
        (fun (c, n) -> (float ((string c) + (string n)))))

let pint_num = (neg_int_num <|> int_num) |>> Num // <!> "pint_num"

let float_num =
    (pseq
        (pmany0 pdigit)
        (pseq
            (pchar '.')
            (pmany1 pdigit)
            (fun (c, n) -> (c, n)))
        (fun (n', (c, n)) -> (float (stringify (n'@[c]@n)))))

let neg_float_num =
    (pseq
        (pchar '-')
        (float_num)
        (fun (c, n) -> (float ((string c) + (string n)))))

let pfloat_num = (neg_float_num <|> float_num) |>> Num // <!> "pfloat_num"

let pnumber = pfloat_num <|> pint_num
let number = int_num <|> neg_int_num <|> float_num <|> neg_float_num

(* COLOR PARSER *)
let pColor = pad (inParens (pmany2sep int_num (pad (pchar ',')))) |>> Color <!> "pColor"

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
         (fun (n1, n2) -> (float n1,float n2))) 

let pCanvas =
    (pright (pstr "canvas")
        (((inParens pCoords) |>> (fun (n1, n2) -> Canvas(n1, n2, Color([255.0;255.0;255.0])))) <|>
        (inParens (pseq
                      (pleft (pCoords) (pad (pchar ',')))
                      (pColor)
                      (fun ((n1, n2), c) -> Canvas(n1, n2, c)))))) <!> "pCanvas"

(* variable parsers *)
let px = ((pad (pchar 'x')) <|> (pad (pchar 'X'))) |>> (fun _ -> X)

let py = ((pad (pchar 'y')) <|> (pad (pchar 'Y'))) |>> (fun _ -> Y)

(* operation parsers *)
let pOpSymbol = (psat
                    (fun c ->
                        c =  '+' || c = '-' || c =  '/' || c = '*' || c= '^')) 

let oper, operImpl = recparser() 

let matchOper1 o1 c o2 =
    match c with
    | '+' -> Add(o1, o2)
    | '-' -> Sub(o1, o2)
    | '/' -> Div(o1, o2)
    | '*' -> Mult(o1, o2)
    | '^' -> Pow(o1, o2)
    | _ -> OperError

let pOp1 =
    inParens
        (pseq
            (pleft oper pws0)
            (pseq
                (pleft pOpSymbol pws0)
                oper
                (fun (c, o2) -> (c, o2)))
            (fun (o1, (c, o2)) -> matchOper1 o1 c o2))

let pOpString = (pstr "abs") <|> (pstr "sin") <|> (pstr "cos") <|> (pstr "sqrt")

let matchOper2 s o =
    match s with
    | "abs" -> Abs(o)
    | "sin" -> Sin(o)
    | "cos" -> Cos(o)
    | "sqrt" -> Sqrt(o)
    | _ -> OperError

let pOp2 =
    (pseq
        (pad pOpString)
        (pad (inParens (pad oper)))
        (fun (s, o) -> matchOper2 s o)) <!> "pOp2"

operImpl := pOp1 <|> pOp2 <|> pnumber <|> px

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

(* BOUNDS PARSER *)
let pVar = pad ((pchar 'y') <|> (pchar 'Y') <|> (pchar 'x') <|> (pchar 'X'))
let matchVar c : Var =
    match c with
    | 'y' -> Yvar
    | 'Y' -> Yvar
    | 'x' -> Xvar
    | 'X' -> Xvar
    | _ -> VarError

let bound =
    (pseq
        (pVar)
        (pseq
            (pad pEquality)
            (pad number)
            (fun (eq, n) -> ((matchEquality eq), n)))
        (fun (v, (e, n)) -> SingleBound((matchVar v), e, n))) <!> "bound"

let pBounds = (inBrackets (pmany2sep bound (pchar ','))) |>> BoundList <!> "pBounds"
let pBound = (inBrackets bound) <|> (inBrackets pws0 |>> NoBounds)

(* BRUSH PARSER *)

(* pBrushName
 *  Parses a brush name. Brush names are at least one character long,
 *  starting with a letter, followed by any combination of letters
 *  or numbers.
 *)
let pBrushChar = pletter <|> pdigit
let pBrushName = pseq pletter (pmany0 pBrushChar |>> stringify)
                     (fun (c, s) -> (string c) + s)

let pBrush = (pad (inQuotes pBrushName)) |>>
                 (fun s ->
                     match s with
                     | "simple" -> Simple
                     | "funky" -> Funky
                     | "thick" -> Thick
                     | _ -> Other(s)) <!> "pBrush"

(* DRAW PARSER *)
let pDraw = (pright (pstr "draw") (inParens (
    (pseq
        (pleft pEquation (pad (pchar ',')))
        (pseq
            (pleft (pBounds <|> pBound) (pad (pchar ',')))
            (pseq
                (pleft pColor (pad (pchar ',')))
                (pad pBrush)
                (fun (xs, s) -> (xs, s)))
            (fun (bs, (xs, s)) -> (bs, xs, s)))
        (fun (e, (bs, xs, s)) -> Draw(e, bs, xs, s)))))) <!> "pDraw"

exprImpl := pDraw <|> pCanvas

(* pexprs
 *  Parses a sequence of expressions.  Sequences are
 *  delimited by whitespace (usually newlines).
 *)
let pexprs = pmany1 (pleft (pad expr) pws0) |>> Sequence

let grammar = pleft pexprs (peof <|> pcomment)

(* parse
 *   User-friendly function that calls the GeoDraw parser
 *   and returns an optional Expr.
 *)
let parse i =
    //let i' = debug i
    let i' = prepare i
    match grammar i' with
    | Success(ast, _) -> Some ast
    | Failure(_, _) -> None

