module ProjectParser

open CS334
open Parser

(* a list of words that should never be brush names *)
let reserved_brushes = ["whispy"; "simple"; "funky"; "thick" ]

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
   ( pmany1
    (pbetween (pchar '#') peof (pmany1 pnotcom)
    <|> pbetween (pchar '#') pws1 (pmany1 pnotcom)))
    |>> (fun _ -> true)
//    <!> "pcomment"

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

(* parses a positive integer *)
let int_num =
    pmany1 pdigit
    |>> (fun ds -> (float (stringify ds)))

(* parses a negative integer *)
let neg_int_num =
    (pseq
        (pchar '-')
        (int_num)
        (fun (c, n) -> (float ((string c) + (string n)))))

(* parses an integer, converts to a Num *)
let pint_num = (neg_int_num <|> int_num) |>> Num // <!> "pint_num"

(* parses a positive float *)
let float_num =
    (pseq
        (pmany0 pdigit)
        (pseq
            (pchar '.')
            (pmany1 pdigit)
            (fun (c, n) -> (c, n)))
        (fun (n', (c, n)) -> (float (stringify (n'@[c]@n)))))

(* parses a negative float *)
let neg_float_num =
    (pseq
        (pchar '-')
        (float_num)
        (fun (c, n) -> (float ((string c) + (string n)))))

(* parses a float, converts to Num *)
let pfloat_num = (neg_float_num <|> float_num) |>> Num // <!> "pfloat_num"

(* parses a number as a Num *)
let pnumber = pfloat_num <|> pint_num

(* parses a number as a float *)
let number = float_num <|> neg_float_num <|> int_num <|> neg_int_num

(* COLOR PARSER *)
let pColor = pad (inParens (pad (pmany2sep (pad int_num) (pad (pchar ','))))) |>> Color <!> "pColor"

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

(*
 * parses either canvas(x, y) or canvas(x, y, c) where
 * x is the width of the canvas, y is the height, and
 * c is a Color for the background. The default background
 * color is white.
 *)
let pCanvas =
    (pright (pstr "canvas")
        (((pad (inParens pCoords)) |>> (fun (n1, n2) -> Canvas(n1, n2, Color([255.0;255.0;255.0])))) <|>
        ((pad (inParens (pseq
                      (pleft (pad pCoords) (pad (pchar ',')))
                      (pad (pColor))
                      (fun ((n1, n2), c) -> Canvas(n1, n2, c)))))))) <!> "pCanvas"

(* variable parsers *)
let px = ((pad (pchar 'x')) <|> (pad (pchar 'X'))) |>> (fun _ -> X)

let py = ((pad (pchar 'y')) <|> (pad (pchar 'Y'))) |>> (fun _ -> Y)

(* operation parsers *)
let oper, operImpl = recparser() 

(*
 * parsers for the first type of operation, of the form (x OP y) where
 * x and y are also operations.
 *)
let pOpSymbol = (psat
                    (fun c ->
                        c =  '+' || c = '-' || c =  '/' || c = '*' || c= '^')) 
(*
* Matches the parsed math symbol to different operation type
* @param o1  The first operation in the operation
* @param c   The parser charactor of the inputed math symbol
* @param o2  The second operation in the operation
*)
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

(*
 * parsers for the second type of operation, of the form OP(x) where x
 * is another operation.
 *)
let pOpString = (pstr "abs") <|> (pstr "sin") <|> (pstr "cos") <|> (pstr "sqrt")

(*
* Matches the parsed math string expression to different operation types
* @param s   The string corresponsing to the operation
* @param c   The operation
*)
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

operImpl := (pad pOp1) <|> (pad pOp2) <|> (pad pnumber) <|> (pad px)

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
    if eq' = Equal then
        Equation(y, eq', o)
    else
        failwith "Equal sign required in Equations"

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


(* Parses a single bound *)
let bound =
    (pseq
        (pVar)
        (pseq
            (pad pEquality)
            (pad number)
            (fun (eq, n) -> ((matchEquality eq), n)))
        (fun (v, (e, n)) ->
            if e = Less || e = Greater then
                SingleBound((matchVar v), e, n)
            else
                failwith "bounds must have < or >, not ="))

let pBounds = (inBrackets (pmany2sep bound (pchar ','))) |>> BoundList <!> "pBounds"
let pBound = (inBrackets bound) <|> ((inBrackets pws0) |>> NoBounds)

(* BRUSH PARSER *)

(* pBrushName
 *  Parses a brush name. Brush names are at least one character long,
 *  starting with a letter, followed by any combination of letters
 *  or numbers.
 *)
let pBrushChar = pletter <|> pdigit
let pBrushName = pseq pletter (pmany0 pBrushChar |>> stringify)
                     (fun (c, s) -> (string c) + s)

let pPoint =
    (inParens
        (pseq
            (pleft (pad number) (pad (pchar ',')))
            (pad number)
            (fun (n1, n2) -> (n1, n2)))) <!> "pPoint"

(* parses a list of points (i.e. float tuples) *)
let pPointList =
    (inBrackets (pad (pmany2sep pPoint (pad (pchar ','))) <|> (pad (pPoint |>> (fun p -> [p]))))) <!> "pPointList"

(* parses the declaration of a new brush that the user declares *)
let pNewBrush =
    (pseq
        (pleft (pstr "brush") pws1)
        (pseq
            (pleft pBrushName pws0)
            (pright (pad (pchar '=')) (pad  pPointList))
            (fun (n, ps) ->
                if List.contains n reserved_brushes then
                    raise (Error ("cannot use preset brush name"))
                else
                    (n, ps)))
    (fun (b, (n, ps)) -> Assignment(n, ps))) <!> "pNewBrush"
        
(* parses the brush type within a call to draw *)
let pBrush = (pad (inQuotes pBrushName)) |>>
                 (fun s ->
                     match s with
                     | "simple" -> Simple
                     | "funky" -> Funky
                     | "thick" -> Thick
                     | "whispy" -> Whispy
                     | "sparse" -> Sparse
                     | _ -> Other(s)) <!> "pBrush"

(* DRAW PARSER *)
let pDraw = (pright (pstr "draw") (pad (inParens (
    (pseq
        (pleft pEquation (pad (pchar ',')))
        (pseq
            (pleft (pBounds <|> pBound) (pad (pchar ',')))
            (pseq
                (pleft pColor (pad (pchar ',')))
                (pad pBrush)
                (fun (xs, s) -> (xs, s)))
            (fun (bs, (xs, s)) -> (bs, xs, s)))
        (fun (e, (bs, xs, s)) -> Draw(e, bs, xs, s))))))) <!> "pDraw"

(* GRIDLINES PARSER *)
let pGrid =
    (pseq
        (pleft (pstr "gridlines") pws0)
        (pad (inParens (pad int_num)))
        (fun (s, n) -> Gridline(int n))) <!> "pGrid"

exprImpl := pDraw <|> pCanvas <|> pNewBrush <|> pGrid

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
    let i' = debug i
    //let i' = prepare i
    match grammar i' with
    | Success(ast, _) -> Some ast
    | Failure(_, _) -> None

