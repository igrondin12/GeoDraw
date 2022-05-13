module CS334

open Parser

(* AST *)
type Oper =
| X
| Num of float
| Mult of Oper * Oper
| Sub of Oper * Oper
| Div of Oper * Oper
| Add of Oper * Oper
| Pow of Oper * Oper
| OperError
//| Sin of Oper
//| Cos of Oper
//| Sqrt of Oper
//| Abs of Oper

type Y =
| Y

type Equality =
| Equal
| Less
| Greater
| EqualityError

type Var =
| Xvar
| Yvar
| VarError

type Equation =
| Equation of Y * Equality * Oper

type Bound =
| SingleBound of Var * Equality * float
| BoundList of Bound list

type Color =
| Color of float list

type Brush =
| Simple of string
| Other of string

type Expr =
| Draw of Equation * Bound * Color * Brush
| Canvas of float * float
| Sequence of Expr list
