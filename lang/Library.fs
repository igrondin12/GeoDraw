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
| Sin of Oper
| Cos of Oper
| Sqrt of Oper
| Abs of Oper
| OperError

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
| NoBounds of char list

type Color =
| Color of float list

type Brush =
| Simple
| Funky
| Thick
| Whispy
| Other of string

type Expr =
| Draw of Equation * Bound * Color * Brush
| Canvas of float * float * Color
| Sequence of Expr list
| Assignment of string * (float * float) list

exception Error of string