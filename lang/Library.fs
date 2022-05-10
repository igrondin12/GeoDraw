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

//type Equation =
//| Equation of Y * Equality * Oper

//type Canvas =
//| Canvas of float * float

type Expr =
| Equation of Y * Equality * Oper
| Canvas of float * float
| Sequence of Expr list
