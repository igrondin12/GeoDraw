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
//| Sin of Oper
//| Cos of Oper
//| Sqrt of Oper
//| Abs of Oper
//| Pow of Oper * Oper

type Y =
| Y

type Equality =
| Equal
| Less
| Greater

type Equation =
| Equation of Y * Equality * Oper

