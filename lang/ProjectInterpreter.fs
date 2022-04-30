module ProjectInterpreter

open CS334

(* EVALUATOR *)

// list of points
// plug in a range of values for x and see what that makes y

// function to evaluate an Oper
let rec evalOp o n =
    match o with
    | X -> n
    | Num m -> m 
    | Add(o1, o2) -> (evalOp o1 n) + (evalOp o2 n)
    | Sub(o1, o2) -> (evalOp o1 n) - (evalOp o2 n)
    | Mult(o1, o2) -> (evalOp o1 n) * (evalOp o2 n)
    | Div(o1, o2) -> (evalOp o1 n) / (evalOp o2 n)

let rec gen_points o n =
    match o with
    | _ -> [0.0..0.1..n]
           |> List.map (fun x -> (x, (evalOp o x)))

let eval e =
    match e with
    | Equation (y, eq, op) -> (gen_points op 3.0) //change the three later
   