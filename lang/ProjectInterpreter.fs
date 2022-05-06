module ProjectInterpreter

open CS334

(*
 * Evaluate an operation given a value for x.
 *
 * @param    o Operation to evaluate.
 * @param    n x's value.
 * @return     Operation's value given x.
 *)
let rec evalOp o n =
    match o with
    | X -> n
    | Num m -> m 
    | Add(o1, o2) -> (evalOp o1 n) + (evalOp o2 n)
    | Sub(o1, o2) -> (evalOp o1 n) - (evalOp o2 n)
    | Mult(o1, o2) -> (evalOp o1 n) * (evalOp o2 n)
    | Div(o1, o2) -> (evalOp o1 n) / (evalOp o2 n)
    | _ -> failwith "Invalid Operation"

(*
 * Generate points to graph for x values between 0 and a given number.
 *
 * @param    n Upper bound for x values.
 * @return     List of float tuples
 *)
let gen_points o n =
    [0.0..0.1..n] |> List.map (fun x -> (x, (evalOp o x)))

(* EVALUATOR *)
let doctype="<?xml version=\"1.0\" standalone=\"no\"?>\n"
let prefix = "<svg viewBox=\"0 0 100 100\"  xmlns=\"http://www.w3.org/2000/svg\">\n"
let suffix = "</svg>\n"

let draw xs : string =
    let xs' = xs |> List.fold (fun acc (a, b) -> acc + (string a) + ", " + (string b) + " ") ""
    "<polyline points=\"" + xs' + "\" fill=\"none\" stroke=\"black\"/>"
    
let eval e =
    let str =
        match e with
        | Equation (y, eq, op) -> (draw (gen_points op 3.0)) //change the three later
    doctype + prefix + str + suffix
        