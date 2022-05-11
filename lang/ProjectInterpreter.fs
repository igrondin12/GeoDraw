module ProjectInterpreter

open CS334

// MACROS
let vbWidth = 200
let vbHeight = 200
let vbDims = " " + (string vbWidth) + " " + (string vbHeight)

// global canvas dimensions
//let canvasWidth = 0.0
//let canvasHeight = 0.0

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
    | Pow(o1, o2) -> (evalOp o1 n) ** (evalOp o2 n)
    | _ -> failwith "Invalid Operation"

(*
 * Generate points to graph for x values between 0 and a given number.
 *
 * @param    n Upper bound for x values.
 * @return     List of float tuples
 *)
let gen_points o n x' y' =
    [0.0..0.1..n] |> List.map (fun x -> (x, (evalOp o x)))
                  |> List.map (fun (x, y) -> (x, y' - y))

(* EVALUATOR *)
let doctype="<?xml version=\"1.0\" standalone=\"no\"?>\n"
let prefix = "<svg viewBox=\"0 0" + vbDims + "\"  xmlns=\"http://www.w3.org/2000/svg\">\n"
let suffix = "</svg>\n"

let draw xs : string =
    let xs' = xs |> List.fold (fun acc (a, b) -> acc + (string a) + ", " + (string b) + " ") ""
    "<polyline points=\"" + xs' + "\" fill=\"none\" stroke=\"black\"/>"
    
let eval expr =
    (* evaluate everything after the canvas line in the program *)
    let rec eval_rest xs cW cH =
        match xs with
        | [] -> ""
        | x::xs' ->
            let s = 
                match x with    
                | Equation (y, eq, op) -> (draw (gen_points op 3.0 cW cH))
                | Canvas (_, _) -> failwith "No canvas calls after first line of program"
                | _ -> failwith "Invalid syntax."
            s + "\n" + (eval_rest xs' cW cH)

    (* format svg code for drawings *)
    let sequence_str =
        match expr with
        | Sequence(es) ->
            let canvas_str = 
                match es.Head with
                | Canvas(x, y) ->
                    let cW = x
                    let cH = y
                    if x > vbWidth || y > vbHeight then
                        failwith "Canvas dimensions outside viewbox macros. See documentation"
                    "<rect width=\"" + (string x) + "\" height=\"" + (string y) + "\" style=\"fill:pink;stroke-width:1;stroke:rgb(0,0,0)\" />\n" + (eval_rest es.Tail cW cH)
                | _ -> failwith "Need to start with a canvas"
            canvas_str
        | _ -> failwith "Need sequence"

    doctype + prefix + sequence_str + suffix

        