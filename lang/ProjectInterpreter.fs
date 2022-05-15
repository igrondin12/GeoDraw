module ProjectInterpreter

open CS334
open System

// MACROS
let vbWidth = 600
let vbHeight = 600
let vbDims = " " + (string vbWidth) + " " + (string vbHeight)
let brushWidth = 5.0
let brushHeight = 5.0
let offset = 40    // used to add randomness to brushes

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
    | Abs(o) -> abs (evalOp o n)
    | Sin(o) -> Math.Sin (evalOp o n)
    | Cos(o) -> Math.Cos (evalOp o n)
    | Sqrt(o) -> sqrt (evalOp o n)
    | _ -> failwith "Invalid Operation"

(*
 * Generate points to graph for x values between 0 and a given number.
 *
 * @param   bm Map of bounds
 * @param    o Operation to evaluate.
 * @param   ch Canvas height (to flip right side up)
 * @return     List of float tuples
 *)
let gen_points o (bm:Map<string, float>) cH =
    let xL:float = bm.["xL"]
    let xU:float = bm.["xU"]
    let yL:float = bm.["yL"]
    let yU:float = bm.["yU"]
    [xL..0.1..xU] |> List.map (fun x -> Math.Round(x, 1)) 
                  |> List.map (fun x -> (x, (evalOp o x)))
                  |> List.filter (fun (x, y) -> x < xU && x > xL)
                  |> List.map (fun (x, y) -> (x, cH - y))
                  |> List.filter (fun (x, y) -> y > (cH - yU) && y < (cH - yL))

(* EVALUATOR *)
let doctype="<?xml version=\"1.0\" standalone=\"no\"?>\n"
let prefix = "<svg viewBox=\"0 0" + vbDims + "\"  xmlns=\"http://www.w3.org/2000/svg\">\n"
let suffix = "</svg>\n"

(*
 * Combine a list of RGB values for a color into one string with colons
 * in between. ie [1;2;3] goes to "1, 2, 3"
 * @param cs    A list of color values
 * @return      A string
 *)
let gen_col_str c : string =
    let cs =
        match c with
        | Color(lst) -> lst
    let rec helper cs : string list =
        match cs with
        | [] -> []
        | c::cs' ->
            if c <= 255.0 then
                [(string c)]@(helper cs')
            else
                failwith "color value must be less than 256"
    (helper cs) |> String.concat ", "

let draw xs cs brush : string =
    let xs' = xs |> List.fold (fun acc (a, b) -> acc + (string a) + ", " + (string b) + " ") ""
    let col_str = "rgb(" + (gen_col_str cs) + ")"
    "<polyline points=\"" + xs' + "\" fill=\"none\" stroke=\"" + col_str + "\" stroke-width=\"0.5\"/>"

(*
 * Fill in a map of bounds for an equation with the correct values
 * @param bound    Bound to add to map.
 * @param map      Map of bounds for the equation to modify
 * @param cW       Total width for the canvas
 * @param cH       Total height for the canvas
 * @return         The modified map with the bound added
 *)
let boundEval bound (map: Map<string, float>) cW cH =
    let (v, eq, f) =
        match bound with
        | SingleBound(v, eq, f) -> (v, eq, f)
        | _ -> failwith "nope"
    let key = 
        match (v, eq, f) with
        | Xvar, Less, _ -> if f < cW then "xU" else failwith "nope"
        | Xvar, Greater, _ -> if f < cW then "xL" else failwith "nope"
        | Yvar, Less, _ -> if f < cH then "yU" else failwith "nope"
        | Yvar, Greater, _ -> if f < cH then "yL" else failwith "nope"
        | _, _, _ -> failwith "invalid bound"

    let map' = map.Add(key, f)
    map'

(*
 * Generate a list of points of a given operation within the bounds provided
 * by the user.
 * @param op      The operation to evaluate.
 * @param bounds  A list of Bounds
 * @param cW      The canvas width
 * @param cH      The canvas height
 * @return        A list of points
 *)
let make_bounds_map op bound cW cH =
    let bm = [("xL", 0.0); ("xU", cW); ("yL", 0.0); ("yU", cH)] |> Map.ofList
    let bs =
        match bound with
        | SingleBound(v, eq, f) -> [SingleBound(v, eq, f)]
        | BoundList(bs) -> bs
        | NoBounds(cs) -> []
    let rec helper bounds map = 
        match bounds with
        | [] -> map
        | b::bs' ->
            let map' = (boundEval b map cW cH)
            helper bs' map'
    helper bs bm

(*
 * Generate svg code for multiple lines that make up a single brush stroke
 * @param op    operation for the line
 * @param bs    bounds for the line
 * @param cW    canvas width
 * @param cH    canvas height
 * @param b     type of brush to use
 *)
let brush_stroke op bs cW cH cs b =
    let bm = make_bounds_map op bs cW cH
    let points = gen_points op bm cH
    let brush_map : Map<string, (float * float) list> =
        Map.empty.
            Add("funky", [(3.0, 3.0); (2.0, 3.0); (3.0, 2.0)]).
            Add("thick", [(1.0, 1.0); (2.0, 2.0); (3.0, 4.0)])

    let rec helper (points: (float * float) list) (xs: (float * float) list) =
        match xs with
            | [] -> ""
            | x::xs' ->
                let difX = (fst x) - brushWidth
                let difY = (snd x) - brushHeight
                let r = System.Random()
                let points' = points |> List.map(fun (px, py) -> ((px + difX + ((float (r.Next offset)) / 100.0)), (py - difY - ((float (r.Next offset)) / 100.0))))
                                     |> List.filter (fun (px, py) -> py > (cH - bm.["yU"]) && py < (cH - bm.["yL"]))
                                     |> List.filter (fun (px, py) -> px < bm.["xU"] && px > bm.["xL"])

                (draw points' cs b) + (helper points xs')

    match b with
    | Simple -> (draw points cs b)
    | Funky -> helper points brush_map.["funky"]
    | Thick -> helper points brush_map.["thick"]
    | Other(s) -> (draw points cs b) 

let eval expr =
    (* evaluate everything after the canvas line in the program *)
    let rec eval_rest xs cW cH =
        match xs with
        | [] -> ""
        | x::xs' ->
            let s = 
                match x with
                | Draw (e, bs, cs, b) ->
                    match e with
                    | Equation(y, eq, op) -> (brush_stroke op bs cW cH cs b) 
                | Canvas (_, _, _) -> failwith "No canvas calls after first line of program"
                | _ -> failwith "Invalid syntax."
            s + "\n" + (eval_rest xs' cW cH)

    (* format svg code for drawings *)
    let sequence_str =
        match expr with
        | Sequence(es) ->
            let canvas_str = 
                match es.Head with
                | Canvas(x, y, c) ->
                    let cW = x
                    let cH = y
                    if x > vbWidth || y > vbHeight then
                        failwith "Canvas dimensions outside viewbox macros. See documentation"
                    "<rect width=\"" + (string x) + "\" height=\"" + (string y) + "\" style=\"fill:rgb(" + (gen_col_str c) + ");stroke-width:1;stroke:rgb(0,0,0)\" />\n" + (eval_rest es.Tail cW cH) + "<rect width=\"" + (string x) + "\" height=\"" + (string y) + "\" style=\"fill:rgba(0,0,0,0);stroke-width:1;stroke:rgb(0,0,0)\" />\n"
//                    "<rect width=\"" + (string x) + "\" height=\"" + (string y) + "\" style=\"fill:rgba(0,0,0,0);stroke-width:1;stroke:rgb(0,0,0)\" />\n" + (eval_rest es.Tail cW cH)
                | _ -> failwith "Need to start with a canvas"
            canvas_str
        | _ -> failwith "Need sequence"

    
    doctype + prefix + sequence_str + suffix

        