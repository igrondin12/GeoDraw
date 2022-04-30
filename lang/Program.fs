open System
open CS334
open Parser
open ProjectParser
open ProjectInterpreter

[<EntryPoint>]
let main argv =
    let input = prepare "y = (x + 0.1)"
//    printfn "%A" (gen_points (Oper("(2 + 3)")) 3.0)
    match (grammar input) with
    | Success(res, _) -> printfn "%A" (eval res)
    | Failure(_,_) -> printfn "nope"
    0
