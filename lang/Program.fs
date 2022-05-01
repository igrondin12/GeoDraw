open System
open CS334
open Parser
open ProjectParser
open ProjectInterpreter

[<EntryPoint>]
let main argv =
    try
        let input = prepare argv.[0]                  // read in input
        match (grammar input) with
        | Success(res, _) -> printfn "%A" (eval res)  // evaluate parsed input
        | Failure(_,_) -> printfn "nope"
    with 
    | _ -> printfn "Usage: please enter an Equation. See semantics table for details."       
    0
      


  
