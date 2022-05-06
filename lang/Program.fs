open System
open CS334
open Parser
open ProjectParser
open ProjectInterpreter
open System.IO

[<EntryPoint>]
let main argv =
    try
        let input = prepare argv.[0]                  // read in input
(*
*        let ast_maybe = grammar input
*        match ast_maybe with
*        | Some ast ->
*            let output = eval ast
*            use sw = new StreamWriter("output.svg")
*            sw.WriteLine(output)
*        | None ->
*            printfn "Invalid program."
*            exit 1
*        0    
*)

        match (grammar input) with
        | Success(res, _) ->
             let output = eval res  // evaluate parsed input
             use sw = new StreamWriter("output.svg")
             sw.WriteLine(output)    
        | Failure(_,_) -> printfn "nope"

    with 
    | _ -> printfn "Usage: please enter an Equation. See semantics table for details."       
    0
      


  
