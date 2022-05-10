open System
open CS334
open Parser
open ProjectParser
open ProjectInterpreter
open System.IO

[<EntryPoint>]
let main argv =
    (* Check for proper usage *)
    if argv.Length <> 1 then
        printfn "Usage: dotnet run <file>"
        exit 1

    (* read in the input file *)
    let file = argv.[0]
    let input = File.ReadAllText file

    (* try to parse what they gave us *)
    let ast_maybe = parse input

    printfn "%A" ast_maybe

    (* try to evaluate what we parsed... or not *)
    match ast_maybe with
    | Some ast ->
//       let output = eval ast
     //  printfn "%A" ast
//        eval ast |> ignore
        0
       //use sw = new StreamWriter("output.svg")
       //sw.WriteLine(output)    
    | None ->
       printfn "Invalid program."
       1

(*
*    try
*        let input = prepare argv.[0]                  // read in input
*
*
*        match (grammar input) with
*        | Success(res, _) ->
*             let output = eval res  // evaluate parsed input
*             use sw = new StreamWriter("output.svg")
*             sw.WriteLine(output)    
*        | Failure(_,_) -> printfn "nope"
*
*    with 
*    | _ -> printfn "Usage: please enter an Equation. See semantics table for details."       
*    0
  *)    


  
