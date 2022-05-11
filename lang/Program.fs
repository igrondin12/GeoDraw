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
        let output = eval ast
        use sw = new StreamWriter("output.svg")    // create svg file
        sw.WriteLine(output)                       // draw in svg file
        0
    | None ->
       printfn "Invalid program."
       1



  
