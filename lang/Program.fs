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

    try
        (* read in the input file *)
        let file = argv.[0]
        let input = File.ReadAllText file

        (* try to parse what they gave us *)
        let ast_maybe = parse input

        (* try to evaluate what we parsed... or not *)
        match ast_maybe with
        | Some ast ->
            let output = eval ast Map.empty
            use sw = new StreamWriter(file + ".svg")    // create svg file
            sw.WriteLine(output)                         // draw in svg file
            printfn "Success! %s created." (file + ".svg")
            0
        | None ->
           printfn "Invalid program."
           1
    with
    | Error(s) -> printfn "Error: %s" s; 1
    | _ -> printfn "Program failed. Exiting"; 1



  
