open System
open System.IO

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    printfn "Problem5: %A" (Solutions.problem5)
    Console.ReadLine()
    0