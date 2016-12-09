open System
open System.IO

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    printfn "Problem1: %A" (Solutions.problem2)
    Console.ReadLine()
    0