open System
open System.IO

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    printfn "Problem14: %A" (Solutions.problem14())
    Console.ReadLine()
    0