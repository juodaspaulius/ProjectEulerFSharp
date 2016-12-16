open System
open System.IO

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    printfn "Problem3: %A" (Solutions.problem4)
    Console.ReadLine()
    0