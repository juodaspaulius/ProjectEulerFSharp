open System
open System.IO

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    printfn "Problem1: %A" (Problem001.answer 1000)
    Console.ReadLine()
    0