module Solutions

let rec integersFrom n fn = seq { 
  yield n
  yield! integersFrom (fn n) fn }

let fibonacciSeq = 
    let rec fibSeq n1 n2 = seq { 
      yield n1
      yield! fibSeq n2 (n1 + n2) }
    fibSeq 1 2

let primeSeq =
    let rec primes n sq = seq {
        let root = float n |> sqrt
        let isPrime = 
            sq 
            |> Seq.filter(fun factor -> float factor < root)
            |> Seq.forall (fun factor -> n % factor <> 0) 
        if isPrime then yield n
        yield! primes (n + 1) (if isPrime then n::sq else sq)
    }
    primes 2 []

let problem1 = 
    let limit = 1000
    let limitSeq = Seq.takeWhile (fun n -> n < limit)
    let seq3 = integersFrom 0 ((+) 3) |> limitSeq 
    let seq5 = integersFrom 0 ((+) 5) |> limitSeq 
    let seq15 = integersFrom 0 ((+) 15) |> limitSeq 
    (seq3 |> Seq.sum) + (seq5 |> Seq.sum) - (seq15 |> Seq.sum)

let problem2 = 
    fibonacciSeq 
    |> Seq.takeWhile(fun number -> number < 4000000)
    |> Seq.filter(fun number -> number % 2 = 0)
    |> Seq.sum

let problem3 = 
    let input = 600851475143L
    // let input = 10L
    let inputRoot = float input |> sqrt
    let isPrime n (sq: int64 list) =
        let root = float n |> sqrt
        sq 
        |> Seq.filter(fun factor -> float factor < root)
        |> Seq.forall (fun factor -> n % factor <> 0L) 

    let rec findMaxFactor input (n:int64) (sq: int64 list) currentMax = 
        // printfn "%A %A" input n
        if n > input then currentMax
        else 
            let isPrimeResult = isPrime n sq
            let newSeq = if isPrimeResult then (n :: sq) else sq
            if isPrimeResult && input % n = 0L then 
                printfn "%A" n
                findMaxFactor (input / n) (n + 1L) newSeq n
            else 
                findMaxFactor input (n + 1L) newSeq currentMax
            

    findMaxFactor input 2L [] 0L 
    
    // let input = 600851475143L
//    primeSeq
//    |> Seq.filter (fun n -> n < input)
//    |> Seq.filter (fun n -> input % n = 0L)
//    |> Seq.max