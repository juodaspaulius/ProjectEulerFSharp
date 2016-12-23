module Solutions

open System
open System.IO

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
            |> Seq.skipWhile(fun factor -> float factor > root)
            |> Seq.forall (fun factor -> n % factor <> 0L) 
        if isPrime then yield n
        yield! primes (n + 1L) (if isPrime then n::sq else sq)
    }
    primes 2L []

let rec seqStep n step = seq {
        if n = 3 then yield 2
        yield n
        yield! seqStep n + step
    }

let problem1 limit = 
    let limit = 1000
    let limitSeq = Seq.takeWhile (fun n -> n < limit)
    let seq3 = integersFrom 0 ((+) 3) |> limitSeq 
    let seq5 = integersFrom 0 ((+) 5) |> limitSeq 
    let seq15 = integersFrom 0 ((+) 15) |> limitSeq 
    (seq3 |> Seq.sum) + (seq5 |> Seq.sum) - (seq15 |> Seq.sum)

let problem2 input = 
    let input = 4000000
    fibonacciSeq 
    |> Seq.takeWhile(fun number -> number < input)
    |> Seq.filter(fun number -> number % 2 = 0)
    |> Seq.sum


let problem3 = 
    let input = 600851475143L
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

let problem4 = 
    [for i in [100..999] do 
        for i2 in [i..999] do
            let product = i * i2
            let productString = product.ToString()
            if String(productString.ToCharArray() |> Array.rev) = productString then
                yield product]
    |> Seq.max

// https://en.wikipedia.org/wiki/Least_common_multiple#Computing_the_least_common_multiple
let rec gcd a b = match b with
    | 0 -> a
    | _ -> gcd b (a % b)
    
let lcm a b = (a / (gcd a b)) * b 

let problem5 =
   let sq = [1..20]
   sq |> Seq.fold lcm 1

let problem6 =
    let sq = [1..100]
    let sumOfSquares = sq |> Seq.map (fun x -> x * x) |> Seq.sum
    let squareOfSum = sq |> Seq.sum |> (fun x -> x * x)
    squareOfSum - sumOfSquares

let problem7 = 
    primeSeq |> Seq.item 10000

let rec findMaxProductSeq seqCount currentResult lst = 
    match lst with
    | [] -> currentResult
    | lst when lst.Length < seqCount -> currentResult
    | x :: xs ->
        let newDigits = lst |> Seq.take seqCount  |> List.ofSeq
        let newProduct = newDigits |> Seq.fold (( * )) 1L
        match currentResult with
        | (_, product) when newProduct > product -> findMaxProductSeq seqCount (newDigits, newProduct) xs
        | _ -> findMaxProductSeq seqCount currentResult xs

let problem8 =
    let input = [
        "73167176531330624919225119674426574742355349194934";
        "96983520312774506326239578318016984801869478851843";
        "85861560789112949495459501737958331952853208805511";
        "12540698747158523863050715693290963295227443043557";
        "66896648950445244523161731856403098711121722383113";
        "62229893423380308135336276614282806444486645238749";
        "30358907296290491560440772390713810515859307960866";
        "70172427121883998797908792274921901699720888093776";
        "65727333001053367881220235421809751254540594752243";
        "52584907711670556013604839586446706324415722155397";
        "53697817977846174064955149290862569321978468622482";
        "83972241375657056057490261407972968652414535100474";
        "82166370484403199890008895243450658541227588666881";
        "16427171479924442928230863465674813919123162824586";
        "17866458359124566529476545682848912883142607690042";
        "24219022671055626321111109370544217506941658960408";
        "07198403850962455444362981230987879927244284909188";
        "84580156166097919133875499200524063689912560717606";
        "05886116467109405077541002256983155200055935729725";
        "71636269561882670428252483600823257530420752963450"]
    let appendOption opt st = 
        match opt with
        | true, v -> v :: st
        | false, _ -> st
    let convertToIntList = string >> System.Int64.TryParse >> appendOption
    input 
    |> Seq.concat 
    |> List.ofSeq
    |> fun lst -> List.foldBack convertToIntList lst []
    |> findMaxProductSeq 13 ([], 0L)

let problem9 = 
    let sum = 1000
    let sq = seq {
        let limit = float sum / 3.0 |> ceil |> int
        for a in [1 .. limit] do
            for b in [a .. sum - a / 2] do
                let c = sum - a - b
                if float a ** 2.0 + float b ** 2.0 = float c ** 2.0 then
                    yield [a; b ; c]}
    sq |> Seq.head |> Seq.fold (*) 1
                
let problem10 = 
    primeSeq 
    |> Seq.takeWhile (fun n -> n < 2000000L)
    |> Seq.sum

let readLines filePath = System.IO.File.ReadLines(filePath)
let baseDirectoryPath = __SOURCE_DIRECTORY__
let baseDirectory = Directory.CreateDirectory(baseDirectoryPath)

let problem11 = 
    let limit = 4
    let fullPath = Path.Combine(baseDirectory.FullName, "Problem11.txt")
    let grid = 
        readLines fullPath 
        |> Seq.map (fun line -> line.Split(' ') |> Seq.map(System.Int32.Parse))
        |> Seq.concat
        |> List.ofSeq
    let gridSize = grid |> List.length |> float |> sqrt |> Math.Round |> Convert.ToInt32
    let findProduct index _ =
        let calcRight = 
            if gridSize - index % gridSize >= limit then 
                [0 .. limit-1] |> Seq.fold (fun s i-> s * grid.[i + index]) 1 
            else 0
        let calcDown = 
            if gridSize - (index / gridSize) >= limit then
                [0 .. limit-1] |> Seq.fold (fun s i -> s * grid.[gridSize * i + index]) 1 
            else 0
        let calcDiag1 =
            if gridSize - index % gridSize  >= limit && gridSize - (index / gridSize) >= limit then
                [0 .. limit-1] |> Seq.fold (fun s i -> s * grid.[index + gridSize * i + i]) 1 
            else 0
        let calcDiag2 =
            if index % gridSize + 1  >= limit && gridSize - (index / gridSize) >= limit then
                [0 .. limit-1] |> Seq.fold (fun s i -> s * grid.[index + gridSize * i - i]) 1 
            else 0
        [calcRight; calcDown; calcDiag1; calcDiag2] 
        |> Seq.max
        |> (fun m -> printfn "%A %A" index m; index, m)
    grid 
    |> Seq.mapi findProduct
    |> Seq.maxBy snd

let problem12 =
    let rec triangleSeq i n = 
        seq {
            yield n + i
            yield! triangleSeq (i + 1) (n+i)
        }
    let countDivisors n =
        let middle = float n |> sqrt |> Math.Floor |> Convert.ToInt32
        [1..middle] 
        |> Seq.filter (fun i -> n % i = 0) 
        |> Seq.length
        |> ( * ) 2
    triangleSeq 1 0
    |> Seq.find (fun i -> countDivisors i > 500)

let problem13 =
    let fullPath = Path.Combine(baseDirectory.FullName, "Problem13.txt")
    let splitLongNumber (n:string) = 
        [0..4] 
        |> Seq.map (fun i -> n.[i*10..i*10+9] |> System.Int64.Parse)
        |> List.ofSeq
    let numbers = 
        readLines fullPath 
        |> Seq.map splitLongNumber
    let countColumnOverflow (s, overflow) i =
        numbers 
        |> Seq.fold (fun s t -> t.[i] + s) overflow
        |> fun i -> (i, i / (Convert.ToInt64 (10.0 ** 10.0)))
    [4.. -1 .. 0]
    |> Seq.fold countColumnOverflow (0L, 0L)
    |> fst
    |> (fun (f:Int64) -> f.ToString().[0..9])
