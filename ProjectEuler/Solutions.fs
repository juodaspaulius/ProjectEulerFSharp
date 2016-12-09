module Solutions

let rec integersFrom n fn = seq { 
  yield n
  yield! integersFrom (fn n) fn }

let fibonacciSeq = 
    let rec fibSeq n1 n2 = seq { 
      yield n1
      yield! fibSeq n2 (n1 + n2) }
    fibSeq 1 2

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