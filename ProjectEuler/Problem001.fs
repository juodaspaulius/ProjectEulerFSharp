module Problem001

let rec integersFrom n fn = seq { 
  yield n
  yield! integersFrom (fn n) fn }

let answer limit = 
    let limitSeq = Seq.takeWhile (fun n -> n < limit)
    let seq3 = integersFrom 0 ((+) 3) |> limitSeq 
    let seq5 = integersFrom 0 ((+) 5) |> limitSeq 
    let seq15 = integersFrom 0 ((+) 15) |> limitSeq 
    (seq3 |> Seq.sum) + (seq5 |> Seq.sum) - (seq15 |> Seq.sum)