def maxProfit (prices : List Int) : Int :=
  let rec go (xs : List Int) (buy1 sell1 buy2 sell2 : Int) : Int :=
    match xs with
    | [] => sell2
    | p :: rest =>
        let buy1 := max buy1 (-p)
        let sell1 := max sell1 (buy1 + p)
        let buy2 := max buy2 (sell1 - p)
        let sell2 := max sell2 (buy2 + p)
        go rest buy1 sell1 buy2 sell2
  go prices (-1000000000) 0 (-1000000000) 0

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := ((String.splitOn input "\n").map fun s => s.trimAscii |>.toString).toArray
  if lines.isEmpty then pure () else do
    let t := (lines.getD 0 "0").toNat!
    let mut idx := 1
    let mut out : List String := []
    for _ in [0:t] do
      let n := (lines.getD idx "0").toNat!
      idx := idx + 1
      let mut prices : List Int := []
      for _ in [0:n] do
        prices := prices ++ [(lines.getD idx "0").toInt!]
        idx := idx + 1
      out := out ++ [toString (maxProfit prices)]
    IO.print (String.intercalate "\n" out)
