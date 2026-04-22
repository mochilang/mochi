def maxProfit (prices : List Int) : Int :=
  let rec go (xs : List Int) (best : Int) : Int :=
    match xs with
    | a :: b :: rest =>
        let best2 := if b > a then best + (b - a) else best
        go (b :: rest) best2
    | _ => best
  go prices 0

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
