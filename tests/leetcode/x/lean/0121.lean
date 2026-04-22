def maxProfit (prices : List Int) : Int :=
  match prices with
  | [] => 0
  | first :: rest =>
      let rec go (xs : List Int) (mn best : Int) : Int :=
        match xs with
        | [] => best
        | p :: ps =>
            let best2 := max best (p - mn)
            let mn2 := if p < mn then p else mn
            go ps mn2 best2
      go rest first 0

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
