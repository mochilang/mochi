partial def sortVals : List Int -> List Int
| [] => []
| x :: xs =>
    let smaller := sortVals (xs.filter (fun y => y <= x))
    let larger := sortVals (xs.filter (fun y => y > x))
    smaller ++ [x] ++ larger

def listString (xs : List Int) : String :=
  "[" ++ String.intercalate "," (xs.map toString) ++ "]"

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := (((String.splitOn input "\n").map fun s => (s.trimAscii).toString).filter (fun line => line != "")).toArray
  if lines.isEmpty then pure () else do
    let t := (lines.getD 0 "0").toNat!
    let mut idx := 1
    let mut out : Array String := #[]
    for _ in [0:t] do
      let k := (lines.getD idx "0").toNat!
      idx := idx + 1
      let mut vals : List Int := []
      for _ in [0:k] do
        let n := (lines.getD idx "0").toNat!
        idx := idx + 1
        for _ in [0:n] do
          vals := vals ++ [(lines.getD idx "0").toInt!]
          idx := idx + 1
      out := out.push (listString (sortVals vals))
    IO.println (String.intercalate "\n" out.toList)
