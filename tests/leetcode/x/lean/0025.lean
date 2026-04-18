def revChunk (xs : List Int) : List Int := xs.reverse
partial def revGroups (xs : List Int) (k : Nat) : List Int :=
  if xs.length < k then xs else
    let a := xs.take k
    let b := xs.drop k
    revChunk a ++ revGroups b k

def listString (xs : List Int) : String := "[" ++ String.intercalate "," (xs.map toString) ++ "]"

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := (((String.splitOn input "\n").map fun s => (s.trimAscii).toString).filter (fun line => line != "")).toArray
  if lines.isEmpty then pure () else do
    let t := (lines.getD 0 "0").toNat!
    let mut idx := 1
    let mut out : Array String := #[]
    for _ in [0:t] do
      let n := (lines.getD idx "0").toNat!
      idx := idx + 1
      let mut arr : List Int := []
      for _ in [0:n] do arr := arr ++ [(lines.getD idx "0").toInt!]; idx := idx + 1
      let k := (lines.getD idx "1").toNat!
      idx := idx + 1
      out := out.push (listString (revGroups arr k))
    IO.println (String.intercalate "\n" out.toList)
