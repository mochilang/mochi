def merge (a b : List Int) : List Int :=
  match a, b with
  | [], ys => ys
  | xs, [] => xs
  | x :: xs, y :: ys =>
      if x <= y then
        x :: merge xs (y :: ys)
      else
        y :: merge (x :: xs) ys

def medianString (a b : List Int) : String :=
  let merged := (merge a b).toArray
  let n := merged.size
  if n % 2 = 1 then
    toString (merged.getD (n / 2) 0) ++ ".0"
  else
    let left := merged.getD (n / 2 - 1) 0
    let right := merged.getD (n / 2) 0
    let s := left + right
    if s % 2 = 0 then
      toString (s / 2) ++ ".0"
    else if s < 0 then
      "-" ++ toString (Int.natAbs s / 2) ++ ".5"
    else
      toString (s / 2) ++ ".5"

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := ((String.splitOn input "\n").map fun s => (s.trimAscii).toString).toArray
  if lines.isEmpty then
    pure ()
  else do
    let t := (lines.getD 0 "0").toNat!
    let mut idx := 1
    let mut out : Array String := #[]
    for _ in [0:t] do
      let n := (lines.getD idx "0").toNat!
      let mut a : Array Int := #[]
      for i in [0:n] do
        a := a.push ((lines.getD (idx + 1 + i) "0").toInt!)
      idx := idx + 1 + n
      let m := (lines.getD idx "0").toNat!
      let mut b : Array Int := #[]
      for i in [0:m] do
        b := b.push ((lines.getD (idx + 1 + i) "0").toInt!)
      idx := idx + 1 + m
      out := out.push (medianString a.toList b.toList)
    IO.println (String.intercalate "\n" out.toList)
