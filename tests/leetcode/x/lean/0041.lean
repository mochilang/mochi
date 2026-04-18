partial def place (arr : Array Int) (i : Nat) : Array Int :=
  if i < arr.size then
    let v := arr.getD i 0
    if v >= 1 && v <= arr.size && arr.getD (Int.toNat (v - 1)) 0 != v then
      let j := Int.toNat (v - 1)
      let target := arr.getD j 0
      place ((arr.set! i target).set! j v) i
    else
      place arr (i + 1)
  else
    arr

partial def scan (arr : Array Int) (i : Nat) : Int :=
  if i < arr.size then
    if arr.getD i 0 != i + 1 then i + 1 else scan arr (i + 1)
  else
    arr.size + 1

def firstMissingPositive (nums : List Int) : Int :=
  let placed := place nums.toArray 0
  scan placed 0

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := ((String.splitOn input "\n").map fun s => s.trimAscii |>.toString).toArray
  if lines.isEmpty || lines.getD 0 "" = "" then
    pure ()
  else
    let t := (lines.getD 0 "0").toNat!
    let mut idx := 1
    let mut out : Array String := #[]
    for _ in [0:t] do
      let n := (lines.getD idx "0").toNat!
      let vals := (List.range n).map fun k => (lines.getD (idx + 1 + k) "0").toInt!
      idx := idx + 1 + n
      out := out.push (toString (firstMissingPositive vals))
    IO.println (String.intercalate "\n" out.toList)
