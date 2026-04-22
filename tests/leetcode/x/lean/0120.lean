import Std

private def minInt (a b : Int) : Int :=
  if a <= b then a else b

partial def buildRow (row dp : Array Int) (j : Nat) (acc : Array Int) : Array Int :=
  if j < row.size then
    let v := row.getD j 0 + minInt (dp.getD j 0) (dp.getD (j + 1) 0)
    buildRow row dp (j + 1) (acc.push v)
  else
    acc

partial def foldRows (rows : List (Array Int)) (dp : Array Int) : Array Int :=
  match rows with
  | [] => dp
  | row :: rest => foldRows rest (buildRow row dp 0 #[])

private def solve (tri : Array (Array Int)) : Int :=
  match tri.back? with
  | none => 0
  | some last =>
      let upper := (tri.pop.toList.reverse)
      (foldRows upper last).getD 0 0

partial def readRow (toks : Array String) (k r idx : Nat) (row : Array Int) : Array Int × Nat :=
  if k = r then
    (row, idx)
  else
    readRow toks (k + 1) r (idx + 1) (row.push ((toks.getD idx "0").toInt!))

partial def readRows (toks : Array String) (r rows idx : Nat) (acc : Array (Array Int)) : Array (Array Int) × Nat :=
  if r > rows then
    (acc, idx)
  else
    let (row, idx2) := readRow toks 0 r idx #[]
    readRows toks (r + 1) rows idx2 (acc.push row)

partial def runCases (toks : Array String) (t tc idx : Nat) (out : List String) : List String :=
  if tc = t then
    out.reverse
  else
    let rows := (toks.getD idx "0").toNat!
    let (tri, idx2) := readRows toks 1 rows (idx + 1) #[]
    runCases toks t (tc + 1) idx2 (toString (solve tri) :: out)

def main : IO Unit := do
  let input ← (← IO.getStdin).readToEnd
  let lines := (((String.splitOn input "\n").map fun s => s.trimAscii |>.toString).filter (fun line => line != "")).toArray
  if lines.isEmpty then
    pure ()
  else
    let t := (lines.getD 0 "0").toNat!
    let mut idx := 1
    let mut out : List String := []
    for _ in [0:t] do
      let rows := (lines.getD idx "0").toNat!
      idx := idx + 1
      let mut tri : Array (Array Int) := #[]
      for r in [1:rows + 1] do
        let mut row : Array Int := #[]
        for _ in [0:r] do
          row := row.push ((lines.getD idx "0").toInt!)
          idx := idx + 1
        tri := tri.push row
      out := out ++ [toString (solve tri)]
    IO.print (String.intercalate "\n" out)
