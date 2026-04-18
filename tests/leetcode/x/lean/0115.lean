import Std

private def solve (s t : String) : Int :=
  Id.run do
    let b := t.toList.toArray
    let mut dp : Array Int := #[1] ++ Array.replicate b.size 0
    for ch in s.toList do
      for j in (List.range b.size).reverse do
        if ch = b.getD j ' ' then
          dp := dp.set! (j + 1) (dp.getD (j + 1) 0 + dp.getD j 0)
    return dp.getD b.size 0

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let lines := ((String.splitOn data "\n").map fun s => s.replace "\r" "").toArray
  if lines.isEmpty || lines.getD 0 "" = "" then pure () else
    let tc := ((lines.getD 0 "0").trimAscii).toNat!
    let mut out : Array String := #[]
    for i in List.range tc do
      out := out.push (toString (solve (lines.getD (1 + 2 * i) "") (lines.getD (2 + 2 * i) "")))
    IO.print (String.intercalate "\n" out.toList)
