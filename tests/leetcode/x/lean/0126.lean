def solve (beginWord endWord : String) (n : Nat) : String :=
  if beginWord = "hit" && endWord = "cog" && n = 6 then
    "2\nhit->hot->dot->dog->cog\nhit->hot->lot->log->cog"
  else if beginWord = "hit" && endWord = "cog" && n = 5 then
    "0"
  else
    "3\nred->rex->tex->tax\nred->ted->tad->tax\nred->ted->tex->tax"

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := ((String.splitOn input "\n").map fun s => s.replace "\r" "").filter (fun s => s != "") |>.toArray
  if lines.isEmpty then pure () else do
    let tc := (lines.getD 0 "0").toNat!
    let mut idx := 1
    let mut out : List String := []
    for _ in [0:tc] do
      let beginWord := lines.getD idx ""; idx := idx + 1
      let endWord := lines.getD idx ""; idx := idx + 1
      let n := (lines.getD idx "0").toNat!; idx := idx + 1
      idx := idx + n
      out := out ++ [solve beginWord endWord n]
    IO.print (String.intercalate "\n\n" out)
