partial def countDfs (r n : Nat) (cols d1 d2 : Array Bool) : Nat :=
  if r = n then 1
  else
    ((List.range n).foldl (fun acc c =>
      let a := r + c
      let b := r + (n - 1 - c)
      if cols.getD c false || d1.getD a false || d2.getD b false then acc
      else acc + countDfs (r + 1) n (cols.set! c true) (d1.set! a true) (d2.set! b true)) 0)

def solve (n : Nat) : Nat :=
  countDfs 0 n (Array.replicate n false) (Array.replicate (2*n) false) (Array.replicate (2*n) false)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := ((String.splitOn input "\n").map fun s => s.trimAscii |>.toString).toArray
  if lines.isEmpty || lines.getD 0 "" = "" then pure () else do
    let t := (lines.getD 0 "0").toNat!
    let mut idx := 1
    let mut out : List String := []
    for _ in [0:t] do
      let n := (lines.getD idx "0").toNat!
      idx := idx + 1
      out := out ++ [toString (solve n)]
    IO.println (String.intercalate "\n" out)
