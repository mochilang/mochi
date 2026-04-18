def flatten : List (List (List String)) → List (List String)
  | [] => []
  | x :: xs => x ++ flatten xs

partial def dfs (r n : Nat) (cols d1 d2 : Array Bool) (board : List String) : List (List String) :=
  if r = n then [board.reverse]
  else
    flatten (List.map (fun c =>
      let a := r + c
      let b := r + (n - 1 - c)
      if cols.getD c false || d1.getD a false || d2.getD b false then []
      else
        let row := String.ofList (List.replicate c '.' ++ ['Q'] ++ List.replicate (n - c - 1) '.')
        dfs (r + 1) n (cols.set! c true) (d1.set! a true) (d2.set! b true) (row :: board)) (List.range n))

def solve (n : Nat) : List (List String) :=
  dfs 0 n (Array.replicate n false) (Array.replicate (2*n) false) (Array.replicate (2*n) false) []

def emitBody : List (List String) → List String
  | [] => []
  | [sol] => sol
  | sol :: rest => sol ++ ["-"] ++ emitBody rest

def emit (sols : List (List String)) : List String :=
  toString sols.length :: emitBody sols

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := ((String.splitOn input "\n").map fun s => s.trimAscii |>.toString).toArray
  if lines.isEmpty || lines.getD 0 "" = "" then pure () else do
    let t := (lines.getD 0 "0").toNat!
    let mut idx := 1
    let mut out : List String := []
    for tc in [0:t] do
      let n := (lines.getD idx "0").toNat!
      idx := idx + 1
      out := out ++ emit (solve n)
      if tc + 1 < t then out := out ++ ["="]
    IO.println (String.intercalate "\n" out)
