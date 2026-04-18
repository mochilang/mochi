def removeAt : Nat → List String → (String × List String)
  | _, [] => ("", [])
  | 0, x :: xs => (x, xs)
  | n + 1, x :: xs =>
      let (y, rest) := removeAt n xs
      (y, x :: rest)

def fact : Nat → Nat
  | 0 => 1
  | n + 1 => (n + 1) * fact n

def build (digits : List String) (k : Nat) : Nat → String
  | 0 => ""
  | rem + 1 =>
      let block := fact rem
      let idx := k / block
      let k' := k % block
      let (picked, rest) := removeAt idx digits
      picked ++ build rest k' rem

def getPermutation (n k : Nat) : String :=
  let digits := (List.range n).map (fun i => toString (i + 1))
  build digits (k - 1) n

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := ((String.splitOn input "\n").map fun s => s.trimAscii |>.toString).toArray
  if lines.isEmpty || lines.getD 0 "" = "" then pure () else do
    let t := (lines.getD 0 "0").toNat!
    let mut idx := 1
    let mut out : List String := []
    for _ in [0:t] do
      let n := (lines.getD idx "0").toNat!; idx := idx + 1
      let k := (lines.getD idx "0").toNat!; idx := idx + 1
      out := out ++ [getPermutation n k]
    IO.println (String.intercalate "\n" out)
