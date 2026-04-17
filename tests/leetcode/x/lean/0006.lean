def convertZigzag (s : String) (numRows : Nat) : String :=
  if numRows <= 1 ∨ numRows >= s.length then
    s
  else
    let cycle := 2 * numRows - 2
    let rec rows (row : Nat) (acc : String) : String :=
      if row < numRows then
        let rec collect (i : Nat) (acc2 : String) : String :=
          if i < s.length then
            let acc3 := acc2.push (s.get ⟨i, by omega⟩)
            let diag := i + cycle - 2 * row
            let acc4 := if row > 0 ∧ row + 1 < numRows ∧ diag < s.length then acc3.push (s.get ⟨diag, by omega⟩) else acc3
            collect (i + cycle) acc4
          else
            acc2
        rows (row + 1) (collect row acc)
      else
        acc
    rows 0 ""

def main : IO Unit := do
  let input <- IO.getStdin.readToEnd
  let lines := input.splitOn "\n"
  if lines.isEmpty then
    pure ()
  else do
    let t := (lines.head!.trim).toNat!
    let rec loop (n idx : Nat) (acc : List String) : List String :=
      if n = 0 then acc.reverse
      else
        let s := (lines.getD idx "").trimRight
        let r := ((lines.getD (idx + 1) "1").trim).toNat!
        loop (n - 1) (idx + 2) (convertZigzag s r :: acc)
    IO.println (String.intercalate "\n" (loop t 1 []))
