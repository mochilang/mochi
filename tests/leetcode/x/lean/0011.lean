def maxArea (h : List Int) : Int :=
  let arr := h.toArray
  let rec go (l r : Nat) (best : Int) : Int :=
    if l >= r then best else
      let left := arr.get! l
      let right := arr.get! r
      let height := if left < right then left else right
      let best := max best ((r - l) * height)
      if left < right then go (l + 1) r best else go l (r - 1) best
  go 0 (arr.size - 1) 0

def main : IO Unit := do
  let input <- IO.getStdin.readToEnd
  let lines := input.splitOn "\n" |>.map String.trim
  if lines.isEmpty then pure () else do
    let t := lines.head!.toNat!
    let rec loop (tc idx : Nat) (out : List String) : List String :=
      if tc = t then out.reverse else
        let n := (lines.getD idx "0").toNat!
        let vals := (List.range n).map fun i => (lines.getD (idx + 1 + i) "0").toInt!
        loop (tc + 1) (idx + 1 + n) (toString (maxArea vals) :: out)
    IO.println (String.intercalate "\n" (loop 0 1 []))
