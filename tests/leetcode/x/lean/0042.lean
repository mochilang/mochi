partial def go (arr : Array Int) (left right : Nat) (leftMax rightMax water : Int) : Int :=
  if left > right then
    water
  else if leftMax <= rightMax then
    let v := arr.getD left 0
    if v < leftMax then
      go arr (left + 1) right leftMax rightMax (water + leftMax - v)
    else
      go arr (left + 1) right v rightMax water
  else
    let v := arr.getD right 0
    if v < rightMax then
      go arr left (right - 1) leftMax rightMax (water + rightMax - v)
    else
      go arr left (right - 1) leftMax v water

def trap (h : List Int) : Int :=
  let arr := h.toArray
  if arr.size = 0 then 0 else go arr 0 (arr.size - 1) 0 0 0

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
      out := out.push (toString (trap vals))
    IO.println (String.intercalate "\n" out.toList)
