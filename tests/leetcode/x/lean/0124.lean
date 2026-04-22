import Std

private def solve (vals : Array Int) (ok : Array Bool) : Int :=
  Id.run do
    let mut best := -1000000000
    let mut gain : Array Int := Array.replicate vals.size 0
    for i in (List.range vals.size).reverse do
      if ok.getD i false then
        let li := 2 * i + 1
        let ri := 2 * i + 2
        let left0 := if li < vals.size then gain.getD li 0 else 0
        let right0 := if ri < vals.size then gain.getD ri 0 else 0
        let left := max 0 left0
        let right := max 0 right0
        let total := vals.getD i 0 + left + right
        if total > best then
          best := total
        gain := gain.set! i (vals.getD i 0 + max left right)
    return best

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let lines := ((String.splitOn data "\n").map fun s => s.replace "\r" "").filter (fun s => s != "") |>.toArray
  if lines.isEmpty then pure () else do
    let tc := (lines.getD 0 "0").toNat!
    let mut idx := 1
    let mut out : List String := []
    for _ in [0:tc] do
      let n := (lines.getD idx "0").toNat!
      idx := idx + 1
      let mut vals : Array Int := Array.replicate n 0
      let mut ok : Array Bool := Array.replicate n false
      for i in [0:n] do
        let tok := lines.getD idx "null"
        idx := idx + 1
        if tok != "null" then
          vals := vals.set! i tok.toInt!
          ok := ok.set! i true
      out := out ++ [toString (solve vals ok)]
    IO.print (String.intercalate "\n" out)
