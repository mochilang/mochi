/- Solution for SPOJ EXPAND - Barn Expansion
https://www.spoj.com/problems/EXPAND/
-/

structure Rect where
  a b c d : Nat

def touches (r1 r2 : Rect) : Bool :=
  not (r1.c < r2.a || r2.c < r1.a || r1.d < r2.b || r2.d < r1.b)

partial def solveCase (rects : Array Rect) : Nat :=
  Id.run do
    let n := rects.size
    let mut ok := Array.mkArray n true
    for i in [0:n] do
      for j in [i+1:n] do
        if touches (rects.get! i) (rects.get! j) then
          ok := ok.set! i false
          ok := ok.set! j false
    return ok.foldl (fun acc b => if b then acc + 1 else acc) 0

partial def solve (toks : Array String) : Array String :=
  Id.run do
    let t := toks[0]!.toNat!
    let mut idx := 1
    let mut res : Array String := #[]
    for _ in [0:t] do
      let n := toks[idx]!.toNat!
      idx := idx + 1
      let mut rects : Array Rect := #[]
      for _ in [0:n] do
        let a := toks[idx]!.toNat!
        let b := toks[idx+1]!.toNat!
        let c := toks[idx+2]!.toNat!
        let d := toks[idx+3]!.toNat!
        idx := idx + 4
        rects := rects.push ⟨a,b,c,d⟩
      res := res.push (toString (solveCase rects))
    return res

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.toArray
  let results := solve toks
  for line in results do
    IO.println line
