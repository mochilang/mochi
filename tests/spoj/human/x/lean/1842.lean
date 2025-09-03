/- Solution for SPOJ LINELAND - Lineland Airport
https://www.spoj.com/problems/LINELAND/
-/

import Std
open Std

partial def solveCase (xs ys : Array Float) (L : Float) : Float := Id.run do
  let n := xs.size
  -- prefix integrals
  let mut pref : Array Float := #[0.0]
  for i in [0:n-1] do
    let dx := xs.get! (i+1) - xs.get! i
    let area := (ys.get! i + ys.get! (i+1)) * dx / 2.0
    let prev := pref.back!
    pref := pref.push (prev + area)
  -- helper to find segment index
  let findSeg := fun (x : Float) =>
    let rec loop (i : Nat) : Nat :=
      if i + 1 < n && xs.get! (i+1) <= x then
        loop (i+1)
      else
        i
    loop 0
  let interp := fun (x : Float) =>
    let i := findSeg x
    if i + 1 = n then
      ys.get! i
    else
      let x0 := xs.get! i
      let y0 := ys.get! i
      let x1 := xs.get! (i+1)
      let y1 := ys.get! (i+1)
      if x1 = x0 then y0 else y0 + (y1 - y0) * (x - x0) / (x1 - x0)
  let areaUpTo := fun (x : Float) =>
    let i := findSeg x
    let acc := pref.get! i
    if i + 1 = n then acc else
      let x0 := xs.get! i
      let y0 := ys.get! i
      let x1 := xs.get! (i+1)
      let y1 := ys.get! (i+1)
      let dx := x - x0
      let yx := y0 + (y1 - y0) * dx / (x1 - x0)
      acc + (y0 + yx) * dx / 2.0
  let left := xs.get! 0
  let right := xs.get! (n-1) - L
  let mut ans : Float := 1e100
  -- collect candidate starts
  let mut cands : Array Float := #[]
  for i in [0:n] do
    cands := cands.push (xs.get! i)
  for i in [0:n] do
    cands := cands.push ((xs.get! i) - L)
  for a in cands do
    if a >= left && a <= right then
      let b := a + L
      let integral := areaUpTo b - areaUpTo a
      let yA := interp a
      let yB := interp b
      let mut m := if yA < yB then yA else yB
      for i in [0:n] do
        let xi := xs.get! i
        if xi > a && xi < b then
          let yi := ys.get! i
          if yi < m then m := yi
      let area := integral - L * m
      if area < ans then ans := area
  return ans

@[main]
def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (· ≠ "") |> Array.ofList
  if toks.size = 0 then
    pure ()
  else
    let t := toks.get! 0 |>.toNat!
    let mut idx := 1
    let mut outs : List String := []
    for _ in [0:t] do
      let n := toks.get! idx |>.toNat!
      let L := toks.get! (idx+1) |>.toFloat!
      idx := idx + 2
      let mut xs : Array Float := #[]
      let mut ys : Array Float := #[]
      for _ in [0:n] do
        xs := xs.push (toks.get! idx |>.toFloat!)
        ys := ys.push (toks.get! (idx+1) |>.toFloat!)
        idx := idx + 2
      let ans := solveCase xs ys L
      outs := (String.formatFloatFixed ans 4) :: outs
    for line in outs.reverse do
      IO.println line
