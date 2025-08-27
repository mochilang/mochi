/- Solution for SPOJ EMPODIA - Empodia
https://www.spoj.com/problems/EMPODIA/
-/
import Std
open Std

/-- Find all empodia in the given biosequence. An empodio is a segment
of consecutive positions which forms a permutation of consecutive
integers, starting with the minimum value and ending with the maximum
value, and containing no shorter segment with the same property. -/
partial def findEmpodia (arr : Array Nat) : Array (Nat × Nat) :=
  let n := arr.size
  Id.run do
    let mut res : Array (Nat × Nat) := #[]
    for l in [0:n] do
      if l + 1 < n then
        let mut minv := arr[l]!
        let mut maxv := arr[l]!
        for r in [l+1:n] do
          let v := arr[r]!
          if v < minv then minv := v
          if v > maxv then maxv := v
          if minv == arr[l]! && maxv == arr[r]! && maxv > minv && maxv - minv == r - l then
            let mut hasSub := false
            for l2 in [l:r+1] do
              for r2 in [l2+1:r+1] do
                if !hasSub && !(l2 == l && r2 == r) then
                  let mut min2 := arr[l2]!
                  let mut max2 := arr[l2]!
                  for k in [l2+1:r2+1] do
                    let v2 := arr[k]!
                    if v2 < min2 then min2 := v2
                    if v2 > max2 then max2 := v2
                  if min2 == arr[l2]! && max2 == arr[r2]! && max2 > min2 && max2 - min2 == r2 - l2 then
                    hasSub := true
            if !hasSub then
              res := res.push (l+1, r+1)
    return res

/-- Parse all test cases and produce output lines. -/
partial def process (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse else
    let m := toks[idx]!.toNat!
    let arr := Id.run do
      let mut a := Array.mkArray m 0
      for i in [0:m] do
        a := a.set! i (toks[idx+1+i]!.toNat!)
      return a
    let emp := findEmpodia arr
    let mut lines : List String := [toString emp.size]
    for p in emp do
      lines := lines ++ [s!"{p.fst} {p.snd}"]
    process toks (idx + 1 + m) (t - 1) (lines.reverse ++ acc)

/-- Entry point. -/
def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                 |>.filter (fun s => s ≠ "") |> List.toArray
  if toks.size = 0 then
    pure ()
  else
    let t := toks[0]!.toNat!
    let lines := process toks 1 t []
    for line in lines do
      IO.println line
