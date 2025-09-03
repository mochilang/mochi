/- Solution for SPOJ HISTOGRA - Largest Rectangle in a Histogram
https://www.spoj.com/problems/HISTOGRA/
-/
import Std
open Std

private def parseNat (s : String) : Nat :=
  s.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0

partial def largestRect (hs : Array Nat) : Nat :=
  let rec loop (i : Nat) (stack : List Nat) (best : Nat) : Nat :=
    if _h : i < hs.size then
      let height := hs[i]!
      match stack with
      | [] =>
          loop (i+1) (i :: stack) best
      | top :: rest =>
          if height < hs[top]! then
            let width :=
              if rest = [] then i else i - rest.head! - 1
            let area := hs[top]! * width
            loop i rest (Nat.max best area)
          else
            loop (i+1) (i :: stack) best
    else
      match stack with
      | [] => best
      | top :: rest =>
        let width :=
          if rest = [] then hs.size else hs.size - rest.head! - 1
        let area := hs[top]! * width
        loop i rest (Nat.max best area)
  loop 0 [] 0

partial def buildHeights (toks : Array String) (start n : Nat) : Array Nat :=
  let rec go (k : Nat) (acc : Array Nat) : Array Nat :=
    if _h : k < n then
      let v := parseNat (toks[start + k]!)
      go (k+1) (acc.push v)
    else acc
  go 0 #[]

partial def solve (toks : Array String) (idx : Nat) : IO Unit := do
  if _h : idx < toks.size then
    let n := parseNat (toks[idx]!)
    if n = 0 then
      pure ()
    else
      let hs := buildHeights toks (idx+1) n
      IO.println (largestRect hs)
      solve toks (idx + 1 + n)
  else
    pure ()

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (· ≠ "") |> List.toArray
  solve toks 0
