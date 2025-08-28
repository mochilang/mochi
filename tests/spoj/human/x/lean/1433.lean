/- Solution for SPOJ KPSUM - The Sum
https://www.spoj.com/problems/KPSUM/
-/

import Std
open Std

abbrev Key := Nat × Bool × Bool × Bool × Bool

private def calcS (X : Nat) : (Int × Int) := Id.run do
  let s := toString X
  let digits : Array Nat := s.data.map (fun c => c.toNat - '0'.toNat)
  let L := digits.size
  -- memoization array: (L+1) * 2^4 states
  let states := (L + 1) * 16
  let mut memo : Array (Option (Int × Int × Nat × Nat)) := Array.mkArray states none
  -- index helper
  let idx (pos : Nat) (tight started sign num : Bool) : Nat :=
    (((((pos * 2 + (if tight then 1 else 0)) * 2 + (if started then 1 else 0)) * 2
        + (if sign then 1 else 0)) * 2 + (if num then 1 else 0)))
  -- recursive function
  partial
  def dfs (pos : Nat) (tight started sign num : Bool) : (Int × Int × Nat × Nat) :=
    if h : pos = L then
      if num then (0, 0, 0, 1) else (0, 0, 1, 0)
    else
      let key := idx pos tight started sign num
      match memo[key]? with
      | some (some v) => v
      | _ =>
        let limit := if tight then digits.get! pos else 9
        let mut totSe : Int := 0
        let mut totSo : Int := 0
        let mut totCe : Nat := 0
        let mut totCo : Nat := 0
        for d in [0:limit+1] do
          let ntight := tight && d == limit
          let nstarted := started || d != 0
          let nnum := ((if num then 1 else 0) * 10 + d) % 2 == 1
          let nsign :=
            if nstarted then
              if started then !sign else true
            else
              sign
          let (seChild, soChild, ceChild, coChild) := dfs (pos+1) ntight nstarted nsign nnum
          let mut seChild := seChild
          let mut soChild := soChild
          if nstarted && (started || d != 0) then
            let currSign := if started then sign else false
            let contrib : Int := if currSign then - (Int.ofNat d) else Int.ofNat d
            seChild := seChild + contrib * Int.ofNat ceChild
            soChild := soChild + contrib * Int.ofNat coChild
          totSe := totSe + seChild
          totSo := totSo + soChild
          totCe := totCe + ceChild
          totCo := totCo + coChild
        let res := (totSe, totSo, totCe, totCo)
        memo := memo.set! key (some res)
        res
  let (se, so, _, _) := dfs 0 true false false false
  (se + so, se - so)

-- prefix sums
private def S0 (X : Nat) : Int :=
  if X == 0 then 0 else (calcS X).fst
private def S1 (X : Nat) : Int :=
  if X == 0 then 0 else (calcS X).snd

private def solve (N : Nat) : Int :=
  let L := (toString N).length
  let rec loop (len : Nat) (ten : Nat) (parity : Bool) (acc : Int) : Int :=
    if ten > N then acc
    else
      let a := ten
      let b := min N (ten * 10 - 1)
      let acc :=
        if len % 2 == 0 then
          let s := if parity then (-1 : Int) else 1
          acc + s * (S0 b - S0 (a - 1))
        else
          let s0 := if parity then (-1 : Int) else 1
          let s1 := if a % 2 == 0 then (1 : Int) else -1
          acc + s0 * s1 * (S1 b - S1 (a - 1))
      let digits := (b - a + 1) * len
      loop (len + 1) (ten * 10) (parity ^^ (digits % 2 == 1)) acc
  loop 1 1 false 0

partial def mainLoop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let n := line.trim.toNat!
  if n == 0 then
    pure ()
  else
    IO.println (solve n)
    mainLoop h

def main : IO Unit := do
  let h ← IO.getStdin
  mainLoop h
