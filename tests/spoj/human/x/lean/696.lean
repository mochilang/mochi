-- Solution for SPOJ LIAR - Truth or not
-- https://www.spoj.com/problems/LIAR
import Std
open Std

/-- Count number of set bits in a natural number. --/
private def bitCount : Nat → Nat
| 0 => 0
| n => (n % 2) + bitCount (n / 2)

/-- Check if assignment `mask` is valid for the given matrix. --/
private def valid (n : Nat) (mat : Array (Array Char)) (mask : Nat) : Bool :=
  let rec loopI (i : Nat) (ok : Bool) : Bool :=
    if h : i < n then
      let liar := ((mask >>> i) % 2) = 1
      let row := mat.get! i
      if liar then
        -- need at least one mismatched statement
        let rec loopJ (j : Nat) (mismatch : Bool) : Bool :=
          if hj : j < n then
            let expLiar := row.get! j = 'L'
            let actualLiar := ((mask >>> j) % 2) = 1
            let mismatch := mismatch || (expLiar ≠ actualLiar)
            loopJ (j+1) mismatch
          else
            mismatch
        let hasFalse := loopJ 0 false
        if hasFalse then loopI (i+1) ok else false
      else
        -- all statements must be correct
        let rec loopJ (j : Nat) : Bool :=
          if hj : j < n then
            let expLiar := row.get! j = 'L'
            let actualLiar := ((mask >>> j) % 2) = 1
            if expLiar = actualLiar then
              loopJ (j+1)
            else
              false
          else
            true
        if loopJ 0 then loopI (i+1) ok else false
    else
      ok
  loopI 0 true

/-- Solve one test case. Returns none if paradoxical. --/
private def solveCase (n : Nat) (mat : Array (Array Char)) : Option (Nat × Nat) :=
  let limit := Nat.shiftLeft 1 n
  let rec loop (mask : Nat) (found : Bool) (mn mx : Nat) :=
    if mask < limit then
      if valid n mat mask then
        let cnt := bitCount mask
        let mn := if found then min mn cnt else cnt
        let mx := if found then max mx cnt else cnt
        loop (mask+1) true mn mx
      else
        loop (mask+1) found mn mx
    else
      if found then some (mn, mx) else none
  loop 0 false 0 0

/-- Read all whitespace-separated tokens from stdin. --/
private def readTokens : IO (Array String) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  return s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r').filter (· ≠ "").toArray

/-- Entry point. --/
def main : IO Unit := do
  let toks ← readTokens
  if toks.isEmpty then return
  let t := toks[0]!.toNat!
  let mut idx := 1
  for case in [1:t+1] do
    let n := toks[idx]!.toNat!
    idx := idx + 1
    let mut mat : Array (Array Char) := #[]
    for _ in [0:n] do
      mat := mat.push (toks[idx]!.data.toArray)
      idx := idx + 1
    match solveCase n mat with
    | some (a,b) =>
        IO.println s!"Class Room#{case} contains atleast {a} and atmost {b} liars"
    | none =>
        IO.println s!"Class Room#{case} is paradoxical"
