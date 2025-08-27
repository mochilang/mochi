/- Solution for SPOJ ATMS - Automatic Teller Machines
https://www.spoj.com/problems/ATMS/
-/

import Std
open Std

/-- Read all natural numbers from stdin. --/
def readNats : IO (Array Nat) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Return the positions of 1s in the negabinary representation of `n`.
    Returns `none` if representation requires indices ≥ 100.  Positions
    are returned in decreasing order. --/
def negabinary (n0 : Int) : Option (List Nat) :=
  let rec loop (n : Int) (i : Nat) (acc : List Nat) : Option (List Nat) :=
    if n == 0 then some acc
    else if i ≥ 100 then none
    else
      let r := n % 2
      let n := (r - n) / 2
      let acc := if r == 1 then i :: acc else acc
      loop n (i+1) acc
  loop n0 0 []

/-- Format positions or `No` when none. --/
def formatOut : Option (List Nat) → String
  | none      => "No"
  | some lst  => String.intercalate " " (lst.map toString)

/-- Main program --/
def main : IO Unit := do
  let data ← readNats
  let t := data.get! 0
  let mut idx := 1
  for _ in [0:t] do
    let aNat := data.get! idx; idx := idx + 1
    let a : Int := Int.ofNat aNat
    let loan := negabinary a
    let ret  := negabinary (-a)
    IO.println (formatOut loan)
    IO.println (formatOut ret)
