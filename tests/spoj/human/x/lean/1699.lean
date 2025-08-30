/- Solution for SPOJ NSYSTEM - Numeral System
https://www.spoj.com/problems/NSYSTEM/
-/

import Std
open Std

/-- Convert an MCXI-string to its numeric value. --/
def parseMCXI (s : String) : Nat :=
  let rec go : List Char → Nat
  | [] => 0
  | c :: cs =>
      let letterVal : Char → Nat
        | 'm' => 1000
        | 'c' => 100
        | 'x' => 10
        | _   => 1
      if c.isDigit then
        match cs with
        | l :: rest =>
            let d := c.toNat - '0'.toNat
            d * letterVal l + go rest
        | [] => 0
      else
        letterVal c + go cs
  go s.data

/-- Convert a number (≤ 9999) to its MCXI-string. --/
def encodeMCXI (n : Nat) : String :=
  let rec build (n : Nat) (pairs : List (Nat × Char)) (acc : String) : String :=
    match pairs with
    | [] => acc
    | (v, ch) :: ps =>
        let q := n / v
        let r := n % v
        let part :=
          if q == 0 then ""
          else if q == 1 then String.singleton ch
          else toString q ++ String.singleton ch
        build r ps (acc ++ part)
  build n [(1000,'m'), (100,'c'), (10,'x'), (1,'i')] ""

/-- Main program: read input, sum pairs, output MCXI representation. --/
def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (·.length > 0)
  let t := (tokens[0]!).toNat!
  let mut idx := 1
  for _ in [0:t] do
    let a := tokens[idx]!
    let b := tokens[idx + 1]!
    let sum := parseMCXI a + parseMCXI b
    IO.println (encodeMCXI sum)
    idx := idx + 2
