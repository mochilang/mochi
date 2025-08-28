/- Solution for SPOJ PAIRINT - Pairs of Integers
https://www.spoj.com/problems/PAIRINT/
-/

import Std
open Std

def repeatChar (c : Char) (n : Nat) : String :=
  String.mk (List.replicate n c)

def numDigits (n : Nat) : Nat :=
  let rec loop (m acc : Nat) :=
    if m < 10 then acc + 1 else loop (m / 10) (acc + 1)
  loop n 0

def padZero (n w : Nat) : String :=
  let s := toString n
  repeatChar '0' (w - s.length) ++ s

def findPairs (N : Nat) : List (Nat × String) :=
  Id.run do
    let mut res : List (Nat × String) := []
    let mut seen : Std.HashSet Nat := {}
    let mut pow : Nat := 1
    while pow ≤ N do
      let rem := N % pow
      let mut lows : List Nat := []
      if rem % 2 == 0 then
        let low := rem / 2
        if low < pow then
          lows := low :: lows
      if (rem + pow) % 2 == 0 then
        let low := (rem + pow) / 2
        if low < pow then
          lows := low :: lows
      for low in lows do
        let N2 := N - 2*low
        if N2 % pow == 0 then
          let q := N2 / pow
          let digit := q % 11
          let high := q / 11
          if digit ≤ 9 then
            let X := high*pow*10 + digit*pow + low
            let Y := high*pow + low
            if X + Y == N && X ≥ 10 && X ≥ pow && !seen.contains X then
              let d := numDigits X
              let yStr := padZero Y (d - 1)
              res := (X, yStr) :: res
              seen := seen.insert X
      pow := pow * 10
    return res.qsort (fun a b => a.fst < b.fst)

def solveCase (N : Nat) : List String :=
  let pairs := findPairs N
  let lines := pairs.map (fun (x, y) => s!"{x} + {y} = {N}")
  toString pairs.length :: lines

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let t := toks.get! 0 |>.toNat!
  let mut idx := 1
  for _ in [0:t] do
    let N := toks.get! idx |>.toNat!
    idx := idx + 1
    let outLines := solveCase N
    for line in outLines do
      IO.println line
