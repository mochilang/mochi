/- Solution for SPOJ MMIND - The Game of Master-Mind
https://www.spoj.com/problems/MMIND/
-/

import Std
open Std

structure Guess where
  vals : Array Nat
  b : Nat
  w : Nat
  cnt : Array Nat -- size C+1

def mkGuess (vals : Array Nat) (b w : Nat) (C : Nat) : Guess :=
  let mut arr := Array.mkArray (C+1) 0
  for v in vals do
    arr := arr.set! v (arr.get! v + 1)
  { vals := vals, b := b, w := w, cnt := arr }

partial def dfs (P C M : Nat) (guesses : Array Guess)
    (pos : Nat) (cand : Array Nat) (pref : Array Nat)
    (bvec mvec : Array Nat) : Option (Array Nat) :=
  if pos = P then
    let rec check (i : Nat) : Bool :=
      if i = M then true else
        let g := guesses.get! i
        let total := g.b + g.w
        bvec.get! i = g.b && mvec.get! i = total && check (i+1)
    if check 0 then some cand else none
  else
    let r := P - (pos+1)
    let rec try (c : Nat) : Option (Array Nat) :=
      if c > C then none else
        let pref' := pref.set! c (pref.get! c + 1)
        let cand' := cand.set! pos c
        let rec upd (idx : Nat) (bv mv : Array Nat) : Option (Array Nat × Array Nat) :=
          if idx = M then some (bv, mv) else
            let g := guesses.get! idx
            let b := bv.get! idx
            let m := mv.get! idx
            let nb := b + (if c = g.vals.get! pos then 1 else 0)
            let nm := m + (if pref'.get! c ≤ g.cnt.get! c then 1 else 0)
            let total := g.b + g.w
            if nb > g.b || nm > total || nb + r < g.b || nm + r < total then
              none
            else
              upd (idx+1) (bv.set! idx nb) (mv.set! idx nm)
        match upd 0 bvec mvec with
        | none => try (c+1)
        | some (bv', mv') =>
            match dfs P C M guesses (pos+1) cand' pref' bv' mv' with
            | some ans => some ans
            | none     => try (c+1)
    try 1

partial def solveCase (toks : Array String) (idx : Nat) : String × Nat :=
  let P := toks.get! idx |>.toNat!
  let C := toks.get! (idx+1) |>.toNat!
  let M := toks.get! (idx+2) |>.toNat!
  let mut i := idx + 3
  let mut gs : Array Guess := Array.mkArray M default
  for j in [0:M] do
    let mut vals := Array.mkArray P 0
    for k in [0:P] do
      vals := vals.set! k (toks.get! i |>.toNat!)
      i := i + 1
    let b := toks.get! i |>.toNat!
    let w := toks.get! (i+1) |>.toNat!
    i := i + 2
    gs := gs.set! j (mkGuess vals b w C)
  let cand := Array.mkArray P 0
  let pref := Array.mkArray (C+1) 0
  let bvec := Array.mkArray M 0
  let mvec := Array.mkArray M 0
  let res := dfs P C M gs 0 cand pref bvec mvec
  match res with
  | some arr =>
      let nums := (List.range P).map (fun k => toString (arr.get! k))
      (String.intercalate " " nums, i)
  | none => ("You are cheating!", i)

partial def solveAll (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse else
    let (out, ni) := solveCase toks idx
    solveAll toks ni (t-1) (out :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r').filter (fun s => s ≠ "") |> Array.ofList
  let T := toks.get! 0 |>.toNat!
  let outs := solveAll toks 1 T []
  for line in outs do
    IO.println line
