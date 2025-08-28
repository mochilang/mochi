/- Solution for SPOJ CTRICK - Card Trick
https://www.spoj.com/problems/CTRICK/
-/

import Std
open Std

structure Queue (α) where
  front : List α
  back : List α

namespace Queue

@[simp] def empty : Queue α := {front := [], back := []}

@[simp] def push (q : Queue α) (a : α) : Queue α :=
  {front := q.front, back := a :: q.back}

@[simp] def normalize (q : Queue α) : Queue α :=
  match q.front with
  | [] => {front := q.back.reverse, back := []}
  | _  => q

@[simp] def pop? (q : Queue α) : Option (α × Queue α) :=
  match q.normalize.front with
  | []      => none
  | x :: xs => some (x, {front := xs, back := q.normalize.back})

end Queue

open Queue

partial def rotate (q : Queue α) (k : Nat) : Queue α :=
  if k == 0 then q
  else
    match pop? q with
    | none => q
    | some (x, q') => rotate (push q' x) (k - 1)

partial def arrangement (n : Nat) : List Nat :=
  let init : Queue Nat := {front := (List.range n).map (· + 1), back := []}
  let rec loop (k : Nat) (q : Queue Nat) (res : Array Nat) : Array Nat :=
    if k > n then res
    else
      let q := rotate q k
      match pop? q with
      | none => res
      | some (pos, q') =>
          loop (k + 1) q' (res.set! (pos - 1) k)
  (loop 1 init (Array.mkArray n 0)).toList

partial def solve (tokens : Array String) (idx : Nat) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let n := (tokens.get! idx).toNat!
    let arr := arrangement n
    IO.println (String.intercalate " " (arr.map (fun x => toString x)))
    solve tokens (idx + 1) (t - 1)

def main : IO Unit := do
  let data ← IO.readStdin
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (fun s => s ≠ "")
  let t := (tokens.get! 0).toNat!
  solve tokens 1 t
