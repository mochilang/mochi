/- Solution for SPOJ HASHIT - Hash it!
https://www.spoj.com/problems/HASHIT/
-/

import Std
open Std

def hashKey (s : String) : Nat :=
  let rec loop (cs : List Char) (i acc : Nat) : Nat :=
    match cs with
    | [] => acc
    | c :: cs => loop cs (i + 1) (acc + c.toNat * (i + 1))
  let h := loop s.toList 0 0
  (19 * h) % 101

def findIdx (table : Array String) (key : String) : Option Nat :=
  let h := hashKey key
  let rec loop (j : Nat) : Option Nat :=
    if j == 20 then none else
      let idx := (h + j*j + 23*j) % 101
      if table.get! idx == key then some idx else loop (j + 1)
  loop 0

def insert (table : Array String) (key : String) : Array String :=
  if (findIdx table key).isSome then table
  else
    let h := hashKey key
    let rec loop (j : Nat) (tab : Array String) : Array String :=
      if j == 20 then tab else
        let idx := (h + j*j + 23*j) % 101
        if tab.get! idx == "" then
          tab.set! idx key
        else
          loop (j + 1) tab
    loop 0 table


def delete (table : Array String) (key : String) : Array String :=
  match findIdx table key with
  | none => table
  | some idx => table.set! idx ""

partial def processOps (h : IO.FS.Stream) (n : Nat) (table : Array String) : IO (Array String) := do
  if n == 0 then
    pure table
  else
    let line := (← h.getLine).trim
    let parts := line.splitOn ":"
    let table :=
      match parts with
      | ["ADD", key] => insert table key
      | ["DEL", key] => delete table key
      | _ => table
    processOps h (n - 1) table

partial def processCases (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let n := (← h.getLine).trim.toNat!
    let table ← processOps h n (Array.mkArray 101 "")
    let entries := (Array.range 101).foldl (fun acc i =>
      let key := table.get! i
      if key == "" then acc else acc.push (i, key)) #[]
    IO.println entries.size
    for (i, key) in entries do
      IO.println s!"{i}:{key}"
    processCases h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  processCases h t
