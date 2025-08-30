/- Solution for SPOJ CASHIER - Blue Mary Needs Help Again
https://www.spoj.com/problems/CASHIER/
-/
import Std
open Std

partial def lowerBoundAux (a : Array Int) (x : Int) (lo hi : Nat) : Nat :=
  if h : lo < hi then
    let mid := (lo + hi) / 2
    if a[mid]! < x then
      lowerBoundAux a x (mid+1) hi
    else
      lowerBoundAux a x lo mid
  else
    lo

def lowerBound (a : Array Int) (x : Int) : Nat :=
  lowerBoundAux a x 0 a.size

def insertSorted (a : Array Int) (x : Int) : Array Int :=
  a.insertAt (lowerBound a x) x

def removeBelow (a : Array Int) (th : Int) : Array Int × Nat :=
  let idx := lowerBound a th
  (a.extract idx a.size, idx)

partial def processCmds (h : IO.FS.Stream) : Nat → Int → Array Int → Int → Nat → IO (Array Int × Int × Nat)
| 0, minW, arr, offset, left =>
    pure (arr, offset, left)
| Nat.succ m, minW, arr, offset, left => do
    let line := (← h.getLine).trim
    let parts := line.split (· = ' ') |>.filter (fun s => s ≠ "")
    let cmd := parts[0]!.get! 0
    let k := parts[1]!.toNat!
    match cmd with
    | 'I' =>
        let w : Int := Int.ofNat k
        let arr := if w < minW then arr else insertSorted arr (w - offset)
        processCmds h m minW arr offset left
    | 'A' =>
        processCmds h m minW arr (offset + Int.ofNat k) left
    | 'S' =>
        let offset := offset - Int.ofNat k
        let th := minW - offset
        let (arr, removed) := removeBelow arr th
        processCmds h m minW arr offset (left + removed)
    | 'F' =>
        if k ≤ arr.size then
          let idx := arr.size - k
          let ans := arr[idx]! + offset
          IO.println ans
        else
          IO.println (-1)
        processCmds h m minW arr offset left
    | _ =>
        processCmds h m minW arr offset left

partial def processCases (h : IO.FS.Stream) : Nat → IO Unit
| 0 => pure ()
| Nat.succ t => do
    let line := (← h.getLine).trim
    let vals := line.split (· = ' ') |>.filter (fun s => s ≠ "")
    let m := vals[0]!.toNat!
    let minW : Int := Int.ofNat (vals[1]!.toNat!)
    let (_, _, left) ← processCmds h m minW #[] 0 0
    IO.println left
    processCases h t

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  processCases h t
