/- Solution for SPOJ NCKLCE - Another Necklace Problem
https://www.spoj.com/problems/NCKLCE/
-/
import Std
open Std

structure Node where
  lc  : Nat
  rc  : Nat
  cnt : Nat
  lazy : Nat

instance : Inhabited Node := ⟨⟨0,0,0,0⟩⟩

private def combine (a b : Node) : Node :=
  if a.cnt = 0 then b
  else if b.cnt = 0 then a
  else { lc := a.lc,
         rc := b.rc,
         cnt := a.cnt + b.cnt - (if a.rc = b.lc then 1 else 0),
         lazy := 0 }

private def nextPow2 (n : Nat) : Nat :=
  let rec loop (k : Nat) :=
    if k < n then loop (k*2) else k
  loop 1

private partial def buildTree (arr : Array Nat) : IO (Nat × IO.Ref (Array Node)) := do
  let n := arr.size
  let size := nextPow2 n
  let mut tree := Array.mkArray (2*size) default
  for i in [0:n] do
    let c := arr.get! i
    tree := tree.set! (size + i) {lc:=c, rc:=c, cnt:=1, lazy:=0}
  let mut i := size - 1
  while i > 0 do
    let left := tree.get! (i*2)
    let right := tree.get! (i*2+1)
    tree := tree.set! i (combine left right)
    i := i - 1
  let ref ← IO.mkRef tree
  return (size, ref)

private partial def push (tr : IO.Ref (Array Node)) (idx : Nat) : IO Unit := do
  let tree ← tr.get
  let node := tree.get! idx
  if node.lazy = 0 then
    pure ()
  else
    let color := node.lazy
    let left := idx*2
    let right := idx*2+1
    let t1 := tree.set! left {lc:=color, rc:=color, cnt:=1, lazy:=color}
    let t2 := t1.set! right {lc:=color, rc:=color, cnt:=1, lazy:=color}
    let t3 := t2.set! idx {node with lazy:=0}
    tr.set t3

private partial def segUpdate (tr : IO.Ref (Array Node)) (idx l r ql qr color : Nat) : IO Unit := do
  if qr < l || r < ql then return
  if ql ≤ l && r ≤ qr then
    tr.modify fun t => t.set! idx {lc:=color, rc:=color, cnt:=1, lazy:=color}
  else
    push tr idx
    let mid := (l+r)/2
    segUpdate tr (idx*2) l mid ql qr color
    segUpdate tr (idx*2+1) (mid+1) r ql qr color
    let tree ← tr.get
    let left := tree.get! (idx*2)
    let right := tree.get! (idx*2+1)
    tr.modify fun t => t.set! idx (combine left right)

private partial def segQuery (tr : IO.Ref (Array Node)) (idx l r ql qr : Nat) : IO Node := do
  if qr < l || r < ql then
    return default
  if ql ≤ l && r ≤ qr then
    let tree ← tr.get
    return tree.get! idx
  push tr idx
  let mid := (l+r)/2
  let left ← segQuery tr (idx*2) l mid ql qr
  let right ← segQuery tr (idx*2+1) (mid+1) r ql qr
  return combine left right

private def mapPos (pos n offset : Nat) (rev : Bool) : Nat :=
  let p := pos - 1
  if !rev then
    let s := offset + p
    if s ≥ n then s - n else s
  else
    if offset ≥ p then offset - p else n + offset - p

private partial def queryAsc (tr : IO.Ref (Array Node)) (size n l r : Nat) : IO Node := do
  if l ≤ r then
    segQuery tr 1 0 (size-1) l r
  else
    let a ← segQuery tr 1 0 (size-1) l (n-1)
    let b ← segQuery tr 1 0 (size-1) 0 r
    return combine a b

private partial def queryDesc (tr : IO.Ref (Array Node)) (size n l r : Nat) : IO Node := do
  if l ≥ r then
    let res ← queryAsc tr size n r l
    return {lc := res.rc, rc := res.lc, cnt := res.cnt, lazy := 0}
  else
    let a ← queryAsc tr size n 0 l
    let b ← queryAsc tr size n r (n-1)
    let a' := {lc := a.rc, rc := a.lc, cnt := a.cnt, lazy := 0}
    let b' := {lc := b.rc, rc := b.lc, cnt := b.cnt, lazy := 0}
    return combine a' b'

private def updateAsc (tr : IO.Ref (Array Node)) (size n l r color : Nat) : IO Unit :=
  segUpdate tr 1 0 (size-1) l r color

private def updateDesc (tr : IO.Ref (Array Node)) (size n l r color : Nat) : IO Unit := do
  if l ≥ r then
    updateAsc tr size n r l color
  else
    updateAsc tr size n 0 l color
    updateAsc tr size n r (n-1) color

private partial def queryRange (tr : IO.Ref (Array Node)) (size n offset : Nat)
    (rev : Bool) (i j : Nat) : IO Node := do
  let a := mapPos i n offset rev
  let b := mapPos j n offset rev
  if !rev then
    queryAsc tr size n a b
  else
    queryDesc tr size n a b

private partial def updateRange (tr : IO.Ref (Array Node)) (size n offset : Nat)
    (rev : Bool) (i j color : Nat) : IO Unit := do
  let a := mapPos i n offset rev
  let b := mapPos j n offset rev
  if !rev then
    updateAsc tr size n a b color
  else
    updateDesc tr size n a b color

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
              |>.filter (fun s => s ≠ "")
              |> Array.ofList
  if toks.size = 0 then return ()
  let mut idx : Nat := 0
  let n := toks[idx]!.toNat!; idx := idx + 1
  let _c := toks[idx]!.toNat!; idx := idx + 1
  let mut arr : Array Nat := Array.mkEmpty n
  for _ in [0:n] do
    arr := arr.push (toks[idx]!.toNat!); idx := idx + 1
  let q := toks[idx]!.toNat!; idx := idx + 1
  let (size, tr) ← buildTree arr
  let mut offset : Nat := 0
  let mut rev : Bool := false
  let mut outs : Array Nat := #[]
  for _ in [0:q] do
    let cmd := toks[idx]!; idx := idx + 1
    match cmd with
    | "R" =>
      let k := toks[idx]!.toNat!; idx := idx + 1
      if rev then
        let s := offset + k; offset := if s ≥ n then s - n else s
      else
        offset := if offset ≥ k then offset - k else n + offset - k
    | "F" =>
      rev := !rev
    | "S" =>
      let i := toks[idx]!.toNat!; let j := toks[idx+1]!.toNat!; idx := idx + 2
      let a := mapPos i n offset rev
      let b := mapPos j n offset rev
      if a ≠ b then
        let na ← segQuery tr 1 0 (size-1) a a
        let nb ← segQuery tr 1 0 (size-1) b b
        segUpdate tr 1 0 (size-1) a a nb.lc
        segUpdate tr 1 0 (size-1) b b na.lc
    | "P" =>
      let i := toks[idx]!.toNat!; let j := toks[idx+1]!.toNat!; let x := toks[idx+2]!.toNat!; idx := idx + 3
      updateRange tr size n offset rev i j x
    | "C" =>
      let res ← queryRange tr size n offset rev 1 n
      let ans := res.cnt - (if res.lc = res.rc then 1 else 0)
      outs := outs.push ans
    | "CS" =>
      let i := toks[idx]!.toNat!; let j := toks[idx+1]!.toNat!; idx := idx + 2
      let res ← queryRange tr size n offset rev i j
      outs := outs.push res.cnt
    | _ => pure ()
  for a in outs do
    IO.println a
