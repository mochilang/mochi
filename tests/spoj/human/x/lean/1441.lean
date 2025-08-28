/- Solution for SPOJ CLEVER - The Clever Typist
https://www.spoj.com/problems/CLEVER/
-/

import Std
open Std

def swapBits (code : UInt32) (i j : Nat) : UInt32 :=
  if i = j then code else
  let shiftI := 3 * i
  let shiftJ := 3 * j
  let mask : UInt32 := 0x7
  let di := (code >>> shiftI) &&& mask
  let dj := (code >>> shiftJ) &&& mask
  let cleared := code &&& (~~~ ((mask <<< shiftI) ||| (mask <<< shiftJ)))
  cleared ||| (di <<< shiftJ) ||| (dj <<< shiftI)

structure State where
  code   : UInt32
  cursor : UInt8
  mask   : UInt8
  deriving BEq, Hashable

abbrev INF : Nat := 1000000

-- precompute distances for permutations and visited masks
structure Precomp where
  perms : Array (Array UInt8)
  dist  : Array (Array Nat)

-- decode permutation code to array
def decodePerm (code : UInt32) : Array UInt8 :=
  Id.run do
    let mut arr := Array.mkArray 6 (0 : UInt8)
    for i in [0:6] do
      let v := ((code >>> (3 * i)) &&& 0x7).toUInt8
      arr := arr.set! i v
    return arr

-- precompute BFS over states
def precompute : Precomp := Id.run do
  let startCode : UInt32 :=
    let mut c : UInt32 := 0
    for i in [0:6] do
      c := c ||| (UInt32.ofNat i <<< (3 * i))
    c
  let start : State := {code := startCode, cursor := 0, mask := 1}
  let mut q : Array State := #[start]
  let mut distMap : Std.HashMap State Nat := Std.HashMap.empty
  distMap := distMap.insert start 0
  let mut idx : Nat := 0
  while idx < q.size do
    let st := q.get! idx
    let d := distMap.find! st
    -- move left
    if st.cursor > 0 then
      let nc := st.cursor - 1
      let nm := st.mask ||| ((1:UInt8) <<< nc.toNat)
      let ns : State := {code := st.code, cursor := nc, mask := nm}
      if distMap.contains ns = false then
        distMap := distMap.insert ns (d+1)
        q := q.push ns
    -- move right
    if st.cursor < 5 then
      let nc := st.cursor + 1
      let nm := st.mask ||| ((1:UInt8) <<< nc.toNat)
      let ns : State := {code := st.code, cursor := nc, mask := nm}
      if distMap.contains ns = false then
        distMap := distMap.insert ns (d+1)
        q := q.push ns
    -- swap with position 0
    if st.cursor ≠ 0 then
      let nc := st.cursor
      let newCode := swapBits st.code (st.cursor.toNat) 0
      let ns : State := {code := newCode, cursor := nc, mask := st.mask}
      if distMap.contains ns = false then
        distMap := distMap.insert ns (d+1)
        q := q.push ns
    -- swap with position 5
    if st.cursor ≠ 5 then
      let nc := st.cursor
      let newCode := swapBits st.code (st.cursor.toNat) 5
      let ns : State := {code := newCode, cursor := nc, mask := st.mask}
      if distMap.contains ns = false then
        distMap := distMap.insert ns (d+1)
        q := q.push ns
    idx := idx + 1

  -- collect unique permutation codes
  let mut permSet : Std.HashSet UInt32 := {}
  for (st, _) in distMap.toArray do
    permSet := permSet.insert st.code
  let permCodes := permSet.toArray
  let permArr := permCodes.map decodePerm
  -- map perm code to index
  let mut permIdx : Std.HashMap UInt32 Nat := Std.HashMap.empty
  for i in [0:permCodes.size] do
    permIdx := permIdx.insert (permCodes.get! i) i
  -- distances array
  let mut distArr : Array (Array Nat) := Array.mkArray permCodes.size (Array.mkArray 64 INF)
  for (st, d) in distMap.toArray do
    let idxp := permIdx.find! st.code
    let arr := distArr.get! idxp
    let m := st.mask.toNat
    let old := arr.get! m
    if d < old then
      distArr := distArr.set! idxp (arr.set! m d)
  return {perms := permArr, dist := distArr}

-- absolute difference of digits
def absDiff (a b : UInt8) : Nat :=
  if a ≥ b then a.toNat - b.toNat else b.toNat - a.toNat

-- solve one query using precomputed data
def solve (pc : Precomp) (start target : Array UInt8) : Nat :=
  let perms := pc.perms
  let dist := pc.dist
  let mut best := INF
  for i in [0:perms.size] do
    let p := perms.get! i
    let mut mask : Nat := 0
    let mut diff : Nat := 0
    for j in [0:6] do
      let s := start.get! (p.get! j).toNat
      let t := target.get! j
      if s ≠ t then
        mask := mask ||| (1 <<< j)
      diff := diff + absDiff s t
    let arr := dist.get! i
    for m in [0:64] do
      if (m &&& mask) = mask then
        let d := arr.get! m
        if d < INF then
          let cand := d + diff
          if cand < best then best := cand
  best

-- parse six-digit string to array of digits
def parseDigits (s : String) : Array UInt8 :=
  Id.run do
    let mut arr := Array.mkArray 6 (0:UInt8)
    for i in [0:6] do
      let c := s.get! i
      arr := arr.set! i (UInt8.ofNat (c.toNat - '0'.toNat))
    return arr

def main : IO Unit := do
  let pc := precompute
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (· ≠ "")
  let t := toks.get! 0 |>.toNat!
  let mut idx := 1
  for _ in [0:t] do
    let a := parseDigits (toks.get! idx)
    let b := parseDigits (toks.get! (idx+1))
    let ans := solve pc a b
    IO.println ans
    idx := idx + 2
