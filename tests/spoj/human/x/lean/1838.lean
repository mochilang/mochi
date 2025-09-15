/- Solution for SPOJ TICKET - Ticket to Ride
https://www.spoj.com/problems/TICKET/
-/

import Std
open Std

private def INF : Nat := 1000000000000000

private def steiner (n : Nat) (dist : Array (Array Nat)) (terms : Array Nat) : Nat :=
  Id.run do
    let k := terms.size
    let full := Nat.shiftLeft 1 k
    let mut dp : Array (Array Nat) := Array.replicate full (Array.replicate n INF)
    -- initialize single terminals
    let mut i := 0
    while i < k do
      let t := terms[i]!
      let row := dp[Nat.shiftLeft 1 i]!
      dp := dp.set! (Nat.shiftLeft 1 i) (row.set! t 0)
      i := i + 1
    -- DP over subsets
    let mut mask := 1
    while mask < full do
      let mut row := dp[mask]!
      let mut sub := Nat.land (mask - 1) mask
      while sub > 0 do
        let other := mask - sub
        let rowSub := dp[sub]!
        let rowOther := dp[other]!
        let mut v := 0
        while v < n do
          let cost := rowSub[v]! + rowOther[v]!
          if cost < row[v]! then
            row := row.set! v cost
          v := v + 1
        sub := Nat.land (sub - 1) mask
      -- relax via distances
      let mut v := 0
      while v < n do
        let mut u := 0
        while u < n do
          let cost := row[v]! + dist[v]![u]!
          if cost < row[u]! then
            row := row.set! u cost
          u := u + 1
        v := v + 1
      dp := dp.set! mask row
      mask := mask + 1
    -- best cost for each mask
    let mut best : Array Nat := Array.replicate full 0
    let mut mask2 := 0
    while mask2 < full do
      let row := dp[mask2]!
      let mut b := INF
      let mut v := 0
      while v < n do
        let x := row[v]!
        if x < b then b := x
        v := v + 1
      best := best.set! mask2 b
      mask2 := mask2 + 1
    -- DP over pair groups
    let pairMasks : Array Nat := #[
      Nat.shiftLeft 1 0 + Nat.shiftLeft 1 1,
      Nat.shiftLeft 1 2 + Nat.shiftLeft 1 3,
      Nat.shiftLeft 1 4 + Nat.shiftLeft 1 5,
      Nat.shiftLeft 1 6 + Nat.shiftLeft 1 7
    ]
    let fullPairs := Nat.shiftLeft 1 4
    let mut dp2 : Array Nat := Array.replicate fullPairs INF
    dp2 := dp2.set! 0 0
    let mut pmask := 1
    while pmask < fullPairs do
      let mut sub := pmask
      let mut ans := INF
      while sub > 0 do
        let mut termMask := 0
        let mut j := 0
        while j < 4 do
          if Nat.land sub (Nat.shiftLeft 1 j) ≠ 0 then
            termMask := termMask + pairMasks[j]!
          j := j + 1
        let cost := best[termMask]! + dp2[pmask - sub]!
        if cost < ans then ans := cost
        sub := Nat.land (sub - 1) pmask
      dp2 := dp2.set! pmask ans
      pmask := pmask + 1
    return dp2[fullPairs - 1]!

partial def solve (toks : Array String) (idx : Nat) : IO Unit := do
  if idx < toks.size then
    let n := toks[idx]!.toNat!
    let m := toks[idx+1]!.toNat!
    let mut idx := idx + 2
    if n == 0 && m == 0 then
      return
    -- read city names
    let mut idMap : Std.HashMap String Nat := {}
    let mut i := 0
    while i < n do
      idMap := idMap.insert toks[idx]! i
      idx := idx + 1
      i := i + 1
    -- adjacency matrix
    let mut dist : Array (Array Nat) := Array.replicate n (Array.replicate n INF)
    let mut d := 0
    while d < n do
      let row := dist[d]!
      dist := dist.set! d (row.set! d 0)
      d := d + 1
    -- edges
    let mut e := 0
    while e < m do
      let a := toks[idx]!
      let b := toks[idx+1]!
      let c := toks[idx+2]!.toNat!
      idx := idx + 3
      let u := match Std.HashMap.get? idMap a with | some v => v | none => 0
      let v := match Std.HashMap.get? idMap b with | some v => v | none => 0
      let rowU := dist[u]!
      if c < rowU[v]! then
        dist := dist.set! u (rowU.set! v c)
        let rowV := dist[v]!
        dist := dist.set! v (rowV.set! u c)
      e := e + 1
    -- assignments
    let mut terms : Array Nat := Array.mkEmpty 8
    let mut t := 0
    while t < 4 do
      let s1 := toks[idx]!
      let s2 := toks[idx+1]!
      idx := idx + 2
      let u := match Std.HashMap.get? idMap s1 with | some v => v | none => 0
      let v := match Std.HashMap.get? idMap s2 with | some v => v | none => 0
      terms := terms.push u
      terms := terms.push v
      t := t + 1
    -- Floyd-Warshall
      let mut k := 0
      while k < n do
        let mut ii := 0
        while ii < n do
          let dik := dist[ii]![k]!
          let mut j := 0
          while j < n do
            let new := dik + dist[k]![j]!
            let row := dist[ii]!
            if new < row[j]! then
              dist := dist.set! ii (row.set! j new)
            j := j + 1
          ii := ii + 1
        k := k + 1
    let ans := steiner n dist terms
    IO.println ans
    solve toks idx
  else
    return

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
              |>.filter (· ≠ "")
              |> List.toArray
  if toks.isEmpty then
    return
  solve toks 0
