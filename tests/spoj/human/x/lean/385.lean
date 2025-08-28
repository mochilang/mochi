/- Solution for SPOJ GAME - Game schedule required
https://www.spoj.com/problems/GAME/
-/

import Std
open Std

private partial def dfs (adj : Array (Array Nat)) (u p : Nat) (parent : Array Nat) : Array Nat :=
  let parent := parent.set! u p
  (adj[u]!).foldl (fun par v => if v = p then par else dfs adj v u par) parent

private def matchRound (n : Nat) (adj : Array (Array Nat)) (alive : Array Bool)
  : (Array (Nat × Nat) × Option Nat) := Id.run do
  let mut deg : Array Nat := Array.replicate n 0
  for i in [0:n] do
    if alive[i]! then
      let mut d := 0
      for v in adj[i]! do
        if alive[v]! then
          d := d + 1
      deg := deg.set! i d
  let mut used : Array Bool := Array.replicate n false
  let mut q : Array Nat := #[]
  for i in [0:n] do
    if alive[i]! ∧ deg[i]! = 1 then
      q := q.push i
  let mut qi : Nat := 0
  let mut pairs : Array (Nat × Nat) := #[]
  while qi < q.size do
    let v := q[qi]!; qi := qi + 1
    if !(used[v]! ∨ !alive[v]! ∨ deg[v]! = 0) then
      let mut u := 0
      let mut found := false
      for nb in adj[v]! do
        if alive[nb]! ∧ !used[nb]! ∧ !found then
          u := nb; found := true
      if found then
        pairs := pairs.push (u, v)
        used := used.set! v true
        used := used.set! u true
        for w in adj[u]! do
          if alive[w]! ∧ !used[w]! then
            let dw := deg[w]!
            let dw1 := dw - 1
            deg := deg.set! w dw1
            if dw1 = 1 then
              q := q.push w
  let mut wildcard : Option Nat := none
  for i in [0:n] do
    if alive[i]! ∧ !used[i]! then
      wildcard := some i; break
  (pairs, wildcard)

private def solve (n : Nat) (names : Array String) (adj : Array (Array Nat)) (root : Nat) : String := Id.run do
  let parent := dfs adj root root (Array.replicate n 0)
  let mut alive : Array Bool := Array.replicate n true
  let mut remaining := n
  let mut round := 1
  let mut out := ""
  while remaining > 1 do
    let (pairs, wildcard) := matchRound n adj alive
    out := out ++ s!"Round #{round}\n"
    for (u, v) in pairs do
      let winner := if parent[v]! = u then u else v
      let loser := if parent[v]! = u then v else u
      out := out ++ s!"{names[winner]!} defeats {names[loser]!}\n"
      alive := alive.set! loser false
      remaining := remaining - 1
    match wildcard with
    | some w => out := out ++ s!"{names[w]!} advances with wildcard\n"
    | none => ()
    round := round + 1
  let mut champ := root
  for i in [0:n] do
    if alive[i]! then champ := i
  out := out ++ s!"Winner: {names[champ]!}"
  out

private def readTokens : IO (Array String) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array String := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p
  return arr

private def indexOf (names : Array String) (s : String) : Nat :=
  (names.findIdx? (fun t => t = s)).getD 0

def main : IO Unit := do
  let tokens ← readTokens
  let mut idx : Nat := 0
  let mut first := true
  while idx < tokens.size do
    let n := tokens[idx]!.toNat!; idx := idx + 1
    if n = 0 then
      break
    let mut names : Array String := Array.mkEmpty n
    for _ in [0:n] do
      let nm := tokens[idx]!; idx := idx + 1
      names := names.push nm
    let mut adj : Array (Array Nat) := Array.replicate n #[]
    for _ in [0:n-1] do
      let a := tokens[idx]!; idx := idx + 1
      let b := tokens[idx]!; idx := idx + 1
      let ia := indexOf names a
      let ib := indexOf names b
      adj := adj.set! ia ((adj[ia]!).push ib)
      adj := adj.set! ib ((adj[ib]!).push ia)
    let root := n - 1
    let res := solve n names adj root
    if first then
      IO.println res
      first := false
    else
      IO.println ""
      IO.println res
  return ()
