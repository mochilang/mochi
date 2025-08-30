/- Solution for SPOJ TWENDS - Two Ends
https://www.spoj.com/problems/TWENDS/
-/
import Std
open Std
open IO

-- recursive DP with memoization
partial def dfs (a : Array Int) (memoRef : IO.Ref (Array (Array (Option Int))))
    (l r : Nat) : IO Int := do
  if l > r then
    return 0
  else
    let memo ← memoRef.get
    match memo[l]![r]! with
    | some v => pure v
    | none =>
      let optL ←
        if a[l+1]! ≥ a[r]! then
          let d ← dfs a memoRef (l+2) r
          pure (a[l]! - a[l+1]! + d)
        else
          let d ← dfs a memoRef (l+1) (r-1)
          pure (a[l]! - a[r]! + d)
      let optR ←
        if a[l]! ≥ a[r-1]! then
          let d ← dfs a memoRef (l+1) (r-1)
          pure (a[r]! - a[l]! + d)
        else
          let d ← dfs a memoRef l (r-2)
          pure (a[r]! - a[r-1]! + d)
      let res := max optL optR
      memoRef.modify fun m =>
        let row := m[l]!
        m.set! l (row.set! r (some res))
      return res

-- solve one game
def solveGame (vals : Array Int) : IO Int := do
  let n := vals.size
  let memo ← IO.mkRef (Array.replicate n (Array.replicate n (Option.none : Option Int)))
  dfs vals memo 0 (n-1)

-- read lines and process games
partial def loop (h : IO.FS.Stream) (game : Nat) : IO Unit := do
  let line ← h.getLine
  let tokens := (line.trim.split (· = ' ')).filter (· ≠ "") |>.toArray
  let n := (tokens[0]!).toNat!
  if n = 0 then
    pure ()
  else
    let mut arr : Array Int := Array.mkEmpty n
    for i in [1:tokens.size] do
      arr := arr.push (Int.ofNat (tokens[i]!.toNat!))
    let diff ← solveGame arr
    IO.println s!"In game {game}, the greedy strategy might lose by as many as {diff} points."
    loop h (game + 1)

def main : IO Unit := do
  loop (← IO.getStdin) 1
