/- Solution for SPOJ HMRO - Help the Military Recruitment Office!
https://www.spoj.com/problems/HMRO/
-/

import Std
open Std

partial def findFinal
  (next : Std.HashMap String String)
  (cacheRef : IO.Ref (Std.HashMap String String))
  (code : String) : IO String := do
  let cache ← cacheRef.get
  match cache.find? code with
  | some v => pure v
  | none =>
    match next.find? code with
    | none =>
      cacheRef.set (cache.insert code code)
      pure code
    | some nxt =>
      let res ← findFinal next cacheRef nxt
      let cache' ← cacheRef.get
      cacheRef.set (cache'.insert code res)
      pure res

partial def processCases (h : IO.FS.Stream) (t idx : Nat) : IO Unit := do
  if idx == t then
    pure ()
  else
    let p1 := (← h.getLine).trim.toNat!
    let mut recruits : Std.HashMap String String := {}
    for _ in [0:p1] do
      let line := (← h.getLine).trim
      let parts := line.splitOn " "
      let pesel := parts.get! 0
      let code := parts.get! 1
      recruits := recruits.insert pesel code
    let z := (← h.getLine).trim.toNat!
    let mut next : Std.HashMap String String := {}
    for _ in [0:z] do
      let line := (← h.getLine).trim
      let parts := line.splitOn " "
      let old := parts.get! 0
      let new := parts.get! 1
      next := next.insert old new
    let q := (← h.getLine).trim.toNat!
    let cacheRef ← IO.mkRef ({} : Std.HashMap String String)
    for _ in [0:q] do
      let pesel := (← h.getLine).trim
      let some code := recruits.find? pesel | pure ()
      let final ← findFinal next cacheRef code
      IO.println s!"{pesel} {final}"
    if idx < t - 1 then
      IO.println ""
    try
      discard (← h.getLine)
    catch _ => pure ()
    processCases h t (idx + 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  processCases h t 0
