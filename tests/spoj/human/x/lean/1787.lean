/- Solution for SPOJ ENCONDIN - Run Length Encoding
https://www.spoj.com/problems/ENCONDIN/
-/

import Std
open Std

/-- encode a single line using the specified run-length scheme -/
def encodeLine (s : String) : String :=
  Id.run do
    let chars := s.data.toArray
    let n := chars.size
    let mut i : Nat := 0
    let mut lit : String := ""
    let mut res : String := ""
    while i < n do
      let c := chars[i]!
      if i + 1 < n && chars[i+1]! = c then
        -- flush literal segment
        if lit ≠ "" then
          let mut tmp := ""
          for ch in lit.data do
            if ch = '1' then
              tmp := tmp.push '1'; tmp := tmp.push '1'
            else
              tmp := tmp.push ch
          res := res ++ "1" ++ tmp ++ "1"
          lit := ""
        -- count run of repeated characters
        let mut cnt := 0
        while i < n && chars[i]! = c do
          i := i + 1
          cnt := cnt + 1
        -- output in chunks of at most 9
        while cnt > 0 do
          let d := Nat.min cnt 9
          res := res.push (Char.ofNat (48 + d))
          res := res.push c
          cnt := cnt - d
      else
        lit := lit.push c
        i := i + 1
    -- flush remaining literal segment
    if lit ≠ "" then
      let mut tmp := ""
      for ch in lit.data do
        if ch = '1' then
          tmp := tmp.push '1'; tmp := tmp.push '1'
        else
          tmp := tmp.push ch
      res := res ++ "1" ++ tmp ++ "1"
    return res

partial def loop (h : IO.FS.Stream) : IO Unit := do
  match (← h.getLine?) with
  | none => pure ()
  | some line =>
      let s := line.takeWhile (· ≠ '\n')
      IO.println (encodeLine s)
      loop h

def main : IO Unit := do
  loop (← IO.getStdin)
