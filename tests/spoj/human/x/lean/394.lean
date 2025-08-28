/- Solution for SPOJ ACODE - Alphacode
https://www.spoj.com/problems/ACODE/
-/

import Std
open Std

private def countDecodings (s : String) : Nat :=
  Id.run do
    let arr := s.toList.toArray
    let n := arr.size
    let mut dp := Array.replicate (n+1) (0 : Nat)
    dp := dp.set! n 1
    for i in [0:n] do
      let j := n - 1 - i
      let c := arr[j]!
      if c = '0' then
        dp := dp.set! j 0
      else
        let mut v := dp[j+1]!
        if j + 1 < n then
          let c2 := arr[j+1]!
          let num := (c.toNat - '0'.toNat) * 10 + (c2.toNat - '0'.toNat)
          if num ≤ 26 then
            v := v + dp[j+2]!
        dp := dp.set! j v
    return dp[0]!

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let s := line.trim
  if s == "0" then
    pure ()
  else
    IO.println (countDecodings s)
    loop h

def main : IO Unit := do
  loop (← IO.getStdin)
