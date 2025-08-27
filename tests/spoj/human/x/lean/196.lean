/- Solution for SPOJ MUSKET - Musketeers
https://www.spoj.com/problems/MUSKET/
-/
import Std
open Std

/-- Read all whitespace-separated tokens from stdin. --/
def readTokens : IO (Array String) := do
  let s ← IO.readStdin
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  return parts.filter (· ≠ "") |>.toArray

/-- Compute winners for a single test case. `beaters[j]` holds a bitset of players
    that defeat player `j`. --/
def compute (n : Nat) (beaters : Array Nat) : String := Id.run do
  let size := n * 2
  let mut dp : Array (Array Nat) := Array.mkArray size (Array.mkArray size 0)
  for i in [0:size] do
    let row := dp.get! i
    dp := dp.set! i (row.set! i (1 <<< (i % n)))
  for len in [2:n+1] do
    for l in [0:size - len + 1] do
      let r := l + len - 1
      let mut res : Nat := 0
      for m in [l:r] do
        let left := (dp.get! l).get! m
        if left ≠ 0 then
          let right := (dp.get! (m+1)).get! r
          if right ≠ 0 then
            -- players from left beating someone in right
            let mut mask : Nat := 0
            for j in [0:n] do
              if (right &&& (1 <<< j)) ≠ 0 then
                mask := mask ||| (beaters.get! j)
            res := res ||| (left &&& mask)
            -- players from right beating someone in left
            let mut mask2 : Nat := 0
            for i2 in [0:n] do
              if (left &&& (1 <<< i2)) ≠ 0 then
                mask2 := mask2 ||| (beaters.get! i2)
            res := res ||| (right &&& mask2)
      let row := dp.get! l
      dp := dp.set! l (row.set! r res)
  let mut final : Nat := 0
  for start in [0:n] do
    final := final ||| (dp.get! start).get! (start + n - 1)
  let mut winners : List String := []
  let mut cnt := 0
  for i in [0:n] do
    if (final &&& (1 <<< i)) ≠ 0 then
      cnt := cnt + 1
      winners := toString (i+1) :: winners
  let lines := toString cnt :: winners.reverse
  return String.intercalate "\n" lines

/-- Parse and solve all test cases. --/
partial def process (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then
    acc.reverse
  else
    let n := toks.get! idx |>.toNat!
    let mut idx := idx + 1
    let mut beaters : Array Nat := Array.mkArray n 0
    for i in [0:n] do
      let s := toks.get! idx; idx := idx + 1
      let chars := s.data.toArray
      for j in [0:n] do
        if chars.get! j = '1' then
          beaters := beaters.set! j ((beaters.get! j) ||| (1 <<< i))
    let out := compute n beaters
    process toks idx (t-1) (out :: acc)

/-- Main program. --/
def main : IO Unit := do
  let toks ← readTokens
  let t := toks.get! 0 |>.toNat!
  let outs := process toks 1 t []
  for o in outs do
    IO.println o
