/- Solution for SPOJ ACMAKER - ACM (ACronymMaker)
https://www.spoj.com/problems/ACMAKER/
-/
import Std
open Std

/-- count how many ways each substring of abbreviation appears in the word as a subsequence -/
private def wordCounts (abbr : Array Char) (word : String) : Array (Array Nat) :=
  let k := abbr.size
  Id.run do
    let mut cnt : Array (Array Nat) := Array.mkArray (k+1) (Array.mkArray (k+1) 0)
    for s in [0:k] do
      let maxLen := k - s
      let mut dp : Array Nat := Array.mkArray (maxLen+1) 0
      dp := dp.set! 0 1
      for c in word.data do
        let mut t := maxLen
        while t > 0 do
          if abbr.get! (s + t - 1) == c then
            dp := dp.set! t (dp.get! t + dp.get! (t - 1))
          t := t - 1
      for t in [1:maxLen+1] do
        let e := s + t
        cnt := cnt.set! s ((cnt.get! s).set! e (dp.get! t))
    return cnt

/-- solve a single abbreviation against its significant words -/
private def solveCase (abbr : String) (words : Array String) : Nat :=
  let abArr : Array Char := (abbr.data.map Char.toLower).toArray
  let k := abArr.size
  let mut ways : Array Nat := Array.mkArray (k+1) 0
  ways := ways.set! 0 1
  for w in words do
    let cnt := wordCounts abArr w
    let mut nw : Array Nat := Array.mkArray (k+1) 0
    for s in [0:k] do
      let v := ways.get! s
      if v > 0 then
        for e in [s+1:k+1] do
          let add := (cnt.get! s).get! e
          if add > 0 then
            nw := nw.set! e (nw.get! e + v * add)
    ways := nw
  ways.get! k

partial def processCases (h : IO.FS.Stream) (insign : Std.HashSet String) : IO Unit := do
  let line := (← h.getLine).trim
  if line = "LAST CASE" then
    pure ()
  else
    let arr := (line.splitOn " ").toArray
    let abbr := arr[0]!
    let mut words : Array String := Array.mkEmpty (arr.size - 1)
    for i in [1:arr.size] do
      let w := arr[i]!
      if !(insign.contains w) then
        words := words.push w
    let cnt := solveCase abbr words
    if cnt == 0 then
      IO.println s!"{abbr} is not a valid abbreviation"
    else
      IO.println s!"{abbr} can be formed in {cnt} ways"
    processCases h insign

partial def process (h : IO.FS.Stream) : IO Unit := do
  let n := (← h.getLine).trim.toNat!
  if n == 0 then
    pure ()
  else
    let mut insign : Std.HashSet String := {}
    for _ in [0:n] do
      let w := (← h.getLine).trim
      insign := insign.insert w
    processCases h insign
    process h

def main : IO Unit := do
  let h ← IO.getStdin
  process h
