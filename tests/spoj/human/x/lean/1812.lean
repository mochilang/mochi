/- Solution for SPOJ LCS2 - Longest Common Substring II
https://www.spoj.com/problems/LCS2/
-/
import Std
open Std

/-- base for rolling hash --/
def base : UInt64 := 911382323

/-- precompute powers of the base up to n --/
def powArray (n : Nat) : Array UInt64 := Id.run do
  let mut arr : Array UInt64 := #[1]
  for _ in [0:n] do
    arr := arr.push (arr[arr.size - 1]! * base)
  return arr

/-- prefix hashes for a string --/
def prefixHash (s : String) : Array UInt64 := Id.run do
  let mut pref : Array UInt64 := #[0]
  for c in s.data do
    let prev := pref[pref.size - 1]!
    let code := UInt64.ofNat c.toNat
    pref := pref.push (prev * base + code)
  return pref

/-- set of hashes of substrings of length `len` using precomputed prefixes --/
def substrHashes (pref : Array UInt64) (len : Nat) (pow : Array UInt64) : Std.HashSet UInt64 :=
  Id.run do
    let mut set : Std.HashSet UInt64 := {}
    let limit := pref.size - len
    for i in [0:limit] do
      let h := pref[i + len]! - pref[i]! * pow[len]!
      set := set.insert h
    return set

/-- check if there exists a common substring of length `len` among all strings --/
def hasCommon (arr : Array (Array UInt64)) (len : Nat) (pow : Array UInt64) : Bool :=
  Id.run do
    if len = 0 then
      return true
    let mut cand := substrHashes arr[0]! len pow
    for idx in [1:arr.size] do
      let set2 := substrHashes arr[idx]! len pow
      let mut newSet : Std.HashSet UInt64 := {}
      for h in cand do
        if set2.contains h then
          newSet := newSet.insert h
      cand := newSet
      if cand.isEmpty then
        return false
    return !cand.isEmpty

/-- main program --/
def main : IO Unit := do
  let s ← (← IO.getStdin).readToEnd
  let lines := (s.trim.splitOn "\n").filter (fun t => t.length > 0)
  if lines.isEmpty then
    IO.println "0"
  else
    let arrPref := (lines.map prefixHash).toArray
    let minLen := lines.foldl (fun m t => Nat.min m t.length) lines.head!.length
    let pow := powArray minLen
    let mut lo := 0
    let mut hi := minLen
    while lo < hi do
      let mid := (lo + hi + 1) / 2
      if hasCommon arrPref mid pow then
        lo := mid
      else
        hi := mid - 1
    IO.println (toString lo)
