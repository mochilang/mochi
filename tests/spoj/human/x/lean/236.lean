/- Solution for SPOJ ROMAN - Converting number formats
https://www.spoj.com/problems/ROMAN/
-/

import Std
open Std

-- Parse a number given as space separated English digits
def parseNumber (s : String) : Nat :=
  let words := s.split (· = ' ')
  let f (acc : Nat) (w : String) :=
    let d :=
      match w.trim with
      | "ZERO" | "OH" => 0
      | "ONE" => 1
      | "TWO" => 2
      | "THREE" => 3
      | "FOUR" => 4
      | "FIVE" => 5
      | "SIX" => 6
      | "SEVEN" => 7
      | "EIGHT" => 8
      | _ => 9 -- NINE
    acc * 10 + d
  words.foldl f 0

-- repeat a character n times
def repeatChar (c : Char) (n : Nat) : String :=
  String.mk (List.replicate n c)

def trimRightSpaces (s : String) : String :=
  String.mk ((s.data.reverse.dropWhile (· = ' ')).reverse)

-- basic roman numerals up to 3999
def romanStd (n : Nat) : String :=
  let thousands := #["", "M", "MM", "MMM"]
  let hundreds  := #["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"]
  let tens      := #["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"]
  let ones      := #["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
  thousands.get! (n / 1000) ++
    hundreds.get! ((n / 100) % 10) ++
    tens.get! ((n / 10) % 10) ++
    ones.get! (n % 10)

-- roman numerals up to 4999 with overline info
def romanUpTo4999 (n : Nat) : String × List Bool :=
  if n >= 4000 then
    let pref := romanStd (n / 1000)
    let suff := romanStd (n % 1000)
    let str := pref ++ suff
    let over := List.replicate pref.length true ++ List.replicate suff.length false
    (str, over)
  else
    let s := romanStd n
    (s, List.replicate s.length false)

-- convert number to extended Roman with top underscore line
def toRoman (n : Nat) : String × String :=
  let g2 := n / 1000000
  let r1 := n % 1000000
  let g1 := r1 / 1000
  let g0 := r1 % 1000
  let mut top : String := ""
  let mut bot : String := ""
  -- millions part (lowercase)
  if g2 > 0 then
    let (s2, over2) := romanUpTo4999 g2
    let s2l := s2.map (fun c => c.toLower)
    bot := bot ++ s2l
    top := top ++ String.mk (over2.map (fun b => if b then '_' else ' '))
  -- thousands part
  let residue := n % 10000
  if g1 > 0 then
    if residue < 4000 then
      let mpart := g1 % 4
      let high := g1 - mpart
      if high > 0 then
        let s1 := romanStd high
        bot := bot ++ s1
        top := top ++ repeatChar '_' s1.length
      if mpart > 0 then
        bot := bot ++ repeatChar 'M' mpart
        top := top ++ repeatChar ' ' mpart
    else
      let s1 := romanStd g1
      bot := bot ++ s1
      top := top ++ repeatChar '_' s1.length
  -- units part
  if g0 > 0 || (g2 == 0 && g1 == 0) then
    let s0 := romanStd g0
    bot := bot ++ s0
    top := top ++ repeatChar ' ' s0.length
  (trimRightSpaces top, bot)

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit :=
  if t = 0 then
    pure ()
  else do
    let line ← h.getLine
    let n := parseNumber line.trim
    let (top, bot) := toRoman n
    IO.println top
    IO.println bot
    loop h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let tLine ← h.getLine
  let t := parseNumber tLine.trim
  loop h t
