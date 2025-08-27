/- Solution for SPOJ MORSE - Decoding Morse Sequences
https://www.spoj.com/problems/MORSE/
-/

import Std
open Std

-- Morse representation for each uppercase letter
def morseOf : Char -> String
  | 'A' => ".-"
  | 'B' => "-..."
  | 'C' => "-.-."
  | 'D' => "-.."
  | 'E' => "."
  | 'F' => "..-."
  | 'G' => "--."
  | 'H' => "...."
  | 'I' => ".."
  | 'J' => ".---"
  | 'K' => "-.-"
  | 'L' => ".-.."
  | 'M' => "--"
  | 'N' => "-."
  | 'O' => "---"
  | 'P' => ".--."
  | 'Q' => "--.-"
  | 'R' => ".-."
  | 'S' => "..."
  | 'T' => "-"
  | 'U' => "..-"
  | 'V' => "...-"
  | 'W' => ".--"
  | 'X' => "-..-"
  | 'Y' => "-.--"
  | 'Z' => "--.."
  | _   => ""

/-- Encode a word into its Morse code string. -/
def encode (w : String) : String :=
  w.foldl (fun acc c => acc ++ morseOf c) ""

/-- Build a map from Morse code length to the set of word codes of that length. -/
def buildDict (ws : List String) : HashMap Nat (HashSet String) :=
  ws.foldl (fun m w =>
    let code := encode w
    let len := code.length
    let set := m.findD len ({} : HashSet String)
    m.insert len (set.insert code)
  ) ({} : HashMap Nat (HashSet String))

/-- Count number of phrases matching the given Morse sequence. -/
def countPhrases (s : String) (dict : HashMap Nat (HashSet String)) : Nat :=
  let n := s.length
  let lens := dict.toList.map (fun kv => kv.fst)
  let rec loop (i : Nat) (dp : Array Nat) : Array Nat :=
    if h : i = 0 then
      dp
    else
      let i1 := i - 1
      let total := lens.foldl (fun acc L =>
        if h2 : i1 + L <= n then
          let codes := dict.findD L ({} : HashSet String)
          let sub := s.extract i1 (i1 + L)
          if codes.contains sub then
            acc + dp.get! (i1 + L)
          else acc
        else acc
      ) 0
      loop i1 (dp.set! i1 total)
  let dp0 := loop n ((Array.mkArray (n+1) (0 : Nat)).set! n 1)
  dp0.get! 0

partial def readWords (h : IO.FS.Stream) : Nat -> IO (List String)
  | 0 => pure []
  | n+1 => do
      let w := (← h.getLine).trim
      let rest ← readWords h n
      pure (w :: rest)

partial def processDataset (h : IO.FS.Stream) : IO Nat := do
  let seq := (← h.getLine).trim
  let n := (← h.getLine).trim.toNat!
  let words ← readWords h n
  let dict := buildDict words
  pure <| countPhrases seq dict

partial def loopDatasets (h : IO.FS.Stream) : Nat -> IO Unit
  | 0 => pure ()
  | d+1 => do
      let ans ← processDataset h
      IO.println ans
      loopDatasets h d

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loopDatasets h t
