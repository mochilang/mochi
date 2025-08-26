/- Solution for SPOJ IKEYB - I-Keyboard
https://www.spoj.com/problems/IKEYB/
-/
import Std
open Std

structure State where
  cost : Nat
  parts : List Nat

def inf : Nat := 1000000000000000

partial def cmpRev : List Nat -> List Nat -> Ordering
  | [], [] => Ordering.eq
  | x::xs, y::ys =>
      match cmpRev xs ys with
      | Ordering.eq => compare x y
      | ord => ord
  | _, _ => Ordering.eq

def better (a b : State) : Bool :=
  if a.cost < b.cost then True
  else if a.cost > b.cost then False
  else
    match cmpRev a.parts b.parts with
    | Ordering.gt => True
    | _ => False

def buildPrefix (freqs : Array Nat) : Array Nat × Array Nat :=
  let L := freqs.size
  let init1 := Array.mkArray (L+1) 0
  let init2 := Array.mkArray (L+1) 0
  (List.range L).foldl
    (fun (p : Array Nat × Array Nat) i =>
      let (s1, s2) := p
      let f := freqs.get! i
      let s1 := s1.set! (i+1) (s1.get! i + f)
      let s2 := s2.set! (i+1) (s2.get! i + f * (i+1))
      (s1, s2)
    ) (init1, init2)

def segCost (s1 s2 : Array Nat) (i j : Nat) : Nat :=
  if h : i ≤ j then
    (s2.get! (j+1) - s2.get! i) - i * (s1.get! (j+1) - s1.get! i)
  else 0

partial def buildRow (L k : Nat) (s1 s2 : Array Nat) (prev : Array State) : Array State :=
  let initRow := Array.mkArray (L+1) {cost := inf, parts := []}
  (List.range (L+1)).foldl
    (fun row l =>
      let best := (List.range (l+1)).foldl
        (fun best t =>
          let prevState := prev.get! t
          if prevState.cost == inf then best
          else
            let segLen := l - t
            if segLen = 0 && k = 1 then
              best
            else
              let c :=
                if segLen = 0 then prevState.cost
                else prevState.cost + segCost s1 s2 t (l-1)
              let cand : State := {cost := c, parts := prevState.parts ++ [segLen]}
              if better cand best then cand else best
        ) {cost := inf, parts := []}
      row.set! l best
    ) initRow

def solveCase (keysStr lettersStr : String) (freqList : List Nat) : List String :=
  let keys := keysStr.toList
  let letters := lettersStr.toList
  let K := keys.length
  let L := letters.length
  let freqs := freqList.toArray
  let (s1, s2) := buildPrefix freqs
  let row0 := (Array.mkArray (L+1) {cost := inf, parts := []}).set! 0 {cost := 0, parts := []}
  let finalRow :=
    (List.range K).foldl
      (fun prev kIdx => buildRow L (kIdx+1) s1 s2 prev)
      row0
  let sizes := (finalRow.get! L).parts
  let rec buildLines (ks : List Char) (ls : List Char) (ss : List Nat) : List String :=
    match ks, ss with
    | [], [] => []
    | k::ks, s::ss =>
        let (take, rest) := ls.splitAt s
        let line := s!"{k}: {String.mk take}"
        line :: buildLines ks rest ss
    | k::ks, [] =>
        (s!"{k}: ") :: buildLines ks ls []
    | [], _ => []
  buildLines keys letters sizes

partial def readFreqs (h : IO.FS.Stream) (n : Nat) (acc : List Nat := []) : IO (List Nat) := do
  if n = 0 then
    pure acc.reverse
  else
    let line ← h.getLine
    readFreqs h (n-1) (line.trim.toNat! :: acc)

partial def loop (h : IO.FS.Stream) (t i : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let [kStr, lStr] := line.trim.splitOn " " | [] => ["0","0"]
    let k := kStr.toNat!
    let l := lStr.toNat!
    let keysLine ← h.getLine
    let lettersLine ← h.getLine
    let freqs ← readFreqs h l
    IO.println s!"Keypad #{i}:"
    for line in solveCase keysLine.trim lettersLine.trim freqs do
      IO.println line
    IO.println ""
    loop h (t-1) (i+1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t 1
