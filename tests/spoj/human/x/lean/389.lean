/- Solution for SPOJ HOSPITAL - Use of Hospital Facilities
https://www.spoj.com/problems/HOSPITAL/
-/

import Std
open Std

structure Patient where
  name : String
  surg : Nat
  rec  : Nat

def padLeft (s : String) (len : Nat) : String :=
  let l := s.length
  if l >= len then s else String.mk (List.replicate (len - l) ' ') ++ s

def padRight (s : String) (len : Nat) : String :=
  let l := s.length
  if l >= len then s else s ++ String.mk (List.replicate (len - l) ' ')

def fmtTime (t : Nat) : String :=
  let h := t / 60
  let m := t % 60
  let mStr := if m < 10 then s!"0{m}" else toString m
  s!"{h}:{mStr}"

def format2 (x : Float) : String :=
  let y := x + 0.005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let frac :=
    if h : parts.length > 1 then
      let f := parts.get! 1
      (f ++ "00").take 2
    else
      "00"
  intPart ++ "." ++ frac

structure Event where
  idx : Nat
  room : Nat
  arr : Nat
  rec : Nat

structure Result where
  name : String
  room : Nat
  sBeg sEnd : Nat
  bed : Nat
  rBeg rEnd : Nat

/-- simulate scheduling of patients -/
def simulate
    (rooms beds start trans prepO prepR : Nat)
    (patients : Array Patient) :
    Array Result × Array Nat × Array Nat × Nat :=
  Id.run do
    let r := rooms
    let b := beds
    let n := patients.size
    let mut orFree := Array.mkArray r start
    let mut orUse := Array.mkArray r 0
    let mut roomAssign := Array.mkArray n 0
    let mut sBegArr := Array.mkArray n 0
    let mut sEndArr := Array.mkArray n 0
    for i in [0:n] do
      let mut best := 0
      for j in [1:r] do
        if orFree[j]! < orFree[best]! then
          best := j
      let begin := orFree[best]!
      let p := patients[i]!
      let finish := begin + p.surg
      orFree := orFree.set! best (finish + prepO)
      orUse := orUse.set! best (orUse[best]! + p.surg)
      roomAssign := roomAssign.set! i (best + 1)
      sBegArr := sBegArr.set! i begin
      sEndArr := sEndArr.set! i finish
    let mut events : Array Event := Array.mkEmpty n
    for i in [0:n] do
      let p := patients[i]!
      events := events.push { idx := i,
                              room := roomAssign[i]!,
                              arr := sEndArr[i]! + trans,
                              rec := p.rec }
    let events := events.qsort (fun a b =>
      if a.arr == b.arr then a.room < b.room else a.arr < b.arr)
    let mut bedFree := Array.mkArray b start
    let mut bedUse := Array.mkArray b 0
    let mut bedAssign := Array.mkArray n 0
    let mut rBegArr := Array.mkArray n 0
    let mut rEndArr := Array.mkArray n 0
    for e in events do
      let arr := e.arr
      let rec firstAvail (j : Nat) : Nat :=
        if j = b then 0 else
          if bedFree[j]! <= arr then j else firstAvail (j+1)
      let chosen := firstAvail 0
      let rBegin := arr
      let rEnd := rBegin + e.rec
      bedFree := bedFree.set! chosen (rEnd + prepR)
      bedUse := bedUse.set! chosen (bedUse[chosen]! + e.rec)
      bedAssign := bedAssign.set! e.idx (chosen + 1)
      rBegArr := rBegArr.set! e.idx rBegin
      rEndArr := rEndArr.set! e.idx rEnd
    let mut maxEnd := start
    for i in [0:n] do
      if rEndArr[i]! > maxEnd then
        maxEnd := rEndArr[i]!
    let mut results : Array Result := Array.mkEmpty n
    for i in [0:n] do
      let p := patients[i]!
      results := results.push
        { name := p.name, room := roomAssign[i]!,
          sBeg := sBegArr[i]!, sEnd := sEndArr[i]!,
          bed := bedAssign[i]!, rBeg := rBegArr[i]!, rEnd := rEndArr[i]! }
    return (results, orUse, bedUse, maxEnd)

partial def readPatients
    (toks : Array String) (i cnt : Nat) (acc : Array Patient) :
    Array Patient × Nat :=
  if cnt = 0 then (acc, i)
  else
    let name := toks[i]!
    let surg := toks[i+1]!.toNat!
    let rec := toks[i+2]!.toNat!
    readPatients toks (i+3) (cnt-1) (acc.push {name, surg, rec})

partial def solve (toks : Array String) (i : Nat) (acc : List String) : List String :=
  if h : i < toks.size then
    let rooms := toks[i]!.toNat!
    let beds := toks[i+1]!.toNat!
    let startHour := toks[i+2]!.toNat!
    let trans := toks[i+3]!.toNat!
    let prepO := toks[i+4]!.toNat!
    let prepR := toks[i+5]!.toNat!
    let n := toks[i+6]!.toNat!
    let (patients, j) := readPatients toks (i+7) n #[]
    let start := startHour * 60
    let (res, orUse, bedUse, endTime) := simulate rooms beds start trans prepO prepR patients
    let total := endTime - start
    let mut lines : Array String := #[]
    lines := lines.push " Patient          Operating Room          Recovery Room"
    lines := lines.push " #  Name     Room#  Begin   End      Bed#  Begin    End"
    lines := lines.push " ------------------------------------------------------"
    for idx in [0:res.size] do
      let p := res[idx]!
      let numStr := padLeft (toString (idx+1)) 2
      let nameStr := padRight p.name 8
      let roomStr := padLeft (toString p.room) 2
      let bedStr := padLeft (toString p.bed) 2
      lines := lines.push
        (numStr ++ "  " ++ nameStr ++ "  " ++ roomStr ++ "    " ++
         fmtTime p.sBeg ++ "    " ++ fmtTime p.sEnd ++ "     " ++
         bedStr ++ "    " ++ fmtTime p.rBeg ++ "    " ++ fmtTime p.rEnd)
    lines := lines.push ""
    lines := lines.push "Facility Utilization"
    lines := lines.push "Type  # Minutes  % Used"
    lines := lines.push "-------------------------"
    for r in [0:rooms] do
      let m := orUse[r]!
      let pct := (Float.ofNat m * 100.0) / Float.ofNat total
      lines := lines.push
        (padRight "Room" 4 ++ " " ++ padLeft (toString (r+1)) 2 ++
         "     " ++ padLeft (toString m) 3 ++ "   " ++ format2 pct)
    for b in [0:beds] do
      let m := bedUse[b]!
      let pct := (Float.ofNat m * 100.0) / Float.ofNat total
      lines := lines.push
        (padRight "Bed" 4 ++ " " ++ padLeft (toString (b+1)) 2 ++
         "     " ++ padLeft (toString m) 3 ++ "   " ++ format2 pct)
    lines := lines.push ""
    lines := lines.push ""
    let runStr := String.intercalate "\n" lines.toList
    solve toks j (runStr :: acc)
  else acc.reverse

def main : IO Unit := do
  let data ← IO.getStdin.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                 |>.filter (fun s => s ≠ "")
                 |>.toArray
  let outs := solve toks 0 []
  IO.print (String.intercalate "" outs)
