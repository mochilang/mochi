/- Solution for SPOJ HOTLINE - Hotline
https://www.spoj.com/problems/HOTLINE/
-/

import Std
open Std

set_option linter.deprecated false

structure Fact where
  subject : String
  predicate : String
  object : String
  positive : Bool

/-- key used for identifying verb-object pair -/
def actKey (p o : String) : String := p ++ "|" ++ o

/-- conjugate verb for given subject -/
def conjVerb (sub verb : String) : String :=
  if sub == "I" || sub == "you" then verb else verb ++ "s"

/-- negative helper word for subject -/
def negWord (sub : String) : String :=
  if sub == "I" || sub == "you" then "don't" else "doesn't"

/-- transform subject for answer (I <-> you) -/
def ansSubject (sub : String) : String :=
  if sub == "I" then "you" else if sub == "you" then "I" else sub

/-- check contradiction when adding a fact -/
def causesContradiction (facts : Array Fact) (f : Fact) : Bool :=
  facts.any fun g =>
    g.predicate == f.predicate && g.object == f.object &&
    (g.subject == f.subject || g.subject == "*" || f.subject == "*") &&
    g.positive != f.positive

/-- parse statement line into Fact -/
def parseStatement (line : String) : Fact :=
  let core := line.dropRight 1
  let parts := core.splitOn " "
  match parts with
  | subj :: "don't" :: pred :: rest =>
      { subject := subj, predicate := pred, object := String.intercalate " " rest, positive := false }
  | subj :: "doesn't" :: pred :: rest =>
      { subject := subj, predicate := pred, object := String.intercalate " " rest, positive := false }
  | subj :: predWord :: rest =>
      let predicate := if subj == "I" || subj == "you" then predWord else predWord.dropRight 1
      let obj := String.intercalate " " rest
      if subj == "everybody" then
        { subject := "*", predicate := predicate, object := obj, positive := true }
      else if subj == "nobody" then
        { subject := "*", predicate := predicate, object := obj, positive := false }
      else
        { subject := subj, predicate := predicate, object := obj, positive := true }
  | _ => { subject := "", predicate := "", object := "", positive := true }

/-- join subjects for who question -/
partial def joinSubjects : List String → String
  | [] => ""
  | [a] => a
  | [a,b] => a ++ " and " ++ b
  | a :: rest => a ++ ", " ++ joinSubjects rest

/-- answer for do/does question -/
def answerDo (sub pred obj : String) (facts : Array Fact) : String :=
  let pos := facts.any fun f => f.predicate == pred && f.object == obj && f.positive && (f.subject == sub || f.subject == "*")
  let neg := facts.any fun f => f.predicate == pred && f.object == obj && !f.positive && (f.subject == sub || f.subject == "*")
  let subA := ansSubject sub
  let objPart := if obj == "" then "" else " " ++ obj
  if pos then
    let verb := conjVerb subA pred
    s!"yes, {subA} {verb}{objPart}."
  else if neg then
    let nw := negWord subA
    s!"no, {subA} {nw} {pred}{objPart}."
  else
    "maybe."

/-- answer for who question -/
def answerWho (pred obj : String) (facts : Array Fact) : String :=
  let (hasEvery, hasNobody, subs) :=
    facts.foldl
      (fun (acc : Bool × Bool × Array String) f =>
        let (he, hn, s) := acc
        if f.predicate == pred && f.object == obj then
          if f.subject == "*" then
            if f.positive then (true, hn, s) else (he, true, s)
          else if f.positive && !s.contains f.subject then
            (he, hn, s.push f.subject)
          else
            acc
        else acc)
      (false, false, #[])
  if hasNobody then
    s!"nobody {conjVerb "nobody" pred}{if obj=="" then "" else " " ++ obj}."
  else if hasEvery then
    s!"everybody {conjVerb "everybody" pred}{if obj=="" then "" else " " ++ obj}."
  else if subs.size > 0 then
    let subjStr := joinSubjects subs.toList
    let verb := if subs.size == 1 then conjVerb subs[0]! pred else pred
    s!"{subjStr} {verb}{if obj=="" then "" else " " ++ obj}."
  else
    "I don't know."

/-- answer for what do question -/
def answerWhat (sub : String) (facts : Array Fact) : String :=
  let (_, acts) :=
    facts.foldl
      (fun (acc : Std.HashSet String × Array (Bool × String × String)) f =>
        let (seen, arr) := acc
        if f.subject == sub || f.subject == "*" then
          let k := actKey f.predicate f.object
          if seen.contains k then (seen, arr)
          else (seen.insert k, arr.push (f.positive, f.predicate, f.object))
        else acc)
      (Std.HashSet.empty, #[])
  if acts.size == 0 then
    "I don't know."
  else
    let subA := ansSubject sub
    let phrases := acts.toList.map (fun (pos, pr, ob) =>
      let base := if pos then conjVerb subA pr else negWord subA ++ " " ++ pr
      if ob == "" then base else base ++ " " ++ ob)
    let rec joinActs : List String → String
      | [] => ""
      | [a] => a
      | [a, b] => a ++ ", and " ++ b
      | a :: rest => a ++ ", " ++ joinActs rest
    subA ++ " " ++ joinActs phrases ++ "."

/-- answer dispatcher -/
def answerLine (line : String) (facts : Array Fact) : String :=
  if line.startsWith "do " || line.startsWith "does " then
    let core := line.dropRight 1
    match core.splitOn " " with
    | _ :: sub :: pred :: rest =>
        let obj := String.intercalate " " rest
        answerDo sub pred obj facts
    | _ => ""
  else if line.startsWith "who " then
    let core := line.dropRight 1
    match core.splitOn " " with
    | _ :: predWord :: rest =>
        let pred := predWord.dropRight 1
        let obj := String.intercalate " " rest
        answerWho pred obj facts
    | _ => ""
  else -- what does ...
    match (line.dropRight 1).splitOn " " with
    | _ :: _ :: sub :: _ => answerWhat sub facts
    | _ => ""

partial def processDialogue (h : IO.FS.Stream) (num : Nat) : IO Unit := do
  IO.println s!"Dialogue #{num}:"
  let rec loop (facts : Array Fact) (contr : Bool) : IO Unit := do
    let line ← h.getLine
    let line := line.trim
    if line.endsWith "!" then
      IO.println line
      IO.println ""
    else if line.endsWith "?" then
      IO.println line
      let ans := if contr then "I am abroad." else answerLine line facts
      IO.println ans
      IO.println ""
      loop facts contr
    else
      let f := parseStatement line
      let contr' := contr || causesContradiction facts f
      loop (facts.push f) contr'
  loop #[] false

def main : IO Unit := do
  let stdin ← IO.getStdin
  let tStr ← stdin.getLine
  let t := tStr.trim.toNat!
  for i in [1:t+1] do
    processDialogue stdin i
