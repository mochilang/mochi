-- https://www.spoj.com/problems/HITOMISS/
import Std.Data.HashSet
open Std

structure Player where
  deck : List Nat
  count : Nat
  last : Nat
  deriving Inhabited

-- perform step 1, returning updated players and pending cards to receive
private def step1 (players : List Player) : (List Player × List (Option Nat)) :=
  let rec go (processed : List Player) (pending : List (Option Nat)) (carry : Option Nat)
      (rest : List Player) : (List Player × List (Option Nat)) :=
    match rest with
    | [] => (processed.reverse, pending.reverse)
    | p :: ps =>
        let pending := carry :: pending
        match p.deck with
        | [] =>
            -- player has no cards, waits
            let p' := p
            go (p' :: processed) pending none ps
        | card :: tail =>
            let count' := if p.count == 13 then 1 else p.count + 1
            if card == p.count then
              -- match: remove card and pass to next (if any)
              let p' := {p with deck := tail, count := count', last := card}
              let newCarry := if ps.isEmpty then none else some card
              go (p' :: processed) pending newCarry ps
            else
              -- no match: move card to bottom
              let p' := {p with deck := tail ++ [card], count := count'}
              go (p' :: processed) pending none ps
  go [] [] none players

-- step2: deliver pending cards to each player (except first)
private def step2 (players : List Player) (pending : List (Option Nat)) : List Player :=
  let rec zip (pls : List Player) (pend : List (Option Nat)) (idx : Nat) : List Player :=
    match pls, pend with
    | [], [] => []
    | p :: ps, pe :: pes =>
        let deck :=
          if idx == 0 then p.deck
          else match pe with
               | some c => p.deck ++ [c]
               | none => p.deck
        let p' := {p with deck := deck}
        p' :: zip ps pes (idx + 1)
    | _, _ => []
  zip players pending 0

private def encodeState (players : List Player) : String :=
  let parts := players.map (fun p =>
    let deckStr := String.intercalate "," (p.deck.map toString)
    s!"{p.count}:{deckStr}")
  String.intercalate "|" parts

private def allEmpty (players : List Player) : Bool :=
  players.all (fun p => p.deck.isEmpty)

partial def simulate (p : Nat) (initialDeck : List Nat) : Option (List Nat) :=
  let first : Player := {deck := initialDeck, count := 1, last := 0}
  let others := List.replicate (p - 1) {deck := [], count := 1, last := 0}
  let players0 := first :: others
  let rec loop (pls : List Player) (vis : HashSet String) : Option (List Nat) :=
    if allEmpty pls then
      some (pls.map (·.last))
    else
      let key := encodeState pls
      if vis.contains key then none
      else
        let (pls1, pend) := step1 pls
        let pls2 := step2 pls1 pend
        loop pls2 (vis.insert key)
  loop players0 HashSet.empty

-- parse all integers from stdin
def readInts : IO (List Nat) := do
  let data ← IO.FS.readFile "/dev/stdin"
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  pure <| toks.filterMap String.toNat?

def main : IO Unit := do
  let nums ← readInts
  let t := nums.headD 0
  let mut rest := nums.tailD []
  for caseIdx in [1:t+1] do
    match rest with
    | p :: rs =>
        let deck := rs.take 52
        let rs' := rs.drop 52
        let res := simulate p deck
        let out := match res with
          | some lst => s!"Case {caseIdx}: {String.intercalate " " (lst.map toString)}"
          | none => s!"Case {caseIdx}: unwinnable"
        IO.println out
        rest := rs'
    | _ => pure ()
