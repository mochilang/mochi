import Std
open Std

/-- Solution for SPOJ problem 1869 (Macro Processor).
    https://www.spoj.com/problems/1869
-/

abbrev Env := List (String × String)

partial def takeUntil (target : Char) : List Char → (List Char × List Char)
| [] => ([], [])
| c :: cs =>
  if c = target then
    ([], cs)
  else
    let (p, r) := takeUntil target cs
    (c :: p, r)

partial def getArg (args : List String) (i : Nat) : String :=
  match args[i]? with
  | some v => v
  | none => ""

mutual
  partial def evalStream : List Char → StateM Env String
  | [] => return ""
  | '[' :: cs => do
    let (out, rest) ← evalMacroCall cs
    let tail ← evalStream rest
    return out ++ tail
  | '<' :: cs => do
    let (lit, rest) := takeUntil '>' cs
    let tail ← evalStream rest
    return String.mk lit ++ tail
  | c :: cs => do
    let tail ← evalStream cs
    return String.singleton c ++ tail

  partial def evalArg : List Char → StateM Env (String × List Char)
  | [] => return ("", [])
  | ']' :: cs => return ("", ']' :: cs)
  | ',' :: cs => return ("", ',' :: cs)
  | '[' :: cs => do
    let (out, rest) ← evalMacroCall cs
    let (tail, rest2) ← evalArg rest
    return (out ++ tail, rest2)
  | '<' :: cs => do
    let (lit, rest) := takeUntil '>' cs
    let (tail, rest2) ← evalArg rest
    return (String.mk lit ++ tail, rest2)
  | c :: cs => do
    let (tail, rest) ← evalArg cs
    return (String.singleton c ++ tail, rest)

  partial def evalMacroCall (cs : List Char) : StateM Env (String × List Char) := do
    let (name, rest) ← evalArg cs
    let rec parseArgs (acc : List String) : List Char → StateM Env (List String × List Char)
    | ']' :: rs => return (acc, rs)
    | ',' :: rs => do
        let (a, rs2) ← evalArg rs
        parseArgs (acc ++ [a]) rs2
    | rs => return (acc, rs)
    let (args, rest') ← parseArgs [] rest
    if name = "def" then
      match args with
      | n :: d :: _ =>
        modify fun e => (n, d) :: e
        return ("", rest')
      | _ => return ("", rest')
    else
      let env ← get
      let body := (env.find? (fun p => p.fst = name)).map (·.snd) |>.getD ""
      let rec subst : List Char → String
      | [] => ""
      | '$' :: d :: cs =>
        if d.isDigit then
          let idx := d.toNat - '0'.toNat
          let repl := if idx = 0 then name else getArg args (idx - 1)
          repl ++ subst cs
        else
          String.singleton '$' ++ String.singleton d ++ subst cs
      | c :: cs => String.singleton c ++ subst cs
      let substituted := subst body.data
      let out ← evalStream substituted.data
      return (out, rest')
end

def hyphens : String := String.mk (List.replicate 79 '-')

partial def processInput (input : String) : String :=
  let (out, _) := (evalStream input.data).run []
  out

partial def readLines (h : IO.FS.Stream) (n : Nat) : IO (List String) := do
  if n = 0 then
    return []
  else do
    let l ← h.getLine
    let ls ← readLines h (n - 1)
    return l :: ls

partial def loopCases (h : IO.FS.Stream) (case : Nat) : IO Unit := do
  let line ← h.getLine
  let line := line.trim
  if line = "0" then
    pure ()
  else
    match line.toNat? with
    | none => pure ()
    | some n => do
      let ls ← readLines h n
      let input := String.intercalate "\n" ls ++ "\n"
      let out := processInput input
      IO.println s!"Case {case}"
      IO.println hyphens
      IO.print out
      IO.println hyphens
      IO.println ""
      loopCases h (case + 1)

def main : IO Unit := do
  loopCases (← IO.getStdin) 1
