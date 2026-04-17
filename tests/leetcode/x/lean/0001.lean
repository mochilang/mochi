import Std

def parseInts (s : String) : Array Int :=
  (s.split (fun c => c == ' ' || c == '\n' || c == '\t' || c == '\r'))
  |>.toList |>.map (fun s => s.toString) |>.filter (fun s => s != "") |>.map String.toInt! |>.toArray

def twoSum (nums : Array Int) (target : Int) : Nat × Nat :=
  let rec outer (i : Nat) : Option (Nat × Nat) :=
    if i < nums.size then
      let rec inner (j : Nat) : Option (Nat × Nat) :=
        if j < nums.size then
          if nums[i]! + nums[j]! = target then
            some (i, j)
          else
            inner (j + 1)
        else
          none
      match inner (i + 1) with
      | some ans => some ans
      | none => outer (i + 1)
    else
      none
  match outer 0 with
  | some ans => ans
  | none => (0, 0)

partial def solveCases (vals : Array Int) (cases pos : Nat) (acc : List String) : List String :=
  if cases = 0 then
    acc.reverse
  else
    let n := (vals[pos]!).toNat
    let target := vals[pos + 1]!
    let nums := (vals.toList.drop (pos + 2)).take n |>.toArray
    let ans := twoSum nums target
    let line := toString ans.1 ++ " " ++ toString ans.2
    solveCases vals (cases - 1) (pos + 2 + n) (line :: acc)

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let vals := parseInts data
  if vals.size > 0 then
    let t := (vals[0]!).toNat
    let lines := solveCases vals t 1 []
    IO.println (String.intercalate "\n" lines)
