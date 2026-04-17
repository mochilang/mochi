import Std

def reverseDigits : Int → Int → Int
  | 0, rev => rev
  | x, rev => reverseDigits (x / 10) (rev * 10 + x % 10)

def isPalindrome (x : Int) : Bool :=
  if x < 0 then false else x == reverseDigits x 0

def parseInts (s : String) : Array Int :=
  ((s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')).filter (fun tok => tok ≠ "")).toArray.map String.toInt!

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let vals := parseInts data
  if vals.size > 0 then
    let t := Int.toNat vals[0]!
    let lines := ((List.range t).map fun i => if isPalindrome vals[i + 1]! then "true" else "false")
    IO.println (String.intercalate "\n" lines)
