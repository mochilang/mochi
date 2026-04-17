import Std

partial def reverseDigits (n : Nat) (rev : Nat) : Nat :=
  if n == 0 then rev
  else reverseDigits (n / 10) (rev * 10 + n % 10)

def isPalindrome (x : Int) : Bool :=
  if x < 0 then false
  else 
    let n := x.toNat
    x == (reverseDigits n 0)

def parseInts (s : String) : Array Int :=
  (s.split (fun c => c == ' ' || c == '\n' || c == '\t' || c == '\r'))
  |>.toList |>.map (fun s => s.toString) |>.filter (fun s => s != "") |>.map String.toInt! |>.toArray

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let vals := parseInts data
  if vals.size > 0 then
    let t := vals[0]!.toNat
    let lines := ((List.range t).map fun i => if isPalindrome vals[i + 1]! then "true" else "false")
    IO.println (String.intercalate "\n" lines)
