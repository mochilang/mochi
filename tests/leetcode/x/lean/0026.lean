import Std

def removeDuplicates (nums : List Int) : List Int :=
  match nums with
  | [] => []
  | x :: xs => 
    let rec loop (prev : Int) (rem : List Int) (acc : List Int) : List Int :=
      match rem with
      | [] => acc.reverse
      | y :: ys =>
        if y == prev then
          loop prev ys acc
        else
          loop y ys (y :: acc)
    loop x xs [x]

def parseTokens (s : String) : List String :=
  (s.split (fun c => c == ' ' || c == '\n' || c == '\t' || c == '\r'))
  |>.toList |>.map (fun s => s.toString) |>.filter (fun s => s != "")

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let tokens := parseTokens data
  match tokens with
  | [] => pure ()
  | tStr :: rest =>
      let t := tStr.toNat!
      let rec solve (tc : Nat) (cur : List String) : IO Unit :=
        if tc == 0 then pure ()
        else
          match cur with
          | nStr :: rem =>
              let n := nStr.toNat!
              let (numsStr, restTokens) := (rem.take n, rem.drop n)
              let nums := numsStr.map (fun s => s.toInt!)
              let ans := removeDuplicates nums
              IO.println (String.intercalate " " (ans.map toString))
              solve (tc - 1) restTokens
          | [] => pure ()
      solve t rest
