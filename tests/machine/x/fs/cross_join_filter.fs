open System

type Anon1 = {
    n: int
    l: string
}
let nums: int list = [1; 2; 3]
let letters: string list = ["A"; "B"]
let pairs: obj list = [ for n in nums do 
  for l in letters do if n % 2 = 0 then yield { n = n; l = l } ]
printfn "%s" "--- Even pairs ---"
for p in pairs do
    printfn "%s" (String.concat " " [string p.n; string p.l])
