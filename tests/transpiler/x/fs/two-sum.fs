// Mochi 0.10.31 - generated 2025-07-19 14:23:19 UTC
open System

let rec twoSum nums target =
    let n = Seq.length nums
    for i in 0 .. (n - 1) do
for j in i + 1 .. (n - 1) do
if ((nums.[i]) + (nums.[j])) = target then
[i; j]
    [-1; -1]
let result = twoSum [2; 7; 11; 15] 9
printfn "%s" (string (result.[0]))
printfn "%s" (string (result.[1]))
