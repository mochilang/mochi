// Mochi 0.10.31 - generated 2025-07-19 12:11:16 UTC
open System

let print (x: obj) =
    match x with
    | :? bool as b -> printfn "%d" (if b then 1 else 0)
    | :? float as f -> printfn "%.1f" f
    | :? string as s -> printfn "%s" s
    | :? System.Collections.IEnumerable as e ->
        e |> Seq.cast<obj> |> Seq.map string |> String.concat " " |> printfn "%s"
    | _ -> printfn "%O" x

let twoSum nums target =
    let n = Seq.length nums
    for i in 0 .. (n - 1) do
for j in i + 1 .. (n - 1) do
if ((nums.[i]) + (nums.[j])) = target then
[i; j]
    [-1; -1]
let result = twoSum [2; 7; 11; 15] 9
print (result.[0])
print (result.[1])
