// Mochi 0.10.31 - generated 2025-07-19 07:54:16 UTC
open System

let print (x: obj) =
    match x with
    | :? bool as b -> printfn "%d" (if b then 1 else 0)
    | :? float as f -> printfn "%.1f" f
    | :? string as s -> printfn "%s" s
    | :? System.Collections.IEnumerable as e ->
        e |> Seq.cast<obj> |> Seq.map string |> String.concat " " |> printfn "%s"
    | _ -> printfn "%O" x

print ([1; 2] union [2; 3])
print ([1; 2; 3] except [2])
print ([1; 2; 3] intersect [2; 4])
print (Seq.length ([1; 2] union [2; 3]))
