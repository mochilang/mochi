// Mochi 0.10.31 - generated 2025-07-19 10:38:09 UTC
open System

let print (x: obj) =
    match x with
    | :? bool as b -> printfn "%d" (if b then 1 else 0)
    | :? float as f -> printfn "%.1f" f
    | :? string as s -> printfn "%s" s
    | :? System.Collections.IEnumerable as e ->
        e |> Seq.cast<obj> |> Seq.map string |> String.concat " " |> printfn "%s"
    | _ -> printfn "%O" x

let boom =
    print "boom"
    true
print (((1 < 2) && (2 < 3)) && (3 < 4))
print (((1 < 2) && (2 > 3)) && (boom))
print ((((1 < 2) && (2 < 3)) && (3 > 4)) && (boom))
